#' Contact Bungie API
#' 
#' Given endpoint path and paramters, this function send requests to Bungie API
#' and return JSON data.
#' 
#' @param path Characters like
#'   "Destiny2/SearchDestinyPlayer/{membershipType}/{displayName}/", which the
#'   URI parameters are indicated by curly braces.
#' @param params a list of parameters like list(membershipType = 2, displayName
#'   = "semibruin"). The names of the list should match the
#'   parameter names.
#' @query query a list of query
bungie_api <- function(path, params = NULL, query = NULL) {
  # apiKey is my secret. DON'T SHARE IT. My key is saved localled in
  # Secret/bungieAPIkey.rds
  apiKey <- readRDS("Secret/bungieAPIkey.rds")
  # Root path of bungie API
  path0 <- "www.bungie.net/Platform/Destiny2"
  # Glue path with root path
  url0 <-  modify_url(url = "https://", hostname = path0, path = path)
  # Bungie API uses URI parameters by curly braces. These parameters cannot be
  # set by modify_url. I wrote a new function `uir_param` to set the paramters.
  url1 <- uri_param(url0, params) # if params = NUll, just return url0
  # Set optional query parameters
  url2 <- modify_url(url = url1, query = query)
  r <- GET(
    url2,
    add_headers("X-Api-Key" = apiKey)
  )
  # Parse the content of response into R objects (typically a nested
  # list)
  parsed <- jsonlite::fromJSON(content(r, "text"), simplifyVector = FALSE)
  # If the status code is not success, stop and return an error.
  if (status_code(r) != 200) {
    err <- paste("Bungie API request failed", status_code(r), "\n", parsed$message, "\n", parsed$documentation_url)
    stop(
      err,
      call. = FALSE
    )
  }
  # Check if the response is json type.
  if (http_type(r) != "application/json") {
    stop("API did not return json", call. = FALSE)
  }
  # Define a S3 class object. So when we print the return, we can easily see
  # the structure of the data.
  structure(
    list(
      content = parsed,
      path = path,
      response = r
      ),
    class = "bungie_api"
  )
}
# Define print method for bungie_api object.
print.bungie_api <- function(x, ...) {
  cat("<Bungie API ", x$path, ">\n", sep = "")
  str(x$content, list.len = 4)
  invisible(x)
}

#' Obtain all PvP stats of a guardian
#' 
#' Given a list with membership type, membership Id, and character Id, retrieve
#' all time PvP information.
#' 
#' @param mTypeIdChr a list of membership type, membership Id, and character Id,
#'   e.g. list(membershipType = 2, destinyMembershipId = 3142142, characterId =
#'   44315453)
queryPvP <- function(mTypeIdChr) {
  p3 <- "/{membershipType}/Account/{destinyMembershipId}/Character/{characterId}/Stats/"
  r3 <- bungie_api(path = p3, params = mTypeIdChr, query = list(periodType = "AllTime", modes = "5"))
  allTime <- r3$content$Response$allPvP$allTime
  pvp <- tibble(
    destinyMembershipId = mTypeIdChr$destinyMembershipId,
    characterId = mTypeIdChr$characterId,
    statId = map_chr(allTime, "statId"),
    basic = map_dbl(allTime, list("basic", "value")),
    pga = map_dbl(allTime, list("pga", "value"), .default = NA)
    ) %>%
    pivot_wider(names_from = statId, values_from = c("basic", "pga"))
  pvp
}

#' Change URI parameters in URL path
#'
#' `modify_url` in `httr` and `urltools` does support feed URI parameters,
#' which are used in the API I am working on.
#'
#' @param path Characters like
#'   "Destiny2/SearchDestinyPlayer/{membershipType}/{displayName}/", which the
#'   URI parameters are indicated by curly braces.
#' @param params a list of parameters like list("membershipType" = 2,
#'   "displayName" = "semibruin"). The names of the list should match the
#'   parameter names.
#'
#' @examples
#' p <- "Destiny2/SearchDestinyPlayer/{membershipType}/{displayName}/"
#' params <- list("membershipType" = 2, "displayName" = "semibruin")
#' uri_param(p, params)
#' @export
#'
uri_param <- function(path, params) {
  if (is.null(params)) {
    return(path)
  }
  library(stringr)
  for (i in 1 : length(params)) {
    pat <- paste0("\\{", names(params)[i], "\\}")
    if (!str_detect(path, pat)) {
      stop(cat("Parameters", pat, " were not found!\n"), .call = FALSE)
    }
    path <- str_replace(
      path, pat, as.character(params[[i]])
    )
  }
  path
}

#' List all found characters.
#' List all found characters that matches the displayName (e.g. "semibruin") and
#' platform (e.g. 2L). The return tibble list the class, gender, race and light.
#'
#' @param displayName Display name like "semibruin".
#' @param platform Integer platform, e.g. 2L is playstation. "Xbox" = 1,
#'   "PlayStation" = 2, "Steam" = 3, "Blizzard" = 4, "Stadia" = 5.
listChar <- function(displayName, platform) {
  # Step 1: Get membershipId
  p1 <- "/SearchDestinyPlayer/{membershipType}/{displayName}/"
  pm1 <- list("membershipType" = platform, "displayName" = displayName)
  r1 <- bungie_api(path = p1, params = pm1)
  if (is.null(pluck(r1, "content", "Response"))) {
    stop("No players found matching your query.", call. = FALSE)
  }
  mTypeId <- r1$content[["Response"]][[1]][c("membershipType", "membershipId")]
  names(mTypeId)[2] <- "destinyMembershipId"
  # Step 2: Path to Destiny Profile. This is to get the character.
  p2 <- "/{membershipType}/Profile/{destinyMembershipId}/"
  r2 <- bungie_api(path = p2, params = mTypeId, query = list(components = "200"))
  r2data <- r2$content$Response$characters$data
  r2tib <- tibble(
    characterId = map_chr(r2data, "characterId"),
    raceHash = map_dbl(r2data, "raceHash"),
    genderHash = map_dbl(r2data, "genderHash"),
    classHash = map_dbl(r2data, "classHash"),
    light = map_dbl(r2data, "light"),
    # sometimes "emblemBackgroundPath" is missing. Use emblemPath instead in that case
    emblem =  map_if(
      r2data,
      ~ "emblemBackgroundPath" %in% names(.),
      ~ pluck(., "emblemBackgroundPath"),
      .else = ~ pluck(., "emblemPath")
    )
    # emblem = map_chr(r2data, "emblemPath")
  )
  # The url of emblem is "https://www.bungie.net/" plus the path
  # Translate the Hash to human readable characters.
  rgc <- list()
  for (i in 1 : nrow(r2tib)) {
    rgcList <- r2tib %>% select(raceHash, genderHash, classHash) %>% slice(i) %>% as.list
    rgc[[i]] <- guardianLookup(rgcList) %>% as_tibble
  }
  rgc <- bind_rows(rgc)
  r2tib <- r2tib %>% select(-raceHash, -genderHash, -classHash) %>% bind_cols(rgc)
  charVec <- r2tib %>% select(classHash, raceHash, genderHash, light) %>%
    unite(all, sep = " ") %>% unlist
  embList <- paste0("https://www.bungie.net", unlist(r2tib$emblem)) # a vector of path to emblem pictures.
  # Create choicenames and choicevalues using HTML
  emblem <- list()
  choicevalues <- set_names(as.list(seq_along(charVec)), charVec)
  mTypeId <- rep(list(mTypeId), length(embList))
  for (i in 1 : length(embList)) {
    # Add character Id
    mTypeId[[i]]$characterId <-  as.character(r2tib[i, "characterId"])
    choicevalues[i] <- i
    # choicenames[[i]] <- HTML(sprintf('<div class="wrapper"><img class="to_the_left" src="%s"/><p style="font-size:160%;">"%s"</p><div class="clear"></div></div>', embList[[i]], charVec[i]))
    emblem[[i]] <- HTML(
      sprintf(
        '<figure><img src="%s""/><figcaption>%s</figcaption> </figure>',
        embList[[i]],
        charVec[i]
      )
    )
  }
  # We emblem to show the emblem of selected character; choicevalues to select
  # the character, mTypeId to download data from Bungie API.
  list(emblem = emblem, choicevalues = choicevalues, mTypeId = mTypeId)
}

#' Download PvP data for specified character.
#'
#' Currently, I only download the recent rumble games.
#' @param mTypeIdChr a list of membership type, membership Id, and character Id,
#'   e.g. list(membershipType = 2, destinyMembershipId = 3142142, characterId =
#'   44315453)
#' @param nGames an integer <= 25. Download the recent nGames data.
downloadGames <- function(mTypeIdChr, nGames, gamesExisting = NULL) {
  p4 <-
    "/{membershipType}/Account/{destinyMembershipId}/Character/{characterId}/Stats/Activities/"
  r4 <-
    bungie_api(
      path = p4,
      params = mTypeIdChr,
      query = list(periodType = "AllTime", modes = "5")
    )
  # Rumble games ID.
  ##################################
  # Change mode == 48 in the future.
  ##################################
  gameId <-
    r4$content$Response %>%
    as_tibble %>%
    unnest_wider(activities) %>%
    unnest_wider(activityDetails) %>%
    filter(mode == 48) %>% # RUMBLE GAMES
    select(period, instanceId, mode)
  ##############################
  # Compare with existing data #
  ##############################
  # Compare with historical data if exists. Query only the new games.
  if (!is.null(gamesExisting)) {
    # Create a column New = TRUE or FALSE based on the existing data.
    gameId <- gameId %>%
      mutate(New = ifelse(instanceId %in% pull(gamesExisting, instanceId), FALSE, TRUE)) %>%
      filter(New)
  }
  # Stop data collection if there isn't any new game.
  if (nrow(gameId) == 0 & !is.null(gamesExisting)) {
    # Just return the most recent 5 games and quit
    recentGames <- head(gamesExisting %>% pull(instanceId), nGames)
    message("No new games. Just pull out the most recent games from the record.")
    return(
      gamesExisting %>%
        filter(instanceId %in% recentGames) %>%
        rename(CombatRating = basic_combatRating)
    )
  }
  # No rumble game and no existing data.
  if (nrow(gameId) == 0 & is.null(gamesExisting)) {
    message("No new rumble games")
  }
  # Otherwise, move on to collect new games.
  p5 <- "/Stats/PostGameCarnageReport/{activityId}/"
  # g <- pull(gameId, instanceId)[1]
  games <- list()
  for (g in 1 : min(nGames, nrow(gameId))) {
    gId <-  slice(gameId, g) %>% pull(instanceId) # gameId
    cat("Collecting game ", g, "/", min(nGames, nrow(gameId)), " data ... \n")
    r5 <- try(bungie_api(path = p5, params = list(activityId = gId))) # use try to proceed if there was error
    ################################################################################
    # Add error handling script. If the one game cannot be extracted
    # successfully, move on to the next.
    is.error <- function(x) inherits(x, "try-error") # test if the return is try-error
    if (is.error(r5)) {
      # run it again
      cat("Got an error in the first attempt. Rerun \n")
      r5 <- try(bungie_api(path = p5, params = list(activityId = gId)))
    }
    if (is.error(r5)) {
      # Still error, move to the next game.
      cat("Rerun was unsuccessful. Skip this game \n")
      next
    }
    ################################################################################
    r5Temp <- r5$content$Response$entries
    # Save the information about the players to query the PvP stats about these
    # players.
    player <- tibble::tibble(
      instanceId = gId,
      displayName = map_chr(r5Temp, list(
        "player", "destinyUserInfo", "displayName"
      )),
      membershipType = map_int(
        r5Temp,
        list("player", "destinyUserInfo", "membershipType")
      ),
      destinyMembershipId = map_chr(r5Temp, list(
        "player", "destinyUserInfo", "membershipId"
      )),
      characterId = map_chr(r5Temp, "characterId")
    )
    # Use as_tibble_col, rather than as_tibble, when the list is not named.
    pvpStat <- bind_cols(select(player, displayName),
                         # bind displayName with pvp stat
                         map(r5Temp, "values") %>% as_tibble_col) %>%
      unnest_longer(value) %>%
      unnest_wider(value) %>%
      unnest_wider(basic)
    # Below, I ignore "displayValue". This could create some problem.
    # To include it, values_from = c(value, displayValue).
    pvpStat <- pvpStat %>%
      select(-displayValue) %>%
      pivot_wider(names_from = value_id, values_from = value)
    pvpStat <- left_join(player, pvpStat, by = "displayName")
    # Weapon loadout
    # When a player did not kill at all (e.g. quit the game early), weapon list
    # might be empty--list(). set_names will then have error for empty list.
    wLoad <- map_if(
      map(r5Temp, list("extended", "weapons")),
      ~ length(.) > 0,
      ~ set_names(# pluck "referenceId" from each list returned from the above map
        map(., "referenceId"),
        # Add names for the list
        paste0("Wpon_refId", seq_len(length(
          .
        )))),
      .else = ~ list(Wpon_refId1 = NA)
    ) %>%
      as_tibble_col %>%
      unnest_wider(value)
    # Add explanation of weapon loadout for the ease of reading
    wLoadName <- map_chr(wLoad %>% unlist, itemLookup)
    # Convert wLoadName to matrix of the same dimension as wLoad.
    dim(wLoadName) <- dim(wLoad)
    # The number of weapons can exceed 3, because players can change their load during the game.
    # Notice that if the equipped weapon did not kill at all, its information is missing.
    dimnames(wLoadName) <-
      list(NULL, paste0("Weapon", seq_len(ncol(wLoad))))
    wLoadName <- as_tibble(wLoadName)
    wLoad <- bind_cols(select(player, displayName), wLoadName)
    pvpStat <- left_join(pvpStat, wLoad, by = "displayName")
    # Collect the aggreagate PvP stats for each player.
    pvpAllTime <- list()
    for (i in 1:nrow(player)) {
      mTypeIdChr <- player %>%
        select(membershipType, destinyMembershipId, characterId) %>%
        slice(i) %>%
        as.list
      # queryPvP is slow. Can we speed up?
      pvpAllTime[[i]] <- queryPvP(mTypeIdChr)
    }
    # Bind the PvP information of all players
    pvpAllTime <- bind_rows(pvpAllTime)
    # Join individual game statistics and historical PvP data
    pvpStat <-
      pvpStat %>% left_join(pvpAllTime, by = c("destinyMembershipId", "characterId"))
    games[[g]] <- pvpStat
  }
  games <- bind_rows(games)
  #####################################################
  # Step 5: Merge with existing data and save the data.
  #####################################################
  if (!is.null(gamesExisting)) {
    gamesExisting <- bind_rows(gamesExisting, games)
    # download data to dropbox
    downloadData(gamesExisting)
  }
  games %>%  rename(CombatRating = basic_combatRating)
}
