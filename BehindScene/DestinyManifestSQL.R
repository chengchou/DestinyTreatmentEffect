# Get manifest database if not exists.
#-------------------------------------------------------------------------------
# A function for downloading, unziping and renaming Destiny manifest, a SQL
# database.
getManifest <- function() {
  p_manifest <- "/Manifest/"
  r_manifest <- bungie_api(p_manifest)
  # Path of manifest in English 
  p_manifest <- r_manifest$content$Response$mobileWorldContentPaths$en
  url_manifest <- paste0('http://www.bungie.net', p_manifest)
  r_manifest <- GET(url_manifest, write_disk("BehindScene/man.zip", overwrite = TRUE))
  # Get the original name of unzipped file without unzipping
  oldName <- unzip("BehindScene/man.zip", list = TRUE)$Name
  # Unzip the file now
  unzip("BehindScene/man.zip")
  # Change file name
  file.rename(from = oldName, to = "BehindScene/Manifest_content.sqlite")
}
# Do not run getManifest() if the database file "Manifest_content.sqlite"
# exists.
if (!file.exists("BehindScene/Manifest_content.sqlite")) {
  getManifest()
}
# Destiny API response use unsigned integers. Destiny manifest database use
# signed, so does R.  We need to convert manually the unsigned to signed.
signed32 <- function(x) {
  # as.integer will return NA if x is beyond the range of signed int.
  if (is.na(suppressWarnings(as.integer(x)))) {
    x <- as.integer(x - 2 ^ 32)
  }
  as.integer(x)
}

# Connect the SQL database.
#-------------------------------------------------------------------------------
dman <- dbConnect(drv  = SQLite(), dbname = "BehindScene/Manifest_content.sqlite")
# List the available tables.
# dbListTables(dman)

# VARIOUS LOOKUP FUNCTIONS.
#-------------------------------------------------------------------------------
#' Lookup weapon, armor etc.
#' 
#' The itemid found in game data are integers. This function translates these
#' number into guardian readable characters using Destiny's manifest SQL
#' database.
#' 
#' @param itemid wepon Id, like 232425253
itemLookup <- function(itemid) {
  if (is.na(itemid)) return(NA)
  id1 <- signed32(itemid) # convert unsigned to signed int
  json0 <- tbl(dman, "DestinyInventoryItemDefinition") %>%
    filter(id == id1)
  if (length(json0 %>% pull(json)) == 0) {
    return("Not found")
  } else {
    json1 <- json0 %>%
      pull(json) %>%
      jsonlite::fromJSON(.)
    return(purrr::pluck(json1, "displayProperties", "name"))
  }
}
#' Lookup guardian
#' 
#' Given a list of raceHash, genderHash, classHash, return the human readable
#' information about the guardian's race, gender and class.
#' 
#' @param rgcList a list of raceHash, genderHash, classHash, e.g. list(raceHash
#'   = 12312424, genderHash = 234234325, classHash = 121425525)
guardianLookup <- function(rgcList) {
  # rgcList: a list of raceHash, genderHash, classHash
  # RACE
  raceHash <- signed32(rgcList$raceHash)
  raceJson <- tbl(dman, "DestinyRaceDefinition") %>%
    filter(id == raceHash) %>%
    pull(json) %>%
    jsonlite::fromJSON(.)
  # overwrite classHash by its display name
  raceHash <- pluck(raceJson, "displayProperties", "name")
  # GENDER
  genderHash <- signed32(rgcList$genderHash)
  genderJson <- tbl(dman, "DestinyGenderDefinition") %>%
    filter(id == genderHash) %>%
    pull(json) %>%
    jsonlite::fromJSON(.)
  # overwrite classHash by its display name
  genderHash <- pluck(genderJson, "displayProperties", "name")
  # CLASS
  classHash <- signed32(rgcList$classHash)
  classJson <- tbl(dman, "DestinyClassDefinition") %>%
    filter(id == classHash) %>%
    pull(json) %>%
    jsonlite::fromJSON(.)
  # overwrite classHash by its display name
  classHash <- pluck(classJson, "displayProperties", "name")
  # Return translated list
  set_names(list(raceHash, genderHash, classHash), names(rgcList))
}