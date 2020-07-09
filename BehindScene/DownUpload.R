################################################################################
# Uncomment if needed. I have authorized.
# token <- drop_auth()
# saveRDS(token, "droptoken.rds")
# Upload droptoken to your server
# ******** WARNING ********
# Losing this file will give anyone 
# complete control of your Dropbox account
# You can then revoke the rdrop2 app from your
# dropbox account and start over.
# ******** WARNING ********
# read it back with readRDS
token <- readRDS("Secret/droptoken.rds")
# Then pass the token to each drop_ function

################################################################################
# Directory where I will save my destiny game data. The path starts from my
# dropbox root: "~/Dropbox"
# outputPath <- "Projects/Destiny2/Data2020June/semibruinGames.rds"
# It has to be directory. It CANNOT be the file path.
outputPath <- "Projects/Destiny2/Data2020June/"

# Modify drop_read_csv to read rds file
drop_read_rds <- function(file, dest = tempdir(), dtoken = get_dropbox_token(), ...) {
  localfile = paste0(dest, "/", basename(file))
  drop_download(file, localfile, overwrite = TRUE, dtoken = dtoken)
  readRDS(localfile)
}

downloadData <- function(data) {
  # Create a unique file name
  filesInfo <- drop_dir(outputPath, dtoken = token)
  filePaths <- filesInfo$path_lower
  fileName <- basename(filePaths)
  # Write the data to a temporary file locally
  filePath <- file.path(tempdir(), fileName)
  # It creates temp file each time. Will this r2drop package create a lot of trash?
  saveRDS(data, filePath)
  # Upload the file to Dropbox
  drop_upload(filePath, path = outputPath)
}

loadData <- function() {
  # Read all the files into a list
  filesInfo <- drop_dir(outputPath, dtoken = token)
  filePaths <- filesInfo$path_lower
  data <- drop_read_rds(filePaths, dtoken = token)
  data
}
