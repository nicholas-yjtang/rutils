library(RCurl)
#=============================================================
# @author Nicholas Tang
# the read data function
# @param filename the target file we wise to write to 
# @param url the url of the location of the original file
# No return is returned
#==============================================================

get_data <- function(filename, url) { 
  #if the source file already exists, we can simply ignore downloading it again
  if (!file.exists(filename)) {
    #find the directory name
    directory_name <- dirname(filename)
    #if the directory does not exist, we create it
    if (!file.exists(directory_name)) {
      dir.create(directory_name)
    }
    #download the file since the source file does not exist
    download_file(filename, url)    
  }
}

#=======================================================
# Function to download the file
# @param destination the location of the file to download to
# @param fileUrl the location of the file to download from 
#========================================================
download_file <- function (destination, fileUrl) {  
  #set the directory name and filename first
  directory_name <- dirname(destination)  #actual directory destination
  filename <- basename(destination)  #destination filename without the directory path

  #check if the file already exists before downloading again
  #if the file does not exist, then just download it again
  if (!file.exists(destination)) {
    #using RCurl, we can skip the verification of the ssl
    x <- getURLContent(fileUrl, ssl.verifypeer=FALSE, header=TRUE)


     download_filename = filename

    #finally write the contents from memory/connection into the file
    #close the file at the end
    download_file_relativepath = paste(directory_name, download_filename, sep="/")
    to.write = file(download_file_relativepath, "wb")    
    writeBin(x$body[1:length(x$body)],to.write)
    close(to.write)

    #perform the unzip if the filename downloaded is a zip file
    if (length(grep(".*\\.zip", download_filename)) != 0) {    
      unzipped_files <- unzip(download_file_relativepath, exdir = directory_name)    
    } 
  }  
}

