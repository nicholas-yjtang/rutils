library(RCurl)
#=============================================================
#@author Nicholas Tang
#the read data function
#@filename the target file we wise to read and load. 
#		Note that this will not perform any renaming operations
#@url the url of the location of the original file
#==============================================================

get_data <- function(filename, url) { 
  #if the source file already exists, we can simply ignore downloading it again
  if (!file.exists(filename)) {
    #file the directory name
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
# @destination the location of the file to download to
# @fileUrl the location of the file to download from 
#========================================================
download_file <- function (destination, fileUrl) {  
  #set the directory name and filename first, along with the zip filename
  directory_name <- dirname(destination)  #actual directory destination
  filename <- basename(fileUrl)  #zip file name
  zip_filename <- paste(directory_name,filename,sep="/")
  
  #check if the zip file already exists before downloading again
  #if the file does not exist, then just download it again
  if (!file.exists(zip_filename)) {
    x <- getBinaryURL(fileUrl, ssl.verifypeer=FALSE)
    to.write = file(zip_filename, "wb") 
    writeBin(x,to.write)
    close(to.write)
  }  

  #perform the unzip if the filename downloaded is a zip file
  if (grep(".*\\.zip", filename)) {    
    unzipped_files <- unzip(zip_filename, exdir = directory_name)    
  }  
}
