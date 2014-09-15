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
# @destination the location of the file to download to
# @fileUrl the location of the file to download from 
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
    content_disposition = ""
    #in some online downloads, the headers contain the name of the file under
    #the http header "content-disposition" or "Content-disposition"
    for (header in names(x$header)) {    
      if (header == "Content-disposition" || header == "content-disposition") {
        content_disposition <- x$header[header]
      }
    }
    #in the case where content disposition exists
    if (content_disposition != "") {
      #we use regular expression to attempt to find the location and hence the substring
      #for the name of the file
      regular_expr_match <- "filename="
      download_filename <- substr(content_disposition, gregexpr(regular_expr_match, content_disposition)[[1]][[1]]+nchar(regular_expr_match), nchar(content_disposition))    
      end_index <- gregexpr(";", download_filename)[[1]][[1]] - 1
      #some cases, the filename is the last item in the content-disposition
      if (end_index == -2) {
        end_index <- nchar(download_filename)
      }
      #set the filename to the substring
      download_filename <- substr(download_filename, 1, end_index)
      #remove the " if it is present in the filename
      download_filename <- gsub("[\"]", "", download_filename)      
    }
    #the case where there is no content_disposition. we have to rely on the original filename given earlier
    else {
      download_filename = filename
    }
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

