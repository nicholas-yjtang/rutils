#####################################################################
# read_rds_data function will read the rds data into the parent frame
# @param variable_name the name of the variable you would want to set 
# the rds data into
# @param the rds_filelocation that you want to load the rds file from
######################################################################

read_rds_data <- function(variable_name, rds_filelocation) {
  #if the variable does not exist (in any frame)
  if (!exists(variable_name)) {
    #if the file does not exist for us to load
    if (!file.exists(rds_filelocation)) {
      print(paste("error, the file",rds_filelocation, 
                  "does not exist. Please download it and place it under the data folder"))
    }
    else {
      #assign the variable with the reasult from readRDS
      assign(variable_name, readRDS(rds_filelocation), envir=parent.frame())
    }
  }
}