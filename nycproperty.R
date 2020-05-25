# Make work later or write all manualla
#' Function to load libraries
#'
#' @param libnames String vector of library names to load
#' @return Returns TRUE if *all* library installations were successful,
#'         FALSE otherwise. 
loadLibrary <- function(libnames = c()) {
    if(length(libnames) == 0) {
        return(FALSE)
    } else {
        for(libname in libnames) {
            # paste0("Loading library ",libname,"...")
            if(!require(package = libname))
                install.packages(pkgs = c(libname))
            library(package = libname)
            }
        }
    return(TRUE)
    } 



# load all needed libraries using above function
# loadLibrary(libnames = c("tidyverse", "ggplot2", "caret", "readr"))
library(tidyverse)
library(ggplot2)
library(caret)
library(readr)

# load data
#kaggle datasets download -d new-york-city/nyc-property-sales

# link address
url <- "https://raw.github.com/danmayen/nycproperty/data/nyc-property-sales.zip"
# download file from url
tmp_filename <- tempfile()
download.file(url, "data//data.zip")