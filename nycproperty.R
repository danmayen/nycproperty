################################################################################
# 0.1 Loading of libraries
################################################################################
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

################################################################################
# 0.2 Loading of data - RUN ONCE ONLY
################################################################################

# link address
url <- "https://raw.githubusercontent.com/danmayen/nycproperty/master/data/nyc-property-sales.zip"
# download file from url
tmp_filename <- tempfile()
csv_filename <- "nyc-rolling-sales.csv"
download.file(url, tmp_filename)
# unzip file
unzip(tmp_filename, csv_filename)
# remove temporary zipfile
file.remove(tmp_filename)

# read csv file into data frame
nycproperty_raw <- read_csv(csv_filename)
# remove csv file
file.remove(csv_filename)
# remove temporary variables
rm(url, tmp_filename, csv_filename)

################################################################################
# 0.3 Cleaning of RAW data
################################################################################
#' Auxiliary function - returns data frame with format of a given data frame
#' 
#' @param df The data frame
#' @param firstvalues=5 Number of first values displayed
#' @param sep=" " Separator for first values displayed
#' @return Data frame with column names, their types and the first values
df_format <- function(df, nfirstvalues = 5, sep = " ") {
    n = ncol(df)
    res <- data.frame(
        colname = colnames(df),
        type = sapply(1:n, function(i) {class(data.frame(df)[, i])}),
        firstvalues = 
            sapply(1:n, function(i){
                paste0(head(df[, i], n = nfirstvalues), collapse = sep)    
            })
        )
    return(res)
    }

# raw summary
summary(nycproperty_raw)

# column names with underscores and lowercase
newcolnames <- colnames(nycproperty_raw) %>%
    str_to_lower() %>%
    str_replace_all(" ", "_")
colnames(nycproperty_raw) <- newcolnames
rm(newcolnames)

# table summary - need in chunks
nycp_format <- df_format(df = nycproperty_raw, nfirstvalues = 10)
nycp_format %>% knitr::kable()

# by column
# "X1"
head(nycproperty_raw[,"X1"])
# -> sequential number, remove

# "BOROUGH"             
head(nycproperty_raw[,"BOROUGH"])
distinct(nycproperty_raw[,"BOROUGH"])
# -> numbers 1 - 5, convert to factor

# "NEIGHBORHOOD"
head(nycproperty_raw[, "NEIGHBORHOOD"])
distinct(nycproperty_raw[, "NEIGHBORHOOD"])
# -> 254 different neighbourhoods

# "BUILDING CLASS CATEGORY"
head(nycproperty_raw[, "BUILDING CLASS CATEGORY"])
distinct(nycproperty_raw[, "BUILDING CLASS CATEGORY"]) %>%
    arrange(.) %>% print(n = 47)
# -> 47 different categories

# "TAX CLASS AT PRESENT"
head(nycproperty_raw[, "TAX CLASS AT PRESENT"])
nycproperty_raw %>%
    count(tax_class_at_present)
# -> 10 different values and some NAs

# "BLOCK"
nycproperty_raw %>%
    count(block)
histogram(nycproperty_raw$block)
range(nycproperty_raw$block)
# -> some numerical values between 1 and 16,322

# "LOT"
# "EASE-MENT"
# "BUILDING CLASS AT PRESENT"     
# "ADDRESS"
# "APARTMENT NUMBER"
# "ZIP CODE"                      
# "RESIDENTIAL UNITS"
# "COMMERCIAL UNITS"
# "TOTAL UNITS"                   
# "LAND SQUARE FEET"
# "GROSS SQUARE FEET"
# "YEAR BUILT"                    
# "TAX CLASS AT TIME OF SALE"
# "BUILDING CLASS AT TIME OF SALE"
# "SALE PRICE"                    
# "SALE DATE"  
