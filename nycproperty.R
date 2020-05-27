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
library(purrr)

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
    str_replace_all(" ", "_") %>%
    str_replace_all("-", "")
colnames(nycproperty_raw) <- newcolnames
rm(newcolnames)

# table summary - need in chunks
nycp_format <- df_format(df = nycproperty_raw, nfirstvalues = 10)
nycp_format %>% knitr::kable()

# by column
# "X1"
head(nycproperty_raw[,"X1"])
# -> sequential number, REMOVE

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

# "BLOCK" - part of the borough / block / lot location triple
nycproperty_raw %>%
    count(block)
histogram(nycproperty_raw$block)
range(nycproperty_raw$block)
# -> some numerical values between 1 and 16,322

# "LOT"
nycproperty_raw %>%
    count(lot)
histogram(nycproperty_raw$lot)
range(nycproperty_raw$lot)
# -> numerical values between 1 and 9106,
# hist -> most of them between1 and 2000
# --> transform log10
nycproperty_raw %>%
    ggplot(aes(x = lot)) +
    scale_x_log10() +
    geom_histogram()
# most between 10 and 100

# "EASE-MENT"
nycproperty_raw$easement %>% head()
# -> lots of NA's, see how many
sum(!is.na(nycproperty_raw$easement))
# all of them are NAs -> REMOVE column

# "BUILDING CLASS AT PRESENT"
nycproperty_raw %>%
    mutate(gen_class_at_present = str_sub(building_class_at_present,
                                          start = 1, end = 1),
           sub_class_at_present = str_sub(building_class_at_present,
                                          start = 2, end = 2)) %>%
    count(gen_class_at_present) %>% 
    arrange(desc(n)) %>%
    print(n = 26)
# most accounted for by general classes R, A, B, D, C
# covering:
mean(str_sub(nycproperty_raw$building_class_at_present,1,1) %in% 
     c("R", "A", "B", "D", "C"))
# -> overall 91%

# "ADDRESS"
nycproperty_raw$address %>% head()
# not sure if will be useful, leave in

# "APARTMENT NUMBER"
length(unique(nycproperty_raw$apartment_number))
# lots of different entries, REMOVE

# "ZIP CODE"
nycproperty_raw$zip_code %>% na.omit() %>% histogram()
# odd distribution, some zeroes?
nycproperty_raw %>%
    mutate(below_1000 = factor(zip_code < 1000)) %>%
    ggplot(aes(x = zip_code)) +
    geom_histogram(bins = 30, col = "black") +
    facet_grid(.~below_1000)
# check out distribution
nycproperty_raw %>%
    mutate(below_1000 = factor(zip_code < 1000)) %>%
    ggplot(aes(x = below_1000, y = zip_code)) +
    geom_boxplot()
# filter only >0
nycproperty_raw %>%
    filter(zip_code > 0) %>%
    ggplot(aes(x = zip_code)) + 
    geom_histogram(bins = 50, col = "black")
# -> some clusters, need to check

# "RESIDENTIAL UNITS"
# check out distribution
nycproperty_raw %>% count(residential_units) %>% arrange(desc(n))
# -> mostly between 0 and 4

# check for NAs
sum(is.na(nycproperty_raw$residential_units))
# -> no NAs, so check proportion b/w 0 and 4

mean(nycproperty_raw$residential_units %in% seq(0,4))
# high, 96%

# "COMMERCIAL UNITS"
# check out distribution
nycproperty_raw %>% count(commercial_units) %>% arrange(desc(n))
# highest 0 and 1

# check for NAs
sum(is.na(nycproperty_raw$commercial_units))
# no NAs

sapply(1:4, function(i) mean(nycproperty_raw$commercial_units %in% seq(0,i)))
# -> mostly between 0 and 1, not much increments by 2, 3, 4,

# "TOTAL UNITS"
# double-check sum
nycproperty_raw %>%
    mutate(tunits = residential_units + commercial_units) %>%
    filter(total_units != tunits) %>%
    select(borough, block, lot, total_units, tunits, 
           residential_units, commercial_units) %>%
    group_by(total_units, tunits) %>%
    summarise(n = n())
# -> so almost all differences have tunits as 0, clearly a mistake

# "LAND SQUARE FEET"
# first check NAs
sum(is.na(nycproperty_raw$land_square_feet))
# -> formally none

# check out 100 random values
set.seed(21, sample.kind = "Rounding")
sample(nycproperty_raw$land_square_feet, size = 100)
# -> Many have "-", so replace by zero

# "GROSS SQUARE FEET"
set.seed(22, sample.kind = "Rounding")
sample(nycproperty_raw$gross_square_feet, size = 100)
# -> Many have "-", so replace by zero


# "YEAR BUILT"                    
# "TAX CLASS AT TIME OF SALE"
# "BUILDING CLASS AT TIME OF SALE"

# "SALE PRICE"    
# check for "outright NAs"
mean(is.na(nycproperty_raw$sale_price))

# look at a sample of 100
set.seed(32, sample.kind = "Rounding")
sample(nycproperty_raw$sale_price, size = 100)
# -> Some are "-", convert these to zeroes
# Many have low values, may filter out as they have very small amount

nycproperty_raw %>%
    mutate(sale_price = str_replace_all(sale_price,"-", "0")) %>%
    mutate(sale_price_num = parse_number(sale_price)) %>%
    filter(between(sale_price_num, 1e4, 1e8)) %>%
    ggplot(aes(x = sale_price_num)) +
    scale_x_log10() +
    geom_histogram(bins = 50, col = "black")
        
# "SALE DATE"  


################################################################################
################################################################################
# Saving progress, for working versions, uncomment as needed
################################################################################
################################################################################
#save.image(file = "nycproperty_20200527.Rdata")

# load on demand
#load(file = "nycproperty_20200525.Rdata")

