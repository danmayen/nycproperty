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
library(lubridate)

################################################################################
# 3.2 Data Structure and Loading
################################################################################
################################################################################
# 3.2.1 Data Download
################################################################################

# link address
url <- "https://raw.githubusercontent.com/danmayen/nycproperty/master/data/
nyc-property-sales.zip"
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

# column names with underscores and lowercase
newcolnames <- colnames(nycproperty_raw) %>%
    str_to_lower() %>%
    str_replace_all(" ", "_") %>%
    str_replace_all("-", "")
colnames(nycproperty_raw) <- newcolnames

# save to one file for report
save(nycproperty_raw, file = "data/nycproperty_raw.Rdata")
# load(file = "data/nycproperty_raw.Rdata")

# remove temporary variables
rm(url, tmp_filename, csv_filename, newcolnames)

################################################################################
# 3.2.2 Split into Training Set and Test Set
################################################################################
# remove columns that are of no use:
# X1 - number only
# easement - all NAs
# apartment number
# address
nycproperty_raw <- nycproperty_raw %>%
    select(-x1, -easement, -apartment_number, -address)

# Test set will be 10% of overall data
set.seed(1, sample.kind="Rounding")
# if using R 3.5 or earlier, use `set.seed(1)` instead
test_index <- createDataPartition(y = nycproperty_raw$sale_price, 
                                  times = 1, p = 0.1, list = FALSE)
train_nycp <- nycproperty_raw[-test_index,]
test_nycp <- nycproperty_raw[test_index,]

# save to file for report
save(train_nycp, test_nycp, file = "data/nycp_split.Rdata")
# load(file = "data/nycp_split.Rdata")

# discard temporary variables
rm(test_index)

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

################################################################################
# 3.2.3 Basic Data Structure
################################################################################

# 3.2.3.1 Borough
train_nycp %>% count(borough)


################################################################################
# 3.3 Data cleaning
################################################################################

# 3.3.1 Convert borough into factor
train_nycp <- train_nycp %>%
    mutate(borough = factor(borough))
test_nycp <- test_nycp %>%
    mutate(borough = factor(borough))

# 3.3.x Convert sale prices to number
train_nycp <- train_nycp %>%
    mutate(sale_price = str_replace_all(sale_price,"-", "0")) %>%
    mutate(sale_price = parse_number(sale_price))
test_nycp <- test_nycp %>%
    mutate(sale_price = str_replace_all(sale_price,"-", "0")) %>%
    mutate(sale_price = parse_number(sale_price))

################################################################################
# 3.4.2 Borough
################################################################################

# plot distribution of sale price by borough
train_nycp %>%
    filter(sale_price > 0) %>%
    ggplot(aes(x = borough, y = sale_price)) +
    scale_y_log10() +
    geom_boxplot()


################################################################################
################################################################################
# Data Exploration Chunks, delete later
################################################################################
################################################################################


# raw summary
summary(nycproperty_raw)

# table summary - need in chunks
nycp_format <- df_format(df = nycproperty_raw, nfirstvalues = 10)
nycp_format %>% knitr::kable()

# by column
# "X1"
head(nycproperty_raw[,"X1"])
# -> sequential number, REMOVE

# "BOROUGH"             
head(nycproperty_raw[,"borough"])
distinct(nycproperty_raw[,"borough"])
# -> numbers 1 - 5, convert to factor

# "NEIGHBORHOOD"
head(nycproperty_raw[, "neighborhood"])
distinct(nycproperty_raw[, "neighborhood"])
# -> 254 different neighbourhoods

# "BUILDING CLASS CATEGORY"
head(nycproperty_raw[, "building_class_category"])
distinct(nycproperty_raw[, "building_class_category"]) %>%
    arrange(.) %>% print(n = 47)
# -> 47 different categories

# "TAX CLASS AT PRESENT"
head(nycproperty_raw[, "tax_class_at_present"])
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
# not sure if will be useful, so check different entries
length(unique(nycproperty_raw$address))

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

# check distribution by borough
nycproperty_raw %>%
    filter(zip_code > 0) %>%
    ggplot(aes(x = factor(borough), y = zip_code)) +
    geom_boxplot()

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
# -> use total_units and not sum of the two unit counts

# "LAND SQUARE FEET"
# first check NAs
sum(is.na(nycproperty_raw$land_square_feet))
# -> formally none

# check out 100 random values
set.seed(21, sample.kind = "Rounding")
sample(nycproperty_raw$land_square_feet, size = 100)
# -> Many have "-", so replace by zero, then split for <10 and box plot
nycproperty_raw %>%
    mutate(land_square_feet = str_replace_all(land_square_feet,"-","0")) %>%
    mutate(land_square_feet_num = parse_number(land_square_feet)) %>%
    mutate(below_10 = (land_square_feet_num < 10)) %>%
    ggplot(aes(x = below_10, y = land_square_feet_num)) +
    geom_boxplot()

# determine for how many number is <10, so effectively missing
nycproperty_raw %>%
    mutate(land_square_feet = str_replace_all(land_square_feet,"-","0")) %>%
    mutate(land_square_feet_num = parse_number(land_square_feet)) %>%
    mutate(below_10 = (land_square_feet_num < 10)) %>% 
    group_by(below_10) %>% count()
# That is a lot, so just over half of sq footage available! 
    
land_square_feet_num <- nycproperty_raw %>%
    mutate(land_square_feet = str_replace_all(land_square_feet,"-","0")) %>%
    mutate(land_square_feet_num = parse_number(land_square_feet)) %>%
    filter(land_square_feet_num >= 10) %>% .$land_square_feet_num

hist(log10(land_square_feet_num), col = "grey", prob = TRUE, 
     xlab = "Land [sq ft]", breaks = 50, axes = FALSE,
     main = "Histogram of Land Area")
lines(density(log10(land_square_feet_num)), lwd = 3, col = "blue")
axis(1, at=seq(1,7), labels = 10^seq(0,6)) # bottom
axis(2) # normal y axis

# "GROSS SQUARE FEET"
set.seed(21, sample.kind = "Rounding")
sample(nycproperty_raw$gross_square_feet, size = 100)
# -> Many have "-", so replace by zero

# check for NA values
sum(is.na(nycproperty_raw$gross_square_feet))

# -> Many have "-", so replace by zero, 
# still left many NAs after parsing, so check these out
nycproperty_raw %>%
    mutate(gross_square_feet = str_replace_all(gross_square_feet,"-","0")) %>%
    mutate(gross_square_feet_num = parse_number(gross_square_feet)) %>%
    mutate(below_10 = (gross_square_feet_num < 10)) %>%
    ggplot(aes(x = below_10, y = gross_square_feet_num)) +
    geom_boxplot()

nycproperty_raw %>%
    mutate(gross_square_feet = str_replace_all(gross_square_feet,"-","0")) %>%
    mutate(gross_square_feet_num = parse_number(gross_square_feet)) %>%
    mutate(below_10 = (gross_square_feet_num < 1)) %>%
    group_by(below_10) %>% count()
# -> so just under half of values missing!

# "YEAR BUILT"  
set.seed(21, sample.kind = "Rounding")
sample(nycproperty_raw$year_built, size = 100)
# some zeroes in there, so filter those out

# group into decades by rounding down to 10
nycproperty_raw %>%
    mutate(year_decade = floor(year_built/10)*10) %>%
    group_by(year_decade) %>% count() %>% print(n = 24)
# -> ok to filter out all before 1800

nycproperty_raw %>%
    mutate(year_decade = floor(year_built/10)*10) %>%
    group_by(year_decade) %>% count() %>% 
    filter(year_decade >= 1880) %>%
    ggplot(aes(x = year_decade, y = n)) +
    geom_bar(stat = "identity")

# "TAX CLASS AT TIME OF SALE"
nycproperty_raw %>%
    count(tax_class_at_present) 
# 10 values and some NAs

# "BUILDING CLASS AT TIME OF SALE"
nycproperty_raw %>%
    mutate(gen_class_at_time_of_sale = str_sub(building_class_at_time_of_sale,
                                               start = 1, end = 1)) %>%
    count(gen_class_at_time_of_sale) %>% 
    arrange(desc(n)) %>%
    print(n = 26)
# most accounted for by general classes R, A, B, D, C
# covering:
mean(str_sub(nycproperty_raw$building_class_at_time_of_sale,1,1) %in% 
         c("R", "A", "B", "D", "C"))
# -> around 92%

# "SALE PRICE"    
# check for "outright NAs"
mean(is.na(nycproperty_raw$sale_price))

# look at a sample of 100
set.seed(32, sample.kind = "Rounding")
sample(nycproperty_raw$sale_price, size = 100)
# -> Some are "-", convert these to zeroes
# Many have low values, may filter out as they have very small amount

# overall distribution
nycproperty_raw %>%
    mutate(sale_price = str_replace_all(sale_price,"-", "0")) %>%
    mutate(sale_price_num = parse_number(sale_price)) %>%
    filter(between(sale_price_num, 1e4, 1e8)) %>%
    ggplot(aes(x = sale_price_num)) +
    scale_x_log10() +
    geom_histogram(bins = 50, col = "black")

# check mean/sd of prices if no sq footage available
nycproperty_raw %>%
    mutate(land_square_feet = str_replace_all(land_square_feet,"-", "0"),
           gross_square_feet = str_replace_all(gross_square_feet, "-", "0"),
           sale_price = str_replace_all(sale_price,"-", "0")) %>%
    mutate(land_square_feet_num = parse_number(land_square_feet),
           gross_square_feet_num = parse_number(gross_square_feet),
           sale_price_num = parse_number(sale_price)) %>%
    mutate(land_square_feet_available = (land_square_feet_num >=10),
           gross_square_feet_available = (gross_square_feet_num >= 10),
           sale_price_available = (sale_price_num > 10000)) %>%
    group_by(sale_price_available,
             land_square_feet_available,
             gross_square_feet_available) %>% count()
        
# "SALE DATE"
# check now many NAs
mean(is.na(nycproperty_raw$sale_date))

# group into months
nycproperty_raw %>%
    mutate(sale_month = month(sale_date)) %>%
    group_by(sale_month) %>% count() %>%
    ggplot(aes(x = sale_month, y = n)) +
    geom_bar(stat = "identity")

unique(year(nycproperty_raw$sale_date))
range(nycproperty_raw$sale_date)
format(min(nycproperty_raw$sale_date), "%d %b %Y")

################################################################################
################################################################################
# Saving progress, for working versions, uncomment as needed
################################################################################
################################################################################
save.image(file = "nycproperty_20200527.Rdata")

# load on demand
#load(file = "nycproperty_20200525.Rdata")

