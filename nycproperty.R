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

if(!require(tidyverse))install.packages("tidyverse")
if(!require(ggplot2))install.packages("ggplot2")
if(!require(gridExtra))install.packages("gridExtra")
if(!require(caret))install.packages("caret")
if(!require(readr))install.packages("readr")
if(!require(purrr))install.packages("purrr")
if(!require(lubridate))install.packages("lubridate")
if(!require(testit))install.packages("testit")
if(!require(gam))install.packages("gam")

# load all needed libraries using above function
# library(tidyverse)
# library(ggplot2)
# library(caret)
# library(readr)
# library(purrr)
# library(lubridate)

################################################################################
# 0.3 Auxiliary Functionss
################################################################################
#' Returns data frame with format of a given data frame
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

#' Function to ensure that all values of a certain column that appear
#' in the test set are also in the training set by transferring missing
#' values from the (temporary) test set to the training set
#' 
#' @param train Training set
#' @param temp_test Temporary test set
#' @return List of data frames named `train` and `test` containing the
#'         training set augmented by values previous in (temporary) test set
#'         but not in training set. 
augment_train_by_test <- function(train, temp_test, column) {
    # checking condition
    nrows_before <- nrow(train) + nrow(temp_test)
    
    # Only transfer values from temporary set into test that are
    # also in the training set
    test <- temp_test %>% semi_join(train, by = column)
    
    # add rows removed from temporary test set into training set
    removed <- anti_join(temp_test, test, by = column)
    train <- rbind(train, removed)
    
    # check condition to ensure data integrity
    assert("Total number of data rows remains the same",
           nrows_before == nrow(train) + nrow(test))
    
    # return as list
    list(train = train, test = test)
    }

#' Harmonic mean
#' @param x Vector to calculate harmonic mean of
#' @return Harmonic mean of x
harm.mean <- function(x) 1/mean(1/x)

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
# if using R 3.5 or earlier, use `set.seed(1)` instead
set.seed(1, sample.kind="Rounding")
test_index <- createDataPartition(y = nycproperty_raw$sale_price, 
                                  times = 1, p = 0.1, list = FALSE)
train_nycp <- nycproperty_raw[-test_index,]
test_nycp <- nycproperty_raw[test_index,]

# save to file for report
save(train_nycp, test_nycp, file = "data/nycp_split_raw.Rdata")
# load(file = "data/nycp_split_raw.Rdata")

# check which data in training and test set are different b/o warnings
# 1. borough
test_nycp %>% anti_join(train_nycp, by = "borough") %>% nrow()
# -> OK

# 2. neighbourhood
test_nycp %>% anti_join(train_nycp, by = "neighborhood") %>% nrow()
# -> 1 row
# -> apply augmentation function to transfer these few rows from test set
#    to training set
df_temp <- augment_train_by_test(train_nycp, test_nycp, 
                                 column = "neighborhood")
train_nycp <- df_temp$train
test_nycp <- df_temp$test
# double-check condition
test_nycp %>% anti_join(train_nycp, by = "neighborhood") %>% nrow()
# discard temporary set
rm(df_temp)

# 3. building class category
test_nycp %>% anti_join(train_nycp, by = "building_class_category") %>% nrow()
# -> OK

# 4. Tax class at present
test_nycp %>% anti_join(train_nycp, by = "tax_class_at_present") %>% nrow()
# -> OK

# 5. Block
train_nycp %>% count(block) %>% arrange(desc(n))
# not worth it, too many distinct values

# 6. Lot
train_nycp %>% count(lot) %>% arrange(desc(n))
# not worth it, too many distinct values

# 7. building class at present
test_nycp %>% anti_join(train_nycp, by = "building_class_at_present") %>% nrow()
# -> 5 rows, apply augmentation function

df_temp <- augment_train_by_test(train_nycp, test_nycp, 
                                 column = "building_class_at_present")
train_nycp <- df_temp$train
test_nycp <- df_temp$test
# double-check condition
test_nycp %>% anti_join(train_nycp, by = "building_class_at_present") %>% nrow()
# discard temporary set
rm(df_temp)

# 8. ZIP code
train_nycp %>% count(zip_code) %>% arrange(desc(n))
# ~180 distinct values, so worth checking
test_nycp %>% anti_join(train_nycp, by = "zip_code") %>% nrow()
# -> 1 row, apply augmentation function

df_temp <- augment_train_by_test(train_nycp, test_nycp, 
                                 column = "zip_code")
train_nycp <- df_temp$train
test_nycp <- df_temp$test
# double-check condition
test_nycp %>% anti_join(train_nycp, by = "zip_code") %>% nrow()
# discard temporary set
rm(df_temp)

# 9. Number of residential units
test_nycp %>% anti_join(train_nycp, by = "residential_units") %>% nrow()
# there are 31 rows, check out details
test_nycp %>% anti_join(train_nycp, by = "residential_units") %>%
    select(borough, building_class_category, building_class_at_time_of_sale,
           residential_units) %>%
    arrange(desc(residential_units)) %>%
    print(n = 100)
# These are all large numbers, ok to augment

df_temp <- augment_train_by_test(train_nycp, test_nycp, 
                                 column = "residential_units")
train_nycp <- df_temp$train
test_nycp <- df_temp$test
# double-check condition
test_nycp %>% anti_join(train_nycp, by = "residential_units") %>% nrow()
# discard temporary set
rm(df_temp)

# 10. Number of commercial units
test_nycp %>% anti_join(train_nycp, by = "commercial_units") %>% nrow()
# there are 12 rows, check out details
test_nycp %>% anti_join(train_nycp, by = "commercial_units") %>%
    select(borough, building_class_category, building_class_at_time_of_sale,
           commercial_units) %>%
    arrange(desc(commercial_units)) %>%
    print(n = 100)
# These are all large numbers, ok to include back

df_temp <- augment_train_by_test(train_nycp, test_nycp, 
                                 column = "commercial_units")
train_nycp <- df_temp$train
test_nycp <- df_temp$test
# double-check condition
test_nycp %>% anti_join(train_nycp, by = "commercial_units") %>% nrow()
# discard temporary set
rm(df_temp)

# 11. Number of total units
test_nycp %>% anti_join(train_nycp, by = "total_units") %>% nrow()
# -> 4 rows, apply augmentation function

df_temp <- augment_train_by_test(train_nycp, test_nycp, 
                                 column = "total_units")
train_nycp <- df_temp$train
test_nycp <- df_temp$test
# double-check condition
test_nycp %>% anti_join(train_nycp, by = "total_units") %>% nrow()
# discard temporary set
rm(df_temp)

# 12. Land square feet -> continuous number -> not checked
# 13. Gross square feet -> continuous number -> not checked

# 14. year build -> only check decade
train_nycp <- train_nycp %>%
    mutate(decade_built = floor(year_built/10)*10)

test_nycp %>% 
    mutate(decade_built = floor(year_built/10)*10) %>%
    anti_join(train_nycp, by = "decade_built")

# -> only 1 row, check out details
test_nycp %>%    
    mutate(decade_built = floor(year_built/10)*10) %>%
    anti_join(train_nycp, by = "decade_built") %>%
    select(borough, building_class_category, building_class_at_time_of_sale,
           decade_built) %>%
    print(n = 100)

# SINGLE value of 1110, much before records existed, so this must
# be an erroneous value and therefore, is removed from test set.
test_nycp <- test_nycp %>%
    filter(year_built == 0 | year_built > 1500)
# remove decade_built column from training set again
train_nycp <- select(train_nycp, -decade_built)

# 15. Tax Class at Time of Sale
test_nycp %>% anti_join(train_nycp, by = "tax_class_at_time_of_sale") %>% nrow()
# -> OK

# 16. Building Class at Time of Sale, in combination with borough
test_nycp %>% anti_join(train_nycp,
                        by = c("borough", "building_class_at_time_of_sale")) %>% 
    nrow()
# -> 27 rows, apply augmentation function

df_temp <- 
    augment_train_by_test(
        train_nycp, test_nycp, 
        column = c("borough", "building_class_at_time_of_sale"))
train_nycp <- df_temp$train
test_nycp <- df_temp$test
# double-check condition
test_nycp %>% 
    anti_join(train_nycp, 
              by =  c("borough", "building_class_at_time_of_sale")) %>% nrow()
# discard temporary set
rm(df_temp)

#####################################
## discard temporary variables
rm(test_index)
save(train_nycp, test_nycp, file = "data/nycp_split_clean.Rdata")


################################################################################
# 3.2.3 Basic Data Structure
################################################################################

# 3.2.3.1 Borough
train_nycp %>% count(borough)

# 3.2.3.18 Sale price
price_thresholds <- c(0,10^seq(0,3))
tbl_price_below <- map_df(price_thresholds, 
                     function(x) {
                         list(threshold = x,
                              n_below = mean(train_nycp$sale_price <= x))
                         })
# -> so only 2/3 of sales have a positive price!
plot(tbl_price_below, log = "x")

################################################################################
# 3.3 Data cleaning
################################################################################




# 3.3.1 Convert borough into factor
train_nycp <- train_nycp %>%
    mutate(borough = factor(borough))
test_nycp <- test_nycp %>%
    mutate(borough = factor(borough))

# 3.3.2 Neighbourhood
# any NAs?
sum(is.na(train_nycp$neighborhood))
# -> no NAs

# determine distinct values
length(unique(train_nycp$neighborhood))
# -> 253 different neighbourhoods, quite granular, leave for now

# 3.3.3 Building Class Category, simply convert to factor
train_nycp <- train_nycp %>%
    mutate(building_class_category = factor(building_class_category)) %>%
    mutate(building_class_at_present = 
               reorder(building_class_category, sale_price, log10mean))
test_nycp <- test_nycp %>%
    mutate(building_class_category = 
               factor(building_class_category,
                      levels = levels(train_nycp$building_class_category)))

# 3.3.4 ZIP code, convert to factor
train_nycp <- train_nycp %>%
    mutate(zip_code = factor(zip_code)) %>%
    mutate(zip_code = reorder(zip_code, sale_price, log10mean))
test_nycp <- test_nycp %>%
    mutate(zip_code = factor(zip_code,
                             levels = levels(train_nycp$zip_code)))

# 3.3.12 Land Square feet
train_nycp <- train_nycp %>%
    mutate(land_square_feet = str_replace_all(land_square_feet, "-", "0")) %>%
    mutate(land_square_feet = parse_number(land_square_feet))
test_nycp <- test_nycp %>%
    mutate(land_square_feet = str_replace_all(land_square_feet, "-", "0")) %>%
    mutate(land_square_feet = parse_number(land_square_feet))

# 3.3.13 Gross Square feet
train_nycp <- train_nycp %>%
    mutate(gross_square_feet = str_replace_all(gross_square_feet, "-", "0")) %>%
    mutate(gross_square_feet = parse_number(gross_square_feet))
test_nycp <- test_nycp %>%
    mutate(gross_square_feet = str_replace_all(gross_square_feet, "-", "0")) %>%
    mutate(gross_square_feet = parse_number(gross_square_feet))

# 3.3.14 year built -> get decade
train_nycp <- train_nycp %>%
    mutate(decade_built = floor(year_built/10)*10)
test_nycp <- test_nycp %>%
    mutate(decade_built = floor(year_built/10)*10)

# 3.3.17 sale date -> get sale month
train_nycp <- train_nycp %>%
    mutate(sale_month = month(sale_date))
test_nycp <- test_nycp %>%
    mutate(sale_month = month(sale_date))

# 3.3.18 Sale price
set.seed(330, sample.kind = "Rounding")
sample(train_nycp$sale_price, size = 100)

# -> convert "-" to "0" and parse as number
train_nycp <- train_nycp %>%
    mutate(sale_price = str_replace_all(sale_price,"-", "0")) %>%
    mutate(sale_price = parse_number(sale_price))
test_nycp <- test_nycp %>%
    mutate(sale_price = str_replace_all(sale_price,"-", "0")) %>%
    mutate(sale_price = parse_number(sale_price))

# box plot and histogram of sale prices (positive only)
p1 <- train_nycp %>% 
    filter(sale_price > 0) %>%
    ggplot(aes(y = sale_price)) +
    scale_y_log10() +
    geom_boxplot()
p2 <- train_nycp %>%
    filter(sale_price > 0)%>%
    ggplot(aes(x = sale_price)) +
    scale_x_log10() +
    geom_histogram(bins = 50, col = "black")
grid.arrange(p1, p2, ncol = 2)

# there are many outliers at both ends, so plot the 90% body only
train_nycp$sale_price[train_nycp$sale_price > 0] %>%
    quantile(probs = seq(0, 1, 0.05))

# so between $100k and $3m
sale_price_lo <- 100000
sale_price_hi <- 3e6

p1 <- train_nycp %>% 
    filter(between(sale_price, sale_price_lo, sale_price_hi)) %>%
    ggplot(aes(y = sale_price)) +
    scale_y_log10() +
    geom_boxplot()
p2 <- train_nycp %>%
    filter(between(sale_price, sale_price_lo, sale_price_hi)) %>%
    ggplot(aes(x = sale_price)) +
    scale_x_log10() +
    geom_histogram(bins = 50, col = "black")
grid.arrange(p1, p2, ncol = 2)

# price likely LOG-normally distributed, so need to calculate
# mean differently

# try it out:
# 1) first log, then mean, then exp again
# 2) simple mean
mu_trf <- 10^mean(log10(train_nycp$sale_price))
mu_smp <- mean(train_nycp$sale_price)

# add these to plots above, transformed in blue, simple in red
p1 <- train_nycp %>% 
    filter(between(sale_price, sale_price_lo, sale_price_hi)) %>%
    ggplot(aes(y = sale_price)) +
    scale_y_log10() +
    geom_boxplot() +
    geom_hline(yintercept = mu_trf, size = 1, col = "blue",
               linetype = "dashed") +
    geom_hline(yintercept = mu_smp, size = 1, col = "red",
               linetype = "dashed")
p2 <- train_nycp %>%
    filter(between(sale_price, sale_price_lo, sale_price_hi)) %>%
    ggplot(aes(x = sale_price)) +
    scale_x_log10() +
    geom_histogram(bins = 50, col = "black") +
    geom_vline(xintercept = mu_trf, size = 1, col = "blue",
               linetype = "dashed") +
    geom_vline(xintercept = c(mu_trf, mu_trf), size = 1, col = "blue",
               linetype = "dashed") +
    geom_vline(xintercept = mu_smp, size = 1, col = "red",
               linetype = "dashed")
grid.arrange(p1, p2, ncol = 2)


sd_trf <- log10sd(train_nycp$sale_price)
# so write function for that, and also for sd
log10mean <- function(x) 10^mean(log10(x))
log10sd <- function(x) 10^sd(log10(x))
# try it out
identical(log10mean(train_nycp$sale_price), mu_trf)

# From 3.2.18 it does not make sense to keep sale prices at
# or below $1,000
train_nycp <- train_nycp %>%
    filter(sale_price > 1000)
# same for test data
test_nycp <- test_nycp %>%
    filter(sale_price > 1000)
# 3.3.xx Net square feet
# overall distribution
train_nycp %>%
    mutate(net_square_feet = gross_square_feet - 
               land_square_feet) %>%
    select(gross_square_feet, land_square_feet, 
           net_square_feet) %>%
    filter(gross_square_feet > 0 &
               land_square_feet > 0 &
               net_square_feet > 0) %>%
    gather(metric, footage) %>% 
    ggplot() +
    scale_x_log10() +
    geom_histogram(aes(x = footage), bins = 50, col = "black") +
    facet_grid(metric ~ ., scales = "free")

# availability in conjunction with sale prices
train_nycp %>%
    mutate(net_square_feet = gross_square_feet - 
               land_square_feet) %>%
    mutate(net_sqf_avail = (net_square_feet >= 10),
           gross_sqf_avail = (gross_square_feet >= 10),
           land_sqf_avail = (land_square_feet >= 10),
           sale_price_avail = (sale_price > 0)) %>%
    group_by(sale_price_avail, net_sqf_avail,
             gross_sqf_avail, land_sqf_avail) %>%
    summarise(n = n())
# -> so for half of data no square footage at all
#    and from other half 2/3 (=16k) have no NET square footage

# -> sample data for 16k rows where gross and land sqf but no net sqf
set.seed(3319, sample.kind = "Rounding")
train_nycp %>%
    mutate(net_square_feet = gross_square_feet - 
               land_square_feet) %>%
    filter(net_square_feet < 10,
           gross_square_feet >= 10,
           land_square_feet >= 10) %>%
    select(borough, block, lot, residential_units,
           total_units,
           gross_square_feet,
           land_square_feet, net_square_feet) %>%
    sample_n(size = 500) %>% 
    arrange(net_square_feet) %>%
    print(n = 500)

# check which building class at TOS these are
train_nycp %>%
    mutate(net_square_feet = gross_square_feet - 
               land_square_feet) %>%
    filter(net_square_feet < 10,
           gross_square_feet >= 10,
           land_square_feet >= 10) %>%
    select(borough, block, lot, 
           building_class_at_time_of_sale,
           gross_square_feet,
           land_square_feet, net_square_feet) %>%
    count(building_class_at_time_of_sale) %>%
    arrange(desc(n))
    

# Have two possibilities to deal with these obviously incorrect data:
# (1) Take net = gross for -ve net square footages
# (2) Take net = -(gross - land) for -ve net square footages

# As pre-check look at number of total units / retail units / com units
# that are zero
train_nycp %>%
    mutate(tu = total_units > 0,
           ru = residential_units > 0,
           cu = commercial_units > 0) %>%
    count(tu, ru, cu)

# see what other properties these have
train_nycp %>%
    filter(total_units == 0) %>%
    group_by(building_class_category,
           building_class_at_time_of_sale,
           building_class_at_present) %>%
    summarise(n = n()) %>% 
    arrange(desc(n)) %>% print(n = 100)
# -> mostly coops, so look only into these

# only building class category is a useful feature
train_nycp %>%
    filter(total_units == 0) %>%
    group_by(building_class_category) %>%
    summarise(n = n()) %>% 
    arrange(desc(n)) %>% print(n = 100)
# so almost all accounted for by coops and vacant land

# check out same for #residential units
train_nycp %>%
    filter(residential_units == 0) %>%
    group_by(building_class_category) %>%
    summarise(n = n()) %>% 
    arrange(desc(n)) %>% print(n = 100)
# -> again, almost all coops

# sample some of these to see what other data can be used
set.seed(3312, sample.kind = "Rounding")
train_no_units <- train_nycp %>%
    filter(total_units == 0) %>%
    filter(str_detect(str_to_lower(building_class_category), "coop")) %>%
    sample_n(size = 500)

write.csv(train_no_units, file = "data/sample_no_units_500.csv")
# -> inspection shows:
# - number of all units (resi, comm, total) are zero
# - square footage is zero
# - sale price is high enough, 850k on average
# - building class is one of C6, D0, D4 or R9
# -> check out sq footage or units for these building classes
#    where data is available

# first get distinct building classes
coop_bclasses <- train_nycp %>%
    filter(total_units == 0) %>%
    filter(str_detect(str_to_lower(building_class_category), "coop")) %>%
    count(building_class_at_time_of_sale, building_class_at_present) %>%
    .$building_class_at_time_of_sale
# -> so C6, C8, D0, D4, R9

# arrange these into lookup
coop_lookup <- train_nycp %>%
    filter(total_units > 0) %>%
    filter(str_detect(str_to_lower(building_class_category), "coop")) %>%
    select(borough, block, lot,
           building_class_at_time_of_sale,
           total_units,
           land_square_feet,
           gross_square_feet,
           sale_price) %>%
    arrange(borough, building_class_at_time_of_sale,
        desc(gross_square_feet), desc(total_units)) %>%
    mutate(price_gross_sqft =
               sale_price / gross_square_feet,
           price_net_sqft = 
               sale_price / 
               (gross_square_feet - land_square_feet))

# then group by borough and building class and take median price/sqft
coop_lookup %>%
    group_by(borough, building_class_at_time_of_sale) %>%
    summarise(n = n(),
              price_sqft_gross_med = median(price_gross_sqft),
              price_sqft_net_med = median(price_net_sqft),
              ratio = price_sqft_net_med/price_sqft_gross_med)

# -> so take price per gross sqft, as less noisy
# -> that also makes the choice from above, so only use gross footage
#    and not net footage for now

# To decide, take the correct 6k rows and plot them against number
# of units, all resi, commercial and total
# For NET square footage
train_nycp %>%
    mutate(net_square_feet = gross_square_feet - 
               land_square_feet) %>%
    filter(gross_square_feet > 0 &
               land_square_feet > 0 &
               net_square_feet > 0 &
               total_units > 0) %>%
    select(residential_units, commercial_units, total_units,
           net_square_feet) %>%
    gather(unit_type, units, residential_units:total_units) %>%
    ggplot(aes(x = units, y = net_square_feet)) +
    scale_x_log10() +
    scale_y_log10() +
    geom_point() +
    geom_smooth(method = "lm") +
    facet_grid(. ~ unit_type)

# For GROSS square footage
train_nycp %>%
    mutate(net_square_feet = gross_square_feet - 
               land_square_feet) %>%
    filter(gross_square_feet > 0 &
               land_square_feet > 0 &
               net_square_feet > 0 &
               total_units > 0) %>%
    select(residential_units, commercial_units, total_units,
           gross_square_feet) %>%
    gather(unit_type, units, residential_units:total_units) %>%
    ggplot(aes(x = units, y = gross_square_feet)) +
    scale_x_log10() +
    scale_y_log10() +
    geom_point() +
    geom_smooth(method = "lm") +
    facet_grid(. ~ unit_type)


# Check what data available if NO GROSS sq ft given
# AND if no #total units available either
train_nycp %>%
    filter(gross_square_feet < 10) %>% 
    mutate(tu_avail = total_units > 0,
           ru_avail = residential_units > 0,
           cu_avail = commercial_units > 0) %>%
    filter(tu_avail == FALSE) %>% 
    count(borough, building_class_at_time_of_sale, 
           tu_avail, ru_avail, cu_avail) %>%
    arrange(desc(n)) %>%
    mutate(cumper = cumsum(n/sum(n))) %>%
    print(n = 200)
    
# -> for first 15 groups get 95%, so collect these building classes
#    also, can shortcut to total units available, as if these are not
#    available, neither retail nor commercial units are available either

# With all this, determine 
# 1) which borough/building class at TOS combinations are covered by 
#    ONLY gross square footage
bbc_avail_L1_tbl <- train_nycp %>%
    mutate(gross_sqft_avail = (gross_square_feet >= 10)) %>%
    filter(gross_sqft_avail) %>%
    count(borough, building_class_at_time_of_sale) %>%
    arrange(desc(n)) %>%
    mutate(cumper = cumsum(n/sum(n)))

train_nycp %>%
    anti_join(bbc_avail_L1_tbl, by = c("borough", 
                                    "building_class_at_time_of_sale")) %>%
    count(borough, building_class_at_time_of_sale) %>%
    arrange(desc(n)) %>%
    mutate(cumper = cumsum(n/sum(n))) %>% .$n %>% sum()
# -> so overall all by 92 combinations, making up 10,559 entries

# 2) which borough/building class at TOS combinations are covered by 
#    gross square footage OR total units
bbc_avail_L2_tbl <- train_nycp %>%
    mutate(gross_sqft_avail = (gross_square_feet >= 10),
           tu_avail = (total_units > 0)) %>%
    filter(gross_sqft_avail | tu_avail) %>%
    count(borough, building_class_at_time_of_sale) %>%
    arrange(desc(n)) %>%
    mutate(cumper = cumsum(n/sum(n)))

# determine which borough / building class at TOS combos NOT covered
train_nycp %>%
    anti_join(bbc_avail_L2_tbl, by = c("borough", 
                                    "building_class_at_time_of_sale")) %>%
    count(borough, building_class_at_time_of_sale) %>%
    arrange(desc(n)) %>%
    mutate(cumper = cumsum(n/sum(n))) %>% .$n %>% sum()
# -> overall 31 combinations, making up 1,140 entries

# PRICE per Square Foot! 
## check availablity first
train_nycp %>%
    mutate(land_square_feet_available = (land_square_feet >=10),
           gross_square_feet_available = (gross_square_feet >= 10),
           sale_price_available = (sale_price > 10000)) %>%
    group_by(sale_price_available,
             land_square_feet_available,
             gross_square_feet_available) %>% count()

# 3.3.xx Check if any (borough, bc at TOS) combons in test not in train set
test_nycp %>% anti_join(train_nycp,
                        by = c("borough", "building_class_at_time_of_sale")) %>% 
    nrow()
# -> 33 rows, apply augmentation function

df_temp <- 
    augment_train_by_test(
        train_nycp, test_nycp, 
        column = c("borough", "building_class_at_time_of_sale"))
train_nycp <- df_temp$train
test_nycp <- df_temp$test
# double-check condition
test_nycp %>% 
    anti_join(train_nycp, 
              by =  c("borough", "building_class_at_time_of_sale")) %>% nrow()
# discard temporary set
rm(df_temp)


# save to file for report
save(train_nycp, test_nycp, file = "data/nycp_split_trans.Rdata")

################################################################################
# 3.4.1 General prices
################################################################################


################################################################################
# 3.4.2 Borough
################################################################################

# strip out average!
mu <- mean(train_nycp$sale_price)

# plot distribution of sale price by borough
train_nycp %>%
    ggplot(aes(x = borough, y = sale_price - mu)) +
    scale_y_log10() +
    geom_boxplot() +
    geom_jitter(width = 0.2, alpha = 0.01)
# -> in particular borough 2 has lower prices
# -> in general, prices below $10,000 are outliers, as are prices
#    above $1,000,000
mean(train_nycp$sale_price > 0)

################################################################################
# 3.4.16 Building Class at Time of Sale
################################################################################

# distribution by building class, R, A, B, D, C and rest
train_nycp %>%
    mutate(building_class_tos_clustered = 
               ifelse(str_sub(building_class_at_time_of_sale,1,1) %in% 
                          c("R", "A", "B", "D", "C"),
                      str_sub(building_class_at_time_of_sale,1,1), "Other")) %>%
    mutate(building_class_tos_clustered = 
               reorder(building_class_tos_clustered, sale_price, median)) %>%
    ggplot(aes(x = factor(building_class_tos_clustered),
               y = sale_price)) +
    scale_y_log10() +
    geom_boxplot()

################################################################################
# 3.5 Modelling
################################################################################
# RMSE function
RMSE_price <- function(pred_price, actual_price) {
    ifelse(length(pred_price) > 0,
           sqrt(mean((pred_price - actual_price)^2)),
           NA)
    }

# other RMSE function - mean absolute difference
RMSE_price <- function(pred_price, actual_price) 
    mean(abs(pred_price - actual_price))

# percentage difference mean function
RMSE_price <- function(pred_price, actual_price)
    mean(pred_price/actual_price - 1)

# percentage median function
RMSE_price <- function(pred_price, actual_price)
    median(pred_price/actual_price - 1)

################################################################################
# 3.5.1 simple stats - average and median
################################################################################

mu <- mean(train_nycp$sale_price)
med <- median(train_nycp$sale_price)

# model to predict average
predict_const_avg <- function(newdata) mean(train_nycp$sale_price)

# predict prices
price_hat_avg <- predict_const_avg(test_nycp)
# calculate RMSE and store in results table
rmse_const_avg <- RMSE_price(price_hat_avg, test_nycp$sale_price)

# model to predict median
predict_const_med <- function(newdata) median(train_nycp$sale_price)

# predict prices
price_hat_med <- predict_const_med(test_nycp)
# calculate RMSE and store in results table
rmse_const_med <- RMSE_price(price_hat_med, test_nycp$sale_price)



################################################################################
# 3.5.2 Models based on borough only
################################################################################

set.seed(351, sample.kind = "Rounding")
train_nycp_1000 <- sample_n(train_nycp, size = 1000)

# there are only 5 boroughs so k = 1, 3 will do
cvControl <- trainControl(method = "cv", number = 10, p = 0.9)
train_borough <- train(sale_price ~ borough, data = train_nycp, 
                       method = "knn",
                       tuneGrid = data.frame(k = seq(1, 3, 2)))
ggplot(train_borough, highlight = TRUE)

# simply use average of each borough as predictor
predict_ba <- function(newdata) {
    mu <- mean(train_nycp$sale_price)
    b_b_tbl <- train_nycp %>% group_by(borough) %>%
        summarise(b_b = mean(sale_price - mu))
    pred_price <- newdata %>%
        left_join(b_b_tbl, by = "borough") %>%
        mutate(price_hat = mu + b_b) %>% .$price_hat
    }

# simply use median of each borough as predictor
predict_bm <- function(newdata) {
    b_m_tbl <- train_nycp %>% group_by(borough) %>%
        summarise(b_m = median(sale_price))
    pred_price <- newdata %>%
        left_join(b_m_tbl, by = "borough") %>%
        mutate(price_hat = b_m) %>% .$price_hat
    }

# TODO: write function that uses kNN

# predict prices, with total data
price_hat_bk <- predict(train_borough, newdata = test_nycp)
# calculate RMSE and store in results table
rmse_bk <- RMSE_price(price_hat_bk, test_nycp$sale_price)

# predict prices, with class average
price_hat_ba <- predict_ba(test_nycp)
# calculate RMSE and store in results table
rmse_ba <- RMSE_price(price_hat_ba, test_nycp$sale_price)

# predict prices, with class median
price_hat_bm <- predict_bm(test_nycp)
# calculate RMSE and store in results table
rmse_bm <- RMSE_price(price_hat_bm, test_nycp$sale_price)

################################################################################
# 3.5.3 Models based on price per square foot (gross)
################################################################################

# Generate list of training and test sets from "edx" for k-fold cross-validation
# Use function createFolds with k = 25 
set.seed(342, sample.kind = "Rounding")
index_list <- createFolds(train_nycp$sale_price, k = 25)


################################################################################
# 3.5.3.1 Square footage directly available
################################################################################

# Step 1: Calculate sale price per gross square foot,
#         using different types of averages

# Decide which of the averages is taken by k-fold cross-validation
rmse_v_L1 <- map_df(1:length(index_list), function (listIdx) {
    # split data into training and test set
    train_set <- train_nycp[-index_list[[listIdx]], ]
    test_set <- train_nycp[index_list[[listIdx]], ]
    
    ppgsf_L1_tbl <- train_set %>%
        select(borough, building_class_at_time_of_sale,
           gross_square_feet, sale_price) %>%
        filter(gross_square_feet >= 10) %>%
        group_by(borough, building_class_at_time_of_sale) %>%
        summarise(nprices = n(),
                  price_per_gsf = sum(sale_price) / sum(gross_square_feet),
                  price_per_gsf_w = log10mean(sale_price) / 
                      log10mean(gross_square_feet),
                  price_per_gsf_med = median(sale_price/gross_square_feet),
                  price_per_gsf_harm = harm.mean(sale_price/gross_square_feet))
    
    predictions <- test_set %>%
        filter(gross_square_feet >= 10) %>%
        inner_join(ppgsf_L1_tbl, 
                   by = c("borough", "building_class_at_time_of_sale")) %>%
        mutate(sale_price_hat = gross_square_feet * price_per_gsf,
               sale_price_hat_w = gross_square_feet * price_per_gsf_w,
               sale_price_hat_med = gross_square_feet * price_per_gsf_med,
               sale_price_hat_harm = gross_square_feet * price_per_gsf_harm) %>%
        mutate(perdiff = (sale_price_hat/sale_price - 1),
               perdiff_w = (sale_price_hat_w/sale_price - 1),
               perdiff_med = (sale_price_hat_med/sale_price - 1),
               perdiff_harm = (sale_price_hat_harm/sale_price - 1))
    predictions %>%
        summarise(RMSE_avg = mean(perdiff),
                  RMSE_avg_w = mean(perdiff_w),
                  RMSE_avg_med = mean(perdiff_med),
                  RMSE_avg_harm = mean(perdiff_harm),
                  RMSE_med = median(perdiff),
                  RMSE_med_w = median(perdiff_w),
                  RMSE_med_med = median(perdiff_med),
                  RMSE_med_harm = median(perdiff_harm))
    })
rm(train_set, test_set, ppgsf_L1_tbl, predictions)

rmse_v_L1 %>% summarise_all(mean)
rmse_v_L1 %>% summarise_all(sd)

# STEP 1 DECISION:
#-> take the HARMONIC MEAN to calculate price / gross sq ft
price_gsf_L1_tbl <- train_nycp %>%
    select(borough, building_class_at_time_of_sale,
           gross_square_feet, sale_price) %>%
    filter(gross_square_feet >= 10) %>%
    group_by(borough, building_class_at_time_of_sale) %>%
    summarise(nprices = n(),
              price_per_gsf_harm = harm.mean(sale_price/gross_square_feet))

################################################################################
# 3.5.3.2 Square footage not available, but approximated by #total_units
################################################################################

# Step 2: Determine borough/building class at TOS combinations for
#         which total_units > 0 and gross ft^2 < 10
#         For these, approximate gross square footage by LOESS
bbc_L1_inc_tbl <- train_nycp %>%
    filter(gross_square_feet < 10 & total_units > 0) %>%
    distinct(borough, building_class_at_time_of_sale)

# need the data for which both gross ft^2 and total units are
# available for LOESS
train_nycp_bbc_L1_inc <- train_nycp %>%
    semi_join(bbc_L1_inc_tbl, by = c("borough",
                                     "building_class_at_time_of_sale")) %>%
    filter(gross_square_feet >= 10 & total_units > 0)

# check again the relation between gross ft^2 and #total units
train_nycp_bbc_L1_inc %>%
    ggplot(aes(x = total_units)) +
    scale_x_log10() +
    scale_y_log10() +
    geom_point(aes(y = gross_square_feet))

# check model parameters for LOESS
modelLookup("gamLoess")
# -> can tune span, degree = 1 is fine

# train LOESS model
train_bbc_L1_inc <- train(gross_square_feet ~ total_units,
                          data = train_nycp_bbc_L1_inc,
                          method = "gamLoess",
                          tuneGrid = data.frame(degree = 1,
                                                span = seq(0.10, 0.70, 0.1)))
ggplot(train_bbc_L1_inc, highlight = TRUE)
train_bbc_L1_inc$bestTune

# DECISION 2: Use LOESS with span = 0.7

# plot fit
train_nycp_bbc_L1_inc %>%
    mutate(gsf_fitted = predict(train_bbc_L1_inc, newdata = .)) %>%
    ggplot(aes(x = total_units)) +
    scale_x_log10() +
    scale_y_log10() +
    geom_point(aes(y = gross_square_feet)) +
    geom_line(aes(y = gsf_fitted), col = "blue")


# Wrap-up activities for Step 2
# Take data with total units > 0 and gross_square_feet < 10 to
# a) put in proxy for gross square feet
# b) use that proxy to predict price/ft^2
price_gsf_L1_tbl_inc <- train_nycp %>%
    filter(gross_square_feet < 10 & total_units > 0) %>%
    mutate(gross_square_feet_hat = predict(train_bbc_L1_inc, newdata = .)) %>%
    group_by(borough, building_class_at_time_of_sale) %>%
    summarise(nprices = n(),
              price_per_gsf_harm = harm.mean(sale_price/gross_square_feet_hat))

# only keep (borough, bldg class at TOS) combos
# that are NOT already in price_gsf_L1_tbl
price_gsf_L1_tbl_inc <- price_gsf_L1_tbl_inc %>%
    anti_join(price_gsf_L1_tbl, by = c("borough",
                                       "building_class_at_time_of_sale"))

# check that no overlaps, should yield zero rows
price_gsf_L1_tbl %>%
    semi_join(price_gsf_L1_tbl_inc, by = c("borough",
                                           "building_class_at_time_of_sale"))

# and get new table
price_gsf_L2_tbl <- rbind(price_gsf_L1_tbl,
                          price_gsf_L1_tbl_inc)
# # check in-sample fit;
# # TBD: REMOVE IN FINAL VERSION, REPLACE WITH k-fold cross-validation
# train_nycp %>%
#     filter(gross_square_feet >= 10 | total_units > 0) %>%
#     left_join(price_gsf_L2_tbl, by = c("borough",
#                                        "building_class_at_time_of_sale")) %>%
#     mutate(gross_square_feet = ifelse(gross_square_feet >= 10,
#                                       gross_square_feet,
#                                       predict(train_bbc_L1_inc, newdata = .))) %>%
#     mutate(sale_price_hat = gross_square_feet * price_per_gsf_harm) %>%
#     mutate(perdiff = (sale_price_hat/sale_price - 1)) %>%
#     select(borough, building_class_at_time_of_sale, gross_square_feet,
#            sale_price, sale_price_hat, perdiff) %>% 
#     filter(!between(perdiff, -2, 2)) %>%
#     filter(building_class_at_time_of_sale %in%
#                c("A1",  "A5", "A9", "B2", "B3", "B9", "C0", "C3")) %>%
#     ggplot(aes(x = perdiff)) +
#     geom_histogram(bins = 40, col = "black") +
#     facet_grid(borough ~ building_class_at_time_of_sale, scale = "free")
# # -> most extreme differences are in A5, B9, C0, B2 
# 
# # investigate these
# largest_L2_diffs <- train_nycp %>%
#     filter(gross_square_feet >= 10 | total_units > 0) %>%
#     left_join(price_gsf_L2_tbl, by = c("borough",
#                                        "building_class_at_time_of_sale")) %>%
#     mutate(gsf_hat = ifelse(gross_square_feet >= 10,
#                              gross_square_feet,
#                              predict(train_bbc_L1_inc, newdata = .))) %>%
#     mutate(sale_price_hat = gsf_hat * price_per_gsf_harm) %>%
#     mutate(perdiff = (sale_price_hat/sale_price - 1)) %>%
#     select(borough, building_class_at_time_of_sale, 
#            gross_square_feet, gsf_hat,
#            sale_price, sale_price_hat, perdiff) %>% 
#     filter(!between(perdiff, -2, 2)) %>%
#     filter(building_class_at_time_of_sale %in%
#                c("A1",  "A5", "A9", "B2", "B3", "B9", "C0", "C3")) %>%
#     arrange(desc(abs(perdiff)))
# # -> large differences come from properties with small sale prices
# 
# #    see more clearly on this plot
# largest_L2_diffs %>%
#     ggplot(aes(x = sale_price, y = perdiff)) +
#     scale_x_log10() +
#     scale_y_log10() +
#     geom_point()
# 
#     
# # check out-of-sample fit; 
# # TBD: REMOVE IN FINAL VERSION
# test_nycp %>%
#     filter(gross_square_feet >= 10 | total_units > 0) %>%
#     inner_join(price_gsf_L2_tbl, by = c("borough",
#                                         "building_class_at_time_of_sale")) %>%
#     mutate(gross_square_feet = ifelse(gross_square_feet >= 10,
#                                       gross_square_feet,
#                                       predict(train_bbc_L1_inc, newdata = .))) %>%
#     mutate(sale_price_hat =  gross_square_feet * price_per_gsf_harm) %>%
#     mutate(perdiff = (sale_price_hat/sale_price - 1)) %>%
#     select(borough, building_class_at_time_of_sale, sale_price,
#            sale_price_hat, perdiff) %>% 
#     summarise(RMSE_avg = mean(perdiff),
#               RMSE_med = median(perdiff))

################################################################################
# 3.5.3.3 Neither square footage available (<10) nor total_units (==0)
################################################################################

# Step 3: Determine borough/building class at TOS combinations for
#         which gross ft^2 < 10 and total_units == 0 
#         For these, approximate price by median price
#         of borough/building class combination
train_nycp %>%
    anti_join(price_gsf_L2_tbl, by = c("borough",
                                     "building_class_at_time_of_sale")) %>%
    count(borough, building_class_at_time_of_sale) %>% 
    arrange(desc(n)) %>% print(n = 100)

# compile table of borough / bldg class at TOS for Step 3
bbc_L2_inc_tbl <- train_nycp %>%
    count(borough, building_class_at_time_of_sale)

# ensure that combos from test set also covered
bbc_L2_inc_tbl_add <- test_nycp %>%
    count(borough, building_class_at_time_of_sale)

# merge the two tables
bbc_L3_tbl <- full_join(bbc_L2_inc_tbl, bbc_L2_inc_tbl_add,
                  by = c("borough", "building_class_at_time_of_sale"))
rm(bbc_L2_inc_tbl, bbc_L2_inc_tbl_add)

# check if now: 
# (1) contains combos from train set that are not in price_gsf_L2_tbl
#    -> should return empty set
# (2) contains combos from test set that are not in price_gsf_L2_tbl
#    -> should return empty set
train_nycp %>%
    select(borough, building_class_at_time_of_sale) %>%
    anti_join(price_gsf_L2_tbl, by = c("borough",
                                       "building_class_at_time_of_sale")) %>%
    anti_join(bbc_L3_tbl, by = c("borough",
                                       "building_class_at_time_of_sale"))
test_nycp %>%
    select(borough, building_class_at_time_of_sale) %>%
    anti_join(price_gsf_L2_tbl, by = c("borough",
                                       "building_class_at_time_of_sale")) %>%
    anti_join(bbc_L3_tbl, by = c("borough","building_class_at_time_of_sale"))
# That all checks out, so can move on

# check if any b/b combos in bbc_L2_inc_tbl NOT in train_nycp
bbc_L3_tbl %>%
    anti_join(train_nycp, by = c("borough","building_class_at_time_of_sale"))
    
# Then determine price for these combinations as average sale_price,
# Determine average using k-fold cross-validation
rmse_v_L3 <- map_df(1:length(index_list), function(listIdx) {
    # split data into training and test set
    train_set <- train_nycp[-index_list[[listIdx]], ]
    test_set <- train_nycp[index_list[[listIdx]], ]
    
    # calculate different average prices on training set
    price_L3_tbl <- train_set %>%
        left_join(bbc_L3_tbl, 
                   by = c("borough","building_class_at_time_of_sale")) %>%
        group_by(borough, building_class_at_time_of_sale) %>%
        summarise(nprices = n(),
                  price_hat = mean(sale_price),
                  price_hat_w = log10mean(sale_price),
                  price_hat_med = median(sale_price))
    
    # calculate RMSE in test set, ONLY FOR THOSE ROWS for which
    # square footage is neither directly available (>=10) nor
    # can be approximated (total_units > 0)
    predictions <- test_set %>%
        filter(gross_square_feet < 10 & total_units == 0) %>%
        inner_join(price_L3_tbl,
                   by = c("borough","building_class_at_time_of_sale")) %>%
        mutate(perdiff = price_hat / sale_price - 1,
               perdiff_w = price_hat_w / sale_price - 1,
               perdiff_med = price_hat_med / sale_price - 1)
    predictions %>%
        summarise(RMSE_avg = mean(perdiff),
                  RMSE_avg_w = mean(perdiff_w),
                  RMSE_avg_med = mean(perdiff_med),
                  RMSE_med = median(perdiff),
                  RMSE_med_w = median(perdiff_w),
                  RMSE_med_med = median(perdiff_med))
    })

# evaluate results 
rmse_v_L3 %>% summarise_all(mean)
rmse_v_L3 %>% summarise_all(sd)

# STEP 3 DECISION:
#-> take the MEDIAN to calculate prices where sq ft not available

price_L3_tbl <- train_nycp %>%
    left_join(bbc_L3_tbl, 
              by = c("borough","building_class_at_time_of_sale")) %>%
    group_by(borough, building_class_at_time_of_sale) %>%
    summarise(nprices = n(),
              price_hat_med = median(sale_price))

# remove temporary variables
rm(train_set, test_set, predictions)

################################################################################
# 3.5.3.4 Extrapolate Square Foot Table - for same building class
################################################################################

# check which b/bc combos are not in price_gsf_L2_tbl,
# but in overall combination of borough/building class
bldg_classes <- 
    sort(unique(nycproperty_raw$building_class_at_time_of_sale))
borough_bldg_class = expand.grid(borough = factor(c(1,2,3,4,5)), 
                           building_class_at_time_of_sale = bldg_classes)

# extend table of prices/sqft with all borough/bldg class combinations
price_gsf_L2_tbl_inc <-
    price_gsf_L2_tbl %>%
    right_join(borough_bldg_class, 
               by = c("borough","building_class_at_time_of_sale"))

# helper function to fill in $/sqft table
price_gsf_lookup <- function(price_tbl, building_class) {
    price_tbl %>%
        filter(building_class_at_time_of_sale == building_class &
                   !is.na(nprices)) %>%
        group_by(building_class_at_time_of_sale) %>%
        summarise(price_per_gsf_harm = sum(nprices*price_per_gsf_harm)/
                      sum(nprices))
    }

# initialise
price_gsf_L2_tbl_aug <- price_gsf_lookup(price_gsf_L2_tbl_inc,"A0")

# algo: for rows with nprices = NA, take average from other rows
# iterate over all building classes
for(bc in bldg_classes_only) {
    # determine number of rows needed to extrapolate
    nrows_needed <- price_gsf_L2_tbl_inc %>%
        filter(building_class_at_time_of_sale == bc &
                   is.na(nprices)) %>% nrow()
    # only do if any rows needed 
    if(nrows_needed > 0) {
        # determine how many rows are available
        nrows_available <- price_gsf_L2_tbl %>%
            filter(building_class_at_time_of_sale == bc) %>% nrow()
        # if none available, nothing will be done
        if(nrows_available > 0) {
            newrows <- price_gsf_L2_tbl_inc %>%
                filter(building_class_at_time_of_sale == bc &
                           is.na(nprices)) %>%
                do(price_gsf_lookup(price_tbl = price_gsf_L2_tbl,
                                    building_class = bc))
            
            if(bc == bldg_classes_only[1]) {
                price_gsf_L2_tbl_aug <- newrows
            } else {
                price_gsf_L2_tbl_aug <- rbind(price_gsf_L2_tbl_aug, newrows)
                }
            }
        }
    }

# merge the two tables
price_gsf_L2_tbl_inc <- price_gsf_L2_tbl_inc %>%
    left_join(price_gsf_L2_tbl_aug,
              by = c("borough", "building_class_at_time_of_sale")) %>%
    mutate(price_per_gsf_harm = ifelse(is.na(nprices), price_per_gsf_harm.y,
                                  price_per_gsf_harm.x)) %>%
    select(borough, building_class_at_time_of_sale, nprices,
           price_per_gsf_harm)

# augmented table no longer needed
rm(price_gsf_L2_tbl_aug)

################################################################################
# 3.5.3.5 Extrapolate Square Foot Table - for major building class
################################################################################

# that still leaves blocks without any price/sqft, look these up
bldg_classes_to_fill <- price_gsf_L2_tbl_inc %>%
    filter(is.na(price_per_gsf_harm)) %>% 
    group_by(building_class_at_time_of_sale) %>% 
    summarise(n = n()) %>% .$building_class_at_time_of_sale %>% sort()

# second helper function to fill in price/sqft
price_gsf_fill <- function(price_tbl, bldg_class) {
    # extract main class
    main_class = str_sub(bldg_class, 1, 1)
    # take average price over all prices/sqft with that main class
    res <- price_tbl %>%
        mutate(mainclass = str_sub(building_class_at_time_of_sale,1,1)) %>%
        filter(mainclass == main_class) %>%
        group_by(mainclass) %>%
        summarise(price_per_gsf_harm = sum(nprices*price_per_gsf_harm)/
                      sum(nprices)) %>%
        select(-mainclass)
    res <- res %>% 
        mutate(building_class_at_time_of_sale = bldg_class)
    return(res)
    }

# initialise
price_gsf_L2_tbl_aug <- price_gsf_fill(price_gsf_L2_tbl, 
                                       bldg_classes_to_fill[1])

# iterate over all building classes
for(bc in bldg_classes_to_fill) {
    # filter for only building class and then apply function to fill
    newrows <- price_gsf_L2_tbl_inc %>%
        filter(building_class_at_time_of_sale == bc) %>%
        do(price_gsf_fill(price_tbl = price_gsf_L2_tbl,
                            bldg_class = bc))
    
    # for first row assign to augmented table, otherwise extend
    # the augmented table
    if(bc == bldg_classes_to_fill[1]) {
        price_gsf_L2_tbl_aug <- newrows
    } else {
        price_gsf_L2_tbl_aug <- rbind(price_gsf_L2_tbl_aug, newrows)
        }
    }

# warnings -> check which ones not filled
setdiff(bldg_classes_to_fill, 
        price_gsf_L2_tbl_aug %>%
            group_by(building_class_at_time_of_sale) %>%
            distinct(building_class_at_time_of_sale) %>% 
            .$building_class_at_time_of_sale)
# So T2, U1, U6

# Anything in training set?
train_nycp %>%
    filter(building_class_at_time_of_sale %in% c("T2", "U1", "U6")) %>% View()
# One data row, not worth bothering

# Anything in test set?
test_nycp %>%
    filter(building_class_at_time_of_sale %in% c("T2", "U1", "U6"))
# Nothing, so no bother

# merge the two tables again
price_gsf_L2_tbl_inc <- price_gsf_L2_tbl_inc %>%
    left_join(price_gsf_L2_tbl_aug,
              by = c("borough", "building_class_at_time_of_sale")) %>%
    mutate(price_per_gsf_harm = ifelse(!building_class_at_time_of_sale %in%
                                           bldg_classes_to_fill, 
                                       price_per_gsf_harm.x,
                                       price_per_gsf_harm.y)) %>%
    select(borough, building_class_at_time_of_sale, nprices, 
           price_per_gsf_harm) 

# discard temporary table
rm(price_gsf_L2_tbl_aug)

################################################################################
# 3.5.3.6 Define Prediction Function
################################################################################

#' Function to predict prices hierarchically
#' 
#' @param newdata Property data for which to predict sale price
#' @param price_gsf_tbl Table of prices per gross sq ft per
#' borough / building class combination
#' @param model_gsqf_totalunits Model to predict gross square feet from number
#' of total units (total_units)
#' @param price_tbl Table of prices per borough / building class combination 
predict_price_sqf <- function(newdata,
                              price_gsf_tbl,
                              model_gsqf_totalunits,
                              price_tbl) {
    newdata %>%
        left_join(price_gsf_tbl, 
                  by = c("borough", "building_class_at_time_of_sale")) %>%
        mutate(method = ifelse(gross_square_feet >= 10, "gsf",
                               ifelse(total_units > 0, "gsf_hat", "price"))) %>% 
        mutate(gsf = ifelse(gross_square_feet >= 10,
                            gross_square_feet,
                            ifelse(total_units > 0, 
                                   predict(model_gsqf_totalunits, newdata = .),
                                   NA))) %>% 
        left_join(price_tbl, 
                  by = c("borough", "building_class_at_time_of_sale")) %>% 
        mutate(sale_price_hat = 
                   ifelse(method %in% c("gsf", "gsf_hat"),
                          gsf * price_per_gsf_harm,
                          price_hat_med),
               perdiff = sale_price_hat/sale_price - 1) %>%
        .$sale_price_hat
    }

# res_ec_test %>%
#     gather(avg_type, per_diff, perdiff:perdiff_harm) %>%
#     filter(between(per_diff, -4, 4)) %>%
#     ggplot(aes(x = per_diff, fill = method)) +
#     geom_density(bw = 0.05, col = "black") +
#     facet_grid(method ~ avg_type, scales = "free")

# prediction function

# predict prices, with class median
price_hat_sqf <- predict_price_sqf(test_nycp,
                             price_gsf_tbl = price_gsf_L2_tbl_inc,
                             model_gsqf_totalunits = train_bbc_L1_inc,
                             price_tbl = price_L3_tbl)
# calculate RMSE and store in results table
rmse_sqf <- RMSE_price(price_hat_sqf, test_nycp$sale_price)

################################################################################
# 3.5.4 Additional Effect - Building Class Category
################################################################################

################################################################################
# 3.5.5 Additional Effect - Location (ZIP Code)
################################################################################

################################################################################
# 3.5.6 Additional Effect - Year Built
################################################################################

################################################################################
# 3.5.7 Additional Effect - Month Sold (Seasonality)
################################################################################


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

# save on demand
save.image(file = "nycproperty_20200529.Rdata")

# load on demand
load(file = "nycproperty_20200529.Rdata")

