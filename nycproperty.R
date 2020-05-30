################################################################################
# 0.1 Loading of libraries
################################################################################
if(!require(tidyverse))install.packages("tidyverse")
if(!require(ggplot2))install.packages("ggplot2")
if(!require(gridExtra))install.packages("gridExtra")
if(!require(caret))install.packages("caret")
if(!require(readr))install.packages("readr")
if(!require(purrr))install.packages("purrr")
if(!require(lubridate))install.packages("lubridate")
if(!require(testit))install.packages("testit")
if(!require(gam))install.packages("gam")


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
#' 
#' @param x Vector to calculate harmonic mean of
#' @return Harmonic mean of x
harm.mean <- function(x) 1/mean(1/x)

#' Geometric mean
#' 
#' @param x Vector to calculate harmonic mean of
#' @return Harmonic mean of x
log10mean <- function(x) 10^mean(log10(x))

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

## discard temporary variables
rm(test_index)

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

## save to file
save(train_nycp, test_nycp, file = "data/nycp_split_clean.Rdata")
# load on demand
# load(file = "data/nycp_split_clean.Rdata")

################################################################################
# 3.2.3 Basic Data Structure
################################################################################

# 3.2.3.1 Borough
train_nycp %>% count(borough)
# only 5 different values, most in boroughs 4 and 3

# 3.2.3.2 Neighbourhood
# any NAs?
sum(is.na(train_nycp$neighborhood))
# -> no NAs

# determine distinct values
length(unique(train_nycp$neighborhood))
# -> 253 different neighbourhoods, quite granular, leave for now

# 3.2.3.3 Building class category
train_nycp %>% count(building_class_category) %>%
    arrange(desc(n)) %>% 
    mutate(cumsum = cumsum(n/sum(n))) %>%
    print(n = 100)

# 3.2.3.4 Tax class at present
train_nycp %>% count(tax_class_at_present) %>% arrange(desc(n)) %>%
    mutate(cumsum = cumsum(n/sum(n)))

# 3.2.3.5 Block
length(unique(train_nycp$block))
# -> around 10k different values, too many

# 3.2.3.6 Lot
length(unique(train_nycp$lot))
# -> around 2k different values, too many

# 3.2.3.7 Building class at present
train_nycp %>% count(building_class_at_present) %>% 
    arrange(desc(n)) %>%
    mutate(cumsum = cumsum(n/sum(n))) %>%
    print(n = 200)
# how many NAs?
mean(is.na(train_nycp$building_class_at_present))

# 3.2.3.8 ZIP code
train_nycp %>% count(zip_code) %>% 
    arrange(desc(n)) %>%
    mutate(cumsum = cumsum(n/sum(n)))
# zeroes?
mean(train_nycp$zip_code == 0)


# 3.2.3.9 Resi units
class(train_nycp$residential_units)
# -> already a number

# 3.2.3.10 Commercial units
class(train_nycp$commercial_units)
# -> already a number

# 3.2.3.11 Total units
class(train_nycp$total_units)
# -> already a number

# check adding of residential and commercial to total
train_nycp %>%
    mutate(tunits = residential_units + commercial_units) %>%
    filter(total_units != tunits) %>%
    select(borough, block, lot, total_units, tunits, 
           residential_units, commercial_units) %>%
    group_by(total_units, tunits) %>%
    summarise(n = n())

# 3.2.3.12 Land square feet, look at sample
set.seed(32311, sample.kind = "Rounding")
train_nycp %>% sample_n(size = 100) %>% .$land_square_feet

# 3.2.3.13 Gross square feet, look at sample
set.seed(32311, sample.kind = "Rounding")
train_nycp %>% sample_n(size = 100) %>% .$gross_square_feet

# Addition: Net Square footage
train_nycp %>%
    mutate(gross_square_feet = str_replace(gross_square_feet,"-","0"),
           land_square_feet = str_replace(land_square_feet,"-","0")) %>%
    mutate(gross_square_feet = parse_number(gross_square_feet),
           land_square_feet = parse_number(land_square_feet)) %>%
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
    mutate(gross_square_feet = str_replace(gross_square_feet,"-","0"),
           land_square_feet = str_replace(land_square_feet,"-","0")) %>%
    mutate(gross_square_feet = parse_number(gross_square_feet),
           land_square_feet = parse_number(land_square_feet)) %>%
    mutate(net_square_feet = gross_square_feet -
               land_square_feet) %>%
    mutate(net_sqf_avail = (net_square_feet > 0),
           gross_sqf_avail = (gross_square_feet >= 10),
           land_sqf_avail = (land_square_feet >= 10),
           sale_price_avail = (sale_price > 0)) %>%
    group_by(sale_price_avail, net_sqf_avail,
             gross_sqf_avail, land_sqf_avail) %>%
    summarise(n = n())

# 3.2.3.14 Year build
class(train_nycp$year_build)

# 3.2.3.15 Tax class at time of sale -> see 3.2.3.4

# 3.2.3.16 Building class at time of sale
train_nycp %>%
    mutate(gen_class_at_time_of_sale = str_sub(building_class_at_time_of_sale,
                                               start = 1, end = 1)) %>%
    count(gen_class_at_time_of_sale) %>% 
    arrange(desc(n)) %>%
    mutate(cumsum = cumsum(n/sum(n))) %>%
    print(n = 26)
# -> most accounted for by general classes A, R, D, B, C,
# covering over 94%
# to get to 99%, need to add S, V, K, G, O

# 3.2.3.17 Sale date
class(train_nycp$sale_date)

# 3.2.3.18 Sale price
price_thresholds <- c(0,10^seq(0,5))
tbl_price_below <- map_df(price_thresholds, 
                     function(x) {
                         list(threshold = x,
                              percentage_below = 
                                  mean(train_nycp$sale_price <= x))
                         })
# -> so only 2/3 of sales have a positive price!
plot(tbl_price_below, log = "x",
     main = "Percentage of prices below given threshold",
     xlab = "Threshold in $",
     ylab = "Percentage of prices below Threshold.")
abline(v = c(tbl_price_below$threshold), lty = "dotted")
abline(h = c(tbl_price_below$percentage_below), lty = "dotted")
rm(price_thresholds, tbl_price_below)

################################################################################
# 3.3 Data cleaning
################################################################################

#' Auxiliary function to convert data column in to factor consistently
#' for training and test set
#' 
#' @param train Training set
#' @param test Test set
#' @param column_factor Column to convert into factor
#' @param  column_order = NA Optional column used to reorder the factor 
#' column_factor
#' @param order_fct = NA Optional column specifying the function applied
#' for reordering the column_factor
#' @return List of data frames named `train` and `test` containing the
#'         training and test sets with factors having the same levels
#'         and order
apply_factor_trainandtest <- function(train, test, column_factor, 
                                      column_order = NA, order_fct = NA) {
    # First for training data: 
    # 1) convert to factor in-place
    res_train <- train %>%
        mutate(!!quo_name(column_factor) := factor(!!sym(column_factor)))
    
    # 2) Reorder factor if requested
    if(!is.na(column_order))
        res_train <- res_train %>%
            mutate(!!quo_name(column_factor) := 
                       reorder(!!sym(column_factor),
                               !!sym(column_order), order_fct))
    
    # retrive levels from training data
    f_levels <- res_train %>% pull(!!sym(column_factor)) %>% levels()
    
    # Then for test data
    # 1) Convert to factor; NB: Take levels from training set
    res_test <- test %>%
        mutate(!!quo_name(column_factor) := 
                   factor(!!sym(column_factor), levels = f_levels))
    
    
    return(list(train = res_train, test = res_test))
    }

# 3.3.0 Sale price - elementary conversion
set.seed(330, sample.kind = "Rounding")
sample(train_nycp$sale_price, size = 100)

# -> convert "-" to "0" and parse as number
train_nycp <- train_nycp %>%
    mutate(sale_price = str_replace_all(sale_price,"-", "0")) %>%
    mutate(sale_price = parse_number(sale_price))
test_nycp <- test_nycp %>%
    mutate(sale_price = str_replace_all(sale_price,"-", "0")) %>%
    mutate(sale_price = parse_number(sale_price))

# 3.3.1 Convert borough into factor
df_temp <- 
    apply_factor_trainandtest(train_nycp, test_nycp, "borough")
train_nycp <- df_temp$train
test_nycp <- df_temp$test
rm(df_temp)

# 3.3.2 Neighbourhood, convert to factor,ordered by sale price
df_temp <- 
    apply_factor_trainandtest(train_nycp, test_nycp, "neighborhood",
                              "sale_price", median)
train_nycp <- df_temp$train
test_nycp <- df_temp$test
rm(df_temp)

# 3.3.3 Building Class Category, convert to factor, also ordered by price
df_temp <- 
    apply_factor_trainandtest(train_nycp, test_nycp, "building_class_category",
                              "sale_price", median)
train_nycp <- df_temp$train
test_nycp <- df_temp$test
rm(df_temp)

# 3.3.4 Tax class, convert to factor for good measure
df_temp <- 
    apply_factor_trainandtest(train_nycp, test_nycp, "tax_class_at_present",
                              "sale_price", median)
train_nycp <- df_temp$train
test_nycp <- df_temp$test
rm(df_temp)

# 3.3.5 Block - already numeric, and too many different values, see 3.2.5
# 3.3.6 Lot - already numeric, and too many different values, see 3.2.6

# 3.3.7 Building class at present, convert to factor ordered by price
df_temp <- 
    apply_factor_trainandtest(train_nycp, test_nycp, 
                              "building_class_at_present",
                              "sale_price", median)
train_nycp <- df_temp$train
test_nycp <- df_temp$test
rm(df_temp)

# 3.3.8 ZIP code, convert to factor, also ordered by price
df_temp <- 
    apply_factor_trainandtest(train_nycp, test_nycp, "zip_code",
                              "sale_price", median)
train_nycp <- df_temp$train
test_nycp <- df_temp$test
rm(df_temp)

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

# 3.3.14 year built -> convert to decade, discard year
train_nycp <- train_nycp %>%
    rename(decade_built = year_built) %>%
    mutate(decade_built = floor(decade_built/10)*10)
test_nycp <- test_nycp %>%
    rename(decade_built = year_built) %>%
    mutate(decade_built = floor(decade_built/10)*10)


# 3.3.15 Tax class at time of sale, convert to factor ordered by price
df_temp <- 
    apply_factor_trainandtest(train_nycp, test_nycp, 
                              "tax_class_at_time_of_sale",
                              "sale_price", median)
train_nycp <- df_temp$train
test_nycp <- df_temp$test
rm(df_temp)

# 3.3.16 Building class at time of sale, convert to factor ordered by price
df_temp <- 
    apply_factor_trainandtest(train_nycp, test_nycp, 
                              "building_class_at_time_of_sale",
                              "sale_price", median)
train_nycp <- df_temp$train
test_nycp <- df_temp$test
rm(df_temp)

# 3.3.17 sale date -> get sale month
train_nycp <- train_nycp %>%
    mutate(sale_month = month(sale_date))
test_nycp <- test_nycp %>%
    mutate(sale_month = month(sale_date))


# 3.3.18 Sale price - further cleaning
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

# From the above and 3.2.18 it does not make sense to keep sale prices at
# or below $1,000, they are as good as zero
train_nycp <- train_nycp %>%
    filter(sale_price > 10000)
# same for test data
test_nycp <- test_nycp %>%
    filter(sale_price > 10000)

# Calculate simple mean and transformed mean
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

# discard temporary variables
rm(p1, p2, mu_smp, mu_trf, sale_price_hi, sale_price_lo)

# 3.3.19 Check if any (borough, bldg class at time of sale) 
# combinations in test not in train set
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
# load on demand
# load(file = "data/nycp_split_trans.Rdata")

################################################################################
# 3.4 Data Exploration and Visualisation
################################################################################

# Define constants for square footage - below 10 sqft area is not meaningful
FOOTAGE_MIN <- 10


################################################################################
# 3.4.1 Borough
################################################################################

# plot distribution of square foot prices by borough
train_nycp %>%
    filter(gross_square_feet >= FOOTAGE_MIN) %>%
    mutate(price_per_gsf = sale_price / gross_square_feet) %>%
    ggplot(aes(x = borough, y = price_per_gsf)) +
    scale_y_log10() +
    geom_boxplot()

# -> borough 2 has lower sqft-prices than borough 1,
#    but sqft-price ranges for boroughs 1, 3, 4, and 5 overlap

################################################################################
# 3.4.2 Building class Category
################################################################################

train_nycp %>%
    filter(gross_square_feet >= FOOTAGE_MIN) %>%
    mutate(price_per_gsf = sale_price / gross_square_feet) %>%
    mutate(building_class_category = 
               reorder(building_class_category, price_per_gsf, median)) %>%
    ggplot(aes(x = building_class_category,
               y = price_per_gsf)) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    scale_y_log10() +
    geom_boxplot()

# -> category 10 (coops elevator apartments) and 9 (coops walkup apartments)
#    are somewhat distinguished from the other classes, but all the other
#    classes have large overlaps

################################################################################
# 3.4.2 ZIP Code
################################################################################
train_nycp %>%
    filter(gross_square_feet >= FOOTAGE_MIN) %>%
    filter(zip_code != 0) %>%
    mutate(price_per_gsf = sale_price / gross_square_feet) %>%
    mutate(zip_code = reorder(zip_code, price_per_gsf, median)) %>%
    ggplot(aes(x = zip_code, y = price_per_gsf)) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    scale_y_log10() +
    geom_boxplot()

        
################################################################################
# 3.4.3 Year built - decade
################################################################################

train_nycp %>%
    filter(gross_square_feet >= FOOTAGE_MIN) %>%
    filter(decade_built > 0) %>%
    mutate(price_per_gsf = sale_price / gross_square_feet) %>%
    mutate(decade_built = reorder(decade_built, price_per_gsf, median)) %>%
    ggplot(aes(x = decade_built, y = price_per_gsf)) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    scale_y_log10() +
    geom_boxplot()

################################################################################
# 3.4.4 Month of sale
################################################################################

train_nycp %>%
    filter(gross_square_feet >= FOOTAGE_MIN) %>%
    mutate(price_per_gsf = sale_price / gross_square_feet) %>%
    mutate(sale_month = reorder(sale_month, price_per_gsf, median)) %>%
    ggplot(aes(x = sale_month, y = price_per_gsf)) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    scale_y_log10() +
    geom_boxplot()


################################################################################
# 3.4.5 Building Class at Time of Sale
################################################################################

# most frequent building classes according to section 3.2.3.16 are
# A, R, D, B, C, followed by S, V, K, G, O

# distribution by building class, R, A, B, D, C and rest
train_nycp %>%
    filter(gross_square_feet >= FOOTAGE_MIN) %>%
    mutate(price_per_gsf = sale_price / gross_square_feet) %>%
    mutate(building_class_tos_clustered = 
               ifelse(str_sub(building_class_at_time_of_sale,1,1) %in% 
                          c("A", "R", "D", "B", "C"),
                      str_sub(building_class_at_time_of_sale,1,1), "Other")) %>%
    mutate(building_class_tos_clustered = 
               reorder(building_class_tos_clustered, price_per_gsf, median)) %>%
    ggplot(aes(x = factor(building_class_tos_clustered),
               y = price_per_gsf)) +
    scale_y_log10() +
    geom_boxplot()

# -> building class R has lower prices than C, B, A and the other classes,
#    wherea all classes other than R overlap.

################################################################################
# 3.4.19 Net Square Footage
################################################################################

# 3.3.xx Net square feet
# overall distribution
# train_nycp %>%
#     mutate(net_square_feet = gross_square_feet - 
#                land_square_feet) %>%
#     select(gross_square_feet, land_square_feet, 
#            net_square_feet) %>%
#     filter(gross_square_feet > 0 &
#                land_square_feet > 0 &
#                net_square_feet > 0) %>%
#     gather(metric, footage) %>% 
#     ggplot() +
#     scale_x_log10() +
#     geom_histogram(aes(x = footage), bins = 50, col = "black") +
#     facet_grid(metric ~ ., scales = "free")
# 
# # availability in conjunction with sale prices
# train_nycp %>%
#     mutate(net_square_feet = gross_square_feet - 
#                land_square_feet) %>%
#     mutate(net_sqf_avail = (net_square_feet >= FOOTAGE_MIN),
#            gross_sqf_avail = (gross_square_feet >= FOOTAGE_MIN),
#            land_sqf_avail = (land_square_feet >= FOOTAGE_MIN),
#            sale_price_avail = (sale_price > 0)) %>%
#     group_by(sale_price_avail, net_sqf_avail,
#              gross_sqf_avail, land_sqf_avail) %>%
#     summarise(n = n())
# # -> so for half of data no square footage at all
# #    and from other half 2/3 (=16k) have no NET square footage
# 

################################################################################
# 3.5 Modelling
################################################################################
# percentage difference mean function
RMSE_price <- function(pred_price, actual_price) {
    mean(pred_price/actual_price - 1) }

# Generate list of training and test sets for k-fold cross-validation
# Use function createFolds with k = 25 
set.seed(342, sample.kind = "Rounding")
index_list <- createFolds(train_nycp$sale_price, k = 25)

################################################################################
# 3.5.1 Simple average - arith. mean, geom. mean, median
################################################################################

# for display
mu <- mean(train_nycp$sale_price)
mu
mu_g <- log10mean(train_nycp$sale_price)
mu_g
med <- median(train_nycp$sale_price)
med

# model to predict arithmetic average
predict_const_avg <- function(newdata) mean(train_nycp$sale_price)

# model to predict geometric average
predict_const_geom <- function(newdata) log10mean(train_nycp$sale_price)

# model to predict median
predict_const_med <- function(newdata) median(train_nycp$sale_price)

# temp variables no longed needed
rm(mu, mu_g, med)

################################################################################
# 3.5.2 Average of square foot prices
################################################################################

# Average of all prices per square foot - arithmetic
predict_sqf_const_avg <- function(newdata) {
    # fallback price: average price
    mu = mean(train_nycp$sale_price)
    
    # calculate average price per square foot
    price_per_sqf <- train_nycp %>%
        filter(gross_square_feet >= FOOTAGE_MIN) %>%
        summarise(price_per_sqf = sum(sale_price)/
                      sum(gross_square_feet)) %>%
        .$price_per_sqf
    
    # calculate sqf x price/sqf if sqf available, fall back to
    # mean price otherwise
    prices_hat <- newdata %>%
        mutate(price_hat = ifelse(gross_square_feet >= FOOTAGE_MIN,
                                  gross_square_feet * price_per_sqf,
                                  mu)) %>% .$price_hat
    
    return(prices_hat)
    }

# Average of all prices per square foot - harmonic average
# for square-foot price and geometric average of abs prices as fallback
predict_sqf_const_harm <- function(newdata) {
    # fallback price: gemetric mean price
    mu = log10mean(train_nycp$sale_price)
    
    # calculate harmonic average price per square foot
    price_per_sqf <- train_nycp %>%
        filter(gross_square_feet >= FOOTAGE_MIN) %>%
        summarise(price_per_sqf = harm.mean(sale_price/gross_square_feet)) %>%
        .$price_per_sqf
    
    # calculate sqf x price/sqf if sqf available, fall back to
    # mean price otherwise
    prices_hat <- newdata %>%
        mutate(price_hat = ifelse(gross_square_feet >= FOOTAGE_MIN,
                                  gross_square_feet * price_per_sqf,
                                  mu)) %>% .$price_hat
    
    return(prices_hat)
    }


################################################################################
# 3.5.3 Models based on price per square foot (gross)
################################################################################

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
    
    # compile table of average sqf prices by borough and building class
    ppgsf_L1_tbl <- train_set %>%
        select(borough, building_class_at_time_of_sale,
           gross_square_feet, sale_price) %>%
        filter(gross_square_feet >= FOOTAGE_MIN) %>%
        group_by(borough, building_class_at_time_of_sale) %>%
        summarise(nprices = n(),
                  price_per_gsf = sum(sale_price) / sum(gross_square_feet),
                  price_per_gsf_w = log10mean(sale_price) / 
                      log10mean(gross_square_feet),
                  price_per_gsf_med = median(sale_price/gross_square_feet),
                  price_per_gsf_harm = harm.mean(sale_price/gross_square_feet))
    
    # predict prices on test set using available (square footage) x (sqf price) 
    predictions <- test_set %>%
        filter(gross_square_feet >= FOOTAGE_MIN) %>%
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
                  RMSE_avg_harm = mean(perdiff_harm))
    })

# summarise RMSE for different average methods
rmse_v_L1 %>% summarise_all(mean)

# STEP 1 DECISION:
#-> take the HARMONIC MEAN to calculate price / gross sq ft
price_gsf_L1_tbl <- train_nycp %>%
    select(borough, building_class_at_time_of_sale,
           gross_square_feet, sale_price) %>%
    filter(gross_square_feet >= FOOTAGE_MIN) %>%
    group_by(borough, building_class_at_time_of_sale) %>%
    summarise(nprices = n(),
              price_per_gsf_harm = harm.mean(sale_price/gross_square_feet))

# RMSE vector no longer needed
rm(rmse_v_L1)

################################################################################
# 3.5.3.2 Square footage not available, but approximated by #total_units
################################################################################

# Step 2: Determine borough/building class at TOS combinations for
#         which total_units > 0 and gross ft^2 < FOOTAGE_MIN
#         For these, approximate gross square footage by LOESS
bbc_L1_inc_tbl <- train_nycp %>%
    filter(gross_square_feet < FOOTAGE_MIN & total_units > 0) %>%
    distinct(borough, building_class_at_time_of_sale)

# need the data for which both gross ft^2 and total units are
# available for LOESS
train_nycp_bbc_L1_inc <- train_nycp %>%
    semi_join(bbc_L1_inc_tbl, by = c("borough",
                                     "building_class_at_time_of_sale")) %>%
    filter(gross_square_feet >= FOOTAGE_MIN & total_units > 0)

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
set.seed(3532, sample.kind = "Rounding")
train_bbc_L1_inc <- train(gross_square_feet ~ total_units,
                          data = train_nycp_bbc_L1_inc,
                          method = "gamLoess",
                          tuneGrid = data.frame(degree = 1,
                                                span = seq(0.10, 0.70, 0.1)))

# save to file for report
save(train_bbc_L1_inc, file = "data/model_sqf_LOESS.Rdata")

# load(file = "data/model_sqf_LOESS.Rdata")
ggplot(train_bbc_L1_inc, highlight = TRUE)
train_bbc_L1_inc$bestTune

# DECISION 2: Use LOESS with span as given above

# plot fit
train_nycp_bbc_L1_inc %>%
    mutate(gsf_fitted = predict(train_bbc_L1_inc, newdata = .)) %>%
    ggplot(aes(x = total_units)) +
    scale_x_log10() +
    scale_y_log10() +
    geom_point(aes(y = gross_square_feet)) +
    geom_line(aes(y = gsf_fitted), col = "blue")


# Wrap-up activities for Step 2
# Take data with total units > 0 and gross_square_feet < FOOTAGE_MIN to
# a) put in proxy for gross square feet
# b) use that proxy to predict price/ft^2
price_gsf_L1_tbl_inc <- train_nycp %>%
    filter(gross_square_feet < FOOTAGE_MIN & total_units > 0) %>%
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

################################################################################
# 3.5.3.3 Neither square footage available (< FOOTAGE_MIN) nor total_units (==0)
################################################################################

# Step 3: Determine borough/building class at TOS combinations for
#         which gross ft^2 < FOOTAGE_MIN and total_units == 0 
#         For these, approximate price by average price
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

# check if any b/c combos in bbc_L2_inc_tbl NOT in train_nycp
bbc_L3_tbl %>%
    anti_join(train_nycp, by = c("borough","building_class_at_time_of_sale"))
# -> zero rows, so OK
    
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
    # square footage is neither directly available (>= FOOTAGE_MIN) nor
    # can be approximated (total_units > 0)
    predictions <- test_set %>%
        filter(gross_square_feet < FOOTAGE_MIN & total_units == 0) %>%
        inner_join(price_L3_tbl,
                   by = c("borough","building_class_at_time_of_sale")) %>%
        mutate(perdiff = price_hat / sale_price - 1,
               perdiff_w = price_hat_w / sale_price - 1,
               perdiff_med = price_hat_med / sale_price - 1)
    predictions %>%
        summarise(RMSE_avg = mean(perdiff),
                  RMSE_avg_w = mean(perdiff_w),
                  RMSE_avg_med = mean(perdiff_med))
    })

# evaluate results 
rmse_v_L3 %>% summarise_all(mean)

# STEP 3 DECISION:
#-> take the MEDIAN to calculate prices where sq ft not available

price_L3_tbl <- train_nycp %>%
    left_join(bbc_L3_tbl, 
              by = c("borough","building_class_at_time_of_sale")) %>%
    group_by(borough, building_class_at_time_of_sale) %>%
    summarise(nprices = n(),
              price_hat_med = median(sale_price))

# remove temporary variables
rm(rmse_v_L3)

################################################################################
# 3.5.3.4 Extrapolate Square Foot Table - for same building class
################################################################################

# check which b/bc combos are not in price_gsf_L2_tbl,
# but in overall combination of borough/building class
bldg_classes <- 
    sort(unique(nycproperty_raw$building_class_at_time_of_sale))
borough_bldg_class = expand.grid(borough = factor(c(1,2,3,4,5)), 
                           building_class_at_time_of_sale = bldg_classes)
borough_bldg_class <- borough_bldg_class %>%
    mutate(building_class_at_time_of_sale =
               factor(building_class_at_time_of_sale,
                      levels = 
                          levels(train_nycp$building_class_at_time_of_sale)))

# extend table of prices/sqft with all borough/bldg class combinations
price_gsf_L2_tbl_inc <-
    price_gsf_L2_tbl %>%
    right_join(borough_bldg_class, 
               by = c("borough","building_class_at_time_of_sale"))

#' Function to fill in $/sqft table,  again use HARMONIC mean,
#' in a weighted variant
#'
#' @param price_tbl Table of prices per square foot
#' @param building_class Building class to extend the table for, e.g. "A0"
#' @return Data frame with average price across available boroughs 
price_gsf_lookup <- function(price_tbl, building_class) {
    price_tbl %>%
        filter(building_class_at_time_of_sale == building_class &
                   !is.na(nprices)) %>%
        group_by(building_class_at_time_of_sale) %>%
        summarise(price_per_gsf_harm = sum(nprices)/
                      sum(nprices/price_per_gsf_harm))
    }

# initialise
price_gsf_L2_tbl_aug <- price_gsf_lookup(price_gsf_L2_tbl_inc,"A0")

# algo: for rows with nprices = NA, take average from other rows
# iterate over all building classes
for(bc in bldg_classes) {
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
            # applies the filling function above for all boroughs which 
            # do not have prices available for this building class bc
            newrows <- price_gsf_L2_tbl_inc %>%
                filter(building_class_at_time_of_sale == bc &
                           is.na(nprices)) %>%
                do(price_gsf_lookup(price_tbl = price_gsf_L2_tbl,
                                    building_class = bc))
            
            if(bc == bldg_classes[1]) {
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
rm(price_gsf_L2_tbl_aug, newrows, nrows_available, nrows_needed)

################################################################################
# 3.5.3.5 Extrapolate Square Foot Table - for major building class
################################################################################

# that still leaves blocks without any price/sqft, look these up
bldg_classes_to_fill <- price_gsf_L2_tbl_inc %>%
    filter(is.na(price_per_gsf_harm)) %>% 
    group_by(building_class_at_time_of_sale) %>% 
    summarise(n = n()) %>% .$building_class_at_time_of_sale %>% sort()

#' second helper function to fill in price/sqft using HARMONIC mean
#'
#' @param price_tbl Table of prices per square foot
#' @param bldg_class Building class to extend the table for, e.g. "A0"
#' @return Data frame of average price across major building class
price_gsf_fill <- function(price_tbl, bldg_class) {
    # extract main class
    main_class = str_sub(bldg_class, 1, 1)
    # take average price over all prices/sqft with that main class
    res <- price_tbl %>%
        mutate(mainclass = str_sub(building_class_at_time_of_sale,1,1)) %>%
        filter(mainclass == main_class) %>%
        group_by(mainclass) %>%
        summarise(price_per_gsf_harm = sum(nprices)/
                      sum(nprices/price_per_gsf_harm)) %>%
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
    filter(building_class_at_time_of_sale %in% c("T2", "U1", "U6"))
# -> One data row, not worth bothering

# Anything in test set?
test_nycp %>%
    filter(building_class_at_time_of_sale %in% c("T2", "U1", "U6"))
# -> Nothing, so no need to do anything

# merge the two tables again
price_gsf_L2_tbl_final <- price_gsf_L2_tbl_inc %>%
    left_join(price_gsf_L2_tbl_aug,
              by = c("borough", "building_class_at_time_of_sale")) %>%
    mutate(price_per_gsf_harm = ifelse(!building_class_at_time_of_sale %in%
                                           bldg_classes_to_fill, 
                                       price_per_gsf_harm.x,
                                       price_per_gsf_harm.y)) %>%
    select(borough, building_class_at_time_of_sale, nprices, 
           price_per_gsf_harm) 

# discard temporary table
rm(price_gsf_L2_tbl_aug, newrows, bc, bldg_classes_to_fill)

# Save all objects needed for prediction to file
save(price_gsf_L2_tbl_final, train_bbc_L1_inc, price_L3_tbl,
     file = "data/model_gsf.Rdata")

# discard all other temporary objects
rm(bbc_L1_inc_tbl, bbc_L3_tbl, borough_bldg_class, price_gsf_L1_tbl,
   price_gsf_L1_tbl_inc, price_gsf_L2_tbl, price_gsf_L2_tbl_inc,
   train_nycp_bbc_L1_inc)

################################################################################
# 3.5.3.6 Define Prediction Function
################################################################################

#' Function to predict prices using cascade of methods
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
        mutate(method = ifelse(gross_square_feet >= FOOTAGE_MIN, "gsf",
                               ifelse(total_units > 0, "gsf_hat", "price"))) %>% 
        mutate(gsf = ifelse(gross_square_feet >= FOOTAGE_MIN,
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




################################################################################
# 4 Results
################################################################################

################################################################################
# 4.1 Constant absolute price
################################################################################

# predict prices
price_hat_avg <- predict_const_avg(test_nycp)
# calculate RMSE and store in results table
rmse_const_avg <- RMSE_price(price_hat_avg, test_nycp$sale_price)
rmse_results <- tibble(Method = "Simple arithmetic average price",
                       AvgPerDiff = rmse_const_avg)

# predict prices
price_hat_med <- predict_const_med(test_nycp)
# calculate RMSE and store in results table
rmse_const_med <- RMSE_price(price_hat_med, test_nycp$sale_price)
rmse_results <- rbind(rmse_results, 
                      tibble(Method = "Simple median price",
                             AvgPerDiff = rmse_const_med))

################################################################################
# 4.2 Constant sqf price
################################################################################
# predict prices, with arithmetic avg for both sqf price and fallback
price_hat_sqf_const_avg <- predict_sqf_const_avg(test_nycp)
# calculate RMSE and store in results table
rmse_sqf_const_avg <- RMSE_price(price_hat_sqf_const_avg, test_nycp$sale_price)
rmse_results <- rbind(rmse_results, 
                      tibble(Method = "Square-foot price arithmetic average",
                             AvgPerDiff = rmse_sqf_const_avg))

# predict prices, with harmonic avg for sqf price and geom mean as fallback
price_hat_sqf_const_harm <- predict_sqf_const_harm(test_nycp)
# calculate RMSE and store in results table
rmse_sqf_const_harm <- RMSE_price(price_hat_sqf_const_harm, 
                                  test_nycp$sale_price)
rmse_results <- rbind(rmse_results, 
                      tibble(Method = "Square-foot price harmonic average",
                             AvgPerDiff = rmse_sqf_const_harm))

################################################################################
# 4.3 Sqf price by borough and building class, cascading
################################################################################

# predict prices, with class median
price_hat_sqf <- predict_price_sqf(test_nycp,
                                   price_gsf_tbl = price_gsf_L2_tbl_final,
                                   model_gsqf_totalunits = train_bbc_L1_inc,
                                   price_tbl = price_L3_tbl)
# calculate RMSE and store in results table
rmse_sqf <- RMSE_price(price_hat_sqf, test_nycp$sale_price)
rmse_results <- 
    rbind(rmse_results,
          tibble(Method = "Cascading sqft price by borough and building class",
                             AvgPerDiff = rmse_sqf))

# display table of results
rmse_results %>% knitr::kable()

# Examine final results
test_nycp %>%
    left_join(price_gsf_L2_tbl_final, 
              by = c("borough", "building_class_at_time_of_sale")) %>%
    mutate(method = ifelse(gross_square_feet >= FOOTAGE_MIN, "gsf",
                           ifelse(total_units > 0, "gsf_hat", "price"))) %>% 
    mutate(gsf = ifelse(gross_square_feet >= FOOTAGE_MIN,
                        gross_square_feet,
                        ifelse(total_units > 0, 
                               predict(train_bbc_L1_inc, newdata = .),
                               NA))) %>% 
    left_join(price_L3_tbl, 
              by = c("borough", "building_class_at_time_of_sale")) %>% 
    mutate(sale_price_hat = 
               ifelse(method %in% c("gsf", "gsf_hat"),
                      gsf * price_per_gsf_harm,
                      price_hat_med),
           perdiff = sale_price_hat/sale_price - 1) %>%
    mutate(method = factor(method)) %>%
    filter(!between(perdiff, -1, 1)) %>%
    ggplot(aes(x = sale_price, y = perdiff, col = method)) +
    scale_x_log10() +
    scale_y_log10() +
    geom_point() +
    labs(title = "Price differences by sale price and cascade method",
         x = "Sale Price [$] (log scale)",
         y = "Percentage difference (log scale)")
    
# Average percentage differences for prices >100k
test_nycp %>%
    mutate(sale_price_hat = price_hat_sqf) %>% 
    mutate(perdiff = (sale_price_hat/sale_price - 1)) %>%
    filter(sale_price > 100000) %>%
    summarise(perdiff = mean(perdiff))



################################################################################
################################################################################
# Saving progress, for working versions, uncomment as needed
################################################################################
################################################################################

# save on demand
#save.image(file = "nycproperty_20200530.Rdata")

# load on demand
#load(file = "nycproperty_20200530.Rdata")

