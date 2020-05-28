library(tools)
library(testit)

# Toy example step by step

# 0) Init: partially overlapping training and test set (first in temp)
train = data.frame(n = seq(1,4),
                   a = c(1, 2, 3, 4))

temp = data.frame(n = seq(1,4),
                  a = c(2, 3, 4, 5))
print("training and  temporary test set:"); print(train); print(temp)
print("Total number of rows: "); print(nrow(train)+nrow(temp))

# 1) Only keep values in test set that are also in training set
#    -> respective rows are removed from test, but still in temp
test <- temp %>% 
    semi_join(train, by = "a")


# 2) Add rows removed from test set back into training set
#    a) determine removed rows, which are the ones in temp,
#       but not in test.
#    b) Append these rows to the training set.
removed <- anti_join(temp, test, by = "a")
train <- rbind(train, removed)

print("training and final test set:"); print(train); print(test)
print("Total number of rows: "); print(nrow(train)+nrow(temp))

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

# 0) Init: partially overlapping training and test set (first in temp)
train = data.frame(n = seq(1,4),
                   a = c(1, 2, 3, 4))

temp = data.frame(n = seq(1,4),
                  a = c(2, 3, 4, 5))
print("training and  temporary test set:"); print(train); print(temp)
print("Total number of rows: "); print(nrow(train)+nrow(temp))


# 1) Execute function
df <- clean_train_test(train, temp, column = "a")
train <- df$train
test <- df$test
print("training and  temporary test set:"); print(train); print(test)
print("Total number of rows: "); print(nrow(train)+nrow(test))

