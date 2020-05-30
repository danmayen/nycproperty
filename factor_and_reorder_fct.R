library(tidyverse)
library(rlang)

# test function, with 1 data frames
reorder_fct <- function(data, column_factor, 
                        column_order = NA, order_fct = NA) {
    res_data <- data %>%
        mutate(!!quo_name(column_factor) := factor(!!sym(column_factor)))
    if(!is.na(column_order))
        res_data <- res_data %>%
            mutate(!!quo_name(column_factor) := 
                       reorder(!!sym(column_factor),
                               !!sym(column_order), order_fct))
    return(res_data)
}

# test function, with 2 data frames
reorder_fct_2 <- function(data_1, data_2, column_factor,
                          column_order = NA, order_fct = NA) {
    res_data_1 <- data_1 %>%
        mutate(!!quo_name(column_factor) := factor(!!sym(column_factor)))
    if(!is.na(column_order))
        res_data_1 <- res_data_1 %>%
            mutate(!!quo_name(column_factor) := 
                       reorder(!!sym(column_factor),
                               !!sym(column_order), order_fct))

    # retrieve factor levels from res_data_1
    f_levels <- res_data_1 %>% pull(!!sym(column_factor)) %>% levels()
    
    # use to apply to res_data_2
    res_data_2 <- data_2 %>%
        mutate(!!quo_name(column_factor) := 
                   factor(!!sym(column_factor), levels = f_levels))
    
    return(list(data_1 = res_data_1, data_2 = res_data_2))
    }

# test frame
df_original <- data.frame(
    x1 = c("a", "b", "c"),
    x2 = c(3, 1, 2),
    stringsAsFactors = FALSE
    )

df_original_2 <- data.frame(
    x1 = c("c", "b", "a"),
    x2 = c(4, 2, 6),
    stringsAsFactors = FALSE
    )


df_test <- reorder_fct_2(df_original, df_original_2, "x1")
levels(df_test$data_1$x1)
levels(df_test$data_2$x1)

df_test <- reorder_fct_2(df_original, df_original_2, "x1", "x2", mean)
levels(df_test$data_1$x1)
levels(df_test$data_2$x1)


##################### Manual variant ###########################################
# want to run this:
# Step 1: Conversion of x1 to factor
df_ordered_man <- df_original %>%
    mutate(x1 = factor(x1))
levels(df_ordered_man$x1)

# Step 2: Order factor x1 by x2
df_ordered_man <- df_original %>%
    mutate(x1 = reorder(x1, x2, mean))
levels(df_ordered_man$x1)

##################### Function variant #########################################
# Step 1: Only conversion of x1 to factor
df_ordered_fct <- reorder_fct(df_original, "x1")
levels(df_ordered_fct$x1)
# should be TRUE
identical(levels(df_ordered_fct$x1), 
          levels(df_original %>% mutate(x1 = factor(x1)) %>% .$x1))

# Step 2: Cconversion to factor and order
df_ordered_fct <- reorder_fct(df_original, "x1", "x2", mean)
levels(df_ordered_fct$x1)
identical(levels(df_ordered_fct$x1), 
          levels(df_original %>% mutate(x1 = factor(x1)) %>% 
                     mutate(x1 = reorder(x1, x2, mean)) %>% .$x1))
