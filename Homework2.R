#install needed packages
install.packages(c("dplyr", "tidyr", "tibble", "readr", "stringr", "forcats", "purrr"))

#load needed packaged
library(dplyr)
library(tidyr)
library(tibble)
library(readr)
library(stringr)
library(forcats)
library(purrr)

#Question 1: original sales data
sales_raw<- tibble(
  sale_id = 1:8,
  customer_id = c("C1", "C2", "C1", "C3", "C2", "C4","C5", "C2"),
  item_raw = c(" Pen", "PEN ", "Notebook", "pen","FOLDER", "Folder", " ERASER ", "pen"),
  amount_raw = c("$10.00","$20.00", NA, "$15.00",
                 "$30.00", "$25.00", NA, "$20.00"),
  discount_raw = c("0%", "10%", "5%", "0%", "20%", "10%", "0%", "0%"),
  code_raw = c("ST-101", "ON-102", "ST-103", "ON-104",
               "bad", "ST-106", "ON-107X", "on-108"),
  tags = c("gift;urgent", "online", "gift", "member",
           "online;member", "gift", "member", "gift;member")
)

#question 1 part a: clean the item names
sales_clean <- sales_raw %>%
  mutate(
    item_clean = str_to_lower(str_squish(str_trim(item_raw))),
    amount = parse_number(amount_raw),
    discount_rate = parse_number(discount_raw) / 100,
    net_amount = amount * (1 - discount_rate),
    size = case_when(
      is.na(net_amount) ~ "unrecorded",
      net_amount >= 20 ~ "large",
      TRUE ~ "small"
    )
  )

# console work (P1 part a)
sales_clean %>% select(sale_id, item_raw, item_clean)
sales_clean %>% select(sale_id, amount_raw, amount, discount_raw, discount_rate)
sales_clean %>% select(sale_id, amount, discount_rate, net_amount)
sales_clean %>%
  select(sale_id, item_clean, amount, discount_rate, net_amount, size)

#question 1 part b: clean and validate codes
sales_codes <- sales_clean %>%
  mutate(
    code_clean = str_to_upper(str_trim(code_raw)),
    valid_code = str_detect(code_clean, "^(ST|ON)-[0-9]{3}$")
  )

# save and display invalid codes
invalid_codes <- sales_codes %>%
  filter(!valid_code) %>%
  select(sale_id, code_raw, code_clean)

invalid_codes

# split valid codes into channel and number
valid_code_parts <- sales_codes %>%
  filter(valid_code) %>%
  tidyr::separate_wider_delim(
    code_clean,
    delim = "-",
    names = c("channel", "number")
  ) %>%
  select(sale_id, channel, number)

valid_code_parts

# split tags into separate rows
sale_tags <- sales_clean %>%
  select(sale_id, tags) %>%
  tidyr::separate_longer_delim(tags, delim = ";")

sale_tags

# console work (P1 part b)
sales_codes %>% select(sale_id, code_raw, code_clean)
sales_codes %>% select(sale_id, code_clean, valid_code)
invalid_codes
valid_code_parts

# question 1 part c: summarize sales by customer 
customer_summary <- sales_clean %>%
  summarise(
    sales_n = n(),
    known_n = sum(!is.na(net_amount)),
    large_n = sum(net_amount >= 20, na.rm = TRUE),
    large_share = if (sum(!is.na(net_amount)) == 0) {
      NA_real_
    } else {
      sum(net_amount >= 20, na.rm = TRUE) /
        sum(!is.na(net_amount))
    },
    .by = customer_id
  ) %>%
  arrange(customer_id)

customer_summary

# for c1, numerator is 0 large sales with k own net amounts
# the denominator is 1 sale with a known net amount
# therefore c1s large_share is 0/1 = 0

# question 1 part c: create factor categories 
sales_factors <- sales_clean %>%
  mutate(
    item_factor = forcats::fct_infreq(factor(item_clean)),
    item_group = forcats::fct_lump_n(
      factor(item_clean),
      n = 2,
      other_level = "Other"
    )
  )

# display the factor levels
levels(sales_factors$item_factor)
levels(sales_factors$item_group)

# display the category counts
sales_factors %>% count(item_factor)
sales_factors %>% count(item_group)

# fct_infreq() changes the order of item categories based on frequency,
# but keeps each category separate.
# fct_lump_n()
# keeps two most frequent categories and combines
# the remaining categories into "Other".

# console work (P1 part c)
sale_tags
customer_summary
levels(sales_factors$item_factor)
levels(sales_factors$item_group)

sales_factors %>% count(item_factor)
sales_factors %>% count(item_group)

# Question 2: Missing Values and Joins
reports_raw <- tibble(
  form = c("A", "A", "A", "B", "B", "B"),
  line = c(1, 2, 3, 1, 2, 3),
  customer_id = c("C1", NA, "C2", "C3", NA, "C4"),
  round = c(1, 3, 1, 1, 2, 1),
  primary_amount = c(10, -999, 20, NA, 0, 30),
  backup_amount = c(NA, 15, 25, NA, 99, NA)
)
customers <- tibble(
  customer_id = c("C1", "C2", "C3", "C5"),
  region = c("North", "South", NA, "West")
)

# question 2 part a: fill missing customer IDs within each form
reports_labeled <- reports_raw %>%
  arrange(form, line) %>%
  group_by(form) %>%
  tidyr::fill(customer_id, .direction = "down") %>%
  ungroup()

# display the result
reports_labeled

# clean primary amounts and use backup amounts when needed
reports_clean <- reports_labeled %>%
  mutate(
    primary_clean = na_if(primary_amount, -999),
    amount = coalesce(primary_clean, backup_amount)
  )

# display the cleaned amounts
reports_clean %>%
  select(form, line, customer_id, round,
         primary_amount, backup_amount, amount)

# keep the original records and complete rounds 1 through 3
reports_complete <- reports_clean %>%
  select(customer_id, round, amount) %>%
  mutate(recorded_row = TRUE) %>%
  complete(
    customer_id,
    round = 1:3
  )

# count original and added rows, and missing amounts
reports_complete %>%
  summarise(
    total_rows = n(),
    original_rows = sum(!is.na(recorded_row)),
    added_rows = sum(is.na(recorded_row)),
    missing_amounts = sum(is.na(amount))
  )

# before completing data, Form b line 1 has a missing amount
# because both the primary and backup amounts are missing.
# a customer-round combination that does not appear in the original
# data is an implicit missing record.
# after completing data, newly added rows have unknown amounts,
# so those amounts remain NA rather than being replaced with guesses


# console work (P2 part a)
reports_raw
reports_clean %>%
  summarise(
    original_rows = n(),
    missing_amounts = sum(is.na(amount))
  )
reports_complete %>%
  arrange(customer_id, round)
customers

# question 2 part b: check the customer IDs

# check for missing and duped customer IDs
customers %>%
  summarise(
    missing_ids = sum(is.na(customer_id)),
    duplicated_ids = sum(duplicated(customer_id))
    )

# check for duped customer-round pairs
reports_clean %>%
  summarise(
    duplicated_pairs = sum(
      duplicated(paste(customer_id, round))
    )
  )

# mark customers that exist in the customer table
customers_marked <- customers %>%
  mutate(customer_found = TRUE)

# join customer information to the cleaned reports
reports_joined <- reports_clean %>%
  left_join(
    customers_marked,
    by = join_by(customer_id),
    relationship = "many-to-one",
    na_matches = "never"
  ) %>%
  mutate(customer_found = coalesce(customer_found, FALSE))

# show the joined information
reports_joined %>%
  select(customer_id, round, amount, region, customer_found)

# IDs in reports_clean that are missing from customers
reports_only <- reports_clean %>%
  distinct(customer_id) %>%
  anti_join(
    customers,
    by = join_by(customer_id),
    na_matches = "never"
  )

# IDs in customers that are missing from reports_clean
customers_only <- customers %>%
  distinct(customer_id) %>%
  anti_join(
    reports_clean,
    by = join_by(customer_id),
    na_matches = "never"
  )

reports_only
customers_only

# c4 appears in reports_clean but not in customers.
# c5 appears in customers but has no report in reports_clean.
# c3 is found in customers, but its region is missing.
# therefore, customer_found is TRUE for c3 regardless of region being NA

# question 2 part c: 
# create a customer table with a dupe customer ID
customers_bad <- bind_rows(
  customers,
  tibble(
    customer_id = "C2",
    region = "East"
  )
)

# display the table
customers_bad

# join reports to the customer table with duplicate IDs
reports_duplicated <- reports_clean %>%
  left_join(
    customers_bad,
    by = join_by(customer_id),
    relationship = "many-to-many",
    na_matches = "never"
  )

#compare row counts and total amounts
reports_clean %>%
  summarise(
    rows_before = n(),
    total_before = sum(amount, na.rm = TRUE)
  )

reports_duplicated %>%
  summarise(
    rows_after = n(),
    total_after = sum(amount, na.rm = TRUE)
  )

reports_duplicated %>%
  count(customer_id, round) %>%
  filter(n > 1)

# c2 round 1 is duped because C2 appears twice in customers_bad
# the row ocunt increases from 6 to 7 and total increases from 75 to 95.
# the amount of 20 is counted twice because the join finds two matching
# customer records for C2.
# allowing a many-to-many join permits the duplication but does not resolve
# the conflicting regions for C2.

reports_wide <- reports_complete %>%
  select(customer_id, round, amount) %>%
  pivot_wider(
    names_from = round,
    values_from = amount,
    names_prefix = "round_"
  )

reports_wide

reports_complete <- reports_clean %>%
  select(customer_id, round, amount) %>%
  mutate(recorded_row = TRUE) %>%
  complete(customer_id, round = 1:3) %>%
  mutate(recorded_row = coalesce(recorded_row, FALSE))

reports_long <- reports_wide %>%
  pivot_longer(
    cols = starts_with("round_"),
    names_to = "round",
    names_prefix = "round_",
    values_to = "amount",
    values_drop_na = TRUE
  )

reports_long

# pivoting back to long format with values_drop_na = TRUE
# removds rows with missing amounts.
# five non-missing amts remain, including the valid zero for C3.
# missing values are removed, but the missing records are not recovered

#console work (P2 part c)
customers_bad

# Question 3: Two-sample t-tests and simulation

# question 3 part a: calculate the sample summaries
two_sample_t <- function(x, y, equal_var = FALSE, alpha = 0.05) {
  
  nx <- length(x)
  ny <- length(y)
  
  mean_x <- mean(x)
  mean_y <- mean(y)
  
  var_x <- var(x)
  var_y <- var(y)
  
  if (equal_var) {
    pooled_var <- ((nx - 1) * var_x + (ny - 1) * var_y) /
      (nx + ny - 2)
    
    se <- sqrt(pooled_var * (1 / nx + 1 / ny))
    df <- nx + ny - 2
    
  } else {
    se <- sqrt(var_x / nx + var_y / ny)
    
    df <- (var_x / nx + var_y / ny)^2 /
      ((var_x / nx)^2 / (nx - 1) +
         (var_y / ny)^2 / (ny - 1))
  }
  
  t_stat <- (mean_x - mean_y) / se
  
  p_value <- 2 * pt(abs(t_stat), df = df, lower.tail = FALSE)
  
  reject <- p_value < alpha
  
  list(
    method = if (equal_var) "Pooled" else "Welch",
    t_stat = t_stat,
    df = df,
    p_value = p_value,
    reject = reject
  )
}


# compare Welch's test
welch_result <- two_sample_t(x_check, y_check)
welch_reference <- t.test(x_check, y_check, var.equal = FALSE)

# compare pooled test
pooled_result <- two_sample_t(x_check, y_check, equal_var = TRUE)
pooled_reference <- t.test(x_check, y_check, var.equal = TRUE)

# check whether differences are smaller than 1e-8
c(
  welch_t = abs(welch_result$t_stat - welch_reference$statistic) < 1e-8,
  welch_df = abs(welch_result$df - welch_reference$parameter) < 1e-8,
  welch_p = abs(welch_result$p_value - welch_reference$p.value) < 1e-8,
  pooled_t = abs(pooled_result$t_stat - pooled_reference$statistic) < 1e-8,
  pooled_df = abs(pooled_result$df - pooled_reference$parameter) < 1e-8,
  pooled_p = abs(pooled_result$p_value - pooled_reference$p.value) < 1e-8
)

# set up the simulation
B <- 1000
n_x <- 20
n_y <- 40
alpha <- 0.05

# define the four scenarios
scenarios <- data.frame(
  scenario = c("equal_null", "unequal_null",
               "equal_shift", "unequal_shift"),
  mean_x = c(0, 0, 0, 0),
  mean_y = c(0, 0, 1, 1),
  sd_x = c(1, 3, 1, 3),
  sd_y = c(1, 1, 1, 1)
)

# make the random results reproducible
set.seed(2026)

# display the scenarios
scenarios

# create an empty table for simulation results
simulation_results <- data.frame(
  scenario = character(B * 4 * 2),
  replicate = integer(B * 4 * 2),
  method = character(B * 4 * 2),
  t_stat = numeric(B * 4 * 2),
  df = numeric(B * 4 * 2),
  p_value = numeric(B * 4 * 2),
  reject = logical(B * 4 * 2)
)

# check the number of rows and columns
dim(simulation_results)

# console work (P3 part a)
x_check <- c(12, 15, 14, 10, 13)
y_check <- c(9, 11, 8, 10, 7, 12, 9, 8)

two_sample_t(x_check, y_check)
two_sample_t(x_check, y_check, equal_var = TRUE)
two_sample_t(x_check, y_check)

# question 3b : generate the samples
# count which result row we are filling
row_index <- 1

# reset the results table so we start fresh
simulation_results <- data.frame(
  scenario = character(B * 4 * 2),
  replicate = integer(B * 4 * 2),
  method = character(B * 4 * 2),
  t_stat = numeric(B * 4 * 2),
  df = numeric(B * 4 * 2),
  p_value = numeric(B * 4 * 2),
  reject = logical(B * 4 * 2)
)

# start at the first row
row_index <- 1

# loop through all four scenarios
for (s in 1:nrow(scenarios)) {
  
  # repeat each scenario 1,000 times
  for (b in 1:B) {
    
    # generate the two samples
    x <- rnorm(
      n_x,
      mean = scenarios$mean_x[s],
      sd = scenarios$sd_x[s]
    )
    
    y <- rnorm(
      n_y,
      mean = scenarios$mean_y[s],
      sd = scenarios$sd_y[s]
    )
    
    # run Welch's t-test
    welch <- two_sample_t(x, y, alpha = alpha)
    
    # run the pooled t-test
    pooled <- two_sample_t(
      x, y,
      equal_var = TRUE,
      alpha = alpha
    )
    
    # save Welch's results
    simulation_results[row_index, ] <- list(
      scenario = scenarios$scenario[s],
      replicate = b,
      method = welch$method,
      t_stat = welch$t_stat,
      df = welch$df,
      p_value = welch$p_value,
      reject = welch$reject
    )
    
    row_index <- row_index + 1
    
    # save pooled-test results
    simulation_results[row_index, ] <- list(
      scenario = scenarios$scenario[s],
      replicate = b,
      method = pooled$method,
      t_stat = pooled$t_stat,
      df = pooled$df,
      p_value = pooled$p_value,
      reject = pooled$reject
    )
    
    row_index <- row_index + 1
    
  } 
  
} 

# summarize the simulation results
simulation_summary <- aggregate(
  cbind(t_stat, reject) ~ scenario + method,
  data = simulation_results,
  FUN = mean
)


# calculate mean and standard deviation of t-statistics
mean_t <- aggregate(
  t_stat ~ scenario + method,
  data = simulation_results,
  FUN = mean
)

sd_t <- aggregate(
  t_stat ~ scenario + method,
  data = simulation_results,
  FUN = sd
)

# calculate the proportion of tests that rejected the null hypothesis
rejection_rate <- aggregate(
  reject ~ scenario + method,
  data = simulation_results,
  FUN = mean
)

# combine the summaries into one table
simulation_summary <- data.frame(
  scenario = mean_t$scenario,
  method = mean_t$method,
  mean_t = mean_t$t_stat,
  sd_t = sd_t$t_stat,
  rejection_rate = rejection_rate$reject
)

# round numeric results to three decimal places
simulation_summary[, c("mean_t", "sd_t", "rejection_rate")] <-
  round(simulation_summary[, c("mean_t", "sd_t", "rejection_rate")], 3)

simulation_summary

# display the first six simulation results
head(simulation_results)

# display the total number of rows
nrow(simulation_results)

# plot rejection rates for each scenario and method
barplot(
  height = matrix(
    simulation_summary$rejection_rate,
    nrow = 4,
    ncol = 2
  ),
  beside = TRUE,
  names.arg = c("Welch", "Pooled"),
  main = "Rejection Rates by Scenario and Method",
  ylab = "Rejection Rate",
  ylim = c(0, 1)
)


# Short report:
# under the null scenarios, rejection rates should be near 0.05,
# although random simulation variation may cause small differences.
# when the population means differ, rejection rates should increase,
# reflecting greater power to detect a difference.
# higher power alone is not enough to judge a test because a test
# should also control its Type I error rate near the chosen alpha.






# console work (P3 part b)
nrow(simulation_results)
table(simulation_results$scenario, simulation_results$method)
head(simulation_results[, c("scenario", "method", "t_stat", "p_value")])
two_sample_t(c(12,15,14,10,13), c(9,11,8,10,7,12,9,8))
tail(simulation_results)


  


