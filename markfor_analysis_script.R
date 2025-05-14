# Install packages (run only once if not already installed)
# install.packages(c("tidyverse", "lubridate", "skimr", "janitor", "rfm", "caret", "jtools", "broom", "Metrics"))

# Load necessary libraries
library(tidyverse) # includes ggplot2, dplyr, tidyr, purrr
library(lubridate)
library(skimr)
library(janitor)
library(knitr)
library(rfm)
library(caret)
library(jtools)
library(broom)
library(Metrics)

############################################################################
# Load the dataset
############################################################################
df <- read_csv("synthetic_beverage_sales_data.csv") %>%
  clean_names()
head(df, 4) # print first 4 rows of data frame
glimpse(df) # print summary of data frame

############################################################################
# Data Preperation
############################################################################
df[df == ""] <- NA # Replace empty strings with NA

colSums(is.na(df)) # Check for missing values

sum(duplicated(df)) # Check for duplicate entries

df %>% # Count rows with invalid or implausible values
  filter(
    unit_price <= 0 | # Unit price should be greater than 0
      quantity <= 0 | # Quantity should be greater than 0
      discount < 0 | discount > 1 | # Discount must be between 0 and 1
      total_price < 0 # Total price should not be negative
  ) %>%
  nrow() # Count number of rows that meet any of the above conditions

############################################################################
# Splitting into B2B and B2C & grouping into different dataframes:
# line-wise, rfm-base, numeric-invoice-wise, full-invoice-wise
############################################################################

# Split into B2B and B2C segments
# Each row represents a product purchase/position(line item)
# by a customer on a specific day.
# A customer may appear multiple times per day for different products.

# B2B orders
line_wise_b2b <- df %>% filter(customer_type == "B2B")
# B2C orders
line_wise_b2c <- df %>% filter(customer_type == "B2C")

# Required for RFM: only customer_id, order_date, and revenue

# B2B orders
rfm_base_b2b <- line_wise_b2b %>%
  group_by(customer_id, order_date) %>% # Group by customer and date
  summarise(
    total_price = sum(total_price, na.rm = TRUE), # Total revenue per order
    .groups = "drop"
  )
# B2C orders
rfm_base_b2c <- line_wise_b2c %>%
  group_by(customer_id, order_date) %>%
  summarise(
    total_price = sum(total_price, na.rm = TRUE),
    .groups = "drop"
  )

# Numerical summary per invoice (for statistical modeling)
# One row per customer and date = one invoice

# B2B orders
numeric_invoice_wise_b2b <- line_wise_b2b %>%
  group_by(customer_id, order_date) %>%
  summarise(
    quantity = sum(quantity, na.rm = TRUE),
    discount = mean(discount, na.rm = TRUE), # Avg. discount per invoice
    unit_price = mean(unit_price, na.rm = TRUE), # Avg. unit price per invoice
    total_price = sum(total_price, na.rm = TRUE), # Total invoice value
    .groups = "drop"
  )
# B2C orders
numeric_invoice_wise_b2c <- line_wise_b2c %>%
  group_by(customer_id, order_date) %>%
  summarise(
    quantity = sum(quantity, na.rm = TRUE),
    discount = mean(discount, na.rm = TRUE),
    unit_price = mean(unit_price, na.rm = TRUE),
    total_price = sum(total_price, na.rm = TRUE),
    .groups = "drop"
  )

# Full invoice-level dataset including order_id and region
# Each row = one invoice (combination of order_id, customer_id, and date)

# B2B orders
full_invoice_wise_b2b <- line_wise_b2b %>%
  group_by(order_id, customer_id, order_date, region) %>%
  summarise(
    quantity = sum(quantity, na.rm = TRUE),
    discount = mean(discount, na.rm = TRUE),
    unit_price = mean(unit_price, na.rm = TRUE),
    total_price = sum(total_price, na.rm = TRUE),
    .groups = "drop"
  )
# B2C orders
full_invoice_wise_b2c <- line_wise_b2c %>%
  group_by(order_id, customer_id, order_date, region) %>%
  summarise(
    quantity = sum(quantity, na.rm = TRUE),
    discount = mean(discount, na.rm = TRUE),
    unit_price = mean(unit_price, na.rm = TRUE),
    total_price = sum(total_price, na.rm = TRUE),
    .groups = "drop"
  )

# ------------------------------ prints ------------------------------
print("line-wise, b2b")
glimpse(line_wise_b2b) # print summary of data frame
head(line_wise_b2b, 4) # print first 4 rows of data frame

print("rfm-base, b2b")
glimpse(rfm_base_b2b) # print summary of data frame
head(rfm_base_b2b, 4) # print first 4 rows of data frame

print("numeric-invoice-wise, b2b")
glimpse(numeric_invoice_wise_b2b) # print summary of data frame
head(numeric_invoice_wise_b2b, 4) # print first 4 rows of data frame

print("full-invoice-wise, b2b")
glimpse(full_invoice_wise_b2b) # print summary of data frame
head(full_invoice_wise_b2b, 4) # print first 4 rows of data frame

############################################################################
# Descriptive Statistical Analysis
############################################################################

# Summary statistics based on line-item level (each row = one product purchase)
# Not aggregated by invoice/customer

# B2B orders
line_wise_summary_b2b <- line_wise_b2b %>%
  summarise(
    discount    = list(discount),
    quantity    = list(quantity),
    total_price = list(total_price),
    unit_price  = list(unit_price)
  ) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "values") %>%
  mutate(
    count  = map_int(values, ~ length(.x)),
    mean   = map_dbl(values, ~ mean(.x, na.rm = TRUE)),
    sd     = map_dbl(values, ~ sd(.x, na.rm = TRUE)),
    min    = map_dbl(values, ~ min(.x, na.rm = TRUE)),
    q25    = map_dbl(values, ~ quantile(.x, 0.25, na.rm = TRUE)),
    median = map_dbl(values, ~ median(.x, na.rm = TRUE)),
    q75    = map_dbl(values, ~ quantile(.x, 0.75, na.rm = TRUE)),
    max    = map_dbl(values, ~ max(.x, na.rm = TRUE))
  ) %>%
  select(variable, count, mean, sd, min, q25, median, q75, max)
# B2C orders
line_wise_summary_b2c <- line_wise_b2c %>%
  summarise(
    discount    = list(discount),
    quantity    = list(quantity),
    total_price = list(total_price),
    unit_price  = list(unit_price)
  ) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "values") %>%
  mutate(
    count  = map_int(values, ~ length(.x)),
    mean   = map_dbl(values, ~ mean(.x, na.rm = TRUE)),
    sd     = map_dbl(values, ~ sd(.x, na.rm = TRUE)),
    min    = map_dbl(values, ~ min(.x, na.rm = TRUE)),
    q25    = map_dbl(values, ~ quantile(.x, 0.25, na.rm = TRUE)),
    median = map_dbl(values, ~ median(.x, na.rm = TRUE)),
    q75    = map_dbl(values, ~ quantile(.x, 0.75, na.rm = TRUE)),
    max    = map_dbl(values, ~ max(.x, na.rm = TRUE))
  ) %>%
  select(variable, count, mean, sd, min, q25, median, q75, max)

# Summary statistics based on invoice-level data
# Each row represents one invoice (aggregated per customer and order date)

# B2B orders
invoice_wise_summary_b2b <- numeric_invoice_wise_b2b %>%
  summarise(
    discount    = list(discount),
    quantity    = list(quantity),
    total_price = list(total_price),
    unit_price  = list(unit_price)
  ) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "values") %>%
  mutate(
    count  = map_int(values, ~ length(.x)),
    mean   = map_dbl(values, ~ mean(.x, na.rm = TRUE)),
    sd     = map_dbl(values, ~ sd(.x, na.rm = TRUE)),
    min    = map_dbl(values, ~ min(.x, na.rm = TRUE)),
    q25    = map_dbl(values, ~ quantile(.x, 0.25, na.rm = TRUE)),
    median = map_dbl(values, ~ median(.x, na.rm = TRUE)),
    q75    = map_dbl(values, ~ quantile(.x, 0.75, na.rm = TRUE)),
    max    = map_dbl(values, ~ max(.x, na.rm = TRUE))
  ) %>%
  select(variable, count, mean, sd, min, q25, median, q75, max)
# B2C orders
invoice_wise_summary_b2c <- numeric_invoice_wise_b2c %>%
  summarise(
    discount    = list(discount),
    quantity    = list(quantity),
    total_price = list(total_price),
    unit_price  = list(unit_price)
  ) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "values") %>%
  mutate(
    count  = map_int(values, ~ length(.x)),
    mean   = map_dbl(values, ~ mean(.x, na.rm = TRUE)),
    sd     = map_dbl(values, ~ sd(.x, na.rm = TRUE)),
    min    = map_dbl(values, ~ min(.x, na.rm = TRUE)),
    q25    = map_dbl(values, ~ quantile(.x, 0.25, na.rm = TRUE)),
    median = map_dbl(values, ~ median(.x, na.rm = TRUE)),
    q75    = map_dbl(values, ~ quantile(.x, 0.75, na.rm = TRUE)),
    max    = map_dbl(values, ~ max(.x, na.rm = TRUE))
  ) %>%
  select(variable, count, mean, sd, min, q25, median, q75, max)

# ------------------------------ Print summary tables ------------------------------
kable(line_wise_summary_b2b, caption = "Line-wise summary statistics of B2B numeric variables", digits = 2)
kable(line_wise_summary_b2c, caption = "Line-wise summary statistics of B2C numeric variables", digits = 2)
kable(invoice_wise_summary_b2b, caption = "Invoice-level summary statistics of B2B orders", digits = 2)
kable(invoice_wise_summary_b2c, caption = "Invoice-level summary statistics of B2C orders", digits = 2)

############################################################################
# RFM Analysis
############################################################################
analysis_date <- as.Date("2023-12-31") # Analysis date for RFM:last day of year
# B2B orders
rfm_result_b2b <- rfm_table_order(
  data = rfm_base_b2b,
  customer_id = customer_id, # Column identifying each customer
  order_date = order_date, # Column with the date of each order
  revenue = total_price, # Column with the monetary value of the order
  analysis_date = analysis_date # Reference point for recency calculation
)
# B2C orders
rfm_result_b2c <- rfm_table_order(
  data = rfm_base_b2c,
  customer_id = customer_id,
  order_date = order_date,
  revenue = total_price,
  analysis_date = analysis_date
)
# ------------------------------ Print RFM results & visualizations ------------------------------
glimpse(rfm_result_b2b$rfm)
# B2B customers
# Visualize distribution of RFM scores (count of customers per total RFM score)
rfm_plot_bar_chart(rfm_result_b2b)
# Distribution of recency scores (how recently customers made a purchase)
rfm_plot_histogram(rfm_result_b2b, metric = "recency")
# Distribution of frequency scores (how often customers purchased)
rfm_plot_histogram(rfm_result_b2b, metric = "frequency")
# Distribution of monetary scores (how much customers spent)
rfm_plot_histogram(rfm_result_b2b, metric = "monetary")
# Frequency of RFM score combinations (e.g., RFM = 555, 444, etc.)
rfm_plot_order_dist(rfm_result_b2b)
# Heatmap of average monetary value by Recency and Frequency segments
rfm_plot_heatmap(rfm_result_b2b)

# B2C customers
# Same set of RFM visualizations applied to B2C segment
rfm_plot_bar_chart(rfm_result_b2c)
rfm_plot_histogram(rfm_result_b2c, metric = "recency")
rfm_plot_histogram(rfm_result_b2c, metric = "frequency")
rfm_plot_histogram(rfm_result_b2c, metric = "monetary")
rfm_plot_order_dist(rfm_result_b2c)
rfm_plot_heatmap(rfm_result_b2c)

############################################################################
# RFM Segmentation
############################################################################

# Manually assign RFM-based customer segments based on score combinations
# These rules are based on the RFM segmentation taught in the lecture
# B2B customers
rfm_segmented_b2b <- rfm_result_b2b$rfm %>%
  mutate(
    segment = case_when(
      recency_score %in% 4:5 & frequency_score %in% 4:5 & monetary_score %in% 4:5 ~ "Champions",
      recency_score %in% 2:5 & frequency_score %in% 3:5 & monetary_score %in% 3:5 ~ "Loyal Customers",
      recency_score %in% 3:5 & frequency_score %in% 1:3 & monetary_score %in% 1:3 ~ "Potential Loyalist",
      recency_score %in% 4:5 & frequency_score <= 1 & monetary_score <= 1 ~ "New Customers",
      recency_score %in% 3:4 & frequency_score <= 1 & monetary_score <= 1 ~ "Promising",
      recency_score %in% 2:3 & frequency_score %in% 2:3 & monetary_score %in% 2:3 ~ "Need Attention",
      recency_score %in% 2:3 & frequency_score <= 2 & monetary_score <= 2 ~ "About To Sleep",
      recency_score <= 2 & frequency_score %in% 2:5 & monetary_score %in% 2:5 ~ "At Risk",
      recency_score <= 1 & frequency_score %in% 4:5 & monetary_score %in% 4:5 ~ "Can’t Lose Them",
      recency_score %in% 1:2 & frequency_score %in% 1:2 & monetary_score %in% 1:2 ~ "Hibernating",
      recency_score <= 2 & frequency_score <= 2 & monetary_score <= 2 ~ "Lost",
      TRUE ~ "Uncategorized"
    )
  )
# B2C customers
rfm_segmented_b2c <- rfm_result_b2c$rfm %>%
  mutate(
    segment = case_when(
      recency_score %in% 4:5 & frequency_score %in% 4:5 & monetary_score %in% 4:5 ~ "Champions",
      recency_score %in% 2:5 & frequency_score %in% 3:5 & monetary_score %in% 3:5 ~ "Loyal Customers",
      recency_score %in% 3:5 & frequency_score %in% 1:3 & monetary_score %in% 1:3 ~ "Potential Loyalist",
      recency_score %in% 4:5 & frequency_score <= 1 & monetary_score <= 1 ~ "New Customers",
      recency_score %in% 3:4 & frequency_score <= 1 & monetary_score <= 1 ~ "Promising",
      recency_score %in% 2:3 & frequency_score %in% 2:3 & monetary_score %in% 2:3 ~ "Need Attention",
      recency_score %in% 2:3 & frequency_score <= 2 & monetary_score <= 2 ~ "About To Sleep",
      recency_score <= 2 & frequency_score %in% 2:5 & monetary_score %in% 2:5 ~ "At Risk",
      recency_score <= 1 & frequency_score %in% 4:5 & monetary_score %in% 4:5 ~ "Can’t Lose Them",
      recency_score %in% 1:2 & frequency_score %in% 1:2 & monetary_score %in% 1:2 ~ "Hibernating",
      recency_score <= 2 & frequency_score <= 2 & monetary_score <= 2 ~ "Lost",
      TRUE ~ "Uncategorized"
    )
  )

# ------------------------------ Segment Summary ------------------------------
# Calculate total number of customers for B2B
total_customers <- nrow(rfm_segmented_b2b)
# Define all possible segments to ensure complete reporting
all_segments <- c(
  "Champions", "Loyal Customers", "Potential Loyalist", "New Customers",
  "Promising", "Need Attention", "About To Sleep", "At Risk",
  "Can’t Lose Them", "Hibernating", "Lost", "Uncategorized"
)
# Aggregate segment metrics for B2B: size, avg. spending, transactions, recency
segment_analysis_b2b <- rfm_segmented_b2b %>%
  group_by(segment) %>%
  summarise(
    no_customers = n(),
    avg_spending = round(mean(amount, na.rm = TRUE), 2),
    avg_transactions = round(mean(transaction_count, na.rm = TRUE), 2),
    avg_recency_days = round(mean(recency_days, na.rm = TRUE), 1),
    .groups = "drop"
  ) %>%
  mutate(
    total_customers = total_customers,
    percentage = round(100 * no_customers / total_customers, 1)
  ) %>%
  right_join(tibble(segment = all_segments), by = "segment") %>%
  replace_na(list(
    no_customers = 0,
    avg_spending = 0,
    avg_transactions = 0,
    avg_recency_days = 0,
    percentage = 0,
    total_customers = total_customers
  )) %>%
  arrange(factor(segment, levels = all_segments))

# Repeat the same steps for B2C
total_customers <- nrow(rfm_segmented_b2c)

segment_analysis_b2c <- rfm_segmented_b2c %>%
  group_by(segment) %>%
  summarise(
    no_customers = n(),
    avg_spending = round(mean(amount, na.rm = TRUE), 2),
    avg_transactions = round(mean(transaction_count, na.rm = TRUE), 2),
    avg_recency_days = round(mean(recency_days, na.rm = TRUE), 1),
    .groups = "drop"
  ) %>%
  mutate(
    total_customers = total_customers,
    percentage = round(100 * no_customers / total_customers, 1)
  ) %>%
  right_join(tibble(segment = all_segments), by = "segment") %>%
  replace_na(list(
    no_customers = 0,
    avg_spending = 0,
    avg_transactions = 0,
    avg_recency_days = 0,
    percentage = 0,
    total_customers = total_customers
  )) %>%
  arrange(factor(segment, levels = all_segments))

# ------------------------------ Output Tables ------------------------------

# Summary tables per segment for reporting
kable(segment_analysis_b2b, caption = "RFM Segment Summary – B2B", digits = 2)
kable(segment_analysis_b2c, caption = "RFM Segment Summary – B2C", digits = 2)

# Display a few examples of customers that could not be assigned to a segment
rfm_segmented_b2c %>%
  filter(segment == "Uncategorized") %>%
  select(customer_id, recency_score, frequency_score, monetary_score, amount, transaction_count, recency_days) %>%
  head(10)

############################################################################
# Custom RFM Segmentation
############################################################################

# Define extended set of RFM segments (custom categories)
# This includes standard segments and new ones like:
# "Active High Value", "Dormant High Value", "Occasional Shoppers", etc.
all_segments <- c(
  "Champions", "Loyal Customers", "Potential Loyalist",
  "New Customers", "Need Attention", "Promising",
  "About To Sleep", "At Risk", "Can’t Lose Them",
  "Hibernating", "Lost", "Active High Value",
  "Active Medium Value", "Dormant High Value",
  "Occasional Shoppers", "Dormant Big Spenders"
)

# B2C Segmentation

# Assign each B2C customer to a segment based on detailed RFM scoring logic
rfm_segmented_b2c <- rfm_result_b2c$rfm %>%
  mutate(
    segment = case_when(
      recency_score >= 5 & frequency_score >= 5 & monetary_score >= 5 ~ "Champions",
      recency_score >= 4 & frequency_score >= 4 & monetary_score >= 4 ~ "Loyal Customers",
      recency_score >= 4 & frequency_score >= 3 & monetary_score >= 3 ~ "Potential Loyalist",
      recency_score >= 4 & frequency_score >= 2 & monetary_score >= 2 ~ "New Customers",
      recency_score >= 3 & frequency_score == 2 ~ "Occasional Shoppers",
      recency_score >= 3 & frequency_score >= 3 & monetary_score >= 3 ~ "Need Attention",
      recency_score >= 3 & frequency_score == 1 ~ "Promising",
      recency_score >= 2 & frequency_score >= 3 ~ "At Risk",
      recency_score >= 2 & frequency_score == 2 ~ "About To Sleep",
      recency_score >= 1 & frequency_score >= 3 ~ "Can’t Lose Them",
      recency_score >= 1 & frequency_score <= 2 & monetary_score <= 3 ~ "Hibernating",
      recency_score <= 3 & frequency_score <= 1 & monetary_score <= 2 ~ "Lost",
      recency_score >= 3 & monetary_score >= 3 ~ "Active High Value",
      recency_score >= 3 & monetary_score >= 2 ~ "Active Medium Value",
      recency_score < 3 & monetary_score >= 2 ~ "Dormant High Value",
      TRUE ~ "Uncategorized"
    )
  )

# Calculate total number of B2C customers (needed for percentage computation)
total_customers <- nrow(rfm_segmented_b2c)

# Aggregate statistics per segment for B2C
segment_analysis_b2c <- rfm_segmented_b2c %>%
  group_by(segment) %>%
  summarise(
    no_customers = n(),
    avg_spending = round(mean(amount, na.rm = TRUE), 2),
    avg_transactions = round(mean(transaction_count, na.rm = TRUE), 2),
    avg_recency_days = round(mean(recency_days, na.rm = TRUE), 1),
    .groups = "drop"
  ) %>%
  mutate(
    total_customers = total_customers,
    percentage = round(100 * no_customers / total_customers, 1)
  ) %>%
  right_join(tibble(segment = all_segments), by = "segment") %>%
  replace_na(list(
    no_customers = 0,
    avg_spending = 0,
    avg_transactions = 0,
    avg_recency_days = 0,
    percentage = 0,
    total_customers = total_customers
  )) %>%
  arrange(factor(segment, levels = all_segments))

# B2B Segmentation
# Apply same custom logic to B2B customers
rfm_segmented_b2b <- rfm_result_b2b$rfm %>%
  mutate(
    segment = case_when(
      recency_score >= 5 & frequency_score >= 5 & monetary_score >= 5 ~ "Champions",
      recency_score >= 4 & frequency_score >= 4 & monetary_score >= 4 ~ "Loyal Customers",
      recency_score >= 4 & frequency_score >= 3 & monetary_score >= 3 ~ "Potential Loyalist",
      recency_score >= 4 & frequency_score >= 2 & monetary_score >= 2 ~ "New Customers",
      recency_score >= 3 & frequency_score == 2 ~ "Occasional Shoppers",
      recency_score >= 3 & frequency_score >= 3 & monetary_score >= 3 ~ "Need Attention",
      recency_score >= 3 & frequency_score == 1 ~ "Promising",
      recency_score >= 2 & frequency_score >= 3 ~ "At Risk",
      recency_score >= 2 & frequency_score == 2 ~ "About To Sleep",
      recency_score >= 1 & frequency_score >= 3 ~ "Can’t Lose Them",
      recency_score >= 1 & frequency_score <= 2 & monetary_score <= 3 ~ "Hibernating",
      recency_score <= 3 & frequency_score <= 1 & monetary_score <= 2 ~ "Lost",
      recency_score >= 3 & monetary_score >= 3 ~ "Active High Value",
      recency_score >= 3 & monetary_score >= 2 ~ "Active Medium Value",
      recency_score < 3 & monetary_score >= 2 ~ "Dormant High Value",
      TRUE ~ "Uncategorized"
    )
  )

# Calculate total number of B2B customers
total_customers <- nrow(rfm_segmented_b2b)

# Aggregate statistics per segment for B2B
segment_analysis_b2b <- rfm_segmented_b2b %>%
  group_by(segment) %>%
  summarise(
    no_customers = n(),
    avg_spending = round(mean(amount, na.rm = TRUE), 2),
    avg_transactions = round(mean(transaction_count, na.rm = TRUE), 2),
    avg_recency_days = round(mean(recency_days, na.rm = TRUE), 1),
    .groups = "drop"
  ) %>%
  mutate(
    total_customers = total_customers,
    percentage = round(100 * no_customers / total_customers, 1)
  ) %>%
  right_join(tibble(segment = all_segments), by = "segment") %>%
  replace_na(list(
    no_customers = 0,
    avg_spending = 0,
    avg_transactions = 0,
    avg_recency_days = 0,
    percentage = 0,
    total_customers = total_customers
  )) %>%
  arrange(factor(segment, levels = all_segments))

# --------------------------- Display Tables ------------------------------------
# Output final segment analysis tables
kable(segment_analysis_b2b, caption = "RFM Analysis – B2B")
kable(segment_analysis_b2c, caption = "RFM Analysis – B2C")

### weiter mit mit sven sachen!!
