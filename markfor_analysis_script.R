# ### Loading packages



install.packages(c("tidyverse", "lubridate", "skimr", "janitor", "rfm"))

# Load necessary packages
library(tidyverse) # for data manipulation and visualization
library(lubridate) # for date functions
library(skimr) # for quick data overview
library(janitor) # to clean column names
library(knitr) # for table creation
library(rfm) # for RFM analysis
library(dplyr) # für Datenmanipulation
library(tidyr) # for data reshaping
library(purrr) # for functional programming




# Load the CSV file
df <- read_csv("synthetic_beverage_sales_data.csv") %>%
  clean_names()




head(df, 4) # print first 4 rows of data frame
glimpse(df) # print summary of data frame


# # Data preperation



df[df == ""] <- NA # Ersetze leere Strings ("") durch NA
colSums(is.na(df)) # Zeigt dir, wie viele NAs pro Spalte vorhanden sind




# Doppelte Einträge prüfen
sum(duplicated(df))
# das dauert länger als 30min, deswegen abgebrochen




# Count rows with invalid or implausible values in key numeric columns
df %>%
  filter(
    unit_price <= 0 | # Unit price should be greater than 0
      quantity <= 0 | # Quantity should be greater than 0
      discount < 0 | discount > 1 | # Discount must be between 0 and 1
      total_price < 0 # Total price should not be negative
  ) %>%
  nrow() # Count the number of rows that meet any of the above conditions


# # Splitting into B2B and B2C & grouping into different dataframes
# ### line-wise, rfm-base, numeric-invoice-wise, full-invoice-wise



# Aufteilen in B2B und B2C
# Hier ist jede Zeile ein Kauf eines Kunden von einem Produkt (Stückzahl egal), der Kunde kann also mehrere Produkte pro Tag gekauft haben, es sind aber alles verschiedene Zeilen
# Wir nennen das line-item level, weil es eine Zeile pro Produkt ist. Gut zu erkennen dass mehrere zeilen für eine order_id existieren

line_wise_b2b <- df %>% filter(customer_type == "B2B")
line_wise_b2c <- df %>% filter(customer_type == "B2C")

# Aggregate data to one row per order per customer

# Für RFM, übergibt nur customer_id, order_date und revenue
# For B2B customers
rfm_base_b2b <- line_wise_b2b %>%
  group_by(customer_id, order_date) %>% # Group by customer and order date
  summarise(
    total_price = sum(total_price, na.rm = TRUE), # Sum total revenue per order (in case multiple products were ordered)
    .groups = "drop"
  )
# For B2C customers
rfm_base_b2c <- line_wise_b2c %>%
  group_by(customer_id, order_date) %>% # Same logic for B2C customers
  summarise(
    total_price = sum(total_price, na.rm = TRUE),
    .groups = "drop"
  )

# due to multiple products ordered per customer per day we group orders by date and customer to understand the actual order
# wir nennen es order-level oder invoice-level, weil es eine Zeile pro Rechnung ist (Tag+Kunde)

# das hier ist nur das numerische, für die stat. berechnung

numeric_invoice_wise_b2b <- line_wise_b2b %>%
  group_by(customer_id, order_date) %>%
  summarise(
    quantity = sum(quantity, na.rm = TRUE),
    discount = mean(discount, na.rm = TRUE), # average discount per invoice
    unit_price = mean(unit_price, na.rm = TRUE), # average unit price per invoice
    total_price = sum(total_price, na.rm = TRUE), # total invoice value
    .groups = "drop"
  )

numeric_invoice_wise_b2c <- line_wise_b2c %>%
  group_by(customer_id, order_date) %>%
  summarise(
    quantity = sum(quantity, na.rm = TRUE),
    discount = mean(discount, na.rm = TRUE), # average discount per invoice
    unit_price = mean(unit_price, na.rm = TRUE), # average unit price per invoice
    total_price = sum(total_price, na.rm = TRUE), # total invoice value
    .groups = "drop"
  )

# das hier ist die komplette invoice-wise, also auch die kunden_id und das datum
full_invoice_wise_b2b <- line_wise_b2b %>%
  group_by(order_id, customer_id, order_date, region) %>%
  summarise(
    quantity = sum(quantity, na.rm = TRUE),
    discount = mean(discount, na.rm = TRUE),
    unit_price = mean(unit_price, na.rm = TRUE),
    total_price = sum(total_price, na.rm = TRUE),
    .groups = "drop"
  )

full_invoice_wise_b2c <- line_wise_b2c %>%
  group_by(order_id, customer_id, order_date, region) %>%
  summarise(
    quantity = sum(quantity, na.rm = TRUE),
    discount = mean(discount, na.rm = TRUE),
    unit_price = mean(unit_price, na.rm = TRUE),
    total_price = sum(total_price, na.rm = TRUE),
    .groups = "drop"
  )




# prints:
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




# Schöne print ausgabe für einzelne Rechnung
# Beispiel: Eine strukturierte Ausgabe für die ersten 3 Rechnungen (B2B)
# line_wise_b2b %>%
#   group_by(order_id) %>%
#   group_split() %>%
#   head(3) %>%
#   walk(function(order) {
#     header <- order[1, ]  # Eine Zeile reicht für Kopf
#     cat("\n==============================\n")
#     cat("Order ID:", header$order_id,
#         "| Customer:", header$customer_id,
#         "| Type:", header$customer_type,
#         "| Region:", header$region,
#         "| Date:", header$order_date, "\n")
#     cat("------------------------------\n")
#     print(order %>% select(product, category, unit_price, quantity, total_price))
#   })




# Das ist die Statistik über alle Zeilen (also Produktkäufe), nicht nur über die aggregierten Rechnungen
# also line-wise level

line_wise_summary_b2b <- line_wise_b2b %>%
  summarise(
    discount        = list(discount),
    quantity        = list(quantity),
    total_price     = list(total_price),
    unit_price      = list(unit_price)
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


line_wise_summary_b2c <- line_wise_b2c %>%
  summarise(
    discount        = list(discount),
    quantity        = list(quantity),
    total_price     = list(total_price),
    unit_price      = list(unit_price)
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

# Berechnung der statistischen Kennzahlen für B2B und B2C des invoice-level/order-level
invoice_wise_summary_b2b <- numeric_invoice_wise_b2b %>%
  summarise(
    discount        = list(discount),
    quantity        = list(quantity),
    total_price     = list(total_price),
    unit_price      = list(unit_price)
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

invoice_wise_summary_b2c <- numeric_invoice_wise_b2c %>%
  summarise(
    discount        = list(discount),
    quantity        = list(quantity),
    total_price     = list(total_price),
    unit_price      = list(unit_price)
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




# print
kable(line_wise_summary_b2b, caption = "Line-wise summary statistics of B2B numeric variables", digits = 2)
kable(line_wise_summary_b2c, caption = "Line-wise summary statistics of B2C numeric variables", digits = 2)

kable(invoice_wise_summary_b2b, caption = "Inovice-wise summary statistics of B2B orders", digits = 2)
kable(invoice_wise_summary_b2c, caption = "Inovice-wise summary statistics of B2C orders", digits = 2)


# # RFM Analyse



analysis_date <- as.Date("2023-12-31") # beliebiges Analyse-Datum (z. B. Ende des Jahres)

rfm_result_b2b <- rfm_table_order(
  data = rfm_base_b2b,
  customer_id = customer_id,
  order_date = order_date,
  revenue = total_price, # <- wichtig: die Spalte mit dem Umsatz heißt total_price
  analysis_date = analysis_date
)

rfm_result_b2c <- rfm_table_order(
  data = rfm_base_b2c,
  customer_id = customer_id,
  order_date = order_date,
  revenue = total_price,
  analysis_date = analysis_date
)




glimpse(rfm_result_b2b$rfm)




# Verteilungen & Heatmaps
# B2B ------------------------------------------------------
# RFM-Visualisierungen für B2B (invoice-wise Datenbasis)
rfm_plot_bar_chart(rfm_result_b2b) # RFM-Score Übersicht
rfm_plot_histogram(rfm_result_b2b, metric = "recency") # Recency-Verteilung
rfm_plot_histogram(rfm_result_b2b, metric = "frequency") # Frequency-Verteilung
rfm_plot_histogram(rfm_result_b2b, metric = "monetary") # Monetary-Verteilung
rfm_plot_order_dist(rfm_result_b2b) # Score-Kombinationen
rfm_plot_heatmap(rfm_result_b2b) # Recency x Frequency Heatmap
rfm_plot_scatter(rfm_result_b2b) # Scatter: Recency vs Monetary




# B2C ------------------------------------------------------
# RFM-Visualisierungen für B2C (invoice-wise Datenbasis)
rfm_plot_bar_chart(rfm_result_b2c)
rfm_plot_histogram(rfm_result_b2c, metric = "recency")
rfm_plot_histogram(rfm_result_b2c, metric = "frequency")
rfm_plot_histogram(rfm_result_b2c, metric = "monetary")
rfm_plot_order_dist(rfm_result_b2c)
rfm_plot_heatmap(rfm_result_b2c)
rfm_plot_scatter(rfm_result_b2c)


# # Erste RFM einteilungsversuch, nach Vorlesung



# Segmentierung der Kunden
# B2B ------------------------------------------------------
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

# B2C ------------------------------------------------------
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




# B2B
# Berechne Gesamtzahl der Kunden (für prozentuale Anteile später)
total_customers <- nrow(rfm_segmented_b2b)

# Definiere alle Segmente explizit, damit später fehlende (0-Kunden-)Segmente ergänzt werden können
all_segments <- c(
  "Champions", "Loyal Customers", "Potential Loyalist", "New Customers",
  "Promising", "Need Attention", "About To Sleep", "At Risk",
  "Can’t Lose Them", "Hibernating", "Lost", "Uncategorized"
)

# Aggregiere Kennzahlen pro RFM-Segment
segment_analysis_b2b <- rfm_segmented_b2b %>%
  group_by(segment) %>%
  summarise(
    no_customers = n(), # Anzahl der Kunden im Segment
    avg_spending = round(mean(amount, na.rm = TRUE), 2), # Durchschnittlicher Gesamtumsatz pro Kunde im Segment
    avg_transactions = round(mean(transaction_count, na.rm = TRUE), 2), # Durchschnittliche Anzahl an Bestellungen pro Kunde
    avg_recency_days = round(mean(recency_days, na.rm = TRUE), 1), # Durchschnittlicher Abstand (in Tagen) zum letzten Kauf
    .groups = "drop"
  ) %>%
  mutate(
    total_customers = total_customers, # Referenzwert für Gesamtzahl
    percentage = round(100 * no_customers / total_customers, 1) # Anteil des Segments an allen Kunden
  ) %>%
  # Ergänze fehlende Segmente mit 0-Kunden
  right_join(tibble(segment = all_segments), by = "segment") %>%
  replace_na(list(
    no_customers = 0,
    avg_spending = 0,
    avg_transactions = 0,
    avg_recency_days = 0,
    percentage = 0,
    total_customers = total_customers
  )) %>%
  arrange(factor(segment, levels = all_segments)) # Behalte die vorgegebene Reihenfolge der Segmente bei


# B2C
# Berechne Gesamtzahl der Kunden (für prozentuale Anteile später)
total_customers <- nrow(rfm_segmented_b2c)

# Definiere alle Segmente explizit, damit später fehlende (0-Kunden-)Segmente ergänzt werden können
all_segments <- c(
  "Champions", "Loyal Customers", "Potential Loyalist", "New Customers",
  "Promising", "Need Attention", "About To Sleep", "At Risk",
  "Can’t Lose Them", "Hibernating", "Lost", "Uncategorized"
)

# Aggregiere Kennzahlen pro RFM-Segment
segment_analysis_b2c <- rfm_segmented_b2c %>%
  group_by(segment) %>%
  summarise(
    no_customers = n(), # Anzahl der Kunden im Segment
    avg_spending = round(mean(amount, na.rm = TRUE), 2), # Durchschnittlicher Gesamtumsatz pro Kunde im Segment
    avg_transactions = round(mean(transaction_count, na.rm = TRUE), 2), # Durchschnittliche Anzahl an Bestellungen pro Kunde
    avg_recency_days = round(mean(recency_days, na.rm = TRUE), 1), # Durchschnittlicher Abstand (in Tagen) zum letzten Kauf
    .groups = "drop"
  ) %>%
  mutate(
    total_customers = total_customers, # Referenzwert für Gesamtzahl
    percentage = round(100 * no_customers / total_customers, 1) # Anteil des Segments an allen Kunden
  ) %>%
  # Ergänze fehlende Segmente mit 0-Kunden
  right_join(tibble(segment = all_segments), by = "segment") %>%
  replace_na(list(
    no_customers = 0,
    avg_spending = 0,
    avg_transactions = 0,
    avg_recency_days = 0,
    percentage = 0,
    total_customers = total_customers
  )) %>%
  arrange(factor(segment, levels = all_segments)) # Behalte die vorgegebene Reihenfolge der Segmente bei

# Tabelle zur Darstellung im Report
kable(segment_analysis_b2b, caption = "RFM Analysis – B2B")
kable(segment_analysis_b2c, caption = "RFM Analysis – B2C")




# Drucke ein paar Beispiele von Uncategorized-Kunden
rfm_segmented_b2c %>%
  filter(segment == "Uncategorized") %>%
  select(customer_id, recency_score, frequency_score, monetary_score, amount, transaction_count, recency_days) %>%
  head(10)




# Definiere alle möglichen Segmente in der gewünschten Reihenfolge
all_segments <- c(
  "Champions", "Loyal Customers", "Potential Loyalist",
  "New Customers", "Need Attention", "Promising",
  "About To Sleep", "At Risk", "Can’t Lose Them",
  "Hibernating", "Lost", "Active High Value",
  "Active Medium Value", "Dormant High Value", # "Low" durch "Medium" ersetzen
  "Occasional Shoppers", "Dormant Big Spenders"
)

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
      recency_score >= 1 & frequency_score <= 2 & monetary_score <= 3 ~ "Hibernating", # Monetary erhöht
      recency_score <= 3 & frequency_score <= 1 & monetary_score <= 2 ~ "Lost",
      recency_score >= 3 & monetary_score >= 3 ~ "Active High Value",
      recency_score >= 3 & monetary_score >= 2 ~ "Active Medium Value", # Neue Mittelkategorie
      recency_score < 3 & monetary_score >= 2 ~ "Dormant High Value",
      TRUE ~ "Uncategorized"
    )
  )
segment_analysis_b2c <- rfm_segmented_b2c %>%
  group_by(segment) %>%
  summarise(
    no_customers = n(),
    avg_spending = round(mean(amount, na.rm = TRUE), 2), # FEHLER HIER WAR EIN FEHLENDES ARGUMENT
    avg_transactions = round(mean(transaction_count, na.rm = TRUE), 2),
    avg_recency_days = round(mean(recency_days, na.rm = TRUE), 1),
    .groups = "drop"
  ) %>%
  mutate(
    total_customers = total_customers, # Stelle sicher dass diese Variable existiert
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


########### B2B###########
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
      recency_score >= 1 & frequency_score <= 2 & monetary_score <= 3 ~ "Hibernating", # Monetary erhöht
      recency_score <= 3 & frequency_score <= 1 & monetary_score <= 2 ~ "Lost",
      recency_score >= 3 & monetary_score >= 3 ~ "Active High Value",
      recency_score >= 3 & monetary_score >= 2 ~ "Active Medium Value", # Neue Mittelkategorie
      recency_score < 3 & monetary_score >= 2 ~ "Dormant High Value",
      TRUE ~ "Uncategorized"
    )
  )
segment_analysis_b2b <- rfm_segmented_b2b %>%
  group_by(segment) %>%
  summarise(
    no_customers = n(),
    avg_spending = round(mean(amount, na.rm = TRUE), 2), # FEHLER HIER WAR EIN FEHLENDES ARGUMENT
    avg_transactions = round(mean(transaction_count, na.rm = TRUE), 2),
    avg_recency_days = round(mean(recency_days, na.rm = TRUE), 1),
    .groups = "drop"
  ) %>%
  mutate(
    total_customers = total_customers, # Stelle sicher dass diese Variable existiert
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

kable(segment_analysis_b2b, caption = "RFM Analysis – B2B")


# # MLR mit Sven ab hier
#



mlr_data <- line_wise_b2c %>%
  mutate(
    product = as.factor(product),
    region = as.factor(region),
    discount = as.numeric(discount),
    order_date = as.Date(order_date),
    month = factor(format(order_date, "%m")) # Extrahiere Monat als saisonaler Effekt
  ) %>%
  select(quantity, discount, product, region, month) %>%
  na.omit()


model <- lm(quantity ~ discount + product + region + month, data = mlr_data)
summary(model)




# # Multiple linear regression ab hier



total_price ~ unit_price + discount + region + product_category + customer_type




install.packages(c("caret"))




# Vorhersage des Gesamtpreises (total_price) auf Basis der anderen Variablen (quantity, discount, unit_price)
# Modell berechnet wie stark jeder dieser drei faktoren den gesmamtpreis beeinflusst

# Daten vorbereiten
regression_data_b2b <- numeric_invoice_wise_b2b %>%
  select(
    total_price, # Zielvariable
    quantity, # Prädiktor 1: Stückzahl pro Rechnung
    discount, # Prädiktor 2: Durchschnittsrabatt
    unit_price # Prädiktor 3: Durchschnittspreis pro Einheit
  ) %>%
  na.omit()

# Train/Test-Split (80/20)
set.seed(123)
train_index_b2b <- createDataPartition(regression_data_b2b$total_price, p = 0.8, list = FALSE)
train_data_b2b <- regression_data_b2b[train_index_b2b, ]
test_data_b2b <- regression_data_b2b[-train_index_b2b, ]

# Modell trainieren
model_b2b <- lm(total_price ~ quantity + discount + unit_price, data = train_data_b2b)
summary(model_b2b)

# Evaluation
predictions_b2b <- predict(model_b2b, newdata = test_data_b2b)
performance_b2b <- data.frame(
  RMSE = RMSE(predictions_b2b, test_data_b2b$total_price),
  R_squared = R2(predictions_b2b, test_data_b2b$total_price)
)

kable(performance_b2b, digits = 3, caption = "📈 Modellgüte der linearen Regression B2B (Testdaten)")

######### B2C#########
regression_data_b2c <- numeric_invoice_wise_b2c %>%
  select(
    total_price, # Zielvariable
    quantity, # Prädiktor 1: Stückzahl pro Rechnung
    discount, # Prädiktor 2: Durchschnittsrabatt
    unit_price # Prädiktor 3: Durchschnittspreis pro Einheit
  ) %>%
  na.omit()

# Train/Test-Split (80/20)
set.seed(123)
train_index_b2c <- createDataPartition(regression_data_b2c$total_price, p = 0.8, list = FALSE)
train_data_b2c <- regression_data_b2c[train_index_b2c, ]
test_data_b2c <- regression_data_b2c[-train_index_b2c, ]

# Modell trainieren
model_b2c <- lm(total_price ~ quantity + discount + unit_price, data = train_data_b2c)
summary(model_b2c)

# Evaluation
predictions_b2c <- predict(model_b2c, newdata = test_data_b2c)
performance_b2c <- data.frame(
  RMSE = RMSE(predictions_b2c, test_data_b2c$total_price),
  R_squared = R2(predictions_b2c, test_data_b2c$total_price)
)

kable(performance_b2c, digits = 3, caption = "📈 Modellgüte der linearen Regression B2C (Testdaten)")




library(dplyr)
library(lubridate)

regression_data_b2b <- full_invoice_wise_b2b %>%
  mutate(
    order_date = as.Date(order_date),
    year = year(order_date),
    month = month(order_date),
    quarter = quarter(order_date),
    region = as.factor(region) # Keine Leerzeichen oder Tippfehler!
  ) %>%
  select(
    total_price,
    quantity,
    discount,
    unit_price,
    region, # Sicherstellen, dass region ausgewählt ist
    quarter
  ) %>%
  na.omit()

# Train/Test-Split
set.seed(123)
train_index_b2b <- createDataPartition(regression_data_b2b$total_price, p = 0.8, list = FALSE)
train_data_b2b <- regression_data_b2b[train_index_b2b, ]
test_data_b2b <- regression_data_b2b[-train_index_b2b, ]

# Modell mit Region und Quartal
model_b2b <- lm(
  total_price ~ quantity + discount + unit_price + region + quarter,
  data = train_data_b2b # Verwenden Sie den korrigierten Datensatz
)

# Ergebnisse anzeigen
summary(model_b2b)

predictions_b2b <- predict(model_b2b, newdata = test_data_b2b)
performance_b2b <- data.frame(
  RMSE = RMSE(predictions_b2b, test_data_b2b$total_price),
  R_squared = R2(predictions_b2b, test_data_b2b$total_price)
)

kable(performance_b2b, digits = 3, caption = "📈 Modellgüte der linearen Regression B2B (Testdaten)")



regression_data_b2c <- full_invoice_wise_b2c %>%
  mutate(
    order_date = as.Date(order_date),
    year = year(order_date),
    month = month(order_date),
    quarter = quarter(order_date),
    region = as.factor(region) # Keine Leerzeichen oder Tippfehler!
  ) %>%
  select(
    total_price,
    quantity,
    discount,
    unit_price,
    region, # Sicherstellen, dass region ausgewählt ist
    quarter
  ) %>%
  na.omit()

# Train/Test-Split
set.seed(123)
train_index_b2c <- createDataPartition(regression_data_b2c$total_price, p = 0.8, list = FALSE)
train_data_b2c <- regression_data_b2c[train_index_b2c, ]
test_data_b2c <- regression_data_b2c[-train_index_b2c, ]

# Modell mit Region und Quartal
model_b2c <- lm(
  total_price ~ quantity + discount + unit_price + region + quarter,
  data = train_data_b2c # Verwenden Sie den korrigierten Datensatz
)

# Ergebnisse anzeigen
summary(model_b2c)

predictions_b2c <- predict(model_b2c, newdata = test_data_b2c)
performance_b2c <- data.frame(
  RMSE = RMSE(predictions_b2c, test_data_b2c$total_price),
  R_squared = R2(predictions_b2c, test_data_b2c$total_price)
)

kable(performance_b2c, digits = 3, caption = "📈 Modellgüte der linearen Regression B2C (Testdaten)")


# Interpretation der Regressionsergebnisse
# Hier sind die zentralen Erkenntnisse aus dem Modell und ihre geschäftliche Bedeutung:
#
# 1. Kernvariablen: Mengen, Rabatte & Stückpreise
# Variable	Koeffizient	Interpretation
# quantity	+5.54	Jede zusätzliche Einheit steigert den Umsatz um €5.54 pro Rechnung.
# discount	–1,207.22	Ein 1%-Punkt höherer Rabatt reduziert den Umsatz um €1,207.22 pro Rechnung.
# unit_price	+118.63	Eine €1 Erhöhung des Stückpreises steigert den Umsatz um €118.63 pro Rechnung.
# Warum ist das wichtig?
#
# Preiselastizität: Der starke Effekt von unit_price zeigt, dass Preiserhöhungen den Umsatz deutlich steigern – ein Hebel für Margenoptimierung.
#
# Rabatt-Dilemma: Rabatte senken den Umsatz drastisch. Ein 5%-Rabatt kostet im Schnitt €6,036.10 pro Rechnung – das muss durch höhere Absatzmengen kompensiert werden.
#
# 2. Regionale Unterschiede
# Die Referenzregion ist Baden-Württemberg (nicht explizit aufgeführt, da sie als Baseline dient).
#
# Region	Koeffizient	Bedeutung
# Bremen	–27.94	Umsätze sind €27.94 niedriger als in Baden-Württemberg.
# Nordrhein-Westfalen	–10.45	€10.45 niedriger (schwach signifikant).
# Sachsen-Anhalt	–12.72	€12.72 niedriger.
# Alle anderen Regionen	Nicht signifikant	Kein relevanter Unterschied zur Referenz.
# Handlungsempfehlungen:
#
# Bremen: Gezielte Rabatte oder Marketingkampagnen, um die niedrigen Umsätze auszugleichen.
#
# Nordrhein-Westfalen/Sachsen-Anhalt: Analyse, warum diese Regionen unterdurchschnittlich abschneiden (z.B. Wettbewerb, Distributionsprobleme).
#
# 3. Saisonalität (Quartal)
# Koeffizient: –0.75 (nicht signifikant, *p = 0.365*).
#
# Interpretation: Es gibt keine starken saisonalen Umsatzschwankungen auf Quartalsebene.
#
# Implikation:
#
# Saisonale Effekte sind entweder vernachlässigbar oder müssen feiner analysiert werden (z.B. monatlich oder produktspezifisch).
#
# 4. Modellgüte
# R² = 0.654: Das Modell erklärt 65.4% der Umsatzvarianz – sehr gut für reale Geschäftsdaten.
#
# RMSE = €851.90: Die durchschnittliche Vorhersageabweichung beträgt €851.90 pro Rechnung.
#
# Beispiel: Bei einem tatsächlichen Umsatz von €5.000 liegt die Prognose zwischen €4,148.10 und €5,851.90.
#
# Strategische Empfehlungen
# Preisstrategie priorisieren:
#
# Nutzen Sie den starken Effekt von unit_price. Eine €2 Preiserhöhung generiert im Schnitt €237.26 mehr Umsatz pro Rechnung.
#
# Achtung: Prüfen Sie, ob höhere Preise die Absatzmenge (quantity) reduzieren.
#
# Rabattkontrolle einführen:
#
# Setzen Sie Rabatte nur ein, wenn die erwartete Mengensteigerung den Umsatzverlust kompensiert:
# Erforderliche Mengensteigerung = (1,207.22 * Rabatt) / 5.54.
# Beispiel: Bei 5% Rabatt benötigen Sie 109 zusätzliche Einheiten pro Rechnung.
#
# Regionale Fokussierung:
#
# Bremen: Testen Sie lokale Promotionen (z.B. "Kostenloser Versand ab €500").
#
# Nordrhein-Westfalen: Analysieren Sie Wettbewerberpreise in der Region.
#
# Fehlende Einflussfaktoren identifizieren:
#
# 36.6% unerklärte Varianz deuten auf weitere Treiber hin:
#
# Produktkategorien: Wirken Rabatte bei "Alkoholika" anders als bei "Wasser"?
#
# Kundensegmente: Nutzen Sie RFM-Segmente als zusätzliche Prädiktoren.
#
# Zusammenfassung
# Das Modell bestätigt grundlegende Zusammenhänge, liefert aber auch überraschende Einblicke:
#
# Rabatte sind extrem kostspielig – sie müssen strategisch eingesetzt werden.
#
# Regionale Unterschiede sind klein, aber in Bremen signifikant.
#
# Preiserhöhungen sind der stärkste Hebel für höhere Umsätze.
#
# Nächste Schritte:
#
# Fügen Sie Interaktionsterme hinzu (z.B. discount * region), um regional unterschiedliche Rabattwirkungen zu testen.
#
# Analysieren Sie nichtlineare Effekte (z.B. abnehmender Grenznutzen von Rabatten).
#
# Erweitern Sie das Modell um Produktkategorien oder Kundensegmente.
#
# Mit diesen Erkenntnissen können Sie datengetriebene Entscheidungen treffen – von der Preisgestaltung bis zur regionalen Steuerung! 🚀



# B2B-Daten
regression_b2b <- full_invoice_wise_b2b %>%
  mutate(region = as.factor(region)) %>%
  select(total_price, quantity, discount, unit_price, region)

# B2C-Daten
regression_b2c <- full_invoice_wise_b2c %>% # Annahme: full_invoice_wise_b2c existiert analog
  mutate(region = as.factor(region)) %>%
  select(total_price, quantity, discount, unit_price, region)

# B2B-Modell
model_b2b <- lm(total_price ~ quantity + discount + unit_price + region, data = regression_b2b)

# B2C-Modell
model_b2c <- lm(total_price ~ quantity + discount + unit_price + region, data = regression_b2c)

# Ergebnisse vergleichen
summary(model_b2b)
summary(model_b2c)




combined_data <- bind_rows(
  full_invoice_wise_b2b %>% mutate(customer_type = "B2B"),
  full_invoice_wise_b2c %>% mutate(customer_type = "B2C")
) %>%
  mutate(
    customer_type = as.factor(customer_type),
    region = as.factor(region)
  )




model_combined <- lm(
  total_price ~ quantity + discount + unit_price + region + customer_type +
    quantity:customer_type + discount:customer_type + unit_price:customer_type,
  data = combined_data
)

# Ergebnisse interpretieren
summary(model_combined)




install.packages("broom") # Falls noch nicht installiert
library(broom) # Für tidy()
library(dplyr)
library(ggplot2)




# Koeffizienten für B2B
coef_b2b <- tidy(model_b2b) %>%
  mutate(group = "B2B")

# Koeffizienten für B2C
coef_b2c <- tidy(model_b2c) %>%
  mutate(group = "B2C")

# Kombinieren
combined_coef <- bind_rows(coef_b2b, coef_b2c)

# Für B2B
coef_b2b <- data.frame(
  term = names(coef(model_b2b)),
  estimate = coef(model_b2b),
  group = "B2B"
)

# Für B2C
coef_b2c <- data.frame(
  term = names(coef(model_b2c)),
  estimate = coef(model_b2c),
  group = "B2C"
)

# Kombinieren
combined_coef <- bind_rows(coef_b2b, coef_b2c)

ggplot(
  combined_coef %>%
    filter(term %in% c("quantity", "discount", "unit_price")),
  aes(x = term, y = estimate, fill = group)
) +
  geom_col(position = "dodge") + # Verwenden Sie geom_col() statt geom_bar(stat = "identity")
  labs(
    title = "B2B vs. B2C: Einflussfaktoren im Vergleich",
    x = "Variable",
    y = "Koeffizient",
    fill = "Kundentyp"
  ) +
  theme_minimal() +
  scale_fill_manual(values = c("B2B" = "#1f78b4", "B2C" = "#33a02c")) # Farben anpassen


# Interpretation der Regressionsergebnisse (B2B vs. B2C)
# Hier sind die zentralen Erkenntnisse und ihre geschäftliche Bedeutung:
#
# 1. Haupteffekte
# Variable	Koeffizient	Interpretation
# quantity	+5.55	Jede zusätzliche Einheit steigert den Umsatz um €5.55 (Basis: B2B).
# discount	–1,205	Ein 1%-Punkt Rabatt reduziert den Umsatz um €1,205 (Basis: B2B).
# unit_price	+118.40	Eine €1 Preiserhöhung steigert den Umsatz um €118.40 (Basis: B2B).
# customer_typeB2C	+458.80	B2C-Rechnungen haben €458.80 höhere Umsätze als B2B (ohne andere Faktoren).
# 2. Interaktionseffekte
# Interaktion	Koeffizient	Bedeutung
# quantity:customer_typeB2C	+0.39	Bei B2C steigt der Umsatz pro Einheit um zusätzliche €0.39 (vs. B2B).
# unit_price:customer_typeB2C	–100.90	Bei B2C reduziert eine €1 Preiserhöhung den Umsatz um €100.90 (vs. B2B).
# discount:customer_typeB2C	NA	Fehlender Effekt (mögliche Kollinearität oder fehlende Daten).
# 3. Regionale Unterschiede
# Signifikant negative Effekte:
#
# Bremen: –€12.54 vs. Referenzregion (Baden-Württemberg).
#
# Nordrhein-Westfalen: –€3.79.
#
# Sachsen-Anhalt: –€4.29.
#
# Andere Regionen: Kein signifikanter Unterschied.
#
# 4. Modellgüte
# R² = 0.694: Das Modell erklärt 69.4% der Umsatzvarianz – sehr stark.
#
# RMSE = €525.70: Durchschnittliche Prognoseabweichung von €525.70 pro Rechnung.
#
# Business-Implikationen
# Preisstrategie:
#
# B2B: Preiserhöhungen sind hochwirksam (+€118.40 pro €1).
#
# B2C: Preiserhöhungen senken den Umsatz (–€100.90 pro €1)!
# → B2C: Preise stabil halten, B2B: Preispolitik als Hebel nutzen.
#
# Rabattgestaltung:
#
# Rabatte vermeiden: Ein 2%-Rabatt kostet €2,410 pro Rechnung (B2B).
#
# Ausnahme B2C: Da der Rabatt-Interaktionsterm fehlt, könnte dies auf fehlende Rabattdaten hinweisen – prüfen!
#
# Mengenförderung:
#
# B2C profitiert stärker von Mengen: +€5.94 pro Einheit (5.55 + 0.39).
# → Bündelangebote (z.B. "3 zum Preis von 2") für B2C einführen.
#
# Regionale Fokussierung:
#
# Bremen/NRW/Sachsen-Anhalt: Gezielte Kampagnen zur Umsatzsteigerung.
#
# Bayern/Hamburg: Kein Handlungsbedarf (keine signifikanten Unterschiede).
#
# Auffälligkeiten & nächste Schritte
# Fehlender Rabatt-Interaktionsterm (B2C):
#
# Mögliche Ursachen:
#
# Multikollinearität: Rabatte sind bei B2C konstant (z.B. immer 0%).
#
# Datenlücken: Keine ausreichenden Rabattdaten für B2C.
# → Prüfen: table(combined_data$discount[combined_data$customer_type == "B2C"]).
#
# Starker B2C-Basiseffekt:
#
# B2C-Rechnungen sind im Schnitt €458.80 teurer – möglicherweise wegen höherer Einzelhandelspreise.
# → Prüfen: Unterschiede in Produktkategorien oder Margen analysieren.
#
# Zusammenfassung
# B2B: Preiserhöhungen und Mengensteigerung sind effektiv.
#
# B2C: Mengen ausbauen, Preise stabil halten.
#
# Rabatte: Sparsam einsetzen (besonders bei B2B).
#
# Regionen: Fokus auf Bremen, NRW und Sachsen-Anhalt.
#
# Mit diesen Erkenntnissen können Sie zielgerichtete Strategien für beide Kundengruppen entwickeln! 🚀
