library(tidyverse)

sales <- read_csv("sales_raw.csv")

View(sales)

glimpse(sales)
summary(sales)

colSums(is.na(sales))

unique(sales$city)
glimpse(sales$sales_amount)


sales_clean <- sales %>% 
  mutate(
    city = str_trim(city),
    city = str_to_title(city),
    order_date = as.Date(order_date),
    customer_id = as.integer(customer_id),
    sales_amount = str_replace_all(sales_amount, " ", ""),
    sales_amount = as.numeric(sales_amount),
    returned = if_else(returned == "ja", TRUE, FALSE, missing = NA)
    #returned = recode(returned, "ja" = TRUE, "nej" = FALSE, .default = NA)
  )


glimpse(sales_clean)
summary(sales_clean)
unique(sales_clean$city)


sales_clean %>% 
  filter(is.na(city) | is.na(sales_amount) | is.na(quantity))

sales_ready <- sales_clean %>% 
  mutate(
    quantity = if_else(
      is.na(quantity),
      median(quantity, na.rm = TRUE),
      quantity
    ),
    order_size = if_else(
      sales_amount >= 1500,
      "Stor order",
      "Mindre order",
      missing = NA_character_
    )
  )


sales_ready %>% 
  summarize(
    total_sales = sum(sales_amount, na.rm = TRUE),
    average_sales = mean(sales_amount, na.rm = TRUE),
    average_quantity = mean(quantity, na.rm = TRUE)
  )


sales_ready %>% 
  group_by(city) %>% 
  summarize(
    total_sales = sum(sales_amount, na.rm = TRUE),
    orders = n(),
    return_rate = mean(returned, na.rm = TRUE),
    average_quantity = mean(quantity, na.rm = TRUE)
  )



sales_ready %>% 
  arrange(order_date)

sales_ready %>% 
  filter(order_date >= as.Date("2026-01-15"))

glimpse(sales_ready)





