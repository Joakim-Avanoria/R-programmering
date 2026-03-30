library(tidyverse)

sales_raw <- read_csv("sales_raw_2.csv")
View(sales_raw)

glimpse(sales_raw)
summary(sales_raw)

sales_ready <- sales_raw %>% 
  mutate(
    customer_id = as.integer(customer_id),
    city = str_trim(city),
    city = str_to_title(city),
    returned = str_trim(returned),
    returned = str_to_lower(returned),
    order_date = as.Date(order_date),
    city = str_replace_all(city, "Goteborg", "Göteborg"),
    city = str_replace_all(city, "Malmo", "Malmö"),
    sales_amount = str_replace_all(sales_amount, " ", ""),
    sales_amount = as.numeric(sales_amount),
    returned = if_else(returned == "ja", TRUE, FALSE, missing = NA),
    quantity = if_else(
      is.na(quantity),
      as.integer(round(median(quantity, na.rm = TRUE))),
      quantity
    ),
    quantity = as.integer(quantity),
    order_size = case_when(
      sales_amount >= 1500 ~ "Stor order",
      sales_amount < 1500 ~ "Mindre order",
      TRUE ~ NA_character_
    )
  )

glimpse(sales_ready)

sales_ready %>% 
  select(customer_name, city, category, sales_amount)

sales_ready %>% 
  select(customer_id:category)

sales_ready %>% 
  select(-customer_id, -customer_name)



sales_ready %>% 
  filter(city == "Stockholm")

sales_ready %>% 
  filter(sales_amount >= 1500)


sales_ready %>% 
  filter(city == "Stockholm" & category == "Teknik")
sales_ready %>% 
  filter(city == "Stockholm", category == "Teknik")


sales_ready %>% 
  filter(city == "Stockholm" | category == "Teknik")


sales_ready %>% 
  filter(city == "Stockholm" | city == "Malmö" | city == "Göteborg")


sales_ready %>% 
  filter(city %in% c("Stockholm", "Göteborg", "Malmö"))

sales_ready %>% 
  filter(!is.na(city))


sales_ready %>% 
  arrange(sales_amount)
sales_ready %>% 
  arrange(desc(sales_amount))

sales_ready %>% 
  arrange(city, desc(sales_amount))


View(sales_ready %>% 
  mutate(sales_per_unit = sales_amount / quantity))


View(sales_ready %>% 
  mutate(above_avg_sales = sales_amount > mean(sales_amount, na.rm = TRUE)))




sales_ready %>% 
  mutate(
    order_level = if_else(sales_amount >= 1500, "Stor", "Liten", missing = NA)
  )


sales_ready %>% 
  mutate(
    order_level = case_when(
      sales_amount >= 1800 ~ "Mycket hög",
      sales_amount >= 1200 ~ "Hög",
      sales_amount < 1200 ~ "Lägre",
      .default = NA_character_
    ))


sales_ready %>% 
  summarise(
    total_sales = sum(sales_amount, na.rm = TRUE),
    average_sales = mean(sales_amount, na.rm = TRUE),
    return_rate = mean(returned, na.rm = TRUE),
    total_orders = n()
  )



sales_ready %>% 
  group_by(city) %>% 
  summarise(
    total_sales = sum(sales_amount, na.rm = TRUE),
    average_sales = mean(sales_amount, na.rm = TRUE),
    return_rate = mean(returned, na.rm = TRUE),
    total_orders = n()
  )

sales_ready %>% 
  group_by(category) %>% 
  summarise(
    total_sales = sum(sales_amount, na.rm = TRUE),
    average_quantity = mean(quantity, na.rm = TRUE),
    total_orders = n()
  )




sales_ready %>% 
  group_by(city, category) %>% 
  summarise(
    total_sales = sum(sales_amount, na.rm = TRUE),
    total_orders = n(),
    .groups = "drop"
  )


sales_ready %>% 
  filter(city %in% c("Stockholm", "Göteborg", "Malmö")) %>% 
  filter(category == "Teknik") %>% 
  filter(sales_amount >= 1000) %>% 
  select(customer_name, city, category, sales_amount, quantity, returned) %>% 
  arrange(desc(sales_amount))


sales_ready %>% 
  group_by(category) %>% 
  mutate(
    category_avg_sales = mean(sales_amount, na.rm = TRUE),
    diff_from_category_avg = sales_amount - category_avg_sales
  )



sales_ready %>% 
  group_by(category) %>% 
  summarise(total_sales = sum(sales_amount, na.rm = TRUE)) %>% 
  ungroup()




city_category_summary <- sales_ready %>% 
  group_by(city, category) %>% 
  summarise(
    total_sales = sum(sales_amount, na.rm = TRUE),
    average_sales = mean(sales_amount, na.rm = TRUE),
    return_rate = mean(returned, na.rm = TRUE),
    total_orders = n(),
    .groups = "drop"
  ) %>% 
  arrange(city, desc(total_sales))


city_category_summary




city_category_summary %>% 
  filter(!is.na(city)) %>% 
  arrange(desc(total_sales))


sales_ready %>% 
  distinct(city, category)


write_csv(sales_ready, "sales_ready.csv")
