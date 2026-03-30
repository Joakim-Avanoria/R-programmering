library(tidyverse)
sales_ready <- read_csv("sales_ready.csv")


city_examples <- c("Stockholm", "Stockholm ", " Göteborg", "Malmö", "New York")

city_examples


str_trim(city_examples)

category_examples <- c("Teknik", "teknik", "TEKNIK", "Möbler", "möbler")

str_to_lower(category_examples)
str_to_upper(category_examples)
str_to_title(category_examples)


name_examples <- c("Anna Andersson", "Bo Berg", "Cecilia Carlsson", "David Dahl")

str_detect(name_examples, "Anna")

sales_ready %>% 
  filter(str_detect(str_to_lower(customer_name), "anna"))


city_examples2 <- c("Goteborg", "Malmo", "Stockholm")

str_replace(city_examples2, "Goteborg", "Göteborg")
str_replace_all(city_examples2, "Goteborg", "Göteborg")


product_codes <- c("TEK-1001", "MOB-2030", "KON-4500")


str_sub(product_codes, 5, 8)


product_data <- tibble(
  product_code = c("TEK-1001", "MOB-2030", "KON-4500")
)

product_data %>% 
  separate(product_code, into = c("category_code", "product_number"), sep = "-") %>% 
  unite("full_code", category_code, product_number, sep = "-")


messy_codes <- c("ID: 1001", "IDsdfsdfdsf: 2045", "IDs: 3099")


str_replace_all(messy_codes, "[^0-9]", "")



messy_text_data <- tibble(
  store_name = c(" butik nord ", "BUTIK syd", "Butik Väst ", " butik Öst"),
  region_code = c("NORD-01", "SYD-02", "VAST-03", "OST-04")
)

messy_text_data %>% 
  mutate(
    store_name = str_trim(store_name),
    store_name = str_to_lower(store_name),
    region_code = str_to_upper(region_code)
  ) %>% 
  separate(region_code, into = c("region", "store_id"), sep = "-")



sales_ready %>% 
  filter(str_detect(str_to_lower(customer_name), "q|z"))
