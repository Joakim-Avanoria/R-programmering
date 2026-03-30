library(tidyverse)
sales_ready <- read_csv("sales_ready.csv")


# Funktioner

average_no_na <- function(x) {
  mean(x, na.rm = TRUE)
}


x <- c(10, 20, NA, 30)
average_no_na(x)

average_no_na(sales_ready$sales_amount)

describe_numeric <- function(x) {
  tibble(
    mean = mean(x, na.rm = TRUE),
    median = median(x, na.rm = TRUE),
    min = min(x, na.rm = TRUE),
    max = max(x, na.rm = TRUE)
  )
}

describe_numeric(sales_ready$sales_amount)
describe_numeric(sales_ready$quantity)

rounded_mean <- function(x, digits = 2) {
  round(mean(x, na.rm = TRUE), digits = digits)
}

rounded_mean(sales_ready$sales_amount)
rounded_mean(sales_ready$sales_amount, digits = 1)

rounded_mean_v2 <- function(x, digits = 2) {
  result <- round(mean(x, na.rm = TRUE), digits = digits)
  return(result)
}

rounded_mean_v2(sales_ready$sales_amount)
rounded_mean_v2(sales_ready$sales_amount, digits = 1)

# Villkor


sales <- 1600

if(sales >= 1500) {
  print("Det här är en stor order")
} else if(sales >= 1200) {
  print("Det här är en mellanstor order")
} else {
  print("Det här är inte en stor order")
}


classify_order <- function(amount) {
  if (is.na(amount)) {
    return(NA_character_)
  } else if (amount >= 1800) {
    return ("Mycket hög")
  } else if (amount >= 1200) {
    return ("Hög")
  } else {
    return ("Lägre")
  }
}


classify_order(1900)
classify_order(1300)
classify_order(800)
classify_order(NA)


amounts <- c(900, 1300, 1900, NA)
order_labels <- character(length(amounts))


for (amount in amounts) {
  print(classify_order(amount))
}

for (i in seq_along(amounts)) {
  order_labels[i] <- classify_order(amounts[i])
}

order_labels


sapply(amounts, classify_order)

numeric_columns <- c("sales_amount", "quantity")

for (col_name in numeric_columns) {
  cat("Kolumn:", col_name, "\n")
  cat("Medelvärde:", mean(sales_ready[[col_name]], na.rm = TRUE), "\n\n")
}



filter_large_orders <- function(data, threshold = 1500) {
  if (!"sales_amount" %in% names(data)) {
    stop("Kolumnen sales_amount finns inte i datan.")
  }
  
  data %>% 
    filter(sales_amount >= threshold)
}


filter_large_orders(sales_ready)
filter_large_orders(sales_ready, threshold = 2000)


categories <- unique(sales_ready$category)

categories

for (cat_name in categories) {
  cat("Kategori:", cat_name, "\n")
  
  avg_sales <- sales_ready %>% 
    filter(category == cat_name) %>% 
    summarise(avg_sales = mean(sales_amount, na.rm = TRUE)) %>% 
    pull(avg_sales)
  
  cat("Genomsnittlig försäljning:", avg_sales, "\n\n")
}

