library(tidyverse)
customers_raw <- read_csv("customer_behavior.csv")

glimpse(customers_raw)
summary(customers_raw)

colSums(is.na(customers_raw))

customers_raw %>% 
  count(segment, sort = TRUE)

customers_raw %>% 
  count(region, sort = TRUE)

customers_raw %>% 
  count(returned_last_order, sort = TRUE)


customers <- customers_raw %>% 
  mutate(
    segment = str_trim(segment),
    segment = str_to_title(segment),
    region = str_trim(region),
    region = str_to_title(region),
    returned_last_order = str_trim(returned_last_order),
    returned_last_order = str_to_title(returned_last_order),
    segment = as.factor(segment),
    region = as.factor(region),
    returned_last_order = as.factor(returned_last_order)
  )

glimpse(customers)


customers %>% 
  count(segment, sort = TRUE)

customers %>% 
  count(region, sort = TRUE)

customers %>% 
  count(returned_last_order, sort = TRUE)


ggplot(customers, aes(x = segment)) +
  geom_bar() +
  labs(
    title = "Antal kunder per segment",
    x = "Segment",
    y = "Antal kunder"
  )


ggplot(customers, aes(x = region)) +
  geom_bar() +
  labs(
    title = "Antal kunder per region",
    x = "Region",
    y = "Antal kunder"
  )


customers %>% 
  summarize(
    avg_spend = mean(total_spend, na.rm = TRUE),
    median_spend = median(total_spend, na.rm = TRUE),
    sd_spend = sd(total_spend, na.rm = TRUE),
    min_spend = min(total_spend, na.rm = TRUE),
    q1_spend = quantile(total_spend, 0.25, na.rm = TRUE),
    q3_spend = quantile(total_spend, 0.75, na.rm = TRUE),
    max_spend = max(total_spend, na.rm = TRUE)
  )


ggplot(customers, aes(x = total_spend)) +
  geom_histogram(bins = 30) +
  labs(
    title = "Fördelning av total spend",
    x = "Total spend",
    y = "Antal kunder"
  )

ggplot(customers, aes(y = total_spend)) +
  geom_boxplot() +
  labs(
    title = "Boxplot för total spend",
    y = "Total spend"
  )


customers %>% 
  group_by(segment) %>% 
  summarise(
    avg_spend = mean(total_spend, na.rm = TRUE),
    median_spend = median(total_spend, na.rm = TRUE),
    sd_spend = sd(total_spend, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  )




ggplot(customers, aes(x = segment, y = total_spend)) +
  geom_boxplot() +
  labs(
    title = "Total spend per segment",
    x = "Segment",
    y = "Total spend"
  )


segment_summary <- customers %>% 
  group_by(segment) %>% 
  summarise(
    avg_spend = mean(total_spend, na.rm = TRUE),
    .groups = "drop"
  )


segment_summary %>% 
  ggplot(aes(x = segment, y = avg_spend)) +
  geom_col() +
  labs(
    title = "Genomsnittlig spend per segment",
    x = "Segment",
    y = "Genomsnittlig spend"
  )


ggplot(customers, aes(x = number_of_orders, y = total_spend)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE)
  labs(
    title = "Samband mellan antal ordrar och total spend",
    x = "Antal ordrar",
    y = "Total spend"
  )




  ggplot(customers, aes(x = number_of_orders, y = total_spend)) +
    geom_point(alpha = 0.6) +
    geom_smooth(method = "lm", se = FALSE) +
    facet_wrap(~ segment) +
    labs(
      title = "Samband mellan antal ordrar och total spend",
      x = "Antal ordrar",
      y = "Total spend"
    )


  customers %>% 
    count(segment, region)
  
  
  customers %>% 
    count(segment, region) %>% 
    group_by(region) %>% 
    mutate(prop = n / sum(n)) %>% 
    ungroup()
  
  
  customers %>% 
    count(segment, region) %>% 
    ggplot(aes(x = region, y = n, fill = segment)) +
    geom_col(position = "fill") +
    labs(
      title = "Andelar segment inom varje region",
      x = "Region",
      y = "Andel"
    )
