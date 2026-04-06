library(tidyverse)

source("11_explorativ_analys.R")

compare_data <- customers %>% 
  filter(segment %in% c("Standard", "Premium"))

compare_data %>% 
  group_by(segment) %>% 
  summarize(
    avg_spend = mean(total_spend, na.rm = TRUE),
    median_spend = median(total_spend, na.rm = TRUE),
    sd_spend = sd(total_spend, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  )

ggplot(compare_data, aes(x = segment, y = total_spend)) +
  geom_boxplot() +
  labs(
    title = "Total spend i Standard och Premium",
    x = "Segment",
    y = "Total spend"
  )


ggplot(compare_data, aes(x = total_spend, fill = segment)) +
  geom_histogram(alpha = 0.5, position = "identity", bins = 25) +
  labs(
    title = "Fördelning av total spend i två segment",
    x = "Total spend",
    y = "Antal kunder"
  )



spend_test <- t.test(total_spend ~ segment, data = compare_data)

spend_test




spend_test$p.value
spend_test$conf.int
spend_test$estimate


compare_data %>% 
  group_by(segment) %>% 
  summarize(
    mean_spend = mean(total_spend, na.rm = TRUE),
    .groups = "drop"
  )


customers %>% 
  group_by(returned_last_order) %>% 
  summarize(
    avg_satisfaction = mean(satisfaction_score, na.rm = TRUE),
    median_satisfaction = median(satisfaction_score, na.rm = TRUE),
    sd_satisfaction = sd(satisfaction_score, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  )




ggplot(customers, aes(x = returned_last_order, y = satisfaction_score)) +
  geom_boxplot() +
  labs(
    title = "Satisfaction score beroende på returstatus",
    x = "Returned last order",
    y = "Satisfaction score"
  )


satisfaction_test <- t.test(satisfaction_score ~ returned_last_order, data = customers)

satisfaction_test

satisfaction_test$p.value
satisfaction_test$conf.int
satisfaction_test$estimate











