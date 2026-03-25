install.packages("tidyverse")
library(tidyverse)

search()

data <- read.csv("data.csv")
head(data)

glimpse(data)


data[data$age > 30, ]

data %>% filter(age > 30)

data[, c("name", "age")]

data %>% select(name, age)

aggregate(age ~ city, data = data, FUN = mean)

data %>% 
  group_by(city) %>% 
  summarize(avg_age = mean(age))

data %>% 
  mutate(age_in_10_years = age + 10)

head(data)

data %>% 
  filter(age > 25) %>% 
  group_by(city) %>% 
  summarize(avg_salary = mean(salary))




