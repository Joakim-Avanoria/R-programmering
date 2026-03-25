library(tidyverse)

data <- read.csv("data.csv")

ggplot(data, aes(x = age, y = salary)) +
  geom_point()

ggplot(data, aes(x = age, y = salary, color = city)) +
  geom_point(size = 3, alpha = 0.7)


ggplot(data, aes(x = age)) +
  geom_histogram(bins = 5)

ggplot(data, aes(x = age)) +
  geom_bar()


ggplot(data, aes(x = age, y = salary)) +
  geom_point() +
  labs(
    title = "Age vs Salary",
    x = "Age",
    y = "Salary"
  )







