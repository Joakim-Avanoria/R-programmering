install.packages("AmesHousing")

library(tidyverse)
library(AmesHousing)
search()
ames_raw <- make_ames()

glimpse(ames_raw)


housing_model_data <- ames_raw %>% 
  mutate(
    sale_price = Sale_Price,
    living_area = Gr_Liv_Area,
    lot_area = Lot_Area,
    overall_quality = Overall_Qual,
    building_age = Year_Sold - Year_Built,
    neighborhood = Neighborhood,
    house_style = House_Style,
    .keep = "none"
  )
glimpse(housing_model_data)


housing_model_data %>% 
  select(sale_price, living_area) %>% 
  slice_head(n = 10)

ggplot(housing_model_data, aes(x = living_area, y = sale_price)) +
  geom_point(alpha = 0.5) +
  labs(
    title = "Bostadspris mot boyta",
    x = "Boyta",
    y = "Försäljningspris"
  ) +
  theme_minimal()


model_1 <- lm(sale_price ~ living_area, data = housing_model_data)


summary(model_1)


coef(model_1)


ggplot(housing_model_data, aes(x = living_area, y = sale_price)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(
    title = "Linjär regression: bostadspris mot boyta",
    x = "Boyta",
    y = "Försäljningspris"
  ) +
  theme_minimal()


new_housing <- tibble(
  living_area = c(1000, 1500, 2000)
)

predict(model_1, newdata = new_housing)
