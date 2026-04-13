library(tidyverse)
library(AmesHousing)

ames_raw <- make_ames()

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


model_2_num <- lm(
  sale_price ~ living_area + building_age + overall_quality,
  data = housing_model_data
)

summary(model_2_num)

model_1 <- lm(sale_price ~ living_area, data = housing_model_data)


tibble(
  model = c(
    "Model 1: living_area",
    "Model 2: + building_age + overall_quality"
  ),
  r_squared = c(
    summary(model_1)$r.squared,
    summary(model_2_num)$r.squared
  ),
  adjusted_r_squared = c(
    summary(model_1)$adj.r.squared,
    summary(model_2_num)$adj.r.squared
  )
)


ggplot(housing_model_data, aes(x = living_area, y = sale_price, color = overall_quality)) +
  geom_point(alpha = 0.5) +
  labs(
    title = "Bostadspris och boyta, färgat efter kvalitet",
    x = "Boyta",
    y = "Försäljningspris",
    color = "Kvalitet"
  ) +
  theme_minimal()



model_3 <- lm(
  sale_price ~ living_area + building_age + overall_quality + house_style,
  data = housing_model_data
)

summary(model_3)


levels(housing_model_data$house_style)


