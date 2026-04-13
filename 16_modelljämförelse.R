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
    house_style = as.factor(House_Style),
    .keep = "none"
  )


model_1 <- lm(sale_price ~ living_area, data = housing_model_data)

model_2 <- lm(
  sale_price ~ living_area + building_age + overall_quality,
  data = housing_model_data
)

model_3 <- lm(
  sale_price ~ living_area + building_age + overall_quality + house_style,
  data = housing_model_data
)

model_comparison <- tibble(
  model = c(
    "Model 1: living_area",
    "Model 2: + building_age + overall_quality",
    "Model 3: + house_style"
  ),
  r_squared = c(
    summary(model_1)$r.squared,
    summary(model_2)$r.squared,
    summary(model_3)$r.squared
  ),
  adjusted_r_squared = c(
    summary(model_1)$adj.r.squared,
    summary(model_2)$adj.r.squared,
    summary(model_3)$adj.r.squared
  ),
  residual_se = c(
    summary(model_1)$sigma,
    summary(model_2)$sigma,
    summary(model_3)$sigma
  )
)

model_comparison


model_3_diagnotics <- housing_model_data %>% 
  mutate(
    fitted_value = fitted(model_3),
    residual = resid(model_3)
  )

model_3_diagnotics %>% 
  select(sale_price, fitted_value, residual) %>% 
  slice_head(n = 10)




ggplot(model_3_diagnotics, aes(x = fitted_value, y = residual)) +
  geom_point(alpha = 0.5) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    title = "Residualer mot predikterade värden",
    x = "Predikterat pris",
    y = "Residual"
  )



ggplot(model_3_diagnotics, aes(x = fitted_value, y = sale_price)) +
  geom_point(alpha = 0.5) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  labs(
    title = "Faktiskt pris mot predikterat pris",
    x = "Predikterat pris",
    y = "Faktiskt pris"
  )


qqnorm(resid(model_3))
qqline(resid(model_3), col = "red")





model_1_diagnostics <- housing_model_data %>% 
  mutate(
    fitted_value = fitted(model_1),
    residual = resid(model_1),
    model = "Model 1"
  )

model_3_diagnostics <- housing_model_data %>% 
  mutate(
    fitted_value = fitted(model_3),
    residual = resid(model_3),
    model = "Model 3"
  )




bind_rows(model_1_diagnostics, model_3_diagnostics) %>% 
  ggplot(aes(x = fitted_value, y = residual)) +
  geom_point(alpha = 0.4) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  facet_wrap(~model) +
  labs(
    title = "Residualer i enkel och mer avancerad modell",
    x = "Predikterat pris",
    y = "Residual"
  )
