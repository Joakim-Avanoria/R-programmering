library(tidyverse)
data()

nrow(starwars)

sw <- starwars

head(sw)
glimpse(sw)
names(sw)
summary(sw)


sw <- sw %>% 
  select(name, height, mass, species, homeworld)

sw <- sw %>% 
  rename(weight = mass)


glimpse(sw)
summary(sw)

sw %>% 
  summarize(
    across(
      everything(), ~ sum(is.na(.))
    )
  )


sw <- na.omit(sw)

nrow(sw)

ggplot(sw, aes(x = height)) +
  geom_histogram(bins = 15)

ggplot(sw, aes(x = weight)) +
  geom_histogram(bins = 15)

ggplot(sw, aes(x = height, y = weight, color = species)) +
  geom_point()


sw <- sw %>% 
  mutate(
    height_m = height / 100,
    bmi = weight / (height_m^2)
  )

summary(sw)

ggplot(sw, aes(x = bmi)) +
  geom_histogram(bins = 15)


sw %>% 
  count(species, sort = TRUE)


summary_species <- sw %>% 
  group_by(species) %>% 
  summarize(
    avg_height = mean(height),
    avg_weight = mean(weight),
    avg_bmi = mean(bmi),
    n = n()
  )

print(n = 50, summary_species)


summary_species %>% 
  filter(n >= 2) %>% 
  ggplot(aes(x = reorder(species, avg_bmi), y = avg_bmi)) +
  geom_col() +
  coord_flip()


sw %>% 
  group_by(species) %>% 
  filter(n() >= 2) %>% 
  ggplot(aes(x = species, y = bmi)) +
  geom_boxplot() +
  coord_flip()







