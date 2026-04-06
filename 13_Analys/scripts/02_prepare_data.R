glimpse(gym_raw)
summary(gym_raw)
colSums(is.na(gym_raw))

gym_raw %>% 
  count(membership_type)
gym_raw %>% 
  count(branch_region)
gym_raw %>% 
  count(cancelled_last_60d)


gym <- gym_raw %>% 
  mutate(
    membership_type = str_trim(membership_type),
    membership_type = str_to_title(membership_type),
    branch_region = str_trim(branch_region),
    branch_region = str_to_title(branch_region),
    cancelled_last_60d = str_trim(cancelled_last_60d),
    cancelled_last_60d = str_to_title(cancelled_last_60d),
    membership_type = as.factor(membership_type),
    branch_region = as.factor(branch_region),
    cancelled_last_60d = as.factor(cancelled_last_60d)
  )

gym %>% 
  count(membership_type)
gym %>% 
  count(branch_region)
gym %>% 
  count(cancelled_last_60d)



gym <- gym %>% 
  mutate(
    high_engagement = if_else(
      visits_last_30d >= 8 | app_engagement_score >= 75,
      "High",
      "Lower",
      missing = "Lower"
    ),
    high_engagement = as.factor(high_engagement),
    spend_per_visit = if_else(
      visits_last_30d > 0,
      monthly_spend / visits_last_30d,
      NA_real_
    )
  )


glimpse(gym)
summary(gym)
