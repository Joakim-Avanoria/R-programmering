# Hur skiljer sig medlemskapstyperna åt i användning och spend?
# Finns det tecken på att lägre engagemang hänger ihop med avhopp?

membership_summary <- gym %>% 
  group_by(membership_type) %>% 
  summarize(
    avg_visits = mean(visits_last_30d, na.rm = TRUE),
    median_visits = median(visits_last_30d, na.rm = TRUE),
    avg_spend = mean(monthly_spend, na.rm = TRUE),
    median_spend = median(monthly_spend, na.rm = TRUE),
    avg_minutes = mean(avg_visit_minutes, na.rm = TRUE),
    avg_pt = mean(personal_training_sessions, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  )

membership_summary



compare_membership <- gym %>% 
  filter(membership_type %in% c("Basic", "Premium"))

compare_membership %>% 
  group_by(membership_type) %>% 
  summarize(
    avg_spend = mean(monthly_spend, na.rm = TRUE),
    median_spend = median(monthly_spend, na.rm = TRUE),
    sd_spend = sd(monthly_spend, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  )


membership_test <- t.test(monthly_spend ~ membership_type, data = compare_membership)

membership_test
membership_test$p.value
membership_test$conf.int
membership_test$estimate


cancellation_summary <- gym %>% 
  group_by(cancelled_last_60d) %>% 
  summarize(
    avg_visits = mean(visits_last_30d, na.rm = TRUE),
    median_visits = median(visits_last_30d, na.rm = TRUE),
    avg_app = mean(app_engagement_score, na.rm = TRUE),
    median_app = median(app_engagement_score, na.rm = TRUE),
    avg_satisfaction = mean(satisfaction_score, na.rm = TRUE),
    median_satisfaction = median(satisfaction_score, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  )

cancellation_summary




cancel_test <- t.test(data = gym, app_engagement_score ~ cancelled_last_60d)

gym %>% 
  group_by(cancelled_last_60d) %>% 
  summarize(
    avg_app = mean(app_engagement_score, na.rm = TRUE),
    median_app = median(app_engagement_score, na.rm = TRUE),
    sd_app = sd(app_engagement_score, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  )

cancel_test
cancel_test$p.value
cancel_test$conf.int
cancel_test$estimate



gym %>% 
  summarise(
    cor_visits_spend = cor(visits_last_30d, monthly_spend, use = "complete.obs"),
    cor_app_satisfaction = cor(app_engagement_score, satisfaction_score, use = "complete.obs"),
  )




















