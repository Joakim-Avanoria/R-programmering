p_spend_membership <- ggplot(gym, aes(x = membership_type, y = monthly_spend)) +
  geom_boxplot() +
  labs(
    title = "Monthly spend per membership type",
    x = "Membership type",
    y = "Monthly spend"
  )

p_spend_membership


p_visits_membership <- ggplot(gym, aes(x = membership_type, y = visits_last_30d)) +
  geom_boxplot() +
  labs(
    title = "Visits last 30 days per membership type",
    x = "Membership type",
    y = "Visits last 30 days"
  )

p_visits_membership




p_app_cancel <- ggplot(gym, aes(x = cancelled_last_60d, y = app_engagement_score)) +
  geom_boxplot() +
  labs(
    title = "App engagement score by cancellation status",
    x = "Cancelled last 60 days",
    y = "App engagement score"
  )

p_app_cancel


p_satisfaction_cancel <- ggplot(gym, aes(x = cancelled_last_60d, y = satisfaction_score)) +
  geom_boxplot() +
  labs(
    title = "Satisfaction score by cancellation status",
    x = "Cancelled last 60 days",
    y = "Satisfaction score"
  )


p_satisfaction_cancel



p_visits_spend <- ggplot(gym, aes(x = visits_last_30d, y = monthly_spend)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    title = "Visits and monthly spend",
    x = "Visits last 30 days",
    y = "Monthly spend"
  )

p_visits_spend


# ggsave("output/figures/monthly_spend_membership.png", p_spend_membership, width = 8, height = 5)
# ggsave("output/figures/visits_membership.png", p_visits_membership, width = 8, height = 5)
# ggsave("output/figures/app_engagement_cancellation.png", p_app_cancel, width = 8, height = 5)
# ggsave("output/figures/satisfaction_cancellation.png", p_satisfaction_cancel, width = 8, height = 5)
# ggsave("output/figures/visits_vs_spend.png", p_visits_spend, width = 8, height = 5)







