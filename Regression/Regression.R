#Regression in R
# ─────────────────────────────────────────────
# Libraries
# ─────────────────────────────────────────────
install.packages("dplyr")
install.packages("ggplot2")
library(dplyr)
library(ggplot2)
library(readr)
getwd()
df <- read_csv("Data/telecom_churn.csv")
head(df, 10)

# Add a linear trend line without a confidence ribbon
ggplot(mtcars, aes(wt, mpg)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE)

# Using mtcars to calculate the slope and intercept of the linear regression
model_1 <- lm(mpg ~ wt, data = mtcars)
model_1

#Intercept (37.285) — when weight is 0, predicted mpg is 37.285. 
#wt (-5.344) — for every 1 unit increase in weight (1000 lbs), mpg decreases by 5.344. 
#So a heavier car gets significantly worse fuel efficiency.

# Using df, plot monthlycharges
ggplot(df, aes(MonthlyCharges)) +
  # Make it a histogram with 10 bins
  geom_histogram(bins = 10) +
  # Facet the plot so each house age group gets its own panel
  facet_wrap(vars(PhoneService))

# Calculating summary statistics 
summary_stats <- df %>% 
  # Group by churn
  group_by(Churn) %>% 
  # Summarize to calculate the mean monthly charges
  summarize(mean_by_group = mean(MonthlyCharges))
# See the result
summary_stats

#Customers who churned paid on average $74.40/month compared to $61.30/month for those who stayed. 


# Linear regression: predict MonthlyCharges from tenure
mdl_charges_vs_tenure <- lm(MonthlyCharges ~ tenure, data = df)
mdl_charges_vs_tenure
# See the result
mdl_charges_vs_tenure_no_intercept <- lm(MonthlyCharges ~ tenure + 0, data = df)
mdl_charges_vs_tenure_no_intercept


# Create a tibble with tenure column from 0 to 72
explanatory_data <- tibble(
  tenure = 0:72
)

# Use mdl_charges_vs_tenure to predict with explanatory_data
predict(mdl_charges_vs_tenure, explanatory_data)

prediction_data <- explanatory_data %>%
  mutate(MonthlyCharges = predict(mdl_charges_vs_tenure, explanatory_data))

# Add to the plot
ggplot(df, aes(tenure, MonthlyCharges)) +
  geom_point(color = "blue") +
  geom_smooth(method = "lm", se = FALSE) +
  # Add a point layer of prediction data, colored yellow
  geom_point(data = prediction_data, color = "yellow")

