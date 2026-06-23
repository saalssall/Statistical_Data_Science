#Regression in R
# ─────────────────────────────────────────────
# Libraries
# ─────────────────────────────────────────────
install.packages("dplyr")
install.packages("ggplot2")
library(dplyr)
library(ggplot2)
library(readr)
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

