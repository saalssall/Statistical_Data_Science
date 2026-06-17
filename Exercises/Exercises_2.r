# ─────────────────────────────────────────────
# Libraries
# ─────────────────────────────────────────────
install.packages("dplyr")      
install.packages("ggplot2")
install.packages("fivethirtyeight")  # contains food_consumption dataset

library(dplyr)
library(ggplot2)
library(fivethirtyeight)

# ─────────────────────────────────────────────
# Load Data
# ─────────────────────────────────────────────
data("food_consumption")  

# ─────────────────────────────────────────────
# Measures of Central Tendency — All Categories
# ─────────────────────────────────────────────

# Mean food consumption
mean(food_consumption$consumption)

# Median food consumption
median(food_consumption$consumption)

# Mode — most frequent food category
food_consumption %>%
  count(food_category, sort = TRUE)  # fixed: missing column name

# ─────────────────────────────────────────────
# Rice Category Analysis
# ─────────────────────────────────────────────

# Histogram of CO2 emissions for rice
food_consumption %>%
  filter(food_category == "rice") %>%
  ggplot(aes(x = co2_emission)) +
  geom_histogram(binwidth = 10, fill = "steelblue", color = "white") +
  labs(title = "CO2 Emissions from Rice Consumption",
       x = "CO2 Emission",
       y = "Count")

# Mean and median CO2 emissions for rice
food_consumption %>%
  filter(food_category == "rice") %>%
  summarise(
    mean_co2   = mean(co2_emission),
    median_co2 = median(co2_emission)
  )