# ─────────────────────────────────────────────
# Libraries
# ─────────────────────────────────────────────
install.packages("dplyr")
install.packages("ggplot2")
library(dplyr)
library(ggplot2)
library(readr)

# ─────────────────────────────────────────────
# Load Data
# ─────────────────────────────────────────────
food_consumption <- 
  read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-02-18/food_consumption.csv")

# ─────────────────────────────────────────────
# Measures of Central Tendency — All Categories
# ─────────────────────────────────────────────

# Mean food consumption
mean(food_consumption$consumption)

# Median food consumption
median(food_consumption$consumption)

# Mode — most frequent food category
food_consumption |>
  count(food_category, sort = TRUE)  

# ─────────────────────────────────────────────
# Rice Category Analysis
# ─────────────────────────────────────────────

# Histogram of CO2 emissions for rice
food_consumption |>
  filter(food_category == "Rice") |>
  ggplot(aes(x = co2_emmission)) +
  geom_histogram(binwidth = 10, fill = "steelblue", color = "white")

#Analysis: only a few countries produce a very high volume of co2 
#while majority contribute to insignificant co2 emission

# Mean and median CO2 emissions for rice
food_consumption |>
  filter(food_category == "Rice") |>
  summarise(
    mean_co2   = mean(co2_emmission),
    median_co2 = median(co2_emmission)
  )

#Analysis: most countries have modest rice co2 emission with 
# 15.2kg being the typical country (better explained by median 
#as mean is sensitive to outlier)

# ─────────────────────────────────────────────
# Measures of Spread
# ─────────────────────────────────────────────

# Calculate the variance of co2_emission
var(food_consumption$co2_emmission)
# Calculate the standard deviation of co2_emission
sd(food_consumption$co2_emmission)

#Calculate the quintiles of the co2_emission column 
#of food_consumption that split up the data into 5 pieces.
quantile(food_consumption$co2_emmission, probs = c(0, 0.2, 0.4, 0.6, 0.8, 1))

#Calculate the quantiles of co2_emission that split up the data into ten pieces.
quantile(food_consumption$co2_emmission, probs = c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1))

# Compute the 25th percentile and 75th percentile of co2_emission
q1 <- quantile(food_consumption$co2_emmission, 0.25)
q3 <- quantile(food_consumption$co2_emmission, 0.75)

# Compute the IQR of co2_emission
iqr <- q3 - q1

# Calculate the lower and upper cutoffs for outliers
lower <- q1 - 1.5 * iqr
upper <- q3 + 1.5 * iqr

lower
upper

# Filter emissions_by_country to find outliers
food_consumption %>%
  filter(co2_emmission < lower 
         | co2_emmission > upper)


# Probability
amir_deals <- data.frame(
  product = c("Product A", "Product B", "Product C", "Product D", "Product E",
              "Product F", "Product G", "Product H", "Product I", "Product J", "Product N"),
  n = c(23, 62, 15, 40, 5, 11, 2, 8, 7, 2, 3)
)

# Calculate probability of each product
amir_deals |>
  mutate(prob = n / sum(n))
# Set random seed to 31
set.seed(31)

# Sample 5 deals without replacement
amir_deals %>%
  sample_n(5, prob = FALSE)

restaurant_groups <- data.frame(
  group_id   = c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J"),
  group_size = c(2, 4, 6, 2, 2, 2, 3, 2, 4, 2)
)

# Create probability distribution
# or builds a probability distribution 
#table from the restaurant groups data
size_distribution <- restaurant_groups |>
  count(group_size) |>
  mutate(probability = n / sum(n))

size_distribution

# Calculate expected group size
expected_val <- sum(size_distribution$group_size *
                      size_distribution$probability)
expected_val

# Analysis: If a random group walks into the restaurant,
# you would expect them to have 2.9 people on average.

# Probability of picking a group of 4 or more:
# This calculates the probability that a randomly
# picked group has 4 or more people.
# Analysis: 30% chance that a randomly selected group
# will have 4 or more people.

size_distribution |>
  filter(group_size >= 4) |>
  summarize(prob_4_or_more = sum(probability))


# Min and max wait times for back-up that happens every 30 min
min <- 0
max <- 30

# Calculate probability of waiting less than 5 mins
prob_less_than_5 <- punif(5, min, max)
prob_less_than_5

# Calculate probability of waiting more than 5 mins
prob_greater_than_5 <- punif(5, min, max, lower.tail = FALSE)
prob_greater_than_5

# Calculate probability of waiting 10-20 mins
prob_between_10_and_20 <- punif(20, min, max) - punif(10, min, max)
prob_between_10_and_20 

