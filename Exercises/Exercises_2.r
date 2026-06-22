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
  n = c(23, 62, 15, 40, 5, 11, 2, 8, 7, 2, 3),
  num_users = c(19, 43, 87, 83, 17, 2, 29, 13, 80, 23, 26)
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

# Set random seed to 10
set.seed(10)

# Simulate a single deal
rbinom(1, 1, 0.3)
# Simulate 1 week of 3 deals
rbinom(1, 3, 0.3)
# Simulate 52 weeks of 3 deals
deals <- rbinom(52, 3, 0.3)

# Calculate mean deals won per week
mean(deals)

# Probability of closing 3 out of 3 deals
dbinom(3, 3, 0.3)

# Probability of closing <= 1 deal out of 3 deals
pbinom(1, 3, 0.3)

# Probability of closing > 1 deal out of 3 deals
pbinom(1, 3, 0.3, lower.tail = FALSE)

# Expected number won with 30% win rate
won_30pct <- 3 * 0.3
won_30pct

# Expected number won with 25% win rate
won_25pct <- 3 * 0.25
won_25pct

# Expected number won with 35% win rate
won_35pct <- 3 * 0.35
won_35pct

# Probability of deal between 3000 and 7000
upper_prob <- pnorm(7000, mean = 5000, sd = 2000) 
lower_prob <- pnorm(3000, mean = 5000, sd = 2000)

probability <- upper_prob - lower_prob
probability
# Calculate amount that 75% of deals will be more than
qnorm(0.75, mean = 5000, sd = 2000, lower.tail = FALSE)

# Calculate new average amount
new_mean <- 5000 * 1.2

# Calculate new standard deviation
new_sd <- 2000 * 1.3

# Simulate 36 sales
new_sales <- new_sales %>% 
  mutate(amount = rnorm(36, mean = new_mean, sd = new_sd))

# Create histogram with 10 bins
ggplot(new_sales, aes (amount)) + geom_histogram(bins = 10)

#Central limit theorem 
# Set seed to 104
set.seed(104)

# Sample 20 num_users with replacement from amir_deals
sample(amir_deals$num_users, 20, replace = TRUE) %>%
  # Take mean
  mean()

# Repeat the above 100 times
sample_means <- replicate(100, sample(amir_deals$num_users, size = 20, replace = TRUE) %>% mean())

# Create data frame for plotting
samples <- data.frame(mean = sample_means)

# Histogram of sample means
ggplot(samples, aes(mean)) + geom_histogram(bins=10)


# Set seed to 321
set.seed(321)
# Take 30 samples of 20 values of num_users, take mean of each sample
sample_means <- replicate(30, sample(all_deals$num_users, 20) %>% mean())
# Calculate mean of sample_means
mean(sample_means)
# Calculate mean of num_users in amir_deals
mean(amir_deals$num_users)

# Possion distribution
# Probability of 5 responses
dpois(5, lambda = 4)
# Probability of 5 responses from coworker
dpois(5, lambda = 5.5)
# Probability of 2 or fewer responses
ppois(2, lambda = 4, lower.tail = TRUE)
# Probability of > 10 responses
ppois(10, lambda = 4, lower.tail = FALSE)


# Exponential distribution
# Probability response takes < 1 hour
pexp(1, rate = 1/2.5)
# Probability response takes > 4 hours
pexp(4, rate = 1/2.5, lower.tail = FALSE)
# Probability response takes 3-4 hours
pexp(4, rate = 1/2.5) - pexp(3, rate = 1/2.5)

#Correlation
# Create a scatterplot of happiness_score vs. life_exp
ggplot(world_happiness, aes(x = life_exp, y = happiness_score)) + geom_point()

# Add a linear trendline to scatterplot
 + geom_smooth(method = "lm", se = FALSE)
# Correlation between life_exp and happiness_score
cor(world_happiness$life_exp, world_happiness$happiness_score)

# Scatterplot of gdp_per_cap and life_exp
ggplot(world_happiness, aes(gdp_per_cap, life_exp)) +
  geom_point()

# Correlation between gdp_per_cap and life_exp
cor(world_happiness$gdp_per_cap, world_happiness$life_exp)

# Create log_gdp_per_cap column
world_happiness <- world_happiness %>%
  mutate(log_gdp_per_cap = log(gdp_per_cap))
# Scatterplot of happiness_score vs. log_gdp_per_cap
ggplot(world_happiness, aes(log_gdp_per_cap, happiness_score)) +
  geom_point()
# Calculate correlation
cor(world_happiness$log_gdp_per_cap, world_happiness$happiness_score)


# Calculate the probability that 2 are heads using dbinom
dbinom(2, 10, 0.3)
# Confirm your answer with a simulation using rbinom
mean(rbinom(10000, 10, 0.3) == 2)

# Calculate the probability that at least five coins are heads
1 - pbinom(4, 10, 0.3)
# Confirm your answer with a simulation of 10,000 trials
mean(rbinom(10000, 10, 0.3)>=5)


#Binomial probability 

#What is the expected value of a binomial distribution where 25 coins are flipped, each having a 30% chance of heads?
# Calculate the expected value using the exact formula
set.seed(2017)
expected_value = 25 * 0.3
expected_value
# Confirm with a simulation using rbinom
simulated_values <- mean(rbinom(10000, 25, 0.3))
simulated_values 

# Calculate the variance using the exact formula
set.seed(2017)
variance = 25 * 0.3 * (1 - 0.3)
variance
# Confirm with a simulation using rbinom
simulated_var <- var(rbinom(10000, 25, 0.3))
simulated_var

# Simulate 100,000 flips of a coin with a 40% chance of heads
A <- rbinom(100000, 1, 0.4)
# Simulate 100,000 flips of a coin with a 20% chance of heads
B <- rbinom(100000, 1, 0.2)
# Estimate the probability both A and B are heads
prob <- A & B
mean(prob)

# You've already simulated 100,000 flips of coins A and B
A <- rbinom(100000, 1, .4)
B <- rbinom(100000, 1, .2)
# Simulate 100,000 flips of coin C (70% chance of heads)
C <- rbinom(100000, 1, .7)
# Estimate the probability A, B, and C are all heads
prob_all_heads <- A & B & C
mean(prob_all_heads)

# Simulate 100,000 flips of a coin with a 60% chance of heads
A <- rbinom(100000, 1, 0.6)
# Simulate 100,000 flips of a coin with a 10% chance of heads
B <- rbinom(100000, 1, 0.1)
# Estimate the probability either A or B is heads
mean (A | B)

# Use rbinom to simulate 100,000 draws from each of X and Y
X <- rbinom(100000, 10, 0.6)
Y <- rbinom(100000, 10, 0.7)

# Estimate the probability either X or Y is <= to 4
mean((X <= 4 | Y <= 4))
# Use pbinom to calculate the probabilities separately
prob_X_less <- pbinom(4, 10, 0.6)
prob_Y_less <- pbinom(4, 10, 0.7)
# Combine these to calculate the exact probability either <= 4
combined_prob <- prob_X_less + prob_Y_less - prob_X_less * prob_Y_less
combined_prob

#Practicing bayes theorem
# Use dbinom to calculate the probability of 11/20 heads with fair or biased coin
probability_fair <- dbinom(11, 20, 0.5) 
probability_biased <- dbinom(11, 20, 0.75) 

# Calculate the posterior probability that the coin is fair
probability_fair / (probability_biased + probability_fair)

#A function for comparing two histograms
compare_histograms <- function(var1, var2) {
  par(mfrow = c(1, 2))
  hist(var1, main = "Binomial", col = "blue", xlab = "")
  hist(var2, main = "Poisson", col = "red", xlab = "")
  par(mfrow = c(1, 1))
}
#Poisson probability
# Draw a random sample of 100,000 from the Binomial(1000, .002) distribution
binom_sample <- rbinom(100000, 1000, .002)
# Draw a random sample of 100,000 from the Poisson approximation
poisson_sample <-  rpois(100000, 2)
# Compare the two distributions with the compare_histograms function
compare_histograms(binom_sample, poisson_sample)

#The two histograms should look nearly identical, 
#which demonstrates that Poisson is a good approximation 
#of Binomial when n is large and p is small.

# Simulate 100,000 draws from Poisson(2)
mean = 2
poisson_sample <- rpois(100000, mean)

# Find the percentage of simulated values that are 0
mean(poisson_sample == 0)

# Use dpois to find the exact probability that a draw is 0
dpois(0, 2)

#Demonstrate additive property of Poisson distributions via simulation
# Simulate 100,000 draws from Poisson(1)
X <- rpois(100000, 1)
# Simulate 100,000 draws from Poisson(2)
Y <- rpois(100000, 2)
# Add X and Y together to create Z
Z <- X + Y
# Use compare_histograms to compare Z to the Poisson(3)
compare_histograms(Z, rpois(100000, 3))

#Simulate geometric distribution via 
#repeated Binomial flips and verify with rgeom
# Simulate 100 instances of flipping a 20% coin
flips <- rbinom(100, 1, .2)
# Use which to find the first case of 1 ("heads")
which(flips == 1)[1]
# Existing code for finding the first instance of heads
which(rbinom(100, 1, 0.2) == 1)[1]
# Replicate this 100,000 times using replicate()
replications <- replicate(100000, which(rbinom(100, 1, 0.2) == 1)[1])
# Histogram the replications with qplot
qplot(replications)
# Generate 100,000 draws from the corresponding geometric distribution
geom_sample <- rgeom(100000, .2)
# Compare the two distributions with compare_histograms
compare_histograms(replications, geom_sample)

# Find the probability the machine breaks on 5th day or earlier
pgeom(4, .1) #41%
# Find the probability the machine is still working on 20th day
1- pgeom(19, .1) #12%

#Modeling
# Calculate the probability of machine working on day 1-30
still_working <- 1 - pgeom(0:29, .1)
# Plot the probability for days 1 to 30
plot(1:30, still_working, type = "l", col = "blue",
     xlab = "Day", ylab = "Probability Still Working",
     main = "Machine Survival Probability Over 30 Days")

