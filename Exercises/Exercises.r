# Hamidullah Rezae
# Discussion Project — Probability, Sampling & Confidence Intervals
# Dataset: cleaned.csv, gradetest.csv

# ─────────────────────────────────────────────
# Libraries
# ─────────────────────────────────────────────
install.packages("Rmisc")
install.packages("dplyr")
library(Rmisc)
library(dplyr)

# ─────────────────────────────────────────────
# Section 1 — Empirical Probability (GPA)
# Dataset: cleaned.csv
# ─────────────────────────────────────────────
setwd("Desktop/Spring 2021/ITC 255")
b <- read.csv("cleaned.csv")

# Pr(GPA <= 3.5) → 73.6% of students have GPA of 3.5 or lower
ecdf(b$GPA)(3.5)

# Pr(GPA > 3) → 78.9% of students have GPA higher than 3
1 - ecdf(b$GPA)(3)

# Pr(GPA <= 4) → 100% of students have GPA no higher than 4
ecdf(b$GPA)(4)

# Pr(3 <= GPA <= 4) → 89.5% of students have GPA between 3 and 4
ecdf(b$GPA)(4) - ecdf(b$GPA)(2.9999)

# Pr(2.5 <= GPA <= 3.5) → 64.4% of students have GPA between 2.5 and 3.5
ecdf(b$GPA)(3.5) - ecdf(b$GPA)(2.499)

# Quantile: 68% of students have GPA <= 3.5
quantile(b$GPA, 0.68)

# Quantile: 70% of students have GPA > 3.2
quantile(b$GPA, 0.30)

# Quantile: 40% of students have GPA > 3.2
quantile(b$GPA, 0.40)

# ─────────────────────────────────────────────
# Section 2 — Joint Analysis (Gender, Class Level, GPA)
# Dataset: cleaned.csv
# ─────────────────────────────────────────────
setwd("Desktop")
z <- read.csv("cleaned.csv")

# 2a. Two Qualitative Variables: Gender vs Class Level
n <- table(z$Gender, z$Cl.level)
n

barplot(n,
        beside = TRUE,
        col = c("green", "red"),
        main = "Joint Barplot of Gender and Class Level",
        xlab = "Class Level",
        ylab = "Joint Frequency")
legend("topleft", legend = c("Female", "Male"), fill = c("green", "red"))

chisq.test(z$Gender, z$Cl.level)
# X-squared = 0.031, p-value > 0.05
# No significant association between Gender and Class Level.
# Students' level of study is not correlated with their gender.

# 2b. One Qualitative + One Quantitative: Gender vs GPA
gm <- z$GPA[z$Gender == "Male"]
gf <- z$GPA[z$Gender == "Female"]

# Joint density plot
plot(density(gm),
     col = "green",
     main = "GPA Distribution by Gender",
     xlab = "GPA",
     xlim = c(1, 5),
     ylim = c(0, 3))
lines(density(gf), col = "pink")
legend("topleft",
       legend = c("Male", "Female"),
       col = c("green", "pink"),
       lty = 1)
# Male students have a more widespread GPA distribution.
# Female students have more consistent, slightly higher GPA scores.

# Overlapping histograms
gpm <- hist(gm, plot = FALSE)
gpf <- hist(gf, plot = FALSE)
plot(gpm, density = 15, col = "green", angle = 90,
     xlim = c(1, 5), ylim = c(0, 20),
     main = "GPA Histogram by Gender",
     xlab = "GPA")
plot(gpf, density = 10, col = "black", angle = 180, add = TRUE)
legend("topright", legend = c("Male", "Female"), fill = c("green", "black"))

# T-test: Gender vs GPA
t.test(z$GPA ~ z$Gender)
# t = 1.43, p-value > 0.05 → weak association between Gender and GPA

# 2c. Two Quantitative Variables: Height vs Weight
cov(z$Weight, z$Height)   # 82.94 → positive covariance
cor(z$Height, z$Weight)   # 0.846 → strong positive correlation
# As height increases, weight tends to increase as well.

plot(z$Height, z$Weight,
     main = "Scatter Plot of Height vs Weight",
     xlab = "Height",
     ylab = "Weight",
     col = "steelblue")
# Strong positive association confirmed by scatter plot.

# ─────────────────────────────────────────────
# Section 3 — Sampling & Confidence Intervals
# Dataset: gradetest.csv
# ─────────────────────────────────────────────
setwd("Desktop")
g <- read.csv("gradetest.csv")
g <- g[, -1]  # Remove index column

# Random sample of size 90
set.seed(123)
d <- sample_n(g, 90)

# Q2: Point Estimation of Q variable
mean(d$Q)   # 78.65 → estimated population mean
sd(d$Q)     # 22.48 → estimated population SD
max(d$Q)    # 100

# Q3: Non-random sample — Oral scores above 80
NS <- d$Oral[d$Oral > 80]
length(NS)  # 43 students

# Q4: Point Estimation of non-random sample
mean(NS)    # 92.47
sd(NS)      # 6.44
max(NS)     # 100

# Q5: Comparison of random vs non-random sampling
compare <- data.frame(
  Method = c("Random", "Non-Random", "Error"),
  Mean   = c(78.6,  92.4,  78.6 - 92.4),
  SD     = c(22.4,   6.4,  22.4 - 6.4),
  Max    = c(100,   100,   0)
)
compare
# Non-random sampling overestimates the mean and underestimates variability.
# Random sampling is a more reliable method for estimating population parameters.

# Q6: Confidence Intervals for Q variable
CI(d$Q, ci = 0.90)
# With 90% confidence: population mean is between [74.6, 80.8]

CI(d$Q, ci = 0.95)
# With 95% confidence: population mean is between [74.1, 81.4]

CI(d$Q, ci = 0.99)
# With 99% confidence: population mean is between [72.9, 82.6]
# Wider interval = more confidence but less precision.

# ─────────────────────────────────────────────
# Section 4 — Midterm Score Sampling
# Dataset: gradetest.csv
# ─────────────────────────────────────────────

# Full population
h <- read.csv("gradetest.csv")
h <- h[, -1]

# Q1: Non-random sample — MT scores >= 80, first 60 observations
k <- h$MT[h$MT >= 80][1:60]
mean(k)    # 93.46
sd(k)      # 4.05
min(k)     # 86
max(k)     # 100
plot(density(k), col = "yellow", main = "Density of MT Scores", xlab = "MT Score")

# Q2: Random sample of size 60
set.seed(123)
w <- sample_n(h, 60)
mean(w$MT)   # 84.4
sd(w$MT)     # 15.7
min(w$MT)    # 30
max(w$MT)    # 100

# Q3: Density comparison — population vs random vs non-random
plot(density(h$MT),
     xlim = c(30, 100),
     col = "red",
     main = "MT Score Density: Population vs Samples",
     xlab = "MT Score")
lines(density(w$MT), col = "green")
lines(density(k),    col = "yellow")
legend("topleft",
       legend = c("Population", "Random Sample", "Non-Random Sample"),
       col = c("red", "green", "yellow"),
       lty = 1)

# Q4: Population statistics
mean(h$MT)   # 85.62
sd(h$MT)     # 14.00
min(h$MT)    # 30
max(h$MT)    # 100

# Q5: Summary comparison
# Mean:  Population = 85.6, Random = 84.4, Non-Random = 93.5
#        Random sample closely estimates the population mean.
#        Non-random sample overestimates due to selection bias (only MT >= 80).
# SD:    Population = 14.0, Random = 15.7, Non-Random = 4.1
#        Random sample captures variability well; non-random underestimates it.
# Min:   Population = 30, Random = 30, Non-Random = 86
#        Non-random misses low scorers entirely — clear evidence of bias.
# Conclusion: Random sampling is a more reliable and unbiased estimator
#             of population parameters than non-random sampling.