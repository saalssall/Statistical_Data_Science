# Hamidullah Rezae
# Practical Project — Descriptive Statistics Analysis of Pension Dataset

# ─────────────────────────────────────────────
# Libraries
# ─────────────────────────────────────────────
install.packages("wooldridge")
install.packages("moments")
library(wooldridge)
library(moments)

# ─────────────────────────────────────────────
# Load Data
# ─────────────────────────────────────────────
data("pension")
p <- pension

# Selected variables:
# Quantitative: age, educ
# Qualitative:  female (Gender), married (Marital Status)

# ─────────────────────────────────────────────
# Data Preprocessing
# ─────────────────────────────────────────────

# Replace 1/0 in female with "Female"/"Male"
p$female <- gsub("1", "Female", p$female)
p$female <- gsub("0", "Male", p$female)
colnames(p)[5] <- "Gender"

# Replace 1/0 in married with "Married"/"Single"
p$married <- gsub("1", "Married", p$married)
p$married <- gsub("0", "Single", p$married)
colnames(p)[6] <- "Marital Status"

# ─────────────────────────────────────────────
# Helper Functions
# ─────────────────────────────────────────────

# Frequency distribution table for qualitative variables
qual.fun <- function(x) {
  t <- table(x)
  Abs.freq <- as.vector(t)
  re.freq <- round(prop.table(t), 2)
  Cum.freq <- cumsum(re.freq)
  result <- data.frame(
    Category = names(t),
    Abs.freq = Abs.freq,
    re.freq  = as.vector(re.freq),
    Cum.freq = as.vector(Cum.freq)
  )
  return(result)
}

# Frequency distribution table for quantitative variables
quant_freq <- function(x) {
  cl <- 3
  classlimit <- seq(min(x), max(x), by = cl)
  br <- cut(x, classlimit, right = TRUE, include.lowest = TRUE)
  t <- table(br)
  relative_freq <- round(prop.table(t), 2)
  cumulative_freq <- cumsum(relative_freq)
  final_table <- cbind(t, relative_freq, cumulative_freq)
  return(final_table)
}

# Mode function
get_mode <- function(x) {
  ux <- unique(x)
  tab <- tabulate(match(x, ux))
  ux[tab == max(tab)]
}

# ─────────────────────────────────────────────
# Question 1 — Frequency Distribution Tables
# ─────────────────────────────────────────────

# Quantitative: age
quant_freq(p$age)

# Quantitative: educ
quant_freq(p$educ)

# Qualitative: Gender
qual.fun(p$Gender)

# Qualitative: Marital Status
qual.fun(p$`Marital Status`)

# ─────────────────────────────────────────────
# Question 2 — Charts
# ─────────────────────────────────────────────

# Pie chart — Marital Status
f <- qual.fun(p$`Marital Status`)
Cl <- c("Single", "Married")
cl <- paste(Cl, round(prop.table(f$Abs.freq) * 100), "%")
pie(f$Abs.freq,
    labels = cl,
    cex = 1.1,
    col = c("indianred1", "seagreen"),
    border = "white",
    radius = 0.8,
    main = "Distribution of Marital Status")
legend("bottomleft", legend = c("Single", "Married"), fill = c("indianred1", "seagreen"))
box(which = "plot", lty = 1)

# Pie chart — Gender
g <- qual.fun(p$Gender)
Cl <- c("Female", "Male")
cl <- paste(Cl, round(prop.table(g$Abs.freq) * 100), "%")
pie(g$Abs.freq,
    labels = cl,
    cex = 1,
    col = c("red", "orange"),
    border = "white",
    radius = 0.9,
    main = "Distribution of Gender")
legend("bottomleft", legend = c("Female", "Male"), fill = c("red", "orange"))
box(which = "plot", lty = 1)

# Histogram — Age
hist(p$age,
     main = "Distribution of Age",
     xlab = "Age of Retired People",
     col = "yellowgreen",
     xlim = c(50, 75),
     ylim = c(0, 100),
     breaks = 4)

# Histogram — Education
hist(p$educ,
     main = "Education Distribution",
     xlab = "Education of Retired People",
     col = "seagreen",
     xlim = c(8, 18),
     ylim = c(0, 100),
     breaks = 4)

# ─────────────────────────────────────────────
# Question 3 — Descriptive Statistics
# ─────────────────────────────────────────────

# --- Age ---

# Measures of Location
mean(p$age)
median(p$age)
get_mode(p$age)
quantile(p$age)

# Measures of Variation
var(p$age)
sd(p$age)
CV_age <- sd(p$age) / mean(p$age)
CV_age
min(p$age)
max(p$age)
Range_age <- max(p$age) - min(p$age)
Range_age
mad(p$age, center = median(p$age))
IQR(p$age)

# Measures of Distribution
skewness(p$age)   # 0.575 → slight right skew
kurtosis(p$age)   # 2.593 → platykurtic

# --- Education ---

# Measures of Location
mean(p$educ)
median(p$educ)
get_mode(p$educ)
quantile(p$educ)

# Measures of Variation
var(p$educ)
sd(p$educ)
CV_educ <- sd(p$educ) / median(p$educ)
CV_educ
min(p$educ)
max(p$educ)
Range_educ <- max(p$educ) - min(p$educ)
Range_educ
mad(p$educ)
IQR(p$educ)

# Measures of Distribution
skewness(p$educ)  # 0.311 → slight right skew
kurtosis(p$educ)  # 2.290 → platykurtic

# ─────────────────────────────────────────────
# Question 4 — Relationships Between Variables
# ─────────────────────────────────────────────

# Part 1: Age vs Education (two quantitative)
cor(p$age, p$educ)   # -0.041 → weak negative correlation
cov(p$age, p$educ)   # -0.444

plot(p$age, p$educ,
     main = "Scatter Plot of Education and Age",
     xlab = "Age in Years",
     ylab = "Education in Years",
     col = "red")
# Weak relationship confirmed by both correlation and scatter plot

# Part 2: Gender vs Age (one qualitative, one quantitative)
plot(density(p$age[p$Gender == "Female"]),
     col = "red",
     main = "Joint Density of Retired People by Gender",
     xlab = "Age in Years",
     ylim = c(0.0, 0.6))
lines(density(p$age[p$Gender == "Male"]), col = "darkgreen")
legend("topright", legend = c("Female", "Male"), fill = c("red", "darkgreen"))

summary(p$age[p$Gender == "Female"])
summary(p$age[p$Gender == "Male"])

t.test(p$age ~ p$Gender)
# t = -4.95 → strong association between gender and age

# Part 3: Gender vs Marital Status (two qualitative)
chisq.test(p$Gender, p$`Marital Status`)
# X-squared = 38.66, p-value < 0.05
# We reject the null hypothesis of independence.
# There is a statistically significant association between Gender and Marital Status.

