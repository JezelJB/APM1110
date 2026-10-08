
library(ggplot2)

# PART 1: CAMPUS-RELATED ISSUES USING EXPONENTIAL DISTRIBUTION

# Data
part1 <- read.csv("D:/r fas/fa7/data1.csv")
part1 <- part1[1:35, 1:3]
print(part1)

# Number of Observations
n1 <- length(part1$SECONDS)
n1

# Mean Waiting Time (Seconds)
mean_seconds <- mean(part1$SECONDS)
mwt <- round(mean_seconds, 4)
mwt

# Mean Waiting Time (Minutes)
mean_minutes <- mean(part1$SECONDS)
mins <- round(mean_minutes / 60, 4)
mins

# Standard Deviation
sd_seconds <- sd(part1$SECONDS)
sd <- round(sd_seconds, 4)
sd

# Lambda per Second
lambda_seconds <- 1 / mean_seconds
lps <- round(lambda_seconds, 6)
lps

# Lambda per Minute
lambda_minutes <- lambda_seconds * 60
lpm <- round(lambda_minutes, 6)
lpm

# Expected Wating Time (Seconds and Minutes respectively)
expected_seconds <- 1 / lambda_seconds
es <- round(expected_seconds, 4)
es

expected_minutes <- expected_seconds / 60
em <- round(expected_minutes, 4)
em

# Probability Density Function
part1$Exponential_PDF <- dexp(
  part1$SECONDS,
  rate = lambda_seconds
)
part1$Exponential_PDF

# Cumulative Distribution Function
part1$Exponential_CDF <- pexp(
  part1$SECONDS,
  rate = lambda_seconds
)
part1$Exponential_CDF

# Probabilities of printing service within time periods
t1 <- 60
t2 <- 120
t3 <- 300

prob1 <- pexp(t1, rate = lambda_seconds)
prob2 <- pexp(t2, rate = lambda_seconds)
prob5 <- pexp(t3, rate = lambda_seconds)

cat("\nP(X <= 1 minute) =",
    round(prob1, 4),
    "or", round(prob1 * 100, 2), "%")

cat("\nP(X <= 2 minutes) =",
    round(prob2, 4),
    "or", round(prob2 * 100, 2), "%")

cat("\nP(X <= 5 minutes) =",
    round(prob5, 4),
    "or", round(prob5 * 100, 2), "%")

coefficient_variation <- sd_seconds / mean_seconds
cv <- round(coefficient_variation, 4)
cv

# Graphs
ggplot(part1, aes(x = SECONDS)) +
  
  geom_histogram(
    aes(y = after_stat(density)),
    bins = 20,
    fill = "lightblue",
    color = "black"
  ) +
  
  stat_function(
    fun = dexp,
    args = list(rate = lambda_seconds),
    linewidth = 1
  ) +
  
  labs(
    title = "Printing Service Intervals",
    subtitle = "Observed Waiting Times with Exponential PDF",
    x = "Time Between Printing Services (Seconds)",
    y = "Density"
  ) +
  
  theme_minimal()

qqplot(
  qexp(ppoints(n1), rate = lambda_seconds),
  part1$SECONDS,
  main = "Q-Q Plot for Exponential Distribution",
  xlab = "Theoretical Exponential Quantiles",
  ylab = "Observed Waiting Times"
)

abline(0, 1)



# PART 2: APPLYING NORMAL DISTRIBUTION ON CAMPUS

# Data
part2 <- read.csv("D:/r fas/fa7/data2.csv")
part2 <- part2[1:38, 1:4]
print(part2)

# Total Number of People
totalp <- sum(part2$PEOPLE)
totalp

# Number of Observations
# if 2 people spent the same time, value appears 2x in sample
duration_data <- rep(
  part2$MINUTES_SPENT,
  part2$PEOPLE
)

observations <- length(duration_data)
observations

# Mean, Standard Deviation, and Median
mu <- mean(duration_data)
mn <- round(mu, 4)
mn

sigma <- sd(duration_data)
sd <- round(sigma, 4)
sd

med <- median(duration_data)
med

# Lowest and Highest Duration Spent
minimum_duration <- min(duration_data)
minimum_duration

maximum_duration <- max(duration_data)
maximum_duration

# Frequency Distribution Table
frequency_table <- as.data.frame(table(duration_data))

names(frequency_table) <- c(
  "MINUTES_SPENT",
  "FREQUENCY"
)

frequency_table$MINUTES_SPENT <-
  as.numeric(as.character(frequency_table$MINUTES_SPENT))

frequency_table$PERCENTAGE <-
  frequency_table$FREQUENCY / totalp * 100

print(frequency_table)

# Normal Distribution Curve
ggplot(
  data.frame(x = duration_data),
  aes(x = x)
) +
  
  geom_histogram(
    aes(y = after_stat(density)),
    bins = 21,
    fill = "lightblue",
    color = "black"
  ) +
  
  stat_function(
    fun = dnorm,
    args = list(
      mean = mn,
      sd = sd
    ),
    linewidth = 0.1
  ) +
  
  labs(
    title = "Time People Spent in Freedom Park",
    subtitle = "Observed Data with Normal Distribution Curve",
    x = "Time Spent (Minutes)",
    y = "Density"
  ) +
  
  theme_minimal()

ggplot(
  data.frame(Time_Spent = duration_data),
  aes(y = Time_Spent)
) +
  
  geom_boxplot() +
  
  labs(
    title = "Box Plot of Time Spent",
    y = "Time Spent (Minutes)"
  ) +
  
  theme_minimal()

# Percentage of Data Within 1σ, 2σ, and 3σ

# Theoretical Normal Distribution:
# Within 1σ = 68.27%
# Within 2σ = 95.45%
# Within 3σ = 99.73%

# Within 1σ
strd1 <- sum(
  duration_data >= mn - sd &
    duration_data <= mn + sd
) / totalp * 100
sd1 <- round(strd1, 2)
sd1

# Within 2σ
strd2 <- sum(
  duration_data >= mn - 2 * sd &
    duration_data <= mn + 2 * sd
) / totalp * 100
sd2 <- round(strd2, 2)
sd2

# Within 3σ
strd3 <- sum(
  duration_data >= mn - 3 * sd &
    duration_data <= mn + 3 * sd
) / totalp * 100
sd3 <- round(strd3, 2)
sd3

# Outlier
# Q1
q1 <- quantile(duration_data, 0.25)
q1

# Q3
q3 <- quantile(duration_data, 0.75)
q3

# IQR
iqr <- q3 - q1
iqr

# Lower Bound
low <- q1 - 1.5 * iqr
low

# Upper Bound
up <- q3 + 1.5 * iqr
up

# Number of Outliers
outliers <- duration_data[
  duration_data < low |
    duration_data > up
]
ol <- length(outliers)
ol

if (length(outliers) > 0) {
  cat("\nOutlier values:\n")
  print(outliers)
} else {
  cat("\nNo outliers detected using the 1.5 x IQR rule.\n")
}

# Is distribution symmetric?
mn
med

if (mn > med) {
  cat("\nThe distribution is likely right-skewed.")
} else if (mn < med) {
  cat("\nThe distribution is likely left-skewed.")
} else {
  cat("\nThe mean and median are equal, suggesting symmetry.")
}
