
# SECTION II - 2

d <- read.csv(
  "D:/r fas/sa 1/Ethereum Historical Data.csv",
  stringsAsFactors = FALSE,
  check.names = FALSE
)

print(names(d))
print(head(d$Date, 10))
print(head(d$Price, 10))

print(head(d$close))
print(class(d$close))
print(nrow(d))

d$time <- as.Date(d$Date, format = "%m/%d/%Y")
d$close <- as.numeric(gsub(",", "", d$Price))

# Invalid Rows

d <- d[
  !is.na(d$time) &
    is.finite(d$close) &
    d$close > 0, ]

# Sort Dates

d <- d[order(d$time), ]
d <- d[!duplicated(d$time), ]

# Selected Periods

d <- d[d$time >= as.Date("2015-08-07") &
         d$time <= as.Date("2026-09-15"), ]

# Data Coverage

cat("Data Coverage:\n")
print(range(d$time))
cat("Price Observations:", nrow(d), "\n")

# Daily Log Returns

r <- diff(log(d$close))
r <- r[is.finite(r)]
cat("Number of valid returns:", length(r), "\n")



# Shapiro-Wilk Normality Test

if (length(r) < 3) {
  stop("Insufficient valid returns for the Shapiro-Wilk test.")
}

if (length(r) > 5000) {
  set.seed(123)
  test_r <- sample(r, 5000)
} else {
  test_r <- r
}

result <- shapiro.test(test_r)

cat("\nSHAPIRO-WILK TEST RESULTS\n")
print(result)

qqnorm(
  test_r,
  main = "Normal Q-Q Plot of Ethereum Returns",
  xlab = "Theoretical Normal Quantiles",
  ylab = "Sample Quantiles",
  pch = 19
)

qqline(test_r, col = "red", lwd = 2)

write.csv(
  data.frame(W = unname(result$statistic),
             p_value = result$p.value,
             sample_size = length(test_r)),
  "ethereum_shapiro_results.csv",
  row.names = FALSE
)
