
# SECTION II - 1

d <- read.csv("D:/r fas/sa 1/btcusd_1-min_data.csv")
names(d) <- c("timestamp", "open", "high", "low", "close", "volume")
print(names(d))

time_col <- intersect(
  c("timestamp", "date", "datetime", "time"),
  names(d)
)[1]

if (is.na(time_col)) {
  stop("Timestamp column not found. Check the column names above.")
}

raw_time <- d[[time_col]]

if (is.numeric(raw_time)) {
  if (median(raw_time, na.rm = TRUE) > 1e12) {
    time <- as.POSIXct(
      raw_time / 1000, origin = "1970-01-01", tz = "UTC"
    )
  } else {
    time <- as.POSIXct(
      raw_time, origin = "1970-01-01", tz = "UTC"
    )
  }
} else {
  time <- as.POSIXct(
    raw_time, tz = "UTC",
    tryFormats = c(
      "%Y-%m-%d %H:%M:%S",
      "%Y-%m-%d %H:%M",
      "%m/%d/%Y %H:%M",
      "%Y-%m-%d"
    )
  )
}

# Closing Price

close_col <- intersect(
  c("close", "closeprice", "closingprice"),
  names(d)
)[1]

if (is.na(close_col)) {
  stop("Closing-price column not found.")
}

d <- data.frame(
  time = time,
  close = as.numeric(d[[close_col]])
)

d <- d[
  !is.na(d$time) & is.finite(d$close) & d$close > 0,
]

d <- d[order(d$time), ]
d <- d[!duplicated(d$time), ]

# Date Coverage

cat("Available date coverage:\n")
print(range(d$time))

# Requested Dates

d <- d[
  d$time >= as.POSIXct("2012-01-01", tz = "UTC") &
    d$time < as.POSIXct("2026-09-16", tz = "UTC"),
]

cat("Date coverage after filtering:\n")
if (nrow(d) > 0) {
  print(range(d$time))
} else {
  stop("No data within the requested date range.")
}

r <- diff(log(d$close))
r <- r[diff(as.numeric(d$time)) == 60]

cat("Number of one-minute returns:", length(r), "\n")

if (length(r) < 10) {
  stop("Insufficient minute-by-minute data.")
}


nll <- list(
  Normal = function(a) -sum(dnorm(r, a[1], exp(a[2]), log = TRUE)),
  Logistic = function(a) -sum(dlogis(r, a[1], exp(a[2]), log = TRUE)),
  Laplace = function(a) -sum(-log(2 * exp(a[2])) -
                               abs(r - a[1]) / exp(a[2])),
  Student_t = function(a) -sum(dt((r - a[1]) / exp(a[2]),
                                  df = exp(a[3]), log = TRUE) - a[2])
)


fits <- lapply(names(nll), function(m) {
  start <- c(median(r), log(sd(r)))
  if (m == "Student_t") start <- c(start, log(5))
  
  f <- optim(start, nll[[m]], method = "BFGS")
  data.frame(Distribution = m, AIC = 2 * length(f$par) + 2 * f$value)
})


results <- do.call(rbind, fits)
results <- results[order(results$AIC), ]
print(results)

best <- results$Distribution[1]
cat("Best-fitting candidate:", best, "\n")


x <- seq(quantile(r, 0.005), quantile(r, 0.995), length.out = 500)

hist(r, breaks = 80, probability = TRUE,
     xlim = range(x), main = "Bitcoin Minute Returns",
     xlab = "Log return")


for (m in names(nll)) {
  a <- optim(
    if (m == "Student_t") c(median(r), log(sd(r)), log(5))
    else c(median(r), log(sd(r))),
    nll[[m]], method = "BFGS"
  )$par
  
  y <- switch(m,
              Normal = dnorm(x, a[1], exp(a[2])),
              Logistic = dlogis(x, a[1], exp(a[2])),
              Laplace = exp(-abs(x - a[1]) / exp(a[2])) / (2 * exp(a[2])),
              Student_t = dt((x - a[1]) / exp(a[2]), df = exp(a[3])) / exp(a[2])
  )
  lines(x, y, col = match(m, names(nll)), lwd = 2)
}

legend("topright", legend = names(nll),
       col = seq_along(nll), lwd = 2, cex = 0.8)

write.csv(results, "bitcoin_distribution_results.csv", row.names = FALSE)
