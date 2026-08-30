results <- read.csv("results.csv")

# 1.1 Skewness Program

skewness <- function(x) {
  xbar <- mean(is.na(x))
  sum2 <- sum((x - xbar) ^ 2, na.rm = T)
  sum3 <- sum((x - xbar) ^ 3, na.rm = T)
  skew <- (sqrt(length(x)) * sum3 / (sum2 ^ (1.5)))
}

skew1 <- c(
  arch1 = skewness(results$arch1),
  prog1 = skewness(results$prog1),
  arch2 = skewness(results$arch2),
  prog2 = skewness(results$prog2)
)

skew1


# 1.2 Pearson's Formula

skew <- function(x) {
  3 * (mean(x, na.rm = TRUE) - median(x, na.rm = TRUE)) /
    sd(x, na.rm = TRUE)
}

skew2 <- c(
  arch1 = skew(results$arch1),
  prog1 = skew(results$prog1),
  arch2 = skew(results$arch2),
  prog2 = skew(results$prog2)
)

skew2



# 2.1 Stem-and-Leaf

f <- c(57, 59, 78, 79, 60, 65, 68, 71, 75, 48, 51, 55, 56, 41, 43,
       44, 75, 78, 80, 81, 83, 83, 85)

m <- c(48, 49, 49, 30, 30, 31, 32, 35, 37, 41, 86, 42, 51, 53, 56,
       42, 44, 50, 51, 65, 67, 51, 56, 58, 64, 64, 75)

stem(f)

stem(m)

# 2.2 Box Plot

boxplot(f,
        main = "Females")
f_bp <- boxplot(f,
        main = "Females")

boxplot(m,
        main = "Males")
m_bp <- boxplot(m,
        main = "Males")

f_bp$stats
m_bp$stats