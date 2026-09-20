
# I. Geometric Distribution

# 1. Probability of Success

p <- 0.2

# 2. 1000 Random Variables

x <- rgeom(1000, prob = p) + 1

# 3. Basic Statistics

mean_x <- mean(x)
var_x <- var(x)
sd_x <- sd(x)

# 4. Results

cat("Number of trials required to achieve first success:", x[1], "\n")
cat("Mean:", round(mean_x, 2), "\n")
cat("Variance:", round(var_x, 2), "\n")
cat("Standard deviation:", round(sd_x, 2), "\n")

# 5. Histogram

hist(x,
     main = "Geometric Distribution",
     xlab = "Number of Trials (until first success)",
     ylab = "Frequency",
     col = "lightpink")

cat("Number of trials required to achieve first success:", x[1], "\n")



# II. Hypergeometric Distribution

samp <- 10

# 1. Sample of 10 from a box of 40

total1 <- 40
defec1 <- 4

num1 <- 1 - phyper(1, defec1, total1 - defec1, samp)

cat("Probability that sample contains more than 10% defectives (from a box of 40):",
    round(num1, 2), "\n")


# 2. Sample of 10 from a box of 5000

total2 <- 5000
defec2 <- 500

num2 <- 1 - phyper(1, defec2, total2 - defec2, samp)

cat("Probability that sample contains more than 10% defectives (from a box of 5000):",
    round(num2, 2), "\n")
