
# 1. Discrete Random Variable

x <- 1:4

# a) Find constant k

k <- round(1 / sum(2*x + 1), 4)
k

# Probability Mass Function

pmf <- round(k * (2*x + 1), 4)
pmf

# b) Expected Value E[X]

ex <- round(sum(x * pmf), 4)
ex

# b) Variance Var(X)

varx <- round(sum((x - ex)^2 * pmf), 4)
varx

# c) Calculate P(X >= 3)

sum(pmf[x >= 3])



# 2. Binomial Distribution

n <- 20
p <- 0.08

# a) Exactly 2

two <- round(dbinom(2, size = n, prob = p), 4)
two

# b) At least 3

thr <- round(1 - pbinom(2, size = n, prob = p), 4)
thr

# c) Mean and Standard Deviation

n * p

sd <- round(sqrt(n * p * (1 - p)), 4)
sd



# 3. Poisson Distribution

lambda1 <- 4.5

# a) Exactly 6

six <- round(dpois(6, lambda1), 4)
six

# b) At least 2

tw <- round(1 - ppois(1, lambda1), 4)
tw

# c) More than 10 hits in 2 minutes

lambda2 <- 4.5 * 2

tentw <- round(1 - ppois(10, lambda2), 4)
tentw



# 4. Continuous Distribution & Normal Distribution

mu <- 120
sigma <- 15

# a) Longer than 135 ms

otf <- round(1 - pnorm(135, mean = mu, sd = sigma), 4)
otf

# b) Between 100 and 130 ms

oot <- round(pnorm(130, mean = mu, sd = sigma) - pnorm(100, mean = mu, sd = sigma), 4)
oot

# c) 95th Percentile

perc <- round(qnorm(0.95, mean = mu, sd = sigma), 4)
perc
