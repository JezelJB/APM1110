
# 2. Binary Communication Channel

r0t0 <- 0.95
r1t0 <- 1 - r0t0

r1t1 <- 0.75

t0 <- 0.70
t1 <- 1 - t0

# Received 1

pr1 <- (t1 * r1t1) + (t0 * r1t0)
pr1

# Received 1, Transmitted 1

ptr1 <- (t1 * r1t1) / pr1
ptr1



# 7. IT Company Employees

library(formattable)

j <- 0.10
ej <- 0.08

am <- 0.30
eam <- 0.05

av <- 0.60
eav <- 0.01

# Overall Percentage of Error

pe <- (j * ej) + (am * eam) + (av * eav)
percent(pe)

# Program with an Error

pje <- (j * ej) / pe
pame <- (am * eam) / pe
pave <- (av * eav) / pe

prog_er <- c(pje, pame, pave)
work_er <- max(prog_er)
percent(work_er)

# A program error was most likely written by Amy.
