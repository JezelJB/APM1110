
# 5. Geospatial Analysis System

GAS = data.frame(
  "Sensor" = c(1, 2, 3, 4),
  Images.Supplied = c(15, 20, 25, 40),
  Relevant.Images = c(50, 60, 80, 85)
)

GAS

Total <- (GAS$Images.Supplied * GAS$Relevant.Images) / 100
GAS <- cbind(GAS, Total)

Overall.Percentage <- list("Overall.Percentage", 100, NA, sum(Total))
GAS <- rbind(GAS, Overall.Percentage)

GAS



# 6. Coin Toss

library(MASS)

expand.grid(Toss1 = c("H", "T"), Toss2 = c("H", "T"))

E1 <- fractions(2/4)
E2 <- fractions(2/4)
E3 <- fractions(2/4)

print(paste("P(E1) = P({HH, TT}) =", E1))
print(paste("P(E2) = P({HH, HT}) =", E2))
print(paste("P(E3) = P({TH, HH}) =", E3))

pairw <- E1 * E2 * E3
mut_ind <- fractions(1/4)

print(paste("P(E1) × P(E2) × P(E3) =", pairw))

print(paste("P(E1 ∩ E2 ∩ E3) = P({HH})", mut_ind))

# P(E1) × P(E2) × P(E3) != P(E1 ∩ E2 ∩ E3)
# E1, E2, and E3 are Pairwise Independent but NOT Mutually Independent.
