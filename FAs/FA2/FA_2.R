coin.flips(100)

coin.flips <- function(flips = NA) {
  
  library(MASS)
  
  nTT <- 0
  nHH <- 0
  nTH <- 0
  nHT <- 0
  
  for(i in 1:flips) {
  
    toss1 <- runif(n = 1, min = 0, max = 1)
    toss2 <- runif(n = 1, min = 0, max = 1)
    
    if((toss1 <= 0.15) & (toss2 <= 0.15)) nTT <- nTT + 1
    if((toss1 > 0.15) & (toss2 > 0.15)) nHH <- nHH + 1
    if((toss1 <= 0.15) & (toss2 > 0.15)) nTH <- nTH + 1
    if((toss1 > 0.15) & (toss2 <= 0.15)) nHT <- nHT + 1
    
  }
  
  Rel.Freq.TT <- nTT/flips
  Rel.Freq.HH <- nHH/flips
  Rel.Freq.TH <- nTH/flips
  Rel.Freq.HT <- nHT/flips
  
  tt <- fractions(Rel.Freq.TT)
  hh <- fractions(Rel.Freq.HH)
  th <- fractions(Rel.Freq.TH)
  ht <- fractions(Rel.Freq.HT)
  
  print(paste("RELATIVE FREQUENCY"))
  print(paste("Tail & Tail: ", tt))
  print(paste("Head & Head: ", hh))
  print(paste("Tail & Head: ", th))
  print(paste("Head & Tail: ", ht))
  
  prob <- ((Rel.Freq.TH + Rel.Freq.HT) / flips) * 100
  p <- fractions(prob)
  
  print(paste("PROBABILITY (1 head and 1 tail, in any order): ", p))
  
}