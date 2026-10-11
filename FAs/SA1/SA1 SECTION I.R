
# SECTION I



# 1)

getval <- function(prompt, n = 3) {
  repeat {
    vals <- scan(
      what = numeric(),
      nmax = n,
      quiet = TRUE
    )
    
    if (lenght(vals) == n && all(is.finite(vals))) {
      return(vals)
    }
    
    cat(prompt, "\n")
    cat("Enter", n,"valid numbers separated by spaces. \n")
  }
}

# Input the Production of Factory 1, 2, and 3

repeat {
  cat("Enter x1, x2, and x3 (0.10 to 0.40 each):\n")
  x <- scan(what = numeric(), nmax = 3, quiet = TRUE)
  
  if (length(x) == 3 &&
      all(is.finite(x)) &&
      all(x >= 0.10 & x <= 0.40) &&
      abs(sum(x) - 1) < 1e-8) {
    break
  }
  
  cat("Invalid input. 0.10 < xi < 0.40, and the sum must equal to 1.\n")
}

# Input the Defective Rates of each Factory

repeat {
  cat("Enter y1, y2, and y3 (0.01 to 0.05 each):\n")
  y <- scan(what = numeric(), nmax = 3, quiet = TRUE)
  
  if (length(y) == 3 &&
      all(is.finite(y)) &&
      all(y >= 0.01 & y <= 0.05) &&
      abs(sum(y) - 0.12) < 1e-8) {
    break
  }
  
  cat("Invalid input. 0.01 < yi < 0.05, and the sum must equal to 0.12.\n")
}

# Probability of a Defective Product

prb <- sum(x * y)

cat("\nProductions:", x, "\n")

cat("\nDefective Rates:", y, "\n")

cat("\nProbability of a Defective Product Selected:", prb, "or", prb*100, "%\n")



# 2)

validprob <- function(p) {
  length(p) > 0 &&
    all(is.finite(p)) &&
    all(p >= 0 & p <= 1) &&
    abs(sum(p) - 1) < 1e-8
}

# Mean and Variance

calc_stats <- function(values, probs) {
  mu <- sum(values * probs)
  variance <- sum((values - mu)^2 * probs)
  
  c(Mean = mu, Variance = variance)
}

# PDF and CDF

plot_pcdf <- function(values, probs, variable_name = "X") {
  combined <- tapply(probs, values, sum)
  vals <- as.numeric(names(combined))
  pr <- as.numeric(combined)
  
  ord <- order(vals)
  vals <- vals[ord]
  pr <- pr[ord]
  cdf <- cumsum(pr)
  
  oldpar <- par(mfrow = c(1, 2))
  on.exit(par(oldpar))
  
  barplot(
    pr,
    names.arg = vals,
    main = paste(variable_name, "PDF"),
    xlab = variable_name,
    ylab = "Probability"
  )
  
  plot(
    vals, cdf,
    type = "s",
    ylim = c(0, 1),
    main = paste(variable_name, "CDF"),
    xlab = variable_name,
    ylab = "Cumulative Probability"
  )
}

# Conditional Distribution

show_conditional <- function(values, probs, label) {
  cat("\n", label, "\n", sep = "")
  print(data.frame(Value = values, Probability = probs))
  print(calc_stats(values, probs))
}



repeat {
  cat("\n DISCRETE RANDOM VARIABLES \n\n")
  cat("1. Univariate Distribution\n")
  cat("2. Bivariate Distribution\n")
  cat("3. Exit\n")
  
  choice <- readline("Choose an option: ")
  
  # 1. UNIVARIATE DISTRIBUTION
  
  if (choice == "1") {
    n <- suppressWarnings(
      as.integer(readline("Number of values: "))
    )
    
    if (is.na(n) || n < 1) {
      cat("Invalid number of values.\n")
      next
    }
    
    repeat {
      cat("Enter", n, "values (separated by spaces):\n")
      values <- scan(what = numeric(), nmax = n, quiet = TRUE)
      
      if (length(values) == n && all(is.finite(values))) break
      cat("Enter exactly", n, "numbers.\n")
    }
    
    repeat {
      cat("Enter", n, "probabilities (separated by spaces):\n")
      probs <- scan(what = numeric(), nmax = n, quiet = TRUE)
      
      if (length(probs) == n && validprob(probs)) break
      cat("Invalid. Each must be in [0,1] and sum must equal 1.\n")
    }
    
    cat("\nUnivariate Distribution:\n")
    print(data.frame(Value = values, Probability = probs))
    
    cat("\nMean and Variance:\n")
    print(calc_stats(values, probs))
    
    plot_pcdf(values, probs, "X")
    
    
    # 2. BIVARIATE DISTRIBUTION
    
  } else if (choice == "2") {
    nx <- suppressWarnings(
      as.integer(readline("Number of X values: "))
    )
    ny <- suppressWarnings(
      as.integer(readline("Number of Y values: "))
    )
    
    if (is.na(nx) || is.na(ny) || nx < 1 || ny < 1) {
      cat("Invalid number of values.\n")
      next
    }
    
    repeat {
      cat("Enter", nx, "X values (separated by spaces):\n")
      xv <- scan(what = numeric(), nmax = nx, quiet = TRUE)
      if (length(xv) == nx && all(is.finite(xv))) break
      cat("Invalid X values.\n")
    }
    
    repeat {
      cat("Enter", ny, "Y values (separated by spaces):\n")
      yv <- scan(what = numeric(), nmax = ny, quiet = TRUE)
      if (length(yv) == ny && all(is.finite(yv))) break
      cat("Invalid Y values.\n")
    }
    
    cat("\nEnter the Joint Probability Matrix row by row.\n")
    cat("X - Rows; Y - Columns.\n")
    
    repeat {
      joint <- matrix(NA_real_, nrow = nx, ncol = ny)
      
      for (i in seq_len(nx)) {
        cat("Probabilities for X =", xv[i],
            "(enter", ny, "numbers):\n")
        
        row_values <- scan(
          what = numeric(),
          nmax = ny,
          quiet = TRUE
        )
        
        if (length(row_values) != ny ||
            any(!is.finite(row_values))) {
          cat("Incorrect row length or invalid numbers.\n")
          break
        }
        
        joint[i, ] <- row_values
      }
      
      if (all(is.finite(joint)) &&
          all(joint >= 0 & joint <= 1) &&
          abs(sum(joint) - 1) < 1e-8) {
        break
      }
      
      cat("Invalid. probabilities must be in [0,1] and sum to 1.\n")
    }
    
    rownames(joint) <- paste0("X=", xv)
    colnames(joint) <- paste0("Y=", yv)
    
    cat("\nJoint Probability Distribution:\n")
    print(joint)
    
    
    px <- rowSums(joint)
    py <- colSums(joint)
    
    cat("\nMarginal Distribution of X:\n")
    print(data.frame(X = xv, Probability = px))
    
    cat("\nMarginal Distribution of Y:\n")
    print(data.frame(Y = yv, Probability = py))
    
    
    cat("\nMean and Variance of X:\n")
    print(calc_stats(xv, px))
    
    cat("\nMean and Variance of Y:\n")
    print(calc_stats(yv, py))
    
    
    plot_pcdf(xv, px, "X")
    plot_pcdf(yv, py, "Y")
    
    # Conditional Distribution P(Y | X)
    
    for (i in seq_len(nx)) {
      if (px[i] > 0) {
        conditional_y <- joint[i, ] / px[i]
        
        show_conditional(
          yv,
          conditional_y,
          paste("P(Y | X =", xv[i], ")")
        )
        
        plot_pcdf(
          yv,
          conditional_y,
          paste0("Y given X = ", xv[i])
        )
      } else {
        cat("\nP(Y | X =", xv[i],
            ") is undefined because P(X) = 0.\n")
      }
    }
    
    # Conditional Distribution P(X | Y)
    
    for (j in seq_len(ny)) {
      if (py[j] > 0) {
        conditional_x <- joint[, j] / py[j]
        
        show_conditional(
          xv,
          conditional_x,
          paste("P(X | Y =", yv[j], ")")
        )
        
        plot_pcdf(
          xv,
          conditional_x,
          paste0("X given Y = ", yv[j])
        )
      } else {
        cat("\nP(X | Y =", yv[j],
            ") is undefined because P(Y) = 0.\n")
      }
    }
    
  } else if (choice == "3") {
    cat("Program ended.\n")
    break
    
  } else {
    cat("Please select 1, 2, or 3.\n")
  }
}



# 3.

n <- 10000

repeat {
  p_input <- suppressWarnings(
    as.numeric(readline(
      "Probability of Success p (0 < p <= 1): "
    ))
  )
  
  if (length(p_input) == 1 &&
      is.finite(p_input) &&
      p_input > 0 &&
      p_input <= 1) {
    p <- p_input
    break
  }
  
  cat("Invalid. 0 < p <= 1.\n")
}

X <- rgeom(n, prob = p) + 1

pdftable <- prop.table(table(X))
cat("Simulated PDF\n")
pdftable

cat("Mean:", mean(X), "\n")
cat("Variance:", var(X), "\n")

# Theoretical

cat("Mean:", 1/p, "\n")
cat("Variance:", (1 - p) / p^2, "\n")

barplot(
  pdftable,
  main = "Simulated PDF (Searches)",
  xlab = "Number of Searches (Until First Success)",
  ylab = "Probability"
)


# Success after the 3rd search

X_cond <- X[X > 3]

# Remaining searches after 3 failures

Y <- X_cond - 3


cat("\nNumber of Simulations (X>3):", length(X_cond), "\n")

cat("\nConditional Distribution of Remaining Searches:\n")
cond <- prop.table(table(Y))
print(cond)

cat("Mean:", mean(Y), "\n")
cat("Variance:", var(Y), "\n")

cat("Mean and Conditional Mean: ", mean(X), "\t", mean(Y), "\n")
cat("Variance and Conditional Variance: ", var(X), "\t", var(Y), "\n")

barplot(
  cond,
  main = "Conditional PDF (After 3 Failures)",
  xlab = "Remaining Searches (Until Success)",
  ylab = "Probability"
)

# Markov Memoryless Property

# (a) P(X=4|X>3) and P(X=1)

p4g3 <- sum(X == 4) / sum(X > 3)
prb1 <- mean(X == 1)

# (b) P(X=5|X>3) and P(X=2)

p5g3 <- sum(X == 5) / sum(X > 3)
prb2 <- mean(X == 2)

cat("\nP(X = 4 | X > 3):", p4g3, "\n")
cat("P(X = 1):", prb1, "\n")

cat("\nP(X = 5 | X > 3):", p5g3, "\n")
cat("P(X = 2):", prb2, "\n")

# Theoretical Probabilities

cat("P(X = 4 | X > 3) = p(1-p)^3 / (1-p)^3 =", p, "\n")
cat("P(X = 1) =", p, "\n")

cat("P(X = 5 | X > 3) = p(1-p)^4 / (1-p)^3 =", p * (1-p), "\n")

cat("P(X = 2) =", p * (1-p), "\n")
