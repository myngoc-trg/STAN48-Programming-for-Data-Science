# Load required libraries
library(ggplot2)
library(dplyr)
library(tidyr)
library(gridExtra)
library(combinat)

# Set seed for reproducibility
set.seed(42)

# Exact permanent computation (only feasible for small matrices)
permanent_exact <- function(A) {
  n <- nrow(A)
  total <- 0
  
  # Generate all permutations
  perms <- permn(1:n)
  
  for (perm in perms) {
    product <- 1
    for (i in 1:n) {
      product <- product * A[i, perm[i]]
    }
    total <- total + product
  }
  
  return(total)
}

# Test on small matrix
A_tiny <- matrix(c(1, 2, 3, 4), nrow = 2)
cat("2 by 2 matrix permanent:", permanent_exact(A_tiny), "\n")
cat("Compare to determinant:", det(A_tiny), "\n")

# Timing experiment to show factorial growth
timing_experiment <- function() {
  sizes <- 2:8
  times <- numeric()
  
  for (n in sizes) {
    A <- matrix(runif(n^2), nrow = n)
    start_time <- Sys.time()
    permanent_exact(A)
    elapsed <- as.numeric(Sys.time() - start_time)
    times <- c(times, elapsed)
    
    cat(sprintf("n=%d: %.4fs (%s permutations)\n", 
                n, elapsed, format(factorial(n), big.mark = ",")))
    
    if (elapsed > 1) {
      cat("Stopping - computation time exceeded 1 second\n")
      break
    }
  }
  
  return(data.frame(n = sizes[1:length(times)], time = times))
}

timing_results <- timing_experiment()

# Naive Monte Carlo permanent estimation
permanent_naive_mc <- function(A, n_samples = 10000) {
  n <- nrow(A)
  estimates <- numeric(n_samples)
  
  for (i in 1:n_samples) {
    # Generate random permutation
    perm <- sample(1:n)
    
    # Calculate product for this permutation
    product <- 1
    for (j in 1:n) {
      product <- product * A[j, perm[j]]
    }
    estimates[i] <- product
  }
  
  # Return n! times the average
  return(factorial(n) * mean(estimates))
}

# Test naive MC
n <- 5
A <- matrix(runif(n^2), nrow = n)
true_perm <- permanent_exact(A)

# Run naive MC multiple times to see variance
naive_results <- replicate(50, permanent_naive_mc(A, 1000))

cat("True permanent:", true_perm, "\n")
cat("Naive MC mean:", mean(naive_results), "\n")
cat("Naive MC std:", sd(naive_results), "\n")


# Importance sampling estimation
permanent_importance_sampling <- function(A, n_samples = 10000) {
  n <- nrow(A)
  weights <- numeric(n_samples)
  
  for (sample_idx in 1:n_samples) {
    available <- 1:n
    weight <- 1.0
    
    # Build permutation sequentially
    for (i in 1:n) {
      # Calculate probabilities proportional to matrix elements
      row_values <- A[i, available]
      row_sum <- sum(row_values)
      
      if (row_sum == 0) {
        weight <- 0
        break
      }
      
      # Choose column with probability proportional to its value
      probs <- row_values / row_sum
      chosen_idx <- sample(length(available), 1, prob = probs)
      
      # Update weight (this is the magic!)
      weight <- weight * row_sum
      
      # Remove chosen column
      available <- available[-chosen_idx]
    }
    
    weights[sample_idx] <- weight
  }
  
  return(mean(weights))
}

# Test importance sampling
importance_results <- replicate(50, permanent_importance_sampling(A, 1000))

cat("\nImportance Sampling Results:\n")
cat("Mean:", mean(importance_results), "\n")
cat("Std:", sd(importance_results), "\n")

# Create comparison plots
create_comparison_plots <- function(naive_results, importance_results, true_perm) {
  # Prepare data for plotting
  df <- data.frame(
    value = c(naive_results, importance_results),
    method = rep(c("Naive MC", "Importance Sampling"), 
                 c(length(naive_results), length(importance_results)))
  )
  
  # Histogram comparison
  p1 <- ggplot(df, aes(x = value, fill = method)) +
    geom_histogram(alpha = 0.7, bins = 20) +
    geom_vline(xintercept = true_perm, color = "red", 
               linetype = "dashed", linewidth = 1.5) +
    facet_wrap(~method, scales = "free_y") +
    labs(title = "Distribution of Estimates (1000 samples each)",
         x = "Estimated Permanent", y = "Count") +
    theme_minimal() +
    theme(legend.position = "none")
  
  # Box plot comparison
  p2 <- ggplot(df, aes(x = method, y = value, fill = method)) +
    geom_boxplot(alpha = 0.7) +
    geom_hline(yintercept = true_perm, color = "red", 
               linetype = "dashed", alpha = 0.5) +
    labs(title = "Variance Comparison",
         x = "", y = "Estimated Permanent") +
    theme_minimal() +
    theme(legend.position = "none")
  
  # Arrange plots
  grid.arrange(p1, p2, ncol = 2)
}

create_comparison_plots(naive_results, importance_results, true_perm)




# Create the diagonal-dominant "hard" matrix
create_hard_matrix <- function(n) {
  A_hard <- matrix(0.1, nrow = n, ncol = n)
  diag(A_hard) <- 5.0
  return(A_hard)
}

# Test on hard matrix
n <- 10
A_hard <- create_hard_matrix(n)

cat("\n=== Testing on 'Hard' Diagonal-Dominant Matrix ===\n")
cat("Matrix structure: 5.0 on diagonal, 0.1 elsewhere\n\n")


# For demonstration, we'll use a smaller version for exact comparison
n_small <- 5
A_hard_small <- create_hard_matrix(n_small)
true_perm_hard <- permanent_exact(A_hard_small)

# Compare methods on hard matrix
naive_hard <- replicate(50, permanent_naive_mc(A_hard_small, 1000))
importance_hard <- replicate(50, permanent_importance_sampling(A_hard_small, 1000))

cat("True permanent (5 by 5 hard matrix):", true_perm_hard, "\n")
cat("\nNaive MC on hard matrix:\n")
cat("  Mean:", mean(naive_hard), "\n")
cat("  Std:", sd(naive_hard), "\n")


cat("\nImportance Sampling on hard matrix:\n")
cat("  Mean:", mean(importance_hard), "\n")
cat("  Std:", sd(importance_hard), "\n")



