# ============================================================================
# STAN48 Mini-Project: Dice Game Simulator in R
# From Procedural to Object-Oriented Programming
# ============================================================================

# Setup
library(R6)
set.seed(42)
cat("=== DICE GAME SIMULATOR ===\n")
cat("Libraries loaded successfully!\n\n")

# ============================================================================
# PART 1: PROCEDURAL PROGRAMMING (PROBLEMS)
# ============================================================================

cat("PART 1: PROCEDURAL APPROACH\n")
cat(rep("-", 40), "\n")

# Global configuration - this is the problem!
dice_config <- list(n_dice = 2, n_sides = 6)
# if we do dice_config <- list(2,6)
# to retrieve element in a list in R: 
# dice_config[[1]], R starts from 1 while Python starts from 0

roll_dice <- function() {
  sample(1:dice_config$n_sides, dice_config$n_dice, replace = TRUE)
} # sample() is a built-in function, replace = True means can sample two identical nums
# sample() gives a numerical vector e.g [1,2]

change_config <- function(n_dice, n_sides) {
  dice_config$n_dice <<- n_dice 
  # <<- because dice_config is global, needs <<- super assign 
  # because dice_config does not live in this local environment
  dice_config$n_sides <<- n_sides
  cat("Config changed to:", n_dice, "dice with", n_sides, "sides\n")
}

# Demonstrate the problem
cat("\nRolling 2d6:\n")
roll1 <- roll_dice()
cat("Roll:", paste(roll1, collapse = ", "), "| Sum:", sum(roll1), "\n")

cat("\nSomeone changes config elsewhere in code...\n")
change_config(3, 4)

cat("You roll again, expecting 2d6:\n")
roll2 <- roll_dice()
cat("Roll:", paste(roll2, collapse = ", "), "| Sum:", sum(roll2), "\n")
cat("PROBLEM: Got", length(roll2), "dice instead of 2!\n\n")

# ============================================================================
# PART 2: OBJECT-ORIENTED SOLUTION
# ============================================================================

cat("PART 2: OBJECT-ORIENTED APPROACH\n")
cat(rep("-", 40), "\n")

# Define R6 class
DiceGame <- R6Class("DiceGame",
  public = list( #this is everything you can access globally like .something
    # Attributes
    n_dice = NULL,
    n_sides = NULL,
    name = NULL,
    history = NULL,
    
    # Constructor
    initialize = function(n_dice = 2, n_sides = 6, name = NULL) {
      self$n_dice <- n_dice
      self$n_sides <- n_sides
      self$name <- if (is.null(name)) paste0(n_dice, "d", n_sides) else name
      self$history <- list()
      cat("Created", self$name, "\n")
    },
    
    # Roll dice method
    roll = function() {
      result <- sample(1:self$n_sides, self$n_dice, replace = TRUE)
      self$history <- append(self$history, list(result))
      return(result)
    },
    
    # Simulate multiple rolls
    simulate = function(n_times) {
      sapply(1:n_times, function(x) sum(self$roll()))
    },
    
    # Get statistics
    get_stats = function() {
      if (length(self$history) == 0) return("No rolls yet!")
      sums <- sapply(self$history, sum)
      list(
        total_rolls = length(self$history),
        average = round(mean(sums), 2),
        range = paste(min(sums), "-", max(sums))
      )
    },
    
    # Print method
    print = function() {
      cat(self$name, "(", self$n_dice, "dice,", self$n_sides, "sides)\n")
    }
  )
)

cat("\nDiceGame class defined!\n")

# Test OOP solution
cat("\nCreating independent game objects:\n")
game1 <- DiceGame$new(2, 6, "Standard")
game2 <- DiceGame$new(1, 20, "D20")
game3 <- DiceGame$new(3, 4, "3d4")

cat("\nRolling each game:\n")
r1 <- game1$roll()
r2 <- game2$roll()
r3 <- game3$roll()

cat(game1$name, ":", paste(r1, collapse = ", "), "| Sum:", sum(r1), "\n")
cat(game2$name, ":", paste(r2, collapse = ", "), "| Sum:", sum(r2), "\n")
cat(game3$name, ":", paste(r3, collapse = ", "), "| Sum:", sum(r3), "\n")

# Roll game1 again - still 2d6!
r4 <- game1$roll()
cat("\n", game1$name, "rolled again:", paste(r4, collapse = ", "), "\n")
cat(" Each game keeps its own configuration!\n\n")

# ============================================================================
# PART 3: PRACTICAL EXAMPLE - TOURNAMENT
# ============================================================================

cat("PART 3: TOURNAMENT EXAMPLE\n")
cat(rep("-", 40), "\n")

tournament <- function() {
  cat("Task: Which dice gets closest to sum of 10?\n\n")
  
  # Create different games
  games <- list(
    DiceGame$new(2, 6, "2d6"),
    DiceGame$new(3, 4, "3d4"),
    DiceGame$new(1, 20, "1d20")
  )
  
  target <- 10
  results <- list()
  
  # Each game plays 50 rounds
  for (i in seq_along(games)) {
    # alt: for (i in 1:length(games))
    game <- games[[i]]
    distances <- numeric(50)
    
    for (j in 1:50) {
      roll_sum <- sum(game$roll())
      distances[j] <- abs(roll_sum - target)
    }
    
    avg_distance <- mean(distances)
    results[[i]] <- list(name = game$name, distance = avg_distance)
    cat(sprintf("%-6s: Average distance = %.2f\n", game$name, avg_distance))
  }
  
  # Find winner
  distances <- sapply(results, function(x) x$distance)
  winner <- results[[which.min(distances)]]$name
  cat("\n🏆 Winner:", winner, "\n")
  
  cat("\n With OOP: Managed multiple games easily!\n")
  cat(" With procedural: Would need constant config switching!\n\n")
}

tournament()

# ============================================================================
# PART 4: EXERCISE - BETTING GAME
# ============================================================================

cat("PART 4: BETTING GAME EXERCISE\n")
cat(rep("-", 40), "\n")

# Create betting game using inheritance
BettingGame <- R6Class("BettingGame",
  inherit = DiceGame,
  
  public = list(
    money = NULL,
    
    initialize = function(n_dice = 2, n_sides = 6, starting_money = 100) {
      super$initialize(n_dice, n_sides, "Betting")
      self$money <- starting_money
    },
    
    bet = function(bet_amount, choice = "high") {
      if (bet_amount > self$money) return("Not enough money!")
      
      # Expected sum
      # sum of i to n = n(n+1)/2
      expected <- self$n_dice * (self$n_sides + 1) / 2
      
      # Roll dice
      roll_result <- self$roll()
      roll_sum <- sum(roll_result)
      
      # Determine win/loss
      win <- (choice == "high" & roll_sum > expected) | 
             (choice == "low" & roll_sum < expected)
      
      if (win) {
        self$money <- self$money + bet_amount
        result <- "WIN"
      } else {
        self$money <- self$money - bet_amount
        result <- "LOSE"
      }
      
      sprintf("Rolled %d (expected %.1f) - You %s! Money: $%d", 
              roll_sum, expected, result, self$money)
    },
    
    print = function() {
      super$print()
      cat("  Money: $", self$money, "\n")
    }
  )
)

# Test betting game
cat("Testing betting game:\n")
betting <- BettingGame$new(2, 6, 100)
betting$print()

cat("\nPlacing bets:\n")
cat(betting$bet(20, "high"), "\n")
cat(betting$bet(15, "low"), "\n")
cat(betting$bet(10, "high"), "\n")

cat("\nFinal status:\n")
betting$print()

# Show individual game stats
cat("\nGame statistics:\n")
for (game in list(game1, game2, game3)) {
  stats <- game$get_stats()
  if (is.list(stats)) {
    cat(game$name, "- Rolls:", stats$total_rolls, "| Avg:", stats$average, 
        "| Range:", stats$range, "\n")
  }
}


