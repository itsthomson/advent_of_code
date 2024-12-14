# Function to check if a solution exists and find minimum tokens
solve_claw_machine <- function(button_a_x, button_a_y, button_b_x, button_b_y, prize_x, prize_y, max_presses = 100) {
  # Check for NA or non-numeric inputs
  if (any(is.na(c(button_a_x, button_a_y, button_b_x, button_b_y, prize_x, prize_y)))) {
    stop("Invalid input: NA values detected")
  }
  
  # Try all combinations of button presses up to max_presses
  for (a in 0:max_presses) {
    for (b in 0:max_presses) {
      # Calculate position after pressing buttons
      x_pos <- a * button_a_x + b * button_b_x
      y_pos <- a * button_a_y + b * button_b_y
      
      # Use all.equal for floating point comparison with some tolerance
      x_match <- isTRUE(all.equal(x_pos, prize_x, tolerance = 1e-10))
      y_match <- isTRUE(all.equal(y_pos, prize_y, tolerance = 1e-10))
      
      # Check if we've reached the prize
      if (x_match && y_match) {
        # Calculate token cost (A costs 3, B costs 1)
        tokens <- 3 * a + 1 * b
        return(list(possible = TRUE, tokens = tokens, a_presses = a, b_presses = b))
      }
    }
  }
  return(list(possible = FALSE, tokens = NA, a_presses = NA, b_presses = NA))
}

# Function to parse input file
parse_input <- function(filepath) {
  # Read all lines from file
  lines <- readLines(filepath)
  
  # Initialize lists to store machines
  machines <- list()
  current_machine <- list()
  
  for (line in lines) {
    # Skip empty lines
    if (nchar(trimws(line)) == 0) {
      if (length(current_machine) > 0) {
        machines[[length(machines) + 1]] <- current_machine
        current_machine <- list()
      }
      next
    }
    
    # Parse button A
    if (grepl("^Button A:", line)) {
      values <- as.numeric(strsplit(gsub("Button A: X\\+|, Y\\+", " ", line), " ")[[1]])
      if (length(values) == 2 && !any(is.na(values))) {
        current_machine$button_a_x <- values[1]
        current_machine$button_a_y <- values[2]
      }
    }
    # Parse button B
    else if (grepl("^Button B:", line)) {
      values <- as.numeric(strsplit(gsub("Button B: X\\+|, Y\\+", " ", line), " ")[[1]])
      if (length(values) == 2 && !any(is.na(values))) {
        current_machine$button_b_x <- values[1]
        current_machine$button_b_y <- values[2]
      }
    }
    # Parse prize location
    else if (grepl("^Prize:", line)) {
      values <- as.numeric(strsplit(gsub("Prize: X=|, Y=", " ", line), " ")[[1]])
      if (length(values) == 2 && !any(is.na(values))) {
        current_machine$prize_x <- values[1]
        current_machine$prize_y <- values[2]
      }
    }
  }
  
  # Add last machine if exists
  if (length(current_machine) > 0 && length(current_machine) == 6) {
    machines[[length(machines) + 1]] <- current_machine
  }
  
  if (length(machines) == 0) {
    stop("No valid machines found in input file")
  }
  
  return(machines)
}

# Main function to solve the puzzle
solve_puzzle <- function(filepath) {
  # Check if file exists
  if (!file.exists(filepath)) {
    stop("Input file not found")
  }
  
  # Parse input
  machines <- parse_input(filepath)
  
  # Track total tokens needed and which machines are possible
  total_tokens <- 0
  possible_wins <- 0
  
  # Process each machine
  for (i in seq_along(machines)) {
    machine <- machines[[i]]
    tryCatch({
      result <- solve_claw_machine(
        machine$button_a_x, machine$button_a_y,
        machine$button_b_x, machine$button_b_y,
        machine$prize_x, machine$prize_y
      )
      
      if (result$possible) {
        possible_wins <- possible_wins + 1
        total_tokens <- total_tokens + result$tokens
        cat(sprintf("Machine %d: Possible to win with %d A presses and %d B presses, costing %d tokens\n",
                    i, result$a_presses, result$b_presses, result$tokens))
      } else {
        cat(sprintf("Machine %d: Impossible to win\n", i))
      }
    }, error = function(e) {
      cat(sprintf("Error processing machine %d: %s\n", i, e$message))
    })
  }
  
  cat(sprintf("\nTotal possible prizes: %d\n", possible_wins))
  cat(sprintf("Total tokens needed: %d\n", total_tokens))
}


solve_puzzle("input.aoc")