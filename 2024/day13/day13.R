gcd <- function(a, b) {
  a <- abs(as.numeric(a))
  b <- abs(as.numeric(b))
  while (b != 0) {
    t <- b
    b <- a %% b
    a <- t
  }
  return(a)
}

solve_machine <- function(machine, offset = 0) {
  a_x <- as.numeric(machine$button_a_x)
  a_y <- as.numeric(machine$button_a_y)
  b_x <- as.numeric(machine$button_b_x)
  b_y <- as.numeric(machine$button_b_y)
  target_x <- as.numeric(machine$prize_x) + as.numeric(offset)
  target_y <- as.numeric(machine$prize_y) + as.numeric(offset)
  det <- a_x * b_y - b_x * a_y
  if (det == 0) return(list(possible = FALSE))
  gx <- gcd(a_x, b_x)
  gy <- gcd(a_y, b_y)
  if (target_x %% gx != 0 || target_y %% gy != 0) {
    return(list(possible = FALSE))
  }
  n_num <- target_x * b_y - b_x * target_y
  m_num <- a_x * target_y - target_x * a_y
  if (n_num %% det != 0 || m_num %% det != 0) {
    return(list(possible = FALSE))
  }
  n <- n_num %/% det
  m <- m_num %/% det
  if (n < 0 || m < 0) {
    k_step_n <- b_x %/% gx
    k_step_m <- -a_x %/% gx
    k <- 0
    while (n < 0 || m < 0) {
      k <- k + 1
      if (k > 10000) return(list(possible = FALSE))
      n <- n + k_step_n
      m <- m + k_step_m
    }
  }
  x_pos <- n * a_x + m * b_x
  y_pos <- n * a_y + m * b_y
  if (x_pos != target_x || y_pos != target_y) {
    return(list(possible = FALSE))
  }
  tokens <- 3 * n + m
  return(list(possible = TRUE, tokens = tokens, a_presses = n, b_presses = m))
}

parse_input <- function(filepath) {
  lines <- readLines(filepath)
  machines <- list()
  current_machine <- list()
  for (line in lines) {
    if (nchar(trimws(line)) == 0) {
      if (length(current_machine) == 6) {
        machines[[length(machines) + 1]] <- current_machine
      }
      current_machine <- list()
      next
    }
    if (grepl("^Button A:", line)) {
      matches <- regexec("X\\+(\\d+), Y\\+(\\d+)", line)
      values <- regmatches(line, matches)[[1]]
      if (length(values) == 3) {
        current_machine$button_a_x <- as.numeric(values[2])
        current_machine$button_a_y <- as.numeric(values[3])
      }
    }
    else if (grepl("^Button B:", line)) {
      matches <- regexec("X\\+(\\d+), Y\\+(\\d+)", line)
      values <- regmatches(line, matches)[[1]]
      if (length(values) == 3) {
        current_machine$button_b_x <- as.numeric(values[2])
        current_machine$button_b_y <- as.numeric(values[3])
      }
    }
    else if (grepl("^Prize:", line)) {
      matches <- regexec("X=(\\d+), Y=(\\d+)", line)
      values <- regmatches(line, matches)[[1]]
      if (length(values) == 3) {
        current_machine$prize_x <- as.numeric(values[2])
        current_machine$prize_y <- as.numeric(values[3])
      }
    }
  }
  if (length(current_machine) == 6) {
    machines[[length(machines) + 1]] <- current_machine
  }
  return(machines)
}

claw_solver <- function(filepath, part2 = FALSE) {
  machines <- parse_input(filepath)
  offset <- if(part2) 10000000000000 else 0
  total_tokens <- 0
  possible_wins <- 0
  for (i in seq_along(machines)) {
    result <- solve_machine(machines[[i]], offset)
    if (result$possible) {
      possible_wins <- possible_wins + 1
      total_tokens <- total_tokens + result$tokens
    }
  }
  total_tokens
}

# For part 1
claw_solver("input.aoc", part2 = FALSE)

# For part 2 (with large offset)
claw_solver("input.aoc", part2 = TRUE)