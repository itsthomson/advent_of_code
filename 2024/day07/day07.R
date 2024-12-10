# https://adventofcode.com/2024/day/7

evaluate_expression <- function(numbers, operators) {
  result <- numbers[1]
  
  for (i in seq_along(operators)) {
    next_num <- numbers[i + 1]
    
    if (operators[i] == "||") {
      result <- as.numeric(paste0(format(result, scientific = FALSE), 
                                  format(next_num, scientific = FALSE)))
    } else if (operators[i] == "+") {
      result <- result + next_num
    } else if (operators[i] == "*") {
      result <- result * next_num
    }
    
    if (is.na(result)) return(Inf)
  }
  
  return(result)
}

generate_operator_combinations <- function(n, operators) {
  if (n < 1) return(list())
  if (n == 1) return(as.list(operators))
  
  grid <- expand.grid(replicate(n, operators, simplify = FALSE))
  return(split(as.matrix(grid), row(grid)))
}

can_make_test_value <- function(test_value, numbers, operators) {
  n_operators_needed <- length(numbers) - 1
  if (n_operators_needed < 1) return(FALSE)
  
  ops_combinations <- generate_operator_combinations(n_operators_needed, operators)
  
  for (ops in ops_combinations) {
    result <- evaluate_expression(numbers, unlist(ops))
    if (!is.na(result) && result == test_value) {
      return(TRUE)
    }
  }
  return(FALSE)
}

parse_input <- function(input_str) {
  lines <- strsplit(trimws(input_str), "\n")[[1]]
  lines <- lines[nzchar(lines)]
  
  result <- vector("list", length(lines))
  valid_count <- 0
  
  for (i in seq_along(lines)) {
    parts <- strsplit(trimws(lines[i]), ":")[[1]]
    test_value <- as.numeric(trimws(parts[1]))
    numbers <- as.numeric(strsplit(trimws(parts[2]), "\\s+")[[1]])
    
    if (!is.na(test_value) && !any(is.na(numbers))) {
      valid_count <- valid_count + 1
      result[[valid_count]] <- list(
        test_value = test_value,
        numbers = numbers
      )
    }
  }
  
  if (valid_count < length(result)) {
    result <- result[1:valid_count]
  }
  
  return(result)
}

solve_calibration <- function(input_str, part = 1) {
  equations <- parse_input(input_str)
  total <- 0
  
  operators <- if(part == 1) c("+", "*") else c("+", "*", "||")
  
  for (eq in equations) {
    if (can_make_test_value(eq$test_value, eq$numbers, operators)) {
      total <- total + eq$test_value
    }
  }
  
  return(total)
}

solve_puzzle <- function(filename, part = 1) {
  input_lines <- suppressWarnings(readLines(filename))
  input_text <- paste(input_lines, collapse="\n")
  result <- solve_calibration(input_text, part)
  return(result)
}

# Part 1
solve_puzzle("input.aoc", 1)

# Part 2
solve_puzzle("input.aoc", 2)
