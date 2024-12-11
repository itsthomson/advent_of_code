# https://adventofcode.com/2024/day/11

split_number <- function(n) {
  digits <- nchar(as.character(n))
  mid <- digits %/% 2
  left_half <- substr(as.character(n), 1, mid)
  right_half <- substr(as.character(n), mid + 1, digits)
  c(as.numeric(left_half), as.numeric(right_half))
}

transform_stone <- function(n) {
  if (n == 0) return(1)
  if (nchar(as.character(n)) %% 2 == 0) return(split_number(n))
  n * 2024
}

transform_stones <- function(stones) {
  new_stones <- numeric(0)
  for (stone in stones) {
    new_stones <- c(new_stones, transform_stone(stone))
  }
  new_stones
}

simulate_blinks <- function(initial_stones, num_blinks) {
  current_stones <- initial_stones
  for (i in 1:num_blinks) {
    current_stones <- transform_stones(current_stones)
  }
  length(current_stones)
}

input <- c(3935565, 31753, 437818, 7697, 5, 38, 0, 123)
simulate_blinks(input, 75)