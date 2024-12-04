# https://adventofcode.com/2024/day/4
library(readr)

# Part 1
check_word <- function(grid, word, row, col, dx, dy) {
  word_length <- nchar(word)
  if (row + (word_length - 1) * dx < 1 || row + (word_length - 1) * dx > nrow(grid) ||
      col + (word_length - 1) * dy < 1 || col + (word_length - 1) * dy > ncol(grid)) {
    return(FALSE)
  }
  
  for (i in 1:word_length) {
    if (grid[row + (i-1)*dx, col + (i-1)*dy] != substr(word, i, i)) {
      return(FALSE)
    }
  }
  
  return(TRUE)
}

count_word_occurrences <- function(grid, word) {
  count <- 0
  
  directions <- list(
    c(0, 1),   # right
    c(0, -1),  # left
    c(-1, 0),  # up
    c(1, 0),   # down
    c(1, 1),   # diagonal down-right
    c(1, -1),  # diagonal down-left
    c(-1, -1), # diagonal up-left
    c(-1, 1)   # diagonal up-right
  )
  
  for (row in 1:nrow(grid)) {
    for (col in 1:ncol(grid)) {
      for (dir in directions) {
        if (check_word(grid, word, row, col, dir[1], dir[2])) {
          count <- count + 1
        }
      }
    }
  }
  return(count)
}

input <- scan('input.aoc', what='character')
input <- do.call(rbind,strsplit(input, ""))

result <- count_word_occurrences(input, "XMAS")
result
