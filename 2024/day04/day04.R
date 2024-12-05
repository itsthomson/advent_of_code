# https://adventofcode.com/2024/day/4
library(readr)

parse_input <- function(input_str) {
  lines <- strsplit(input_str, "\n")[[1]]
  lines <- lines[nzchar(lines)]
  do.call(rbind, lapply(lines, function(x) strsplit(x, "")[[1]]))
}

count_xmas <- function(matrix) {
  rows <- nrow(matrix)
  cols <- ncol(matrix)
  count <- 0
  
  directions <- list(
    c(0, 1),   # right
    c(0, -1),  # left
    c(-1, 0),  # up
    c(1, 0),   # down
    c(1, 1),   # diagonal down-right
    c(-1, 1),  # diagonal up-right
    c(-1, -1), # diagonal up-left
    c(1, -1)   # diagonal down-left
  )
  
  for(i in 1:rows) {
    for(j in 1:cols) {
      for(dir in directions) {
        dx <- dir[1]
        dy <- dir[2]

        if(i + 3*dx >= 1 && i + 3*dx <= rows && 
           j + 3*dy >= 1 && j + 3*dy <= cols) {
          
          chars <- c(
            matrix[i, j],
            matrix[i + dx, j + dy],
            matrix[i + 2*dx, j + 2*dy],
            matrix[i + 3*dx, j + 3*dy]
          )
          
          if(paste(chars, collapse="") == "XMAS") {
            count <- count + 1
          }
        }
      }
    }
  }
  count
}

# fuck this part
count_xmas_x <- function(matrix) {
  rows <- nrow(matrix)
  cols <- ncol(matrix)
  count <- 0
  
  for(i in 2:(rows-1)) {
    for(j in 2:(cols-1)) {
      if(matrix[i,j] == "A") {
        patterns <- list(
          list(c("M","A","S"), c("M","A","S")),
          list(c("M","A","S"), c("S","A","M")),
          list(c("S","A","M"), c("M","A","S")),
          list(c("S","A","M"), c("S","A","M"))
        )
        
        for(pattern in patterns) {
          if(
            matrix[i-1,j-1] == pattern[[1]][1] &&
            matrix[i,j] == pattern[[1]][2] &&
            matrix[i+1,j+1] == pattern[[1]][3] &&
            matrix[i-1,j+1] == pattern[[2]][1] &&
            matrix[i,j] == pattern[[2]][2] &&
            matrix[i+1,j-1] == pattern[[2]][3]
          ) {
            count <- count + 1
          }
        }
      }
    }
  }
  count
}

input <- parse_input(paste(readLines("input.aoc"), collapse="\n"))

# Part 1
count_xmas(input)
# Part 2
count_xmas_x(input)