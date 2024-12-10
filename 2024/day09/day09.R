# https://adventofcode.com/2024/day/9

parse_disk_map <- function(input) {
  numbers <- as.numeric(strsplit(input, "")[[1]])

  files <- list()
  spaces <- list()
  file_id <- 0
  pos <- 1
  
  for (i in seq_along(numbers)) {
    length <- numbers[i]
    if (i %% 2 == 1) {
      files[[length(files) + 1]] <- list(
        id = file_id,
        length = length,
        start = pos
      )
      file_id <- file_id + 1
    } else {
      spaces[[length(spaces) + 1]] <- list(
        length = length,
        start = pos
      )
    }
    pos <- pos + length
  }
  
  list(files = files, spaces = spaces)
}

create_blocks <- function(disk_data) {
  total_length <- sum(sapply(disk_data$files, function(x) x$length)) +
    sum(sapply(disk_data$spaces, function(x) x$length))
  
  blocks <- rep(".", total_length)
  
  for (file in disk_data$files) {
    blocks[file$start:(file$start + file$length - 1)] <- file$id
  }
  
  blocks
}

calculate_checksum <- function(blocks) {
  sum(sapply(seq_along(blocks), function(i) {
    if (blocks[i] != ".") {
      as.numeric(blocks[i]) * (i - 1)
    } else {
      0
    }
  }))
}

solve_part1 <- function(input) {
  disk_data <- parse_disk_map(input)
  blocks <- create_blocks(disk_data)
  
  while (TRUE) {
    right_pos <- which(blocks != ".")
    if (length(right_pos) == 0) break
    right_pos <- max(right_pos)
    
    left_pos <- which(blocks == ".")
    if (length(left_pos) == 0) break
    left_pos <- min(left_pos)
    
    if (left_pos > right_pos) break
    
    blocks[left_pos] <- blocks[right_pos]
    blocks[right_pos] <- "."
  }
  
  calculate_checksum(blocks)
}

solve_part2 <- function(input) {
  disk_data <- parse_disk_map(input)
  blocks <- create_blocks(disk_data)
  
  file_ids <- sort(unique(as.numeric(blocks[blocks != "."])), decreasing = TRUE)
  
  for (id in file_ids) {
    file_pos <- which(blocks == id)
    file_length <- length(file_pos)
    
    empty_spaces <- rle(blocks == ".")
    empty_start <- c(1, 1 + cumsum(empty_spaces$lengths)[-length(empty_spaces$lengths)])
    
    possible_pos <- NULL
    for (i in seq_along(empty_spaces$lengths)) {
      if (empty_spaces$values[i] && empty_spaces$lengths[i] >= file_length) {
        possible_pos <- empty_start[i]
        break
      }
    }
    
    if (!is.null(possible_pos) && possible_pos < min(file_pos)) {
      blocks[file_pos] <- "."
      blocks[possible_pos:(possible_pos + file_length - 1)] <- id
    }
  }
  
  calculate_checksum(blocks)
}


input <- readLines("input.aoc")

# Part 1
solve_part1(input)

# Part 2
solve_part2(input)