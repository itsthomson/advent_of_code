# https://adventofcode.com/2024/day/10

parse_input <- function(input) {
  lines <- strsplit(input, "\n")[[1]]
  matrix(as.numeric(unlist(strsplit(lines, ""))), 
         nrow = length(lines), 
         byrow = TRUE)
}

get_valid_neighbors <- function(map, row, col, current_height) {
  nrow <- nrow(map)
  ncol <- ncol(map)
  neighbors <- list()
  
# kinda like day 4
  directions <- list(
    c(-1, 0),  # up
    c(1, 0),   # down
    c(0, -1),  # left
    c(0, 1)    # right
  )
  
  for (dir in directions) {
    new_row <- row + dir[1]
    new_col <- col + dir[2]
    
    if (new_row >= 1 && new_row <= nrow &&
        new_col >= 1 && new_col <= ncol &&
        map[new_row, new_col] == current_height + 1) {
      neighbors[[length(neighbors) + 1]] <- c(new_row, new_col)
    }
  }
  
  return(neighbors)
}

count_reachable_nines <- function(map, start_row, start_col) {
  if (map[start_row, start_col] != 0) return(0)
  
  visited <- matrix(FALSE, nrow = nrow(map), ncol = ncol(map))
  reachable_nines <- vector("logical", 0)
  
  dfs <- function(row, col, height) {
    if (height == 9) {
      pos_key <- paste(row, col)
      if (!(pos_key %in% names(reachable_nines))) {
        reachable_nines[pos_key] <<- TRUE
      }
      return()
    }
    
    visited[row, col] <<- TRUE
    neighbors <- get_valid_neighbors(map, row, col, height)
    
    for (neighbor in neighbors) {
      new_row <- neighbor[1]
      new_col <- neighbor[2]
      if (!visited[new_row, new_col]) {
        dfs(new_row, new_col, height + 1)
      }
    }
    
    visited[row, col] <<- FALSE
  }
  
  dfs(start_row, start_col, 0)
  return(length(reachable_nines))
}

count_distinct_paths <- function(map, start_row, start_col) {
  if (map[start_row, start_col] != 0) return(0)
  
  paths_count <- 0
  current_path <- matrix(FALSE, nrow = nrow(map), ncol = ncol(map))
  
  dfs <- function(row, col, height) {
    if (height == 9) {
      paths_count <<- paths_count + 1
      return()
    }
    
    current_path[row, col] <<- TRUE
    neighbors <- get_valid_neighbors(map, row, col, height)
    
    for (neighbor in neighbors) {
      new_row <- neighbor[1]
      new_col <- neighbor[2]
      if (!current_path[new_row, new_col]) {
        dfs(new_row, new_col, height + 1)
      }
    }
    
    current_path[row, col] <<- FALSE
  }
  
  dfs(start_row, start_col, 0)
  return(paths_count)
}

solve_puzzles <- function(input) {
  map <- parse_input(input)
  
  trailheads <- which(map == 0, arr.ind = TRUE)
  
  scores <- apply(trailheads, 1, function(pos) {
    count_reachable_nines(map, pos[1], pos[2])
  })
  
  ratings <- apply(trailheads, 1, function(pos) {
    count_distinct_paths(map, pos[1], pos[2])
  })
  
  return(list(
    part1 = sum(scores),
    part2 = sum(ratings)
  ))
}

# Read input from file and solve both parts
input <- paste(readLines("input.aoc", warn = FALSE), collapse = "\n")
solve_puzzles(input)
