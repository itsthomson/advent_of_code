# https://adventofcode.com/2024/day/6

MOVES <- list(
  up = c(-1, 0),
  right = c(0, 1),
  down = c(1, 0),
  left = c(0, -1)
)

DIRECTIONS <- c("up", "right", "down", "left")

turn_right <- function(dir) {
  DIRECTIONS[((match(dir, DIRECTIONS) - 1L) %% 4L) + 1L]
}

is_valid <- function(pos, dims) {
  pos[1] >= 1 && pos[1] <= dims[1] && pos[2] >= 1 && pos[2] <= dims[2]
}

move <- function(pos, direction) {
  pos + MOVES[[direction]]
}

count_visited <- function(map) {
  dims <- dim(map)
  start_pos <- which(map == "^", arr.ind = TRUE)[1,]
  pos <- start_pos
  direction <- "up"
  
  visited <- matrix(FALSE, nrow = dims[1], ncol = dims[2])
  visited[start_pos[1], start_pos[2]] <- TRUE
  
  repeat {
    next_pos <- move(pos, direction)
    if (!is_valid(next_pos, dims)) break
    
    if (map[next_pos[1], next_pos[2]] == "#") {
      direction <- turn_right(direction)
    } else {
      pos <- next_pos
      visited[pos[1], pos[2]] <- TRUE
    }
  }
  
  sum(visited)
}

detect_loop <- function(map, start_pos) {
  dims <- dim(map)
  pos <- start_pos
  direction <- "up"
  
  encode_state <- function(p, d) {
    (p[1] - 1) * dims[2] * 4 + (p[2] - 1) * 4 + match(d, DIRECTIONS)
  }
  
  visited <- integer(1000)
  state_count <- 0
  
  for(step in 1:1000) {
    current_state <- encode_state(pos, direction)
    
    prev_occurrence <- match(current_state, visited[1:state_count])
    if (!is.na(prev_occurrence)) {
      cycle_length <- step - prev_occurrence
      if (cycle_length < 50) return(TRUE)
    }
    
    state_count <- state_count + 1
    visited[state_count] <- current_state
    
    next_pos <- move(pos, direction)
    if (!is_valid(next_pos, dims)) return(FALSE)
    
    if (map[next_pos[1], next_pos[2]] == "#") {
      direction <- turn_right(direction)
    } else {
      pos <- next_pos
    }
  }
  
  FALSE
}

find_obstacles <- function(map) {
  dims <- dim(map)
  start_pos <- which(map == "^", arr.ind = TRUE)[1,]
  map[start_pos[1], start_pos[2]] <- "."
  
  empty_spaces <- which(map == ".", arr.ind = TRUE)
  empty_spaces <- empty_spaces[!(empty_spaces[,1] == start_pos[1] & 
                                   empty_spaces[,2] == start_pos[2]),, drop = FALSE]
  
  test_map <- map
  count <- 0
  
  for(i in 1:nrow(empty_spaces)) {
    pos <- empty_spaces[i,]
    test_map[] <- map
    test_map[pos[1], pos[2]] <- "#"
    
    if(detect_loop(test_map, start_pos)) {
      count <- count + 1
    }
  }
  
  count
}

input <- paste(readLines("input.aoc"), collapse="\n")
map <- do.call(rbind, strsplit(strsplit(input, "\n")[[1]], ""))
count_visited(map)
find_obstacles(map)