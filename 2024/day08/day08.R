# https://adventofcode.com/2024/day/8

parse_map <- function(input_lines) {
  t(sapply(input_lines, function(x) strsplit(x, "")[[1]]))
}

find_antennas <- function(map) {
  positions <- which(map != ".", arr.ind = TRUE)
  freqs <- map[positions]
  unique_freqs <- unique(freqs)
  antennas <- list()
  
  for (freq in unique_freqs) {
    freq_pos <- positions[freqs == freq, , drop = FALSE]
    if (nrow(freq_pos) >= 2) {
      antennas[[freq]] <- freq_pos
    }
  }
  return(antennas)
}

find_line_points_part1 <- function(p1, p2, map_dims) {
  points <- matrix(ncol = 2, nrow = 0)
  dx <- p2[2] - p1[2]
  dy <- p2[1] - p1[1]
  
  for (factor in c(-1, 1)) {

    new_x <- p1[2] + factor * 2 * dx
    new_y <- p1[1] + factor * 2 * dy
    
    if (new_x >= 1 && new_x <= map_dims[2] && 
        new_y >= 1 && new_y <= map_dims[1]) {
      points <- rbind(points, c(new_y, new_x))
    }
    
    new_x <- p2[2] - factor * dx
    new_y <- p2[1] - factor * dy
    
    if (new_x >= 1 && new_x <= map_dims[2] && 
        new_y >= 1 && new_y <= map_dims[1]) {
      points <- rbind(points, c(new_y, new_x))
    }
  }
  
  unique(points)
}

find_line_points_part2 <- function(p1, p2, map_dims) {
  points <- matrix(ncol = 2, nrow = 0)
  dx <- p2[2] - p1[2]
  dy <- p2[1] - p1[1]
  
  if (dx == 0 && dy == 0) return(points)
  
  gcd <- abs(if(dx == 0) dy else if(dy == 0) dx else {
    a <- abs(dx)
    b <- abs(dy)
    while (b > 0) {
      t <- b
      b <- a %% b
      a <- t
    }
    a
  })
  
  step_x <- dx / gcd
  step_y <- dy / gcd
  curr_x <- p1[2]
  curr_y <- p1[1]
  
  while (curr_x >= 1 && curr_x <= map_dims[2] && 
         curr_y >= 1 && curr_y <= map_dims[1]) {
    points <- rbind(points, c(curr_y, curr_x))
    curr_x <- curr_x - step_x
    curr_y <- curr_y - step_y
  }
  
  curr_x <- p1[2] + step_x
  curr_y <- p1[1] + step_y
  
  while (curr_x >= 1 && curr_x <= map_dims[2] && 
         curr_y >= 1 && curr_y <= map_dims[1]) {
    points <- rbind(points, c(curr_y, curr_x))
    curr_x <- curr_x + step_x
    curr_y <- curr_y + step_y
  }
  
  unique(points)
}

find_antinodes <- function(antenna_positions, map_dims, part = 1) {
  if (!is.matrix(antenna_positions) || nrow(antenna_positions) < 2) {
    return(matrix(ncol = 2, nrow = 0))
  }
  
  all_points <- matrix(ncol = 2, nrow = 0)
  pairs <- t(combn(nrow(antenna_positions), 2))
  
  find_line_points <- if(part == 1) find_line_points_part1 else find_line_points_part2
  
  for (i in 1:nrow(pairs)) {
    points <- find_line_points(antenna_positions[pairs[i,1], ], 
                               antenna_positions[pairs[i,2], ], 
                               map_dims)
    all_points <- rbind(all_points, points)
  }
  
  if (part == 2 && nrow(antenna_positions) >= 2) {
    all_points <- rbind(all_points, antenna_positions)
  }
  
  if (nrow(all_points) > 0) {
    all_points <- unique(all_points)
  }
  
  return(all_points)
}

solve_puzzle <- function(input_lines, part = 1) {
  map <- parse_map(input_lines)
  map_dims <- dim(map)
  antennas <- find_antennas(map)
  
  if (length(antennas) == 0) return(0)
  
  all_antinodes <- matrix(0, ncol = 2, 
                          nrow = length(antennas) * map_dims[1] * map_dims[2])
  current_row <- 1
  
  for (freq_positions in antennas) {
    freq_antinodes <- find_antinodes(freq_positions, map_dims, part)
    if (is.matrix(freq_antinodes) && nrow(freq_antinodes) > 0) {
      n_new <- nrow(freq_antinodes)
      all_antinodes[current_row:(current_row + n_new - 1), ] <- freq_antinodes
      current_row <- current_row + n_new
    }
  }
  
  if (current_row > 1) {
    all_antinodes <- all_antinodes[1:(current_row-1), , drop = FALSE]
    return(nrow(unique(all_antinodes)))
  }
  return(0)
}

input <- readLines("input.aoc")

# Part 1
solve_puzzle(input, 1)

# Part 2
solve_puzzle(input, 2)
