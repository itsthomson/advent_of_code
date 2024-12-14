find_region <- function(grid, i, j, char, visited) {
  rows <- nrow(grid)
  cols <- ncol(grid)
  
  if (i < 1 || i > rows || j < 1 || j > cols || 
      visited[i, j] || grid[i, j] != char) {
    return(list())
  }
  
  visited[i, j] <- TRUE
  cells <- list(c(i, j))
  
  directions <- list(c(-1,0), c(1,0), c(0,-1), c(0,1))
  for (dir in directions) {
    next_cells <- find_region(grid, i + dir[1], j + dir[2], char, visited)
    cells <- c(cells, next_cells)
  }
  
  return(cells)
}

calculate_perimeter <- function(grid, region) {
  perimeter <- 0
  rows <- nrow(grid)
  cols <- ncol(grid)
  
  region_matrix <- matrix(FALSE, nrow=rows, ncol=cols)
  for (cell in region) {
    region_matrix[cell[1], cell[2]] <- TRUE
  }
  
  for (cell in region) {
    i <- cell[1]
    j <- cell[2]

    if (i == 1 || !region_matrix[i-1, j]) perimeter <- perimeter + 1
    if (i == rows || !region_matrix[i+1, j]) perimeter <- perimeter + 1
    if (j == 1 || !region_matrix[i, j-1]) perimeter <- perimeter + 1
    if (j == cols || !region_matrix[i, j+1]) perimeter <- perimeter + 1
  }
  
  return(perimeter)
}

calculate_sides <- function(grid, region) {
  rows <- nrow(grid)
  cols <- ncol(grid)
  
  region_matrix <- matrix(FALSE, nrow=rows, ncol=cols)
  for (cell in region) {
    region_matrix[cell[1], cell[2]] <- TRUE
  }
  
  horizontal_segments <- list()
  vertical_segments <- list()
  
  for (cell in region) {
    i <- cell[1]
    j <- cell[2]
    
    if (i == 1 || !region_matrix[i-1, j]) {
      segment <- paste(i, j, "h", "top")
      horizontal_segments[[segment]] <- TRUE
    }
    if (i == rows || !region_matrix[i+1, j]) {
      segment <- paste(i+1, j, "h", "bottom")
      horizontal_segments[[segment]] <- TRUE
    }
    if (j == 1 || !region_matrix[i, j-1]) {
      segment <- paste(i, j, "v", "left")
      vertical_segments[[segment]] <- TRUE
    }
    if (j == cols || !region_matrix[i, j+1]) {
      segment <- paste(i, j+1, "v", "right")
      vertical_segments[[segment]] <- TRUE
    }
  }
  
  total_sides <- length(horizontal_segments) + length(vertical_segments)
  return(total_sides)
}

process_garden <- function(input, part = 1) {
  lines <- strsplit(input, "\n")[[1]]
  lines <- lines[lines != ""]
  grid <- do.call(rbind, lapply(lines, function(x) strsplit(x, "")[[1]]))
  
  rows <- nrow(grid)
  cols <- ncol(grid)
  visited <- matrix(FALSE, rows, cols)
  total_price <- 0
  
  for (i in 1:rows) {
    for (j in 1:cols) {
      if (!visited[i, j]) {
        region <- find_region(grid, i, j, grid[i, j], visited)
        if (length(region) > 0) {
          area <- length(region)
          if (part == 1) {
            measure <- calculate_perimeter(grid, region)
          } else {
            measure <- calculate_sides(grid, region)
          }
          price <- area * measure
          total_price <- total_price + price
        }
      }
    }
  }
  
  return(total_price)
}

input <- paste(readLines("input.aoc"), collapse="\n")

# Part 1
process_garden(input,1)

# Part 2
process_garden(input,2)
