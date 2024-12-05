# https://adventofcode.com/2024/day/3
library(readr)

parse_mul_instructions <- function(memory, part_two = FALSE) {
  mul_instructions <- str_extract_all(memory, "mul\\((\\d+),(\\d+)\\)")[[1]]
  mul_positions <- str_locate_all(memory, "mul\\((\\d+),(\\d+)\\)")[[1]]
  
  do_positions <- str_locate_all(memory, "do\\(\\)")[[1]]
  dont_positions <- str_locate_all(memory, "don't\\(\\)")[[1]]
  
  state_changes <- data.frame(
    position = c(do_positions[,1], dont_positions[,1]),
    state = c(rep(TRUE, nrow(do_positions)), rep(FALSE, nrow(dont_positions)))
  )
  state_changes <- state_changes[order(state_changes$position),]
  
  total_sum <- 0
  current_state <- TRUE
  
  for (i in seq_along(mul_instructions)) {
    if (part_two && nrow(state_changes) > 0) {
      recent_changes <- state_changes[state_changes$position < mul_positions[i,1], ]
      if (nrow(recent_changes) > 0) {
        current_state <- recent_changes$state[nrow(recent_changes)]
      }
    }
    
    if (!part_two || current_state) {
      nums <- as.numeric(str_extract_all(mul_instructions[i], "\\d+")[[1]])
      total_sum <- total_sum + prod(nums)
    }
  }
  
  return(total_sum)
}

input <- read_file("input.aoc")
parse_mul_instructions(input,part_two=FALSE)
parse_mul_instructions(input,part_two=TRUE)