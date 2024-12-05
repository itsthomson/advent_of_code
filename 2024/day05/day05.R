# https://adventofcode.com/2024/day/5

# Part 1
solve_print_queue <- function(input_text) {
  sections <- strsplit(trimws(input_text), '\n\n')[[1]]
  rule_lines <- strsplit(sections[1], '\n')[[1]]
  rules_matrix <- t(sapply(rule_lines, function(line) {
    as.integer(strsplit(line, '\\|')[[1]])
  }))
  rules_df <- data.frame(before = rules_matrix[,1], after = rules_matrix[,2])
  
  update_lines <- strsplit(sections[2], '\n')[[1]]
  updates <- lapply(update_lines, function(line) {
    as.integer(strsplit(line, ',')[[1]])
  })
  
  is_update_correctly_ordered <- function(update) {
    relevant_rules <- rules_df[
      rules_df$before %in% update & rules_df$after %in% update, ]
    
    for (i in 1:nrow(relevant_rules)) {
      before_idx <- match(relevant_rules$before[i], update)
      after_idx <- match(relevant_rules$after[i], update)
      
      if (before_idx > after_idx) {
        return(FALSE)
      }
    }
    return(TRUE)
  }
  
  middle_pages <- sapply(updates, function(update) {
    if (is_update_correctly_ordered(update)) {
      update[ceiling(length(update) / 2)]
    } else {
      NA
    }
  })
  
  sum(middle_pages, na.rm = TRUE)
}

input <- readChar('input.aoc',file.info('input.aoc')$size)
solve_print_queue(input)