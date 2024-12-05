# https://adventofcode.com/2024/day/5

solve_print_queue <- function(input_text, part = 1) {
  sections <- strsplit(trimws(input_text), '\n\n')[[1]]

  rules <- do.call(rbind, strsplit(strsplit(sections[1], '\n')[[1]], '\\|'))
  rules_df <- data.frame(
    after = as.integer(rules[,1]),
    before = as.integer(rules[,2])
  )
  
  updates <- lapply(strsplit(strsplit(sections[2], '\n')[[1]], ','), as.integer)
  
  is_correct_order <- function(update) {
    for (i in seq_along(update)) {
      for (j in seq_along(update)) {
        if (i == j) next
        rule_match <- rules_df$before == update[i] & rules_df$after == update[j]
        if (any(rule_match) && i < j) return(FALSE)
      }
    }
    return(TRUE)
  }
  
  order_update <- function(update) {
    adj_list <- list()
    for (page in update) {
      adj_list[[as.character(page)]] <- rules_df$after[rules_df$before == page & rules_df$after %in% update]
    }
    
    visited <- numeric()
    temp_visited <- numeric()
    result <- numeric()
    
    visit <- function(page) {
      if (page %in% temp_visited) stop()
      if (page %in% visited) return()
      
      temp_visited <<- c(temp_visited, page)
      
      deps <- adj_list[[as.character(page)]]
      if (length(deps) > 0) {
        for (dep in deps) {
          visit(dep)
        }
      }
      
      temp_visited <<- temp_visited[temp_visited != page]
      visited <<- c(visited, page)
      result <<- c(page, result)
    }
    
    for (page in update) {
      if (!(page %in% visited)) {
        visit(page)
      }
    }
    
    return(result)
  }
  
  process_updates <- function(updates, part) {
    sapply(updates, function(update) {
      is_ordered <- is_correct_order(update)
      if ((part == 1 && is_ordered) || (part == 2 && !is_ordered)) {
        final_update <- if (part == 1) update else order_update(update)
        mid_idx <- ceiling(length(final_update) / 2)
        return(final_update[mid_idx])
      }
      return(NA)
    })
  }
  
  middle_pages <- process_updates(updates, part)
  sum(middle_pages, na.rm = TRUE)
}

input <- readChar('input.aoc',file.info('input.aoc')$size)
  
solve_print_queue(input, 1)
solve_print_queue(input, 2)
