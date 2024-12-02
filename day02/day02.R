# https://adventofcode.com/2024/day/2

library(zoo) # needed for na.trim

input <- read.csv("input.aoc", header=FALSE, sep='')

# Part 1
is_safe <- function(report){
  distances <- report[1:(length(report)-1)]-report[2:(length(report))]
  safe <- abs(sum(sign(distances))) == 
          length(na.trim(as.numeric(report)))-1 &
            all(abs(distances) >=1 & abs(distances) <= 3)
  return(safe)
}

is_safe_dampener <- function(report){
  if(is_safe(report)){
    return(TRUE)
  }
  for (i in 1:length(report)){
    modified_report <- report[-i]  # Remove the i-th level
    if (is_safe(modified_report)){
      return(TRUE)
    }
  }
  return(FALSE)
}

count_safes <- function(df,dampener=FALSE){
  counter <- vector()
  for(i in 1:nrow(df)){
    if(dampener==TRUE){
      counter <- c(counter, is_safe_dampener(na.trim(as.numeric(df[i,]))))
    }else{
      counter <- c(counter, is_safe(na.trim(as.numeric(df[i,]))))
      }
    }
  return(sum(counter))
}

count_safes(input, FALSE)

# Part 2

count_safes(input, TRUE)