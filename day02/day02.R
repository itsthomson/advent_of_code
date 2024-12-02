# https://adventofcode.com/2024/day/2

library(zoo) # needed for na.trim

# Part 1
input <- read.csv("input.aoc", header=FALSE, sep='')

is_safe<-vector()
for (i in 1:nrow(input)){
  distances <- vector()
  for (k in 1:(length(input[i,])-1)){
    distances <- c(distances, input[i,][[k+1]]-input[i,][[k]])
  }
  distances <- na.trim(distances)
  is_safe <- c(is_safe, abs(sum(sign(distances))) == 
                 length(na.trim(as.numeric(input[i,])))-1 &
          all(abs(distances) >=1 & abs(distances) <= 3))
}

sum(is_safe)

# Part 2