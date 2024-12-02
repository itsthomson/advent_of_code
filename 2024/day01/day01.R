# https://adventofcode.com/2024/day/1

# Part 1
input <- read.csv("input.aoc", header=FALSE, sep='', col.names = c('left','right'))
left <- input$left[order(input$left, decreasing=FALSE)]
right <- input$right[order(input$right, decreasing=FALSE)]
abs(sum(left-right))

# Part 2
rolling_sum <- 0
for (x in left){
  rolling_sum <- rolling_sum + sum(x==right)*x
}
rolling_sum