# https://adventofcode.com/2024/day/1


input <- read.csv("input.aoc", header=FALSE, sep='', 
                  col.names = c('left','right'))
left <- input$left[order(input$left, decreasing=FALSE)]
right <- input$right[order(input$right, decreasing=FALSE)]
abs(sum(left-right))