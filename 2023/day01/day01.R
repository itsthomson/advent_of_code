# Part 1
input <- read.csv("input.aoc", sep="",header=FALSE)
  
numbers <- gregexpr("[0-9]+", input[[1]])
result <- regmatches(input[[1]], numbers)

output <- vector()
for(i in 1:length(result)){
  tmp <- paste(result[[i]],collapse='')
  output <- c(output, as.numeric(paste0(substring(tmp,1,1),
                                       substring(tmp,nchar(tmp),nchar(tmp)))))
}

sum(output)

# Part 2
