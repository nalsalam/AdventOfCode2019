Intcode <- c(1,0,0,3,1,1,2,3,1,3,4,3,1,5,0,3,2,1,13,19,1,9,19,23,1,6,23,27,2,27,9,31,2,6,31,35,1,5,35,39,1,10,39,43,1,43,13,47,1,47,9,51,1,51,9,55,1,55,9,59,2,9,59,63,2,9,63,67,1,5,67,71,2,13,71,75,1,6,75,79,1,10,79,83,2,6,83,87,1,87,5,91,1,91,9,95,1,95,10,99,2,9,99,103,1,5,103,107,1,5,107,111,2,111,10,115,1,6,115,119,2,10,119,123,1,6,123,127,1,127,5,131,2,9,131,135,1,5,135,139,1,139,10,143,1,143,2,147,1,147,5,0,99,2,0,14,0)

exec_instruction <- function(position, d) {
  p <- position + 1
  
  if(d[p] == 1) {
    d[d[p + 3] + 1] <- d[d[p + 1] + 1] + d[d[p + 2] + 1]
    return(d) 
  } else if(d[p] == 2) {
    d[d[p + 3] + 1] <- d[d[p + 1] + 1] * d[d[p + 2] + 1]
    return(d)
  }  else {
    print("Invalid instruction")
  }
  
  # return(computer)
}
# tests
exec_instruction(0, c(1,9,10,3,  2,3,11,0, 99, 30,40,50))
exec_instruction(4, c(3500,9,10,70, 2,3,11,0, 99,30,40,50))
exec_instruction(0, c(1,0,0,0,99)) 
exec_instruction(0, c(2,3,0,3,99))
exec_instruction(0, c(2,4,4,5,99,0))

run <- function(computer) {
  position <- 0
  while(computer[position + 1] != 99) {
    computer <- exec_instruction(position, computer)
    position = position + 4
  }
  return(computer)
}

# tests

run(c(1,0,0,0,99))
run(c(2,3,0,3,99))
run(c(2,4,4,5,99,0))
run(c(1,1,1,4,99,5,6,0,99))

run2 <- function(computer, noun, verb) {
  position <- 0
  computer[2] <- noun
  computer[3] <- verb
  while(computer[position + 1] != 99) {
    computer <- exec_instruction(position, computer)
    position = position + 4
  }
  return(computer[1])
}

run2(Intcode, 12, 2)
# 5305097 which is correct

# PART 2
library(tidyverse)

results <- vector(mode = "integer", length = 100 * 100)

for(noun in 0:99) {
  for(verb in 0:99) {
    results[noun * 100 + verb + 1] <- run2(Intcode, noun, verb)
  }
}

which(results == 19690720)
results[4926] # so answer is 4925


