# Advent of Code
#2019 

library(tidyverse)

test_masses <- 
  c("63455
  147371
  83071
  57460
  138295") %>% # read puzzle data as long string with \n between lines
str_split(pattern = "\\n") %>% # split string on newline character
unlist() %>% # from list to vector
map_dbl(as.numeric) # from string to numeric

test_masses %>%
  map_dbl(function(mass) {
    floor(mass / 3) - 2
  }) %>%
  sum()

# Day 2
codes <- c(1,9,10,3,2,3,11,0,99,30,40,50) 
# opcodes are at 1, 5, 9 / 0, 4, 8 in base 0
codes <- c(1,0,0,0,99)
codes <- c(2,3,0,3,99)
codes <- c(2,4,4,5,99,0)
codes <- c(1,1,1,4,99,5,6,0,99)

# Part 1
codes <- c(1,0,0,3,1,1,2,3,1,3,4,3,1,5,0,3,2,1,13,19,1,9,19,23,1,6,23,27,2,27,9,31,2,6,31,35,1,5,35,39,1,10,39,43,1,43,13,47,1,47,9,51,1,51,9,55,1,55,9,59,2,9,59,63,2,9,63,67,1,5,67,71,2,13,71,75,1,6,75,79,1,10,79,83,2,6,83,87,1,87,5,91,1,91,9,95,1,95,10,99,2,9,99,103,1,5,103,107,1,5,107,111,2,111,10,115,1,6,115,119,2,10,119,123,1,6,123,127,1,127,5,131,2,9,131,135,1,5,135,139,1,139,10,143,1,143,2,147,1,147,5,0,99,2,0,14,0)
result <- execute_program(12, 2, codes)
result

execute_program <- function(noun, verb, codes) {
  codes[1+1] <- noun
  codes[2+1] <- verb
  for(opcode in seq(0, length(codes) - 1, 4)) {
    codes <- execute_instruction(opcode, codes)
  }
return(codes[0 + 1])
}

# this function changes a vector in the calling environment
# but those changes are not there when the function completes
execute_instruction <- function(pos0, codes) {
  pos1 <- pos0 + 1
  
  if(codes[pos1] == 1) {
    # add
    codes[codes[pos1 + 3] + 1] <- codes[codes[pos1 + 1] + 1] + codes[codes[pos1 + 2] + 1]
  } else if(codes[pos1] == 2) {
    # multiply
    codes[codes[pos1 + 3] + 1] <- codes[codes[pos1 + 1] + 1] * codes[codes[pos1 + 2] + 1]
  } else if(codes[pos1] == 99) {
    # do nothing
  }  else {
    # print message
    # print(paste("Invalid Intcode", codes[pos1]))
  }
  return(codes)
}


# Part 2
Intcode <- c(1,0,0,3,1,1,2,3,1,3,4,3,1,5,0,3,2,1,13,19,1,9,19,23,1,6,23,27,2,27,9,31,2,6,31,35,1,5,35,39,1,10,39,43,1,43,13,47,1,47,9,51,1,51,9,55,1,55,9,59,2,9,59,63,2,9,63,67,1,5,67,71,2,13,71,75,1,6,75,79,1,10,79,83,2,6,83,87,1,87,5,91,1,91,9,95,1,95,10,99,2,9,99,103,1,5,103,107,1,5,107,111,2,111,10,115,1,6,115,119,2,10,119,123,1,6,123,127,1,127,5,131,2,9,131,135,1,5,135,139,1,139,10,143,1,143,2,147,1,147,5,0,99,2,0,14,0)

for(noun in 0:99) {
  for(verb in 0:99) {
  result <- execute_program(noun, verb, Intcode)
  # if(result == 19690720) {
  if(result == 39130784) {
    answer <- 100 * noun + verb
    break()
  }
}
}
answer
result
execute_program(12, 2, Intcode)
execute_program(12, 3, Intcode)
execute_program(10, 2, Intcode)
execute_program(99, 89, Intcode)
execute_program(0, 0, Intcode)
