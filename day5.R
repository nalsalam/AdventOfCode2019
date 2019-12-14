# day 5


# 3,0,4,0,99 saves input to address 0, outputs the value at address 0, halt

# 1002,4,3,4,33 
# code 02 is multiply
# parameter modes are 0, 1, 0
# parameter 1, address 1 with value 4, position mode, use address 4 value of 33
# parameter 2, 3, address 2 with value 3, immediate mode, use value 3
# multiply 33 * 3 = 99
# the third indicates the position at which the output should be stored.
# parameter 3, address 3 with value 4, position mode, use address 4 value 33, no! store at position in address 3 which is 4
# put 99 in address 33 (far ahead in program), no at address 4


# 

inst_len <- function(position, d) {
  p <- position + 1
  Opcode <- d[p] %% 100
  if(Opcode %in% c(1, 2)) return(4)
  else return(2)
}

# execute instruction AND maintain pointer
exec_instr <- function(position, d, input) {
  p <- position + 1
  Opcode <- d[p] %% 100
  para_modes <- 
    d[p] %/% (10 * 10^c(1:3)) %% (10^c(1:3)) # vector of parameter modes
  para_values <- 
    dplyr::if_else(para_modes == 0,
      c(d[p + 1] + 1, d[p + 2] + 1, d[p + 3] + 1), # position mode 
      c(p + 1, p + 2, p + 3) # immediate mode
      )
  
    if(Opcode == 1) {
      d[para_values[3]] <- d[para_values[1]] + d[para_values[2]]
      return(d) 
    } else if(Opcode == 2) {
      d[para_values[3]] <- d[para_values[1]] * d[para_values[2]]
      return(d)
    } else if(Opcode == 3) {
      d[para_values[1]] <- input
      return(d)
    } else if(Opcode == 4) {
      output <- d[para_values[1]]
      print(paste("Output: ", output, " at position ", p))
      return(d)
    } else {
       print(paste("Invalid instruction at position ", p))
      # halt
    }
}

# test
exec_instr(0, c(1002,4,3,4,33))
exec_instr(0, c(1,9,10,3,  2,3,11,0, 99, 30,40,50))
exec_instr(4, c(3500,9,10,70, 2,3,11,0, 99,30,40,50))

inst_len(0, c(1002,4,3,4,33))
inst_len(4, c(3500,9,10,70, 2,3,11,0, 99,30,40,50))

run <- function(position, program, noun, verb) {
  program[2] <- noun
  program[3] <- verb
  while(program[position + 1] != 99) {
    delta <- inst_len(position, program)
    program <- exec_instr(position, program)
    position = position + delta
  }
  return(program[1])
}

# Check new computer with Day2 task
Intcode <- c(1,0,0,3,1,1,2,3,1,3,4,3,1,5,0,3,2,1,13,19,1,9,19,23,1,6,23,27,2,27,9,31,2,6,31,35,1,5,35,39,1,10,39,43,1,43,13,47,1,47,9,51,1,51,9,55,1,55,9,59,2,9,59,63,2,9,63,67,1,5,67,71,2,13,71,75,1,6,75,79,1,10,79,83,2,6,83,87,1,87,5,91,1,91,9,95,1,95,10,99,2,9,99,103,1,5,103,107,1,5,107,111,2,111,10,115,1,6,115,119,2,10,119,123,1,6,123,127,1,127,5,131,2,9,131,135,1,5,135,139,1,139,10,143,1,143,2,147,1,147,5,0,99,2,0,14,0)

# part 1
run(0, Intcode, 12, 2)
# 5305097

# part 2
results <- vector(mode = "integer", length = 100 * 100)
for(noun in 0:99) {
  for(verb in 0:99) {
    results[noun * 100 + verb + 1] <- run(0, Intcode, noun, verb)
  }
}
which(results == 19690720)
results[4926] # so answer is 4925

# Day5 - part 1

# TEST program
diagnostic_program <- c(3,225,1,225,6,6,1100,1,238,225,104,0,1002,114,19,224,1001,224,-646,224,4,224,102,8,223,223,1001,224,7,224,1,223,224,223,1101,40,62,225,1101,60,38,225,1101,30,29,225,2,195,148,224,1001,224,-40,224,4,224,1002,223,8,223,101,2,224,224,1,224,223,223,1001,143,40,224,101,-125,224,224,4,224,1002,223,8,223,1001,224,3,224,1,224,223,223,101,29,139,224,1001,224,-99,224,4,224,1002,223,8,223,1001,224,2,224,1,224,223,223,1101,14,34,225,102,57,39,224,101,-3420,224,224,4,224,102,8,223,223,1001,224,7,224,1,223,224,223,1101,70,40,225,1102,85,69,225,1102,94,5,225,1,36,43,224,101,-92,224,224,4,224,1002,223,8,223,101,1,224,224,1,224,223,223,1102,94,24,224,1001,224,-2256,224,4,224,102,8,223,223,1001,224,1,224,1,223,224,223,1102,8,13,225,1101,36,65,224,1001,224,-101,224,4,224,102,8,223,223,101,3,224,224,1,223,224,223,4,223,99,0,0,0,677,0,0,0,0,0,0,0,0,0,0,0,1105,0,99999,1105,227,247,1105,1,99999,1005,227,99999,1005,0,256,1105,1,99999,1106,227,99999,1106,0,265,1105,1,99999,1006,0,99999,1006,227,274,1105,1,99999,1105,1,280,1105,1,99999,1,225,225,225,1101,294,0,0,105,1,0,1105,1,99999,1106,0,300,1105,1,99999,1,225,225,225,1101,314,0,0,106,0,0,1105,1,99999,8,677,226,224,1002,223,2,223,1006,224,329,1001,223,1,223,1108,226,226,224,1002,223,2,223,1005,224,344,101,1,223,223,1108,226,677,224,1002,223,2,223,1006,224,359,101,1,223,223,107,226,226,224,1002,223,2,223,1005,224,374,101,1,223,223,1107,226,226,224,1002,223,2,223,1005,224,389,101,1,223,223,107,677,677,224,102,2,223,223,1006,224,404,101,1,223,223,1008,226,226,224,1002,223,2,223,1006,224,419,101,1,223,223,108,677,226,224,1002,223,2,223,1006,224,434,101,1,223,223,1108,677,226,224,102,2,223,223,1005,224,449,101,1,223,223,1008,677,226,224,102,2,223,223,1006,224,464,1001,223,1,223,108,677,677,224,102,2,223,223,1005,224,479,101,1,223,223,7,677,677,224,102,2,223,223,1005,224,494,1001,223,1,223,8,226,677,224,102,2,223,223,1006,224,509,101,1,223,223,107,677,226,224,1002,223,2,223,1005,224,524,1001,223,1,223,7,677,226,224,1002,223,2,223,1005,224,539,1001,223,1,223,1007,226,677,224,1002,223,2,223,1005,224,554,1001,223,1,223,8,677,677,224,102,2,223,223,1006,224,569,101,1,223,223,7,226,677,224,102,2,223,223,1006,224,584,1001,223,1,223,1008,677,677,224,102,2,223,223,1005,224,599,101,1,223,223,1007,677,677,224,1002,223,2,223,1006,224,614,101,1,223,223,1107,677,226,224,1002,223,2,223,1006,224,629,101,1,223,223,1107,226,677,224,1002,223,2,223,1006,224,644,101,1,223,223,1007,226,226,224,102,2,223,223,1005,224,659,1001,223,1,223,108,226,226,224,102,2,223,223,1006,224,674,101,1,223,223,4,223,99,226)

run_w_input <- function(program, start_position = 0, input = 1) {
  position <- start_position
  
  while(program[position + 1] != 99) {
    delta <- inst_len(position, program)
    program <- exec_instr(position, program, input)
    position <- position + delta
  }
  print("Done")
}
run_w_input(diagnostic_program)
# diagnostic code 15314507

# part 2
length(diagnostic_program)

# ptr is base zero
exec_instr2 <- function(ptr, d, input) {
  p <- ptr + 1
  Opcode <- d[p] %% 100
  para_modes <- 
    d[p] %/% (10 * 10^c(1:3)) %% (10^c(1:3)) # vector of parameter modes
  para_values <- 
    dplyr::if_else(para_modes == 0,
                   c(d[p + 1] + 1, d[p + 2] + 1, d[p + 3] + 1), # position mode 
                   c(p + 1, p + 2, p + 3) # immediate mode
    )
  
  if(Opcode == 1) {
    d[para_values[3]] <- d[para_values[1]] + d[para_values[2]]
    ptr <- ptr + 4
    return(c(d, ptr)) 
  } else if(Opcode == 2) {
    d[para_values[3]] <- d[para_values[1]] * d[para_values[2]]
    ptr <- ptr + 4
    return(c(d, ptr)) 
  } else if(Opcode == 3) {
    d[para_values[1]] <- input
    ptr <- ptr + 2
    return(c(d, ptr)) 
  } else if(Opcode == 4) {
    output <- d[para_values[1]]
    ptr <- ptr + 2
    print(paste("Output: ", output, " at position ", p))
    return(c(d, ptr))
  } else if(Opcode == 5) {
      if(d[para_values[1]] != 0) ptr <- d[para_values[2]] 
      else ptr <- ptr + 3
    return(c(d, ptr)) 
  } else if(Opcode == 6) {
      if(d[para_values[1]] == 0) ptr <- d[para_values[2]]
      else ptr <- ptr + 3
    return(c(d, ptr)) 
  } else if(Opcode == 7) {
      if(d[para_values[1]] < d[para_values[2]]) d[para_values[3]] <- 1
    else d[para_values[3]] <- 0
    ptr <- ptr + 4
    return(c(d, ptr)) 
  } else if(Opcode == 8) {
      if(d[para_values[1]] == d[para_values[2]]) d[para_values[3]] <- 1
      else d[para_values[3]] <- 0
    ptr <- ptr + 4  
    return(c(d, ptr))       
  } else {
    print(paste("Invalid instruction at position ", p))
    # halt
  }
}

run_w_input2 <- function(program, start_ptr = 0, input = 1) {
  pp <- length(program)
  # stopifnot(length(program) == 678)
  ptr <- start_ptr
  program <- c(program, ptr)
  
  while(program[program[pp + 1] + 1] != 99) {
    program <- exec_instr2(program[pp + 1], program[-(pp + 1)], input)
  }
  print("Done")
}
# tests
run_w_input2(diagnostic_program, input = 1) # 15314507 good

# Equal to 8 or not
c(3,9,8,9,10,9,4,9,99,-1,8) %>% run_w_input2(input = 7)
c(3,3,1108,-1,8,3,4,3,99) %>% run_w_input2(input = 7)
# Less than 8 or not
c(3,9,7,9,10,9,4,9,99,-1,8) %>% run_w_input2(input = 7)
c(3,3,1107,-1,8,3,4,3,99) %>% run_w_input2(input = 9)
# is input non-zero
c(3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9) %>% run_w_input2(input = 0) 
c(3,3,1105,-1,9,1101,0,0,12,4,12,99,1) %>% run_w_input2(input = 1) 
# input <=> 8 999/1000/10001
c(3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,
  1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,
  999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99) %>% run_w_input2(input = 9) 

run_w_input2(diagnostic_program, input = 5) # 652726
