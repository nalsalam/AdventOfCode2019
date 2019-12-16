# day 7 with R6 

library(tidyverse)
library(R6)

# Execute an instruction -- used in run method below
exec_instr <- function(ptr) {
  input_available <- TRUE
  output_collected <- FALSE
  
  p <- self$ptr + 1
  Opcode <- self$pgm[p] %% 100
  para_modes <- 
    self$self$pgm[p] %/% (10 * 10^c(1:3)) %% (10^c(1:3)) # vector of parameter modes
  para_values <- 
    dplyr::if_else(para_modes == 0,
                   c(self$pgm[p + 1] + 1, self$pgm[p + 2] + 1, self$pgm[p + 3] + 1), # position mode 
                   c(p + 1, p + 2, p + 3) # immediate mode
    )
  
  if(Opcode == 1) {
    self$pgm[para_values[3]] <- self$pgm[para_values[1]] + self$pgm[para_values[2]]
    self$ptr <- self$ptr + 4
    
  } else if(Opcode == 2) {
    self$pgm[para_values[3]] <- self$pgm[para_values[1]] * self$pgm[para_values[2]]
    self$ptr <- self$ptr + 4
    
  } else if(Opcode == 3) {
    if(input_available) {
      self$pgm[para_values[1]] <- input
      self$ptr <- self$ptr + 2
      input_available <- FALSE
    } else {
      if(output_collected) return(ouput)
      else(stop("No output produced since restarting"))
    }

  } else if(Opcode == 4) {
      output <- self$pgm[para_values[1]]
      self$ptr <- self$ptr + 2
      output_collected <- TRUE
  
  } else if(Opcode == 5) {
    if(self$pgm[para_values[1]] != 0) self$ptr <- self$pgm[para_values[2]] 
    else self$ptr <- self$ptr + 3
    
  } else if(Opcode == 6) {
    if(self$pgm[para_values[1]] == 0) self$ptr <- self$pgm[para_values[2]]
    else self$ptr <- self$ptr + 3
    
  } else if(Opcode == 7) {
    if(self$pgm[para_values[1]] < self$pgm[para_values[2]]) self$pgm[para_values[3]] <- 1
    else self$pgm[para_values[3]] <- 0
    self$ptr <- self$ptr + 4
    
  } else if(Opcode == 8) {
    if(self$pgm[para_values[1]] == self$pgm[para_values[2]]) self$pgm[para_values[3]] <- 1
    else self$pgm[para_values[3]] <- 0
    self$ptr <- self$ptr + 4  
    
  } else {
    print(paste("Invalid instruction at position ", p))
    # halt
    # Don't I want to return some output?  
    # And how about the state of the program?
  }
}


# an amplifier program
ACT <- c(3,31,3,32,1002,32,10,32,1001,31,-2,31,1007,31,0,33,1002,33,7,33,1,33,31,31,1,32,31,31,4,31,99,0,0,0)

Amp <- R6Class("Amp",
  public = list(
    pgm = NULL,
    ptr = NULL,
    phase = NULL,
    
    # initialize the Amp with a phase setting
    initialize = function(pgm, ptr = 0, phase) {
      stopifnot(phase %in% 5:9)
      self$pgm <- pgm
      self$ptr <- ptr
      self$phase <- phase
      
      self$pgm[self$pgm[2] + 1] <- self$phase
      self$ptr <- self$ptr + 2
    },
    
    # (continue) running from current position with one input 
    # half when a second input is request and return the last output produced
    run = function(input) {
      Opcode <- self$pgm[self$pgm[self$ptr + 1] + 1] 
      while( != 99) {
      exec_instr(self$ptr)
      }
      self$halt <- TRUE
    }
  ),
  
  active = list(
    ptr = function(value) {
      if(missing(value)) {
        self$ptr
      } else {
        self$ptr <- value
      }
    }
  )
)

A <- Amp$new(ACT, phase = 5)  
B <- Amp$new(ACT, phase = 6)  
  
A$ptr  
A$phase
B$phase

ACT[32]
A$pgm[32]
B$pgm[32]
A$ptr
B$ptr
  
  function(program, input) {
  ptr <- 0
  input_ptr <- 1
  output <- NULL
  
  # Calling Intcode creates exec_instr and consequently exec_instr() will have Intcode parameter bindings in its surrounding scope, i.e. environment
  # Alternatively create exec_instr in global environment and pass in as parameters values such as input
  
  exec_instr <- function(ptr, d) {
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
      if(input_ptr <=2) {
        d[para_values[1]] <- input[input_ptr]
        print(paste("Taking input: ", input_ptr, " which as value ", input[input_ptr]))
        input_ptr <<- input_ptr + 1 # 
        ptr <- ptr + 2
        return(c(d, ptr))}
      else {
        
      }
    } else if(Opcode == 4) {
      output <<- d[para_values[1]]
      ptr <- ptr + 2
      # print(paste("Output: ", output, " at position ", p))
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
      # Don't I want to return some output?  
      # And how about the state of the program?
    }
  }
  
  pp <- length(program)
  program <- c(program, ptr)
  
  while(program[program[pp + 1] + 1] != 99) {
    program <- exec_instr(program[pp + 1], program[-(pp + 1)])
  }
  # print("Done")
  return(output)
}


# Two inputs

Enhance `Intcode` to take two inputs. See above.  Passed in a input vector and used the super-assignment operator `<<-` to increment an input vector.

# Amplifier Controller Software

First test `thruster` which is built on `Intcode` with two length 2 vector inputs and returning the last `output`.

```{r}
thruster <- function(ACT, phase_sequence) {
  A <- Intcode(ACT, c(phase_sequence[1], 0)) 
  B <- Intcode(ACT, c(phase_sequence[2], A))
  C <- Intcode(ACT, c(phase_sequence[3], B)) 
  D <- Intcode(ACT, c(phase_sequence[4], C))   
  E <- Intcode(ACT, c(phase_sequence[5], D))
  return(E)
}

ACT <- c(3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0)
phase_sequence <- c(4,3,2,1,0)
thruster(ACT, phase_sequence) # 43210
ACT <- c(3,23,3,24,1002,24,10,24,1002,23,-1,23,101,5,23,23,1,24,23,23,4,23,99,0,0)
phase_sequence <- c(0,1,2,3,4)
thruster(ACT, phase_sequence) # 54321
ACT <- c(3,31,3,32,1002,32,10,32,1001,31,-2,31,1007,31,0,33,1002,33,7,33,1,33,31,31,1,32,31,31,4,31,99,0,0,0)
phase_sequence <- c(1,0,4,3,2)
thruster(ACT, phase_sequence) # 65210 
```
