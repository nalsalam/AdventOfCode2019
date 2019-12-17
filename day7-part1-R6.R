# day 7 part 1 (no feedback) with R6 

library(R6)

# Amplifier program
ACT <- c(3,8,1001,8,10,8,105,1,0,0,21,34,59,68,89,102,183,264,345,426,99999,3,9,102,5,9,9,1001,9,5,9,4,9,99,3,9,101,3,9,9,1002,9,5,9,101,5,9,9,1002,9,3,9,1001,9,5,9,4,9,99,3,9,101,5,9,9,4,9,99,3,9,102,4,9,9,101,3,9,9,102,5,9,9,101,4,9,9,4,9,99,3,9,1002,9,5,9,1001,9,2,9,4,9,99,3,9,1002,9,2,9,4,9,3,9,101,2,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,101,1,9,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,1001,9,2,9,4,9,3,9,1001,9,2,9,4,9,3,9,1001,9,2,9,4,9,3,9,102,2,9,9,4,9,99,3,9,1001,9,1,9,4,9,3,9,102,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,101,1,9,9,4,9,3,9,101,1,9,9,4,9,3,9,102,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,101,1,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,1001,9,2,9,4,9,99,3,9,1002,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,1001,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,101,1,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,1002,9,2,9,4,9,3,9,102,2,9,9,4,9,99,3,9,101,1,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,1,9,9,4,9,3,9,101,2,9,9,4,9,99,3,9,1001,9,1,9,4,9,3,9,1001,9,2,9,4,9,3,9,101,1,9,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,1,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,1001,9,2,9,4,9,3,9,1002,9,2,9,4,9,99)

Amp <- R6Class("Amp",
  public = list(
    pgm = NULL,
    ptr = NULL,
    phase = NULL,
    halt = NULL,
    
    # initialize the Amp with a phase setting
    initialize = function(pgm, ptr = 0, phase) {
      stopifnot(phase %in% 0:4)
      self$pgm <- pgm
      self$ptr <- ptr
      self$phase <- phase
      
      self$pgm[self$pgm[2] + 1] <- self$phase
      self$ptr <- self$ptr + 2
    },
    
    # No feedback, single input
    # run until Opcode is 99
    # return the last output produced
    
    run = function(input) {
      
      while(TRUE) {
        
        Opcode <- self$pgm[self$ptr + 1] %% 100
        if(Opcode == 99) {
          self$halt <- TRUE
          break()
        }
        
        modes <- 
          self$pgm[self$ptr + 1] %/% (10 * 10^c(1:3)) %% (10^c(1:3)) # vector of parameter modes
        values <- 
          dplyr::if_else(
            modes == 0,
              c(self$pgm[self$ptr + 2] + 1, self$pgm[self$ptr + 3] + 1, self$pgm[self$ptr + 4] + 1), # position mode 
              c(self$ptr + 2, self$ptr + 3, self$ptr + 4) # immediate mode
          )
        
        if(Opcode == 1) {
          self$pgm[values[3]] <- self$pgm[values[1]] + self$pgm[values[2]]
          self$ptr <- self$ptr + 4
          
        } else if(Opcode == 2) {
          self$pgm[values[3]] <- self$pgm[values[1]] * self$pgm[values[2]]
          self$ptr <- self$ptr + 4
          
        } else if(Opcode == 3) {
          # simpler than feedback version
          self$pgm[values[1]] <- input
          self$ptr <- self$ptr + 2
            
        } else if(Opcode == 4) {
          output <- self$pgm[values[1]]
          self$ptr <- self$ptr + 2
          
        } else if(Opcode == 5) {
          if(self$pgm[values[1]] != 0) self$ptr <- self$pgm[values[2]] 
          else self$ptr <- self$ptr + 3
          
        } else if(Opcode == 6) {
          if(self$pgm[values[1]] == 0) self$ptr <- self$pgm[values[2]]
          else self$ptr <- self$ptr + 3
          
        } else if(Opcode == 7) {
          if(self$pgm[values[1]] < self$pgm[values[2]]) self$pgm[values[3]] <- 1
          else self$pgm[values[3]] <- 0
          self$ptr <- self$ptr + 4
          
        } else if(Opcode == 8) {
          if(self$pgm[values[1]] == self$pgm[values[2]]) self$pgm[values[3]] <- 1
          else self$pgm[values[3]] <- 0
          self$ptr <- self$ptr + 4  
          
        } else {
          print(paste("Invalid instruction at position ", self$ptr))
        }
      }
      
      return("No output")
      }
  )
)

perm <- function(v) {
  n <- length(v)
  if (n == 1) v
  else {
    X <- NULL
    for (i in 1:n) X <- rbind(X, cbind(v[i], perm(v[-i])))
    X
  }
}
phase_sequence <- perm(0:4)

thruster <- function(ACT, phase = phase_sequence) {
  A <- Amp$new(ACT, phase = phase_sequence[1])$run(0)
  B <- Amp$new(ACT, phase = phase_sequence[2])$run(A)
  C <- Amp$new(ACT, phase = phase_sequence[3])$run(B) 
  D <- Amp$new(ACT, phase = phase_sequence[4])$run(C)   
  E <- Amp$new(ACT, phase = phase_sequence[5])$run(D)
  return(E)
}

thruster_output <- as.vector(120)
i <- 1 
while(i <= 120) {
  thruster_output[i] <- thruster(ACT, phase_sequence[i, ])
  cat(i)
  i <- i + 1
  cat(",")
}
max(thruster_output) # 70597


