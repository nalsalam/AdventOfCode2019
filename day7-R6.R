# day 7 - part 2 with R6 
# feedback
library(R6)

# program must pause

Amp <- R6Class("Amp",
  public = list(
    pgm = NULL,
    ptr = NULL,
    phase = NULL,
    halt = FALSE,
    
    # initialize the Amp with a phase setting
    initialize = function(pgm, phase) {
      # stopifnot(phase %in% 0:4)
      self$pgm <- pgm
      self$ptr <- 0
      self$phase <- phase # don't really need this 
      
      self$pgm[self$pgm[2] + 1] <- phase
      self$ptr <- self$ptr + 2
    },
    
    # continue running from current position with one input 
    # stop when a second input is requested or the Opcode is 99
    # and return the last output produced
    
    run = function(input) {
      input_available <- TRUE
      output_collected <- FALSE

      while(TRUE) {  # break() will exit the loop
        
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
          if(input_available) {
            self$pgm[values[1]] <- input # consume the input
            self$ptr <- self$ptr + 2
            input_available <- FALSE
          } else {
            break() # return the output and "pause"
          }
          
        } else if(Opcode == 4) {
          output <- self$pgm[values[1]]
          self$ptr <- self$ptr + 2
          output_collected <- TRUE
          
        } else if(Opcode == 5) { # jump if true
          if(self$pgm[values[1]] != 0) self$ptr <- self$pgm[values[2]] 
          else self$ptr <- self$ptr + 3
          
        } else if(Opcode == 6) { # jump if false
          if(self$pgm[values[1]] == 0) self$ptr <- self$pgm[values[2]]
          else self$ptr <- self$ptr + 3
          
        } else if(Opcode == 7) { # 1 less than 2
          if(self$pgm[values[1]] < self$pgm[values[2]]) self$pgm[values[3]] <- 1
          else self$pgm[values[3]] <- 0
          self$ptr <- self$ptr + 4
          
        } else if(Opcode == 8) { # 1 equal to 2
          if(self$pgm[values[1]] == self$pgm[values[2]]) self$pgm[values[3]] <- 1
          else self$pgm[values[3]] <- 0
          self$ptr <- self$ptr + 4  
          
        } else {
          print(paste("Invalid instruction at position ", self$ptr))
        }
      }
      
      if(output_collected) return(output)
      else return("No output")
      }
  )
)

Amp$new(c(3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5), phase = 9)$halt
Amp$new(c(3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5), phase = 9)$pgm

# Add feedback loop
thruster <- function(ACT, phase) {
  # create the Amps
  AmpA <- Amp$new(ACT, phase = phase[1])
  AmpB <- Amp$new(ACT, phase = phase[2])
  AmpC <- Amp$new(ACT, phase = phase[3])
  AmpD <- Amp$new(ACT, phase = phase[4])
  AmpE <- Amp$new(ACT, phase = phase[5])
  
  # Start off with input 0
  # Run in a loop until A halts
  E <- 0
  while(AmpA$halt == FALSE) {
  A <- AmpA$run(E)
  B <- AmpB$run(A)
  C <- AmpC$run(B) 
  D <- AmpD$run(C)   
  E <- AmpE$run(D)
  }
  return(E)
  }

thruster(c(3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,
           27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5),
         c(9,8,7,6,5)) # 139629729.  YES!

thruster(c(3,52,1001,52,-5,52,3,53,1,52,56,54,1007,54,5,55,1005,55,26,1001,54,
           -5,54,1105,1,12,1,53,54,53,1008,54,0,55,1001,55,1,55,2,53,55,53,4,
           53,1001,56,-1,56,1005,56,6,99,0,0,0,0,10),
         c(9,7,8,5,6)) # 18216.  YES!

# Bug was in 
# self$pgm[self$pgm[2] + 1] <- self$phase  should be
# self$pgm[self$pgm[2] + 1] <- phase  sublte!

perm <- function(v) {
  n <- length(v)
  if (n == 1) v
  else {
    X <- NULL
    for (i in 1:n) X <- rbind(X, cbind(v[i], perm(v[-i])))
    X
  }
}
phase_sequence <- perm(5:9)
nrow(phase_sequence)
thruster_output <- as.vector(120)

ACT <- c(3,8,1001,8,10,8,105,1,0,0,21,34,59,68,89,102,183,264,345,426,99999,3,9,102,5,9,9,1001,9,5,9,4,9,99,3,9,101,3,9,9,1002,9,5,9,101,5,9,9,1002,9,3,9,1001,9,5,9,4,9,99,3,9,101,5,9,9,4,9,99,3,9,102,4,9,9,101,3,9,9,102,5,9,9,101,4,9,9,4,9,99,3,9,1002,9,5,9,1001,9,2,9,4,9,99,3,9,1002,9,2,9,4,9,3,9,101,2,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,101,1,9,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,1001,9,2,9,4,9,3,9,1001,9,2,9,4,9,3,9,1001,9,2,9,4,9,3,9,102,2,9,9,4,9,99,3,9,1001,9,1,9,4,9,3,9,102,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,101,1,9,9,4,9,3,9,101,1,9,9,4,9,3,9,102,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,101,1,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,1001,9,2,9,4,9,99,3,9,1002,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,1001,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,101,1,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,1002,9,2,9,4,9,3,9,102,2,9,9,4,9,99,3,9,101,1,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,1,9,9,4,9,3,9,101,2,9,9,4,9,99,3,9,1001,9,1,9,4,9,3,9,1001,9,2,9,4,9,3,9,101,1,9,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,1,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,1001,9,2,9,4,9,3,9,1002,9,2,9,4,9,99)
i <- 1 
while(i <= 120) {
  thruster_output[i] <- thruster(ACT, phase_sequence[i, ])
  cat(i)
  i <- i + 1
  cat(",")
}
max(thruster_output) 


