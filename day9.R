# day 9 
# add relative mode 
# start with day 7 part 1 because I don't need the computer to pause

# day 7 part 1 (no feedback) with R6 

library(R6)

Amp <- R6Class("Amp",
               public = list(
                 pgm = NULL,
                 ptr = 0,
                 base = 0,
                 phase = NULL,
                 halt = FALSE,
                 
                 # initialize the Amp with a phase setting
                 initialize = function(pgm, phase) {
                   self$pgm <- pgm
                   # self$ptr <- 0
                   # self$base <- 0
                   self$phase <- phase # don't really need this
                   
                   # no phase input for example
                   # self$pgm[self$pgm[2] + 1] <- phase
                   # self$ptr <- self$ptr + 2
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
                       self$pgm[self$ptr + 1] %/% (10 * 10^c(1:3)) %% (10^c(1:3)) # vector of 0, 1, 2's
                     values <- 
                       dplyr::case_when(
                         modes == 0 ~ c(self$pgm[self$ptr + 2] + 1, self$pgm[self$ptr + 3] + 1, self$pgm[self$ptr + 4] + 1), # position mode
                         modes == 1 ~ c(self$ptr + 2, self$ptr + 3, self$ptr + 4), # immediate mode
                         modes == 2 ~ c(self$pgm[self$ptr + 2] + 1 + self$base, self$pgm[self$ptr + 3] + 1 + self$base, self$pgm[self$ptr + 4] + 1 + self$base), # relative mode
                         TRUE ~ NA_real_
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
                       
                     } else if(Opcode == 9) { # adjust the relative base
                       self$base <- self$pgm[values[1]] + self$base  
                       self$ptr <- self$ptr + 2 

                     } else {
                       print(paste("Invalid instruction at position ", self$ptr))
                     }
                   
                   # return(paste("Output: ", output)) # For day 5
                   return(output) # For day 7, part 1
                 }}
               )
)

A <- Amp$new(c(109,19,204,-34, 99, rep(0, 2100)), phase = 0)
A$ptr
A$base
A$pgm[1:50]
A$run(0) # object output not found
A$halt
A$ptr
A$pgm[2019:2021] # Opcode 9 did not work
      

# no phase, so commented that out
# no input so no worries about that parameter or the pause behavior
Amp$new(c(109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99), phase = 0)$run(0)
