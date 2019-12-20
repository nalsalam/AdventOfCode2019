# Day 11 part 1
# registration identifier
# emergency hull painting robot

# ship currently black
# Intcode computer is the brain
0 over black
1 over white
output 2 values
1 color 0 black 1 white 
2 direction 0 left 1 right
after turn move 1 panel
half when finished drawing 
do not restart Intcode compute 
number of panels it will be painting at least once 
How many panels does it paint at least once?  
  
painting_robot <- function(color = c("0" = black, "1" = white))  {   # camera sees color
  

    
  
return(color_to_paint, direction_to_turn)
}



robot_brain <- c("3,8,1005,8,311,1106,0,11,0,0,0,104,1,104,0,3,8,1002,8,-1,10,101,1,10,10,4,10,108,0,8,10,4,10,1002,8,1,28,2,103,7,10,3,8,1002,8,-1,10,101,1,10,10,4,10,1008,8,1,10,4,10,1001,8,0,55,2,3,6,10,1,101,5,10,1,6,7,10,3,8,1002,8,-1,10,101,1,10,10,4,10,1008,8,0,10,4,10,1001,8,0,89,1,1108,11,10,2,1002,13,10,1006,0,92,1,2,13,10,3,8,102,-1,8,10,1001,10,1,10,4,10,1008,8,0,10,4,10,101,0,8,126,3,8,1002,8,-1,10,101,1,10,10,4,10,108,1,8,10,4,10,1002,8,1,147,1,7,0,10,3,8,1002,8,-1,10,1001,10,1,10,4,10,108,0,8,10,4,10,101,0,8,173,1006,0,96,3,8,102,-1,8,10,101,1,10,10,4,10,108,0,8,10,4,10,1001,8,0,198,1,3,7,10,1006,0,94,2,1003,20,10,3,8,102,-1,8,10,1001,10,1,10,4,10,1008,8,1,10,4,10,102,1,8,232,3,8,102,-1,8,10,101,1,10,10,4,10,108,1,8,10,4,10,102,1,8,253,1006,0,63,1,109,16,10,3,8,1002,8,-1,10,101,1,10,10,4,10,1008,8,1,10,4,10,101,0,8,283,2,1107,14,10,1,105,11,10,101,1,9,9,1007,9,1098,10,1005,10,15,99,109,633,104,0,104,1,21102,837951005592,1,1,21101,328,0,0,1105,1,432,21101,0,847069840276,1,21101,0,339,0,1106,0,432,3,10,104,0,104,1,3,10,104,0,104,0,3,10,104,0,104,1,3,10,104,0,104,1,3,10,104,0,104,0,3,10,104,0,104,1,21102,179318123543,1,1,21102,386,1,0,1106,0,432,21102,1,29220688067,1,21102,1,397,0,1106,0,432,3,10,104,0,104,0,3,10,104,0,104,0,21102,709580567396,1,1,21102,1,420,0,1105,1,432,21102,1,868498694912,1,21102,431,1,0,1106,0,432,99,109,2,22101,0,-1,1,21101,40,0,2,21101,0,463,3,21101,0,453,0,1105,1,496,109,-2,2106,0,0,0,1,0,0,1,109,2,3,10,204,-1,1001,458,459,474,4,0,1001,458,1,458,108,4,458,10,1006,10,490,1102,1,0,458,109,-2,2105,1,0,0,109,4,1202,-1,1,495,1207,-3,0,10,1006,10,513,21102,0,1,-3,21201,-3,0,1,21202,-2,1,2,21101,0,1,3,21101,0,532,0,1106,0,537,109,-4,2106,0,0,109,5,1207,-3,1,10,1006,10,560,2207,-4,-2,10,1006,10,560,22102,1,-4,-4,1105,1,628,21201,-4,0,1,21201,-3,-1,2,21202,-2,2,3,21101,0,579,0,1105,1,537,22101,0,1,-4,21102,1,1,-1,2207,-4,-2,10,1006,10,598,21102,1,0,-1,22202,-2,-1,-2,2107,0,-3,10,1006,10,620,22102,1,-1,1,21101,0,620,0,106,0,495,21202,-2,-1,-2,22201,-4,-2,-4,109,-5,2106,0,0")
it decides colors to paint and direction to turn  

input <- 0 or 1 depending on color of current panel
output <- color to paint  
output <- direction to turn and move 1 

Input
0 must be the first input (everything is black)
0 must be the 2nd 

all black to start
i.e. inputs are 0 
keep track of area and colors
use that to provide inputs (what the camera sees at current location)

library(R6)

Intcode <- 
  R6Class("Intcode",
   public = list(
     pgm = NULL,
     ptr = 0,
     base = 0,
     phase = NULL,
     halt = FALSE,
     
     # initialize the Amp with a phase setting
     initialize = function(pgm, phase = 0) {
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
     # return the last built output
     
     run = function(input = 1, stop = 9999999999) {
       output <- NULL
       
       while(TRUE) {
         
         if(self$ptr >= stop) break() # debugging
         
         Opcode <- self$pgm[self$ptr + 1] %% 100
         if(Opcode == 99) {
           self$halt <- TRUE
           break()
         }
         
         modes <- 
           map_dbl(3:1, function(i) self$pgm[self$ptr + 1] %>% str_pad(5, "left", "0") %>% str_sub(i, i) %>% as.numeric())
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
           # No need to pause
           self$pgm[values[1]] <- input
           self$ptr <- self$ptr + 2
           
         } else if(Opcode == 4) {
           # build output
           if(is.null(output)) output <- self$pgm[values[1]]
           else output <- c(output, self$pgm[values[1]])
           # print(paste("Output:", self$pgm[values[1]]))
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
           break()
         }
       } # end of while
       return(output) # For day 7, part 1
     } # end of run
   )
)

Robot <- 
  R6Class("Robot",
  public = list(
    dir = NULL,
    hull = NULL,
    computer = NULL,
    
    initialize = function(brain) {
      self$dir <- 2  # 1, 2, 3, 4 for left, up, right, down; start  up
      self$hull <- tibble(x = 0, y = 0, color = 0, painted = 0) # dataframe x, y, color, painted; add rows after each move
      self$computer <- Intcode$new(brain) # puzzle input
    },
    
    run = function(input = 0) {
      self$computer$run(input)
    }
    
    # Need to modify where Intcode gets input
    # Need to implement the outputs from Intcode to paint and turn
    
  )
)

painting_robot <- Robot$new(robot_brain)
painting_robot$dir
painting_robot$hull
painting_robot$run()

Error in self$pgm[self$ptr + 1]%%100 : 
  non-numeric argument to binary operator


