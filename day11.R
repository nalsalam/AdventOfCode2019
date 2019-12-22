# Day 11 part 1

# hull painting robot

# The robot will continue running for a while like this and halt when it is finished drawing.

# How many panels does it paint at least once?  

library(tidyverse)


# 0 to 3
face <- function(n, d) {
  stopifnot(d %in% 0:1)
  if(d == 0) return((n - 1) %% 4)
  if(d == 1) return((n + 1) %% 4)
}

paint_pgm <- c(3,8,1005,8,311,1106,0,11,0,0,0,104,1,104,0,3,8,1002,8,-1,10,101,1,10,10,4,10,108,0,8,10,4,10,1002,8,1,28,2,103,7,10,3,8,1002,8,-1,10,101,1,10,10,4,10,1008,8,1,10,4,10,1001,8,0,55,2,3,6,10,1,101,5,10,1,6,7,10,3,8,1002,8,-1,10,101,1,10,10,4,10,1008,8,0,10,4,10,1001,8,0,89,1,1108,11,10,2,1002,13,10,1006,0,92,1,2,13,10,3,8,102,-1,8,10,1001,10,1,10,4,10,1008,8,0,10,4,10,101,0,8,126,3,8,1002,8,-1,10,101,1,10,10,4,10,108,1,8,10,4,10,1002,8,1,147,1,7,0,10,3,8,1002,8,-1,10,1001,10,1,10,4,10,108,0,8,10,4,10,101,0,8,173,1006,0,96,3,8,102,-1,8,10,101,1,10,10,4,10,108,0,8,10,4,10,1001,8,0,198,1,3,7,10,1006,0,94,2,1003,20,10,3,8,102,-1,8,10,1001,10,1,10,4,10,1008,8,1,10,4,10,102,1,8,232,3,8,102,-1,8,10,101,1,10,10,4,10,108,1,8,10,4,10,102,1,8,253,1006,0,63,1,109,16,10,3,8,1002,8,-1,10,101,1,10,10,4,10,1008,8,1,10,4,10,101,0,8,283,2,1107,14,10,1,105,11,10,101,1,9,9,1007,9,1098,10,1005,10,15,99,109,633,104,0,104,1,21102,837951005592,1,1,21101,328,0,0,1105,1,432,21101,0,847069840276,1,21101,0,339,0,1106,0,432,3,10,104,0,104,1,3,10,104,0,104,0,3,10,104,0,104,1,3,10,104,0,104,1,3,10,104,0,104,0,3,10,104,0,104,1,21102,179318123543,1,1,21102,386,1,0,1106,0,432,21102,1,29220688067,1,21102,1,397,0,1106,0,432,3,10,104,0,104,0,3,10,104,0,104,0,21102,709580567396,1,1,21102,1,420,0,1105,1,432,21102,1,868498694912,1,21102,431,1,0,1106,0,432,99,109,2,22101,0,-1,1,21101,40,0,2,21101,0,463,3,21101,0,453,0,1105,1,496,109,-2,2106,0,0,0,1,0,0,1,109,2,3,10,204,-1,1001,458,459,474,4,0,1001,458,1,458,108,4,458,10,1006,10,490,1102,1,0,458,109,-2,2105,1,0,0,109,4,1202,-1,1,495,1207,-3,0,10,1006,10,513,21102,0,1,-3,21201,-3,0,1,21202,-2,1,2,21101,0,1,3,21101,0,532,0,1106,0,537,109,-4,2106,0,0,109,5,1207,-3,1,10,1006,10,560,2207,-4,-2,10,1006,10,560,22102,1,-4,-4,1105,1,628,21201,-4,0,1,21201,-3,-1,2,21202,-2,2,3,21101,0,579,0,1105,1,537,22101,0,1,-4,21102,1,1,-1,2207,-4,-2,10,1006,10,598,21102,1,0,-1,22202,-2,-1,-2,2107,0,-3,10,1006,10,620,22102,1,-1,1,21101,0,620,0,106,0,495,21202,-2,-1,-2,22201,-4,-2,-4,109,-5,2106,0,0)
paint_pgm["99"] # none
paint_pgm[paint_pgm > 1e6]
length(paint_pgm) # 633

library(R6)

Intcode <- 
  R6Class("Intcode",
   public = list(
     pgm = NULL,
     ptr = 0,
     base = 0,
     halt = FALSE,
     
     initialize = function(pgm) {
       self$pgm <- pgm
     },
     
     # take an input
     # assemble outputs (two)
     # return output on next input request
     
     run = function(input = 1, stop = 99999) {
       input_available <- TRUE
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
           if(input_available) {
             self$pgm[values[1]] <- input # consume the input
             self$ptr <- self$ptr + 2
             input_available <- FALSE
           } else {
             break() # return the output and "pause"
           }
           
         } else if(Opcode == 4) {
           # build output
           if(is.null(output)) output <- self$pgm[values[1]]
           else output <- c(output, self$pgm[values[1]])
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
       return(output) 
     } # end of run
   )
)

Robot <- 
  R6Class("Robot",
  public = list(
    brain = NULL, 
    facing = NULL,  # direction it is facing, 1:4
    pos_id = NULL, # id of robot's position
    hull = NULL, # dataframe map of the current and past positions of the robot, color, and paint history
    camera = NULL, # color robot's camera sees -- can affect future outputs, i.e. robot actions

    initialize = function(paint_pgm) {
      self$brain <- Intcode$new(paint_pgm)
      self$facing <- 1 # 0:3 left/up/right/down
      self$pos_id <- 1 # at id 1
      self$hull <- tibble(id = 1, x = 0, y = 0, color = 0, painted = FALSE, looking = 1)
      self$camera <- 0 # black
    },
    
    paint = function(col) { # output from brain

      self$hull <- 
        self$hull %>% 
          mutate(
            color = if_else(id == self$pos_id, col , color),
            painted = if_else(id == self$pos_id, TRUE, painted)
          )
      
    },
    
    move = function(dir) { 

      # turn
      self$facing <- face(self$facing, dir)
      # take step and add x, y coordinates & looking
      self$pos_id <- self$pos_id + 1
      
      self$hull <- 
        self$hull %>% 
        add_row(
          id = self$pos_id,
          x = {     if(self$facing == 0) self$hull$x[1] - 1
               else if(self$facing == 2) self$hull$x[1] + 1
               else                      self$hull$x[1]}, 
          # y rise from top to bottom
          y = {     if(self$facing == 1) self$hull$y[1] - 1
               else if(self$facing == 3) self$hull$y[1] + 1
               else                      self$hull$y[1]}, 
          looking = self$facing,
          # add new row at the top 
          .before = 1
        )
    },
     
    detect_color = function() { 
      # if I have been here before ... otherwise ...
      if(nrow(self$hull %>% filter(x == self$hull$x[1], y == self$hull$y[1])) > 1) {
        
        visits_here <- self$hull %>% filter(x == self$hull$x[1], y == self$hull$y[1])
        self$hull$color[1] <- visits_here$color[2] # assign previous color to here
        self$hull$painted[1] <- TRUE # mark here as painted
        # self$hull <- self$hull %>% distinct(x, y, .keep_all = TRUE) # drop old value
        
      } else {
        self$hull$color[1] <- 0 # black
        self$hull$painted[1] <- FALSE # not painted
      }
      
      # return color to brain
      return(self$hull$color[1])
    },
    
    start = function(col = 0) {

      while(self$brain$pgm[self$brain$ptr + 1] != 99) {
      
        if(!(col %in% 0:1)) {
          print(col)
          stop("col not 0 or 1")
        }
        
    
        output <- self$brain$run(col)

        stopifnot(length(output) == 2, output[1] %in% 0:1, output[2] %in% 0:1)
        
        self$paint(output[1]) 
        
        self$move(output[2])
        
        col <- self$detect_color()

      }
      
      return({
        self$hull %>% 
          distinct(x, y, .keep_all = TRUE) %>% 
          summarize(n_painted = sum(painted)) %>%
          pull()
        })
    }  
  )
)

painting_robot <- Robot$new(paint_pgm)
painting_robot$start(0) # 292 -- too low

painting_robot$brain$ptr
nrow(painting_robot$hull) # 10982 -- seems like a lot of painting when on!
nrow(painting_robot$hull %>% distinct(x, y)) # 292
summary(painting_robot$hull) # is in a very narrow band
painting_robot$hull %>% count(looking)


ggplot(data = painting_robot$hull, 
  aes(x = x, y = y, fill = color)) + geom_tile()

ggplot(data = painting_robot$hull %>% distinct(x, y, .keep_all = TRUE), 
       aes(x = x, y = y, fill = color)) + geom_tile()


painting_robot$hull %>% slice(1)

ggplot(data = painting_robot$hull, 
  aes(x = x, y = y, color = id)) +  geom_path()

ggplot(data = painting_robot$hull %>% distinct(x, y, .keep_all = TRUE), 
       aes(x = x, y = y, color = id)) +  geom_path()

painting_robot <- Robot$new(paint_pgm)

output[1] <- 0
while(painting_robot$brain$pgm[painting_robot$brain$ptr + 1] != 99) {
output <- painting_robot$brain$run(output[1]); painting_robot$brain$ptr
}



painting_robot$brain$pgm[painting_robot$brain$pgm > 1e6]
paint_pgm[paint_pgm > 1e6]
