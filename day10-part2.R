# Day 10 part 2
# Rotating Laser

library(tidyverse)

asteroid_map <- c("
.##.#.#....#.#.#..##..#.#.
#.##.#..#.####.##....##.#.
###.##.##.#.#...#..###....
####.##..###.#.#...####..#
..#####..#.#.#..#######..#
.###..##..###.####.#######
.##..##.###..##.##.....###
#..#..###..##.#...#..####.
....#.#...##.##....#.#..##
..#.#.###.####..##.###.#.#
.#..##.#####.##.####..#.#.
#..##.#.#.###.#..##.##....
#.#.##.#.##.##......###.#.
#####...###.####..#.##....
.#####.#.#..#.##.#.#...###
.#..#.##.#.#.##.#....###.#
.......###.#....##.....###
#..#####.#..#..##..##.#.##
##.#.###..######.###..#..#
#.#....####.##.###....####
..#.#.#.########.....#.#.#
.##.#.#..#...###.####..##.
##...###....#.##.##..#....
..##.##.##.#######..#...#.
.###..#.#..#...###..###.#.
#..#..#######..#.#..#..#.#
") %>%
  str_replace_all("\\n", "" )
r <- sqrt(str_length(asteroid_map))

asteroid_matrix <-
  map_lgl(1:r^2, 
          function(i) {
            str_sub(asteroid_map, i, i) == "#"
          }) %>%
  matrix(nrow = r, ncol = r, byrow = TRUE, dimnames = list(y = 0:(r-1), x = 0:(r-1)))

ast <-
  as_tibble(as.table(asteroid_matrix)) %>% # x/y are chr columns
  rename(asteroid_here = n) %>%
  mutate_all(as.numeric) %>%
  filter(asteroid_here == TRUE) %>% # just asteroid
  mutate(id = 1:n()) %>%
  select(id, x, y)
View(ast)

############

ast_here <- function(X, Y) {
  nrow(ast %>% filter(x == X, y == Y)) == 1
}
ast_here(19, 14)

rise <- c(rep(0, 25), 0:24, rep(25, 25), 25:1)
run <- c(0:24, rep(25, 25), 25:1, rep(0, 25))
slopes <- tibble(y = rise, x = run) 
ggplot(slopes, aes(x = x, y = -y, group = 1)) + geom_point()

rise <- c(rep(-14, 25-19), -14:(25-14), rep(25, 19), 25:0, rep(25-19))

run <- 19:(25-19), 
# The laser location is given
# Need the increments for rotating the laser
#   move along the perimeter adding 1 to x until it reaches the max
#   then add 1 to y unti it reaches the max
#   then subtrct 1 from x until it reaches the min
#   then subtract 1 from y unti it reach the min
#   then add 1 to x ...

# Function to find first asteroid on the line 
#   Similar to view_line 
# Eliminate the asteroid
# Create an R6 object to keep track of remaining asteroids and laser direction

Asteroids  <- R6Class("Asteroids",
  public = list(
  initialize = function(map) {
  },
  fire = function
  
    
  )
)
)

asteroid = function(x, y) {
  if(self$ast(x, y) == TRUE) TRUE
}

blow_up <- function(run, rise) {
  sx <- 19
  sy <- 14
  
  if(run == 0) {
      ay <- sy + 1
    while(asteroid(sx, ay) == FALSE || ay > ay_mx) {
      ay <- ay + 1
    }
      self$asteroid <- FALSE  
      self$n_destroyed <- self$n_destroyed + 1
      
  } else(rise == 0) {
    
  } else {
    gcf <- gcd(run, rise)
  
  }
  
  
  
  if(ax == sx) { # same col
    map_dfr(0:(ay - sy), function(i) {
      tibble(x = sx, y = sy + i)
    })
  } else if(ay == sy) {  # same row
    map_dfr(0:(ax - sx), function(i) {
      tibble(x = sx + i, y = sy)
    })
  } else {
    gcf <- gcd(ax - sx, ay - sy)
    map_dfr(0:gcf, function(i) {
      tibble(x = sx + i * floor((ax - sx) / gcf), 
             y = sy + i * floor((ay - sy) / gcf))
    })
  }
}
