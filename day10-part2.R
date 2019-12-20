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

############

ast_here <- function(X, Y) {
  nrow(ast %>% filter(x == X, y == Y)) == 1
}
ast_here(19, 14)

# angles for gun
# these are not enough
run <- c(0:24, rep(25, 25), 25:1, rep(0, 25))
run <- c(run[-(1:19)], run[1:19])
run <- run - 19

rise <- c(rep(0, 25), 0:24, rep(25, 25), 25:1)
rise <- c(rise[-(1:19)], rise[1:19])
rise <- rise - 14
perimeter <- tibble(y = rise, x = run)

# instead build slopes to all the other asteroids -- 363 of them
dir_to_asteroids <-
  ast %>%
  filter(x != 19 | y != 14) %>%
  mutate(slope  = - (y - 14) / (x - 19) ) %>%
  mutate(
    quadrant = case_when(
      (x - 19) >=0 & (y - 14) <=  0 ~ 1,
      (x - 19) >=0 & (y - 14) >   0 ~ 2,
      (x - 19) < 0 & (y - 14) >   0 ~ 3,
      (x - 19) < 0 & (y - 14) <=  0 ~ 4,
      TRUE ~ NA_real_
    )
  ) %>% 
  arrange(quadrant, desc(slope), abs(x - 19), abs(y - 14))

# but the laser needs to keep turning -- there are 274 unique directions
slopes <- 
  dir_to_asteroids %>% 
  distinct(quadrant, slope) %>% 
  mutate(slope_id = 1:n()) %>% select(slope_id, quadrant, slope)

# for any given slope_id between 1 and 274 detect where the asteroid is
detect <- function(slope_id) {
  slope <- slopes$slope[slope_id]
  quadrant <- slopes$quadrant[slope_id]

  if(slope == Inf) {
    for(Y in 13:0) {
        point <- ast %>% filter(x == 19, y == Y)
        if(nrow(point) == 1) return(point)
      }
      return(FALSE)

  } else if(slope == -Inf) {
    for(Y in 15:25) {
      point <- ast %>% filter(x == 19, y == Y)
      if(nrow(point) == 1) return(point)
    }
    return(FALSE)
  
  } else if(quadrant %in% 1:2) {
    for(X in 20:25) {
      point <- ast %>% filter(x == X, y == 14 - (X - 19) * slope)
      if(nrow(point) == 1) return(point)
    }
    return(FALSE)
    
  } else if(quadrant %in% 3:4) {
    for(X in 18:0) {
      point <- ast %>% filter(x == X, y == 14 - (X - 19) * slope)
      if(nrow(point) == 1) return(point)
    }
    return(FALSE)

  } else {
    print("What did I miss?")
  }
}

### destroy 200 and get that the coordinates
cycle <- function(id) {
  (id - 1) %% 274 + 1
}

slope_id <- 1
n_destroyed <- 0
repeat {
  a <- detect(cycle(slope_id))
  if(a == FALSE) {
    slope_id <- slope_id + 1
  } else {
    ast_id <- a$id
    ast <- ast %>% filter(id != ast_id) 
    n_destroyed <- n_destroyed + 1
    if(n_destroyed == 200) {
      print(a)
      print(slope_id)
      print(n_destroyed)
      break()
    } else {
      slope_id <- slope_id + 1
    }
  } 
}  
# id 45, x 3, y 5
# 305




# I needed to flip the picture vertically to comply with the top is zero view
ggplot() + 
  # geom_point(data = perimeter, aes(x = x, y = y)) +
  geom_point(data = ast, aes(x = x - 19, y = (y - 14)), symbol = "a", color = "red") +
  geom_segment(data = dir_to_asteroids, aes(xend = x - 19, yend = y - 14, x = 0, y = 0, color = factor(quadrant))) +
  scale_color_discrete() +
  scale_y_reverse(breaks = c(-14, -6, 0, 6, 11)) +
  scale_x_continuous(breaks = c(-19, -12, -6, 0, 6)) +
  labs(color = "Quadrant") +
  theme_minimal()

###########

library(R6)

Asteroids <- 
  R6Class("Asteroids", 
          
    public = list(
      hits = NULL,
      map = NULL,

      initialize = function(map) {
        self$map <- map
        self$hits <- 0
        
        private$slopes <-
          ast %>%
          filter(x != 19 | y != 14) %>%
          mutate(slope  = - (y - 14) / (x - 19) ) %>%
          mutate(
            quadrant = case_when(
              (x - 19) >=0 & (y - 14) >  0 ~ 1,
              (x - 19) >=0 & (y - 14) <= 0 ~ 2,
              (x - 19) < 0 & (y - 14) <= 0 ~ 3,
              (x - 19) < 0 & (y - 14) >  0 ~ 4,
              TRUE ~ NA_real_
            )
          ) %>% 
          arrange(quadrant, desc(slope), abs(x - 19), abs(y - 14))
        
          private$slope_num <- 1
      },
      
      ast_here = function(X, Y) {
        nrow(self$map %>% filter(x == X, y == Y)) == 1
      },
      
      aim = function() {
        private$in_sights <- slopes %>% slice(private$slope_num) %>% unlist()
        print(private$in_sights)
      },
      
      fire = function() {
        self$map <- self$map %>% filter(!private$in_sights["id"])
        print(paste("Asteroid", private$in_sights["id"], "blown up!"))
        self$hits <- self$hits + 1
        private$slope_num <- private$slope_num + 1
      }
      
    ),
    
    private = list(
      slopes = NULL,
      slope_num = NULL,
      in_sights = NULL
      
    ))

A <- Asteroids$new(map = ast)
A$hits
A$aim()
A$fire()
A$ast_here(19, 10)
A$aim()


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
