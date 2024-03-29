# Day 10
# Map of asteroids
library(tidyverse)

###########################

.#..#
.....
#####
....#
...##

......#.#.
#..#.#....
..#######.
.#.#.###..
.#..#.....
..#....#.#
#..#....#.
.##.#..###
##...#..#.
.#....####

#.#...#.#.
.###....#.
.#....#...
##.#.#.#.#
....#.#.#.
.##..###.#
..#...##..
..##....##
......#...
.####.###.

.#..#..###
####.###.#
....###.#.
..###.##.#
##.##.#.#.
....###..#
..#.#..#.#
#..#.#.###
.##...##.#
.....#.#..

.#..##.###...#######
##.############..##.
.#.######.########.#
.###.#######.####.#.
#####.##.#.##.###.##
..#####..#.#########
####################
#.####....###.#.#.##
##.#################
#####.##.###..####..
..######..##.#######
####.##.####...##..#
.#####..#.######.###
##...#.##########...
#.##########.#######
.####.#.###.###.#.##
....##.##.###..#####
.#.#.###########.###
#.#.#.#####.####.###
###.##.####.##.#..##

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

#######################################

# greatest common divisor with recursion
# https://community.rstudio.com/t/tidiest-way-to-do-recursion-safely-in-r/1408/7
# gcd <- function(a, b) {
#   stopifnot(a != 0, b != 0)
#   if(a == b) {
#     return(a)
#   }
#   else {
#     gcd(min(a, b), max(a, b) - min(a, b))
#   }
# }

gcd <- function(a, b) {
  stopifnot(a != 0, b != 0)
  a <- abs(a)
  b <- abs(b)
  while(a != b) {
    A <- min(a, b)
    B <- max(a, b) - min(a, b)
    a <- A
    b <- B
  }
  return(a)
}
# gcd(105, 252)
# gcd(105, -252)
# gcd(-105, -252)

view_line <- function(sta_id, ast_id) {
  ax <- ast$x[ast$id == ast_id]
  ay <- ast$y[ast$id == ast_id]
  sx <- ast$x[ast$id == sta_id]
  sy <- ast$y[ast$id == sta_id]

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

ast
n_views <- map_int(ast$id,
  function(sid) {     
    map_lgl(ast$id[-sid], 
      function(aid) {
        nrow(inner_join(ast, view_line(sid, aid), by = c("x", "y"))) == 2
    }) %>% sum()
})    
max_id <- which.max(n_views)
n_views[max_id]
ast %>% filter(id == max_id)

# 274  from 19, 14, id 280



