# Day 10
# Map of asteroids
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
str_length(asteroid_map) # 676 or 26x26
astroid_array <-
  map_lgl(1:676, 
          function(i) str_sub(asteroid_map, i, i) == "#")
astroid_matrix <-
  matrix(astroid_array, nrow = 26, ncol = 26, byrow = TRUE, dimnames = list(1:26, 1:26))

astroid_df <-
  as_tibble(as.table(astroid_matrix), .name_repair = "minimal")

##########

astroid_matrix[26, 26]

gcd <- function(a, b) {
  if(a == b) {
    return(a)
  }
  else {
    gcd(min(a, b), max(a, b) - min(a, b))
  }
}
gcd(105, 252)

station <- tibble(x = 0, y = 0)
asteroid <- tibble(x = 26, y = 13)

# Points on line between station and another location
f <- function(station = c(x = 0, y = 0), asteroid = c(x = 26, y = 13)) {
  line <- asteroid - station
  f <- gcd(line[1], line[2])
  g <- line / f
  map(1:(f-1), function(i) {station + g * i})
}
blocked <- f()
transpose(blocked) %>% unlist()
as_tibble()
astroid_matrix[blocked[[1]][1], blocked[[1]][2]]


str_length(asteroid_map) # 703 including \n
str_length(c(".##.#.#....#.#.#..##..#.#.")) # rows of 26
floor(703 / 26)
26 * 26 + 26

direct line of sight
The best location is the asteroid that can detect the largest number of other asteroids.

from x,y, X,Y is viewable if no astroid is along the line connecting x,y to X,Y, i.e.
has coordinates a, b such that
(a - x) / (b - y) != (X - y) / (Y - y) and
x < a < X and y < b < Y or
X < a < x and Y < b < y

for each astroid position
iterate over all other asteroid positions and 
all point on a line between the two 
and sum the viewables 





