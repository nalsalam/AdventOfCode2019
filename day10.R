# Day 10
# Map of asteroids

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
str_replace_all("\\n", "" ) %>%
str_replace_all("\\.", "0") %>%
str_replace_all("#", "1") %>%
matrix(nrow = 26, ncol = 26)

asteroid_map


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





