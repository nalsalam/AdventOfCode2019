# Day 8 
# BIOS Password
library(tidyverse)

# 25 cols by 6 rows
# layer with fewest 0 digits
# what is number of 1 digits by the number of 2 digits on that layer

# read it
file.size("Day8_puzzle_input.txt") # 15000
single_string <- readChar("Day8_puzzle_input.txt", 15000)

# split apart
string_of_digits <-
purrr::map_chr(1:15000,
        function(i) stringr::str_sub(single_string, i, i)
)
# coerce to numeric
string_of_digits <- as.numeric(string_of_digits)

# convert to 3 dimensional array
layers <- array(string_of_digits, dim = c(25, 6, 1000))

n_zeros <-
  purrr::map_dbl(1:1000,
    function(i) sum(layers[, , i] == 0))

sum(layers[, , which.min(n_zeros)] == 1) * sum(layers[, , which.min(n_zeros)] == 2)
# 1905 

# PART 2
# 0 is black
# 1 is white
# 2 is transparent
# first non transparent, i.e not 2

# !(... == 2) drops the transparent colors
# [1] takes the first remaining color
# map not available for matrix so convert to r, c
x <- map_dbl(1:150, 
  function(i) {
    layers[(i - 1) %% 25 + 1, (i - 1) %/% 25 + 1, !(layers[(i - 1) %% 25 + 1, (i - 1) %/% 25 + 1, ] == 2)][1]
    }
  )

# tough to read
y <- t(matrix(x, nrow = 25, ncol = 6))

# for ggplot
y <- tibble(i = 1:150, color = x, c = (i - 1) %% 25 + 1, r = (i - 1) %/% 25 + 1)

ggplot(data = y, aes(x = c, y = 6 - r, fill = color)) +
  geom_tile()
# ACKPZ

