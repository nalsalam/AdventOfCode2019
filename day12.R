# Day 12 
# N-body problem

# 4 objects
# each has 
# 3 coordinate position 
# 3 coordinate velocity
# potential energy = sum of coordinates
# kinetic energy = sum of velocities
# total energy = pot * kin

# time steps
# gravity is the interaction between pairs of objects
# ** apply gravity between all pairs to determine velocity **
# gravity of an object with itself is zero, so all pairs can include the object itself

# apply velocity to determine position 

# gravity matrix
# off diagonal elements are the gravity effects on velocity
# sum of a row is the total gravity effect on velocity
# not symmetric but negative

library(tidyverse)

gravity <- function(position, velocity) {
  rowSums(sign(-outer(position, position, FUN = "-")))
}

velocity <- function(p, v) {
  return(p + v)
}

# EXAMPLE 1

# <x=-1, y=0, z=2>
# <x=2, y=-10, z=-7>
# <x=4, y=-8, z=8>
# <x=3, y=5, z=-1>

moon_scan <-
  tibble(x = NA_real_, y = NA_real_, z = NA_real_) %>%
  add_row(x=-1, y=0, z=2) %>%
  add_row(x=2, y=-10, z=-7) %>%
  add_row(x=4, y=-8, z=8) %>%
  add_row(x=3, y=5, z=-1) %>%
  slice(-1)

# initial value -- zero steps
moon_scan <-
  tibble(
    x = c(-1, 2, 4, 3),
    y = c(0, -10, -8, 5),
    z = c(2, -7, 8, -1),
    x_v = rep(0, 4),
    y_v = rep(0, 4),
    z_v = rep(0, 4),
    pot = abs(x) + abs(y) + abs(z),
    kin = abs(x_v) + abs(y_v) + abs(z_v),
    tot = pot * kin
    )

i <- 1
while(i <= 10) {
  moon_scan <-
    moon_scan %>%
      mutate(
        x_v = gravity(x, x_v),
        y_v = gravity(y, y_v),
        z_v = gravity(z, z_v)
      ) %>%
      mutate(
        x = velocity(x, x_v),
        y = velocity(y, y_v),
        z = velocity(z, z_v)
      ) %>%
      mutate(
        pot = abs(x) + abs(y) + abs(z),
        kin = abs(x_v) + abs(y_v) + abs(z_v),
        tot = pot * kin
      )
  i <- i + 1
}
moon_scan
moon_scan %>% summarize(total = sum(tot))

# EXAMPLE 2

moon_scan <-
tibble(x = NA_real_, y = NA_real_, z = NA_real_) %>%
add_row(x=-8, y=-10, z=0) %>%
add_row(x=5, y=5, z=10) %>%
add_row(x=2, y=-7, z=3) %>%
add_row(x=9, y=-8, z=-3) %>%
slice(-1) %>%
mutate(
  x_v = 0,  y_v = 0, z_v = 0
  ) %>%
mutate(
  pot = abs(x) + abs(y) + abs(z),
  kin = abs(x_v) + abs(y_v) + abs(z_v),
  tot = pot * kin
)  
i <- 1
while(i <= 100) {
  moon_scan <-
    moon_scan %>%
    mutate(
      x_v = x_v + gravity(x, x_v),
      y_v = y_v + gravity(y, y_v),
      z_v = z_v + gravity(z, z_v)
    ) %>%
    mutate(
      x = velocity(x, x_v),
      y = velocity(y, y_v),
      z = velocity(z, z_v)
    ) %>%
    mutate(
      pot = abs(x) + abs(y) + abs(z),
      kin = abs(x_v) + abs(y_v) + abs(z_v),
      tot = pot * kin
    )
  i <- i + 1
}
moon_scan
moon_scan %>% summarize(total = sum(tot))

# PUZZLE 

moon_scan <-
  tibble(x = NA_real_, y = NA_real_, z = NA_real_) %>%
  
  add_row(x=7, y=10, z=17) %>%
  add_row(x=-2, y=7, z=0) %>% 
  add_row(x=12, y=5, z=12) %>% 
  add_row(x=5, y=-8, z=6) %>%
  
  slice(-1) %>%
  mutate(
    x_v = 0,  y_v = 0, z_v = 0
  ) %>%
  mutate(
    pot = abs(x) + abs(y) + abs(z),
    kin = abs(x_v) + abs(y_v) + abs(z_v),
    tot = pot * kin
  )  
system.time({
  i <- 1
  while(i <= 1000) {
    moon_scan <-
      moon_scan %>%
      mutate(
        x_v = x_v + gravity(x, x_v),
        y_v = y_v + gravity(y, y_v),
        z_v = z_v + gravity(z, z_v)
      ) %>%
      mutate(
        x = x + x_v,
        y = y + y_v,
        z = z + z_v
      ) # %>%
    # mutate(
    #   pot = abs(x) + abs(y) + abs(z),
    #   kin = abs(x_v) + abs(y_v) + abs(z_v),
    #   tot = pot * kin
    # )
    i <- i + 1
  }
})

moon_scan %>% 
  mutate(
    pot = abs(x) + abs(y) + abs(z),
    kin = abs(x_v) + abs(y_v) + abs(z_v),
    tot = pot * kin
  ) %>% 
  summarize(TOT = sum(tot)) ## 9958

###############################################################################################

# Determine the number of steps that must occur before all of the moons' positions 
# and velocities exactly match a previous point in time.

# Clearly, you might need to find a more efficient way to simulate the universe.

# EXAMPLE 1 AGAIN

moon_scan <-
  tibble(
    x = c(-1, 2, 4, 3),
    y = c(0, -10, -8, 5),
    z = c(2, -7, 8, -1),
    x_v = rep(0, 4),
    y_v = rep(0, 4),
    z_v = rep(0, 4),
    pot = abs(x) + abs(y) + abs(z),
    kin = abs(x_v) + abs(y_v) + abs(z_v),
    tot = pot * kin
  )
moon_scan_start <- moon_scan

system.time({
i <- 1
while(i <= 2772) {
  moon_scan <-
    moon_scan %>%
    mutate(
      x_v = x_v + gravity(x, x_v),
      y_v = y_v + gravity(y, y_v),
      z_v = z_v + gravity(z, z_v)
    ) %>%
    mutate(
      x = x + x_v,
      y = y + y_v,
      z = z + z_v
    ) # %>%
    # mutate(
    #   pot = abs(x) + abs(y) + abs(z),
    #   kin = abs(x_v) + abs(y_v) + abs(z_v),
    #   tot = pot * kin
    # )
  i <- i + 1
  if(i >= 2770) print(moon_scan)
}
}) # 1.52  (
# 5.31 on work computer, 
# 1.97 when energy stuff pulled off to the end, 
# substituting out velocity had very small effect
moon_scan %>% 
mutate(
  pot = abs(x) + abs(y) + abs(z),
  kin = abs(x_v) + abs(y_v) + abs(z_v),
  tot = pot * kin
)
moon_scan_start
# same

# REPEAT BUT TEST FOR RETURN 

moon_scan <-
  tibble(
    x = c(-1, 2, 4, 3),
    y = c(0, -10, -8, 5),
    z = c(2, -7, 8, -1),
    x_v = rep(0, 4),
    y_v = rep(0, 4),
    z_v = rep(0, 4)
  )
moon_scan_start <- moon_scan

system.time({
  i <- 1
  repeat {
    moon_scan <-
      moon_scan %>%
      mutate(
        x_v = x_v + gravity(x, x_v),
        y_v = y_v + gravity(y, y_v),
        z_v = z_v + gravity(z, z_v)
      ) %>%
      mutate(
        x = x + x_v,
        y = y + y_v,
        z = z + z_v
      ) # %>%
    # mutate(
    #   pot = abs(x) + abs(y) + abs(z),
    #   kin = abs(x_v) + abs(y_v) + abs(z_v),
    #   tot = pot * kin
    # )
    if(all(moon_scan_start == moon_scan)) {
      print(i)
      break()
    }
    i <- i + 1
  }
}) 
# slowed to 3.05 due to the constant testing
moon_scan
moon_scan_start

### OK they are right.  What is a more efficient way?

# PUZZLE 

library(profvis)
library(bench)
install.packages("bench")

moon_scan <-
  tibble(x = NA_real_, y = NA_real_, z = NA_real_) %>%
  
  add_row(x=7, y=10, z=17) %>%
  add_row(x=-2, y=7, z=0) %>% 
  add_row(x=12, y=5, z=12) %>% 
  add_row(x=5, y=-8, z=6) %>%
  
  slice(-1) %>%
  mutate(
    x_v = 0,  y_v = 0, z_v = 0
  )
moon_scan_start <- moon_scan

system.time({
  i <- 1
  repeat {
    moon_scan <-
      moon_scan %>%
      mutate(
        x_v = x_v + gravity(x, x_v),
        y_v = y_v + gravity(y, y_v),
        z_v = z_v + gravity(z, z_v)
      ) %>%
      mutate(
        x = x + x_v,
        y = y + y_v,
        z = z + z_v
      ) # %>%
    # mutate(
    #   pot = abs(x) + abs(y) + abs(z),
    #   kin = abs(x_v) + abs(y_v) + abs(z_v),
    #   tot = pot * kin
    # )
    if(all(moon_scan_start == moon_scan)) {
      steps <- i
      break()
    }
    i <- i + 1
  }
}) 
print(i)

# (1.94 * 4686774924 / 2772) / 3600 / 24 -- 38 days!

# Ideas:

# Split x, y, and z and run in parallel

# Speed up time, e.g. take 100 time steps at a time
# I think this would require doing math to figure out what happens



