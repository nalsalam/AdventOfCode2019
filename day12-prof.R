library(tidyverse)
# library(profvis)

position <- c(-1, 2, 4, 3)
velocity <- rep(0, 4)

gravity <- function(position, velocity) {
  rowSums(sign(-outer(position, position, FUN = "-")))
}

gravity_fast <- function(position, velocity) {
  .rowSums(sign(-outer(position, position, FUN = "-")), 4, 4, na.rm = FALSE)
}

outer_diff_fst <- function(X, Y) {
  Y <- rep.int(position, rep.int(4, 4))
  X <- rep(position, times = 1)
  robj <- `-`(X, Y)
  dim(robj) <- c(4, 4)
  robj
}
bench::mark(
  outer(position, position, FUN = "-"),
  outer_diff_fst(position)
) %>% View() # total time 93 vs. 43 ms

gravity_faster <- function(position, velocity) {
  .rowSums(sign(-outer_diff_fst(position)), 4, 4, na.rm = FALSE)
}
# gravity_faster(position, velocity)

bench::mark(
  gravity(position, velocity), 
  gravity_fast(position, velocity),
  gravity_faster(position, velocity)
  ) %>% View() # 207, 103, 43 -- Nice!

moons_upd <- function(x, x_v) {
    moon_scan %>%
    mutate(
      x_v = x_v + gravity(x, x_v) #,
      # y_v = y_v + gravity(y, y_v),
      # z_v = z_v + gravity(z, z_v)
    ) %>%
    mutate(
      x = x + x_v # ,
      # y = y + y_v,
      # z = z + z_v
    )
}

moons_upd(position, velocity)
  
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

sim_moons <- function() {
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
    if(all(moon_scan_start == moon_scan) || i >= 1000) {
      steps <- i
      break()
    }
    i <- i + 1
  }
  return(i)
} 

moon_scan$x_v <- moon_scan$x_v + gravity(moon_scan$x, moon_scan$x_v)
typeof(mutate)
dplyr:::mutate # not .Primitive
class(dplyr:::mutate)
typeof(all)
all # .Primitive, i.e. C++

