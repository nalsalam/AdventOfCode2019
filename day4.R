# day 4

# Password is in
# range: 168630-718098
# two adjacent numbers are the same
# the number left to right never decrease
# How many passwords meet those criteria? 

# Can I use regex? str_detect()?

# Brute force:
# create tests and step through all the numbers

never_decrease <- function(s) {
  i <- 1
  while(str_sub(s, i, i) <= str_sub(s, i + 1, i + 1)) {
    if(i == 5) {
      return(TRUE)
    }
    i <- i + 1
  }
  return(FALSE)
}

# test
never_decrease(as.character(168630))

# Without regex
repeat2 <- function(s) {
  i <- 1
  while(str_sub(s, i, i) != str_sub(s, i + 1, i + 1)) {
    if(i == 5) {
      return(FALSE)
    }
    i <- i + 1
  }
  return(TRUE)
}

# Using regex
repeat2 <- function(s) {
  str_detect(s, "(\\d)\\1")
}

# test
repeat2("168630")
repeat2("168633")

count_passwords <- function(low, high) {
  n_passwords <- 0
  n <- low
  
  while(n <= high) {
    s <- as.character(n)
    if(never_decrease(s) & repeat2(s)) {
      n_passwords <- n_passwords + 1
    }
    n <- n + 1
  }  
  return(n_passwords)
}

start <- Sys.time()
count_passwords(168630, 718098) #31 seconds, 1686; 54 seconds with str_detect version
Sys.time() - start

# Part 2

# More regex using lookahead
# https://stackoverflow.com/questions/42622292/match-a-repeating-digit-the-same-one-exactly-two-times-in-javascript-with-rege

repeat2 <- function(s) {
  str_detect(s, "(?:^|(\\d)(?!\\1))(\\d)\\2(?!\\2)")
}  

# tests
s <- c("112233", "123444", "111122")
str_detect(s, "(?:^|(\\d)(?!\\1))(\\d)\\2(?!\\2)")
# expecting TRUE FALSE TRUE

start <- Sys.time()
count_passwords(168630, 718098) # 1.38 minutes 1145
Sys.time() - start

