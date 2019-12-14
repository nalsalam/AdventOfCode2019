
# Can a character value in R, i.e. a character vector of length 1 be treated as a list of its individual characters?
# Both python and javascript use the bracket this way,
# E.g.   
str <- "543210" 
# str[0] returns 5 in both of those languages, i.e. bracket is a string method

# Lets do this in R

# define a method 
bracket <- .Primitive("[")
# test
bracket(stringr::str_sub("123", 2, 2))
# bind this method to [ for class string
`[.string` <- function(s, i) bracket(stringr::str_sub(s, i, i))

# give str the string class
class(str) <- c("string", class(str))
# the `[` function now works like in python and javascript
str[1]
# Yay!

# inspired by inscaven's answer at
# https://stackoverflow.com/questions/25307549/zero-based-arrays-vectors-in-r





