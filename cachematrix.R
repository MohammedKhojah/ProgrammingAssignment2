## This solution is for Assignment 2 in R Programing course
## The solution aims to solve input matrix to return its invers matrix
## (by using solve() function). The solution has two parts.
## Part1: A function that creates a matrix
## Part2: A function that checks if the inverse has been already calculated. If not,
## it will calculate the inverse and return its value.

## This is main function (1) which will create the matrix
## It has 4 sub functions:
## Set the matrix
## Get the matrix
## Set the solve
## Get the solve

makeCacheMatrix <- function(x = numeric()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) m <<- solve
    getsolve <- function() m
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}


## This is main function (2) which will check for and calculate the inverse
## The function will first check if the inverse has been already created and
## return that value.
## If the inverse has never been cached, the inverse will be calculated and
## its value will be returnd.

cacheSolve <- function(x, ...) {
    m <- x$getsolve()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setsolve(m)
    m
}
