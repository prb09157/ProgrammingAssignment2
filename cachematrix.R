# Assignment:Caching the Inverse of a Matrix
# Matrix inversion can be a costly computation, with this there may be some 
# advantage to caching the inverse of a matrix compared with computing it ##repeatedly.
# Examples below show a pair of functions that are used to create a unique  
# object that cache the inverse of a matrix.


MakeCache <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set,get = get,
       setInverse = setInverse,getInverse = getInverse)
}


# The below function computes the inverse of the unique "matrix" from the
# MakeCacheMatrix above. It checks if the inverse has already been calculated,
# if so it gets the result and skips the computation, otherwise it should 
# then retrieve the inverse from the cache.

Solve <- function(x, ...) {
  # Return a matrix the inverse of 'x'
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("Test R")
    return(inv)
  }
  matrix <- x$get()
  inv <- solve(matrix,...)
  x$setInverse(inv)
  inv
}

#> source('~/Downloads/ProgrammingAssignment2-master-2/cachematrix.R')
#> m_1 <- MakeCache(matrix(1:4, 2, 2))
#> m_1$get()
#[,1] [,2]
#[1,]    1    3
#[2,]    2    4
#> m_1$getInverse()
#NULL
#> Solve(m_1)
#[,1] [,2]
#[1,]   -2  1.5
#[2,]    1 -0.5
#> m_1$set(matrix(c(4, 4, 2, 1), 2, 2))
#> m_1$get()
#[,1] [,2]
#[1,]    4    2
#[2,]    4    1
#> m_1$getInverse()
#NULL
#> Solve(m_1)
#[,1] [,2]
#[1,] -0.25  0.5
#[2,]  1.00 -1.0
#> Solve(m_1)
#Test R
#[,1] [,2]
#[1,] -0.25  0.5
#[2,]  1.00 -1.0