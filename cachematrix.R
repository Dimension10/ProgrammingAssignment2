## From the assignment: 
## Matrix inversion is usually a costly computation and there may be 
## some benefit to caching the inverse of a matrix rather than  
## computing it repeatedly. Beneath you will find  
## a pair of functions that cache the inverse of a matrix. 
## Solution: This function creates a special "matrix" object that can 
## cache its inverse. 


makeCacheMatrix <- function(x = matrix()) {
  m <- NULL 
  
  set <- function(y) { 
    x <<- y 
    m <<- NULL 
  } 
  
  get <- function() x 
  
  setsolve <- function(solve) { 
    m <<- solve 
  } 
  
  getsolve <- function() m 
  
  list(set = set, get = get, 
       setsolve = setsolve, 
       getsolve = getsolve) 
} 
## This following function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inversealready has already 
## been calculated (and the matrix has not changed), then the cacheSolve
## will retrieve the inverse from the cache. 

  cacheSolve <- function(x, ...) { 
    ## Return a matrix that is the inverse of 'x'
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
  
## Tested with: 
  ## A <- matrix(c(1,2,3,4), nrow =2, ncol=2, byrow = TRUE)
  ## B <- makeCacheMatrix(A)
  
  ## A
  ## [,1] [,2]
  ## [1,]    1    2
  ## [2,]    3    4
  
  ## B <- makeCacheMatrix(A)
  ## cacheSolve(B)
  ## [,1] [,2]
  ## [1,] -2.0  1.0
  ## [2,]  1.5 -0.5
