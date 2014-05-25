## Put comments here that give an overall description of what your
## functions do


## cachematrix.R -- contains two functions that solve the inverse of a matrix and cache
## it.  If the inverse for a matrix has already been solved return
## the cached solution.
300
## makeCacheMatrix() - creates a matrix object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  # if no methods are called, set m to null to clear out cache
  m <- NULL
  
  # takes the matrix that is fed in and clears the cache
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  # returns the matrix
  get <- function() x
  
  # takes inverse of the original matrix and places it in the cache
  setinv <- function(inv) m <<- inv
  
  # returns the matrix stored in the cache
  getinv <- function() m
  
  # public methods
  list(set = set, get = get, setinv = setinv, getinv = getinv)

}


## cacheSolve() -- TFunction computes the inverse of the special
## matrix returned by makeCacheMatrix above. If the inverse has already been
## calculated and the matrix is unchanged then cacheSolve should retrieve the inverse from the cache.


cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  # get the contents of the cache
  m <- x$getinv() 
  
  # if the cache is not empty return the cache
  if(!is.null(m)) { 
    message("getting cached data")
    return(m)
  }
  # cache was empty, so get the matrix ...
  data <- x$get() 
  
  # ... invert it ...
  m <- solve(data, ...) 
  
  # ... store the inverse in the cache ...
  x$setinv(m) 
  
  # ... and return the inverse to the caller
  m 

}