## Put comments here that give an overall description of what your
## functions do

## 'makeCacheMatrix' function creates a matrix that is able to cache its inverse
## Returns a list of functions to get/set the matrix and it's inverse
makeCacheMatrix <- function(x = matrix()) {
   inv <- NULL
   
   set <- function(m) {
      x <<- m
      inv <<- NULL
   }
   
   get <- function() x
   
   setinverse <- function(m) inv <<- m
   
   getinverse <- function() inv
   
   list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## 'cacheSolve' return a matrix that is inverse of 'x'. 
## Use cached result if available
cacheSolve <- function(x, ...) {
   m <- x$getinverse()
   
   if(is.null(m)) {
      m <- solve(x$get(), ...)
      x$setinverse(m)
   } else {
      message("getting cached data")
   }
   
   m
}
