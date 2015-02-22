##  cachematrix is a program containing two independent functions 
##  that cache the inverse of a matrix.

##  makeCacheMatrix creates a object containing a special "matrix" 
##  capable of being cached as its inverse.
##  It uses a list of functions to create the special "matrix" object


makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL                       # Sets inverse variable to Null
  set <- function(y) {              # Sets default values to cache
    x <<- y                         # Cache is stored in new environment
    inv <<- NULL
  }
  
  get <- function() x                           #  Retrieves original matrix
  setinverse <- function(flip) inv <<- flip     #  Saves a new inversed matrix
  getinverse <- function() inv                  #  Retrieves inversed matrix
  list(set = set, get = get,                    #  Creates list of functions
       setinverse = setinverse,                 #  to return
       getinverse = getinverse)
}


##  cacheSolve is a function that computes the inverse using a special "matrix" 
##  returned by makeCacheMatrix if it hasn't been calculated previously, 
##  otherwise it returns cached inverted matrix

cacheSolve <- function(x, ...){

## Returns a matrix that is the inverse of 'x' if none exists or the cached 
  ## inversed matrix if it has already been caluclated
  
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
