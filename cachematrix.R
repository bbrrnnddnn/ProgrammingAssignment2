## Put comments here that give an overall description of what your
## functions do

##This will create the cache for a given matrix and utillize set and get in order to retrieve a matrix and output it. 
##Then it wil set and get the inverse of the given matrix. 

makeCacheMatrix <- function(x = matrix()) {
 inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function() inv <<- solve(x) #this will calculate the inverse of a function
  getInverse <- function() inv
  list(set = set, get = get,setInverse = setInverse, getInverse = getInverse)
}

##Will store previously called matrices

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()  
  if (!is.null(inv)) {   
    message("getting cached data")
    return(inv) ## Return a matrix that is the inverse of 'x' if it is not null.
  }
  mat <- x$get() 
  inv <- solve(mat, ...)
  x$setInverse(inv) ## Will utilize the previously established 'setInverse' function
  inv
}
