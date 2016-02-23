## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# this function can cacht a matrix and its inverse. 
# if the matrix is set, the inverse will be set to Null
# it can be used together with the function below
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInv <- function(i) inv <<- i
  getInv <- function() inv
  list(set = set, 
       get = get,
       setInv = setInv,
       getInv = getInv)
}


## Write a short comment describing this function
# this function will calculate the inverse of a matrix and store it in am
# instance of the cach function above. 
# If the cahce function already stored the inverse, 
# the matrix is not inverted again.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getInv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setInv(inv)
  inv
  
}

