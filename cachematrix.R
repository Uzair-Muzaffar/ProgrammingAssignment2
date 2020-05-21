## makeCacheMatrix takes on argument "x" as a matrix.
## the function stores the value of the matrix and its
## inverse using "<<-" so that the values are stored in the parent environment (G.E)
## The next function is the cacheSolve func. This function calculates the inverse of 
## the given matrix using "solve" function and stores it in cache for future. If the 
## inverse is already available in the cache, then that value is returned.


## The function returns the list of functions to set/get the matrix and set/get the 
##inverse of that matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse 
  getinv <- function() inv
  list(set = set, get = get, setinv = setinv, getinv = getinv)

}


## The cacheSolve function calculates the inverse of 
## the given matrix using "solve" function and stores it in cache for future. If the 
## inverse is already available in the cache, then that value is returned.


cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        if(!is.null(inv)){
          message("Getting cached Data")
          return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setinv(inv)
        inv
}
