##This code is used for inverting an invertible matrix using the Solve function in R.

## Write a short comment describing this function
## this function takes the matrix as an argument and returns a list of functions which help in setting, getting, setting inverse and getting inverse values for the matrix given.
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
      x <<- y
      inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Write a short comment describing this function
## this function takes the list output of makeCacheMatrix as argument and checks if the inverse is precalculated or not and returns the inverse of the matrix.
cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)){
              message("getting cached data")
              return(inv)
        }else{
              data <- x$get()
              inv <- solve(data, ...)
              x$setinverse
              return(inv)
        }
}
