## Put comments here that give an overall description of what your
## functions do

## Create a cacheable object relating to a matrix which specificed by the argument x.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  
  get <- function() x
  
  setinv <- function(z){
    inv <<- z
  }
  
  getinv <- function() inv
  
  list(set = set, get = get, getinv = getinv, setinv = setinv)
}


## Sovle a inverse of a matrix specified by the cacheable object x, 
## which is created by the makeCacheMatrix function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)){
    message('get a inverse from the cache')
    return(inv)
  }
  
  ## Retrieve the original matrix of which inverse would be solved.
  m <- x$get()
  
  ## Actually, not all matrices have inverses.
  ## In order to make this script run properly here,suppose the matrix
  ## was non-invertible if an error has been raise with the solve function
  ## invoked and define the inverse is NULL.
  r <-try(solve(m), silent = TRUE)
  if('try-error' == class(r)){
    x$setinv(NULL)
    return(NULL)
  }
  x$setinv(r)
  r
}

