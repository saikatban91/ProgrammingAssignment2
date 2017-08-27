## In the functions below, the  <<- operator is used 
## to assign values to objects "i" and "x" in a different environment
## from the current environment they are in. The two functions below 
## can compute and cache the inverse of a matrix and provide a useful
## solution to otherwise expensive and time-consuming computations. 

## The following makeCacheMatrix () function creates a special "matrix"
## that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y){
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
 ## The following list is returned from this function and used as the 
 ## input to cacheSolve()
   list(set = set, get = get, setinverse = setinverse, 
       getinverse = getinverse)
}

## The following cacheSolve () function computes the inverse of
## an invertible matrix that is the output of the
## makeCacheMatrix()cach function. The first job it does is verify 
## whether the inverse has been already computed.
## If yes, then returns the inverse from the cache
## and skips the computation. Or, it calculates the 
## inverse of the matrix and sets the value of the 
## inverse in the cache through the setinverse 
## function

cacheSolve <- function(x, ...) {
  ## input matrix to cacheSolve() is from the output of makeCacheMatrix()
  i <- x$getinverse()
  ## Checking whether the inverse has been 
  ## calculated; if yes, then gets it from cache
  ## otherwise continue.
  if(!is.null(i)){
    message("getting cached data")
    return(i)
  }
  ## Computing the inverse after checking that
  ## the value is not available
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
