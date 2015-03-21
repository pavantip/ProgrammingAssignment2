## This function creates a special "matrix" object that can cache its inverse.
## invocation ex: a <- makeCacheMatrix(matrix(c(1,2,3,4),nrow=2,ncol=2))

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL # setting the inverse to null
  set <- function(y) { 
    x <<- y
    inv <<- NULL
  }
  #use a$get to retrieve the set matrix
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  # creating the special matrix
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

## This function computes the inverse of the special
## "matrix" returned by `makeCacheMatrix` above. If the inverse has
## already been calculated, then
## `cacheSolve` should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data.")
    return(inv)
  }
  initmatrix <- x$get()
  inv <- solve(initmatrix)
  x$setinverse(inv)
  inv
}

## usage
##a <- makeCacheMatrix(matrix(c(1,2,3,4),nrow=2,ncol=2))
## > a$get()
##     [,1] [,2]
##[1,]    1    3
##[2,]    2    4
##> cacheSolve(a)
##     [,1] [,2]
##[1,]   -2  1.5
##[2,]    1 -0.5
##> cacheSolve(a)
##getting cached data.
##     [,1] [,2]
##[1,]   -2  1.5
##[2,]    1 -0.5
