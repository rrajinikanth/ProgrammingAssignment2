## Put comments here that give an overall description of what your
## Matrix inversion is usually costly computation. And there may be
## some benefit in caching the inverse of a matrix rather than 
## computing it repeatedly. 
## This R program caches the inverse of a matrix
##
## functions do
##
## Write a short comment describing this function
## This function creates a special matrix that can
## cache in inverse 
##
## mat <- matrix(c(1,2,3,0,1,4,5,6,0),3,3)
## thisMat <- makeCacheMatrix(mat)
## cacheSolve(thisMat)
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
    message("SET-ting matrix")
  }
  get <- function() {
    message("GET-ting matrix")
    x
  }
  setinverse <- function(inverse) {
    m <<- inverse
    message("SET inverse")
  }
  getinverse <- function() {
    message("GET inverse")
    m
  }
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function
## This function checks if the inverse of the matrix
## was already found and available in cache
## If available in cache, returns values from cache
## else, calculates the inverse of the matrix
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  else {
    message("NOT from cache")
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
  
}
