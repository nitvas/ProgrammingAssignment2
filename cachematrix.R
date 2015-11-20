## Put comments here that give an overall description of what your
## functions do

## This function creates the Matrix which is a list that returns four function
## 1. set - sets the matrix
## 2. get - gets the matrix
## 3. setinv - sets the value of the inverse
## 4. getinv - gets the value of the inverse
#

makeCacheMatrix <- function(x = matrix()) {
## initialize inv to Null. Here inv will be used to store the 
## inverse of the matrix
  inv <- NULL
## Sets the matrix to y
## Also assigns NULL to inv if it is called first time
## or if matrix gets changed
  set <- function(y){
    x <<- y
    inv <- NULL
  }
## Returns the matrix   
  get <- function()x
## Computes the inverse of the matrix and sets it to setinv  
  setinv <- function(solve) inv <<- solve
## Returns the value of the inverse inv   
  getinv <- function () inv
## All the 4 functions are returned as a list  
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## This function gets the inverse of the matrix created from 
## the other function
## if it is already cached, get the inverse from the cache
## if not caches it

cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
  
  inv <- x$getinv()
  if (!is.null(inv)){
    message("getting cached data - Matrix Inverse")
    return (inv)
  }
## if the inverse not cached , computes inverse and gets it  
  data <- x$get()
  inv <- solve(data,...)
  x$setinv(inv)
  inv
## returns the inverse
}
