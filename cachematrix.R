## The comments here will give an overall description of what your
## functions do

## A short comment describing this function
## The first function, makeCacheMatrix creates a special "cacheMatrix", 
## which is really a list containing a function to
## 1. set the value of the cacheMatrix
## 2. get the value of the cacheMatrix
## 3. set the value of the inverse of the cacheMatrix
## 4. get the value of the inverse of the cacheMatrix

## the " <<- " operator which can be used to assign a value to 
## an object in an environment that is different from the current environment

makeCacheMatrix <- function(x = matrix()) {
  matInv <- NULL
  set <- function(y) {
    x <<- y
    matInv <<- NULL
  }
  get <- function() x
  setInv <- function(inverse) matInv <<- inverse
  getInv <- function() matInv
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}

## A short comment describing this function
## The following function calculates the inverse of the special "cahceMatrix" created with the above function. 
## However, it first checks to see if the inverse has already been calculated. 
## If so, it gets the inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the data and sets the value of the inverse in the cache via the setInv function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  matInv <- x$getInv()
  if(!is.null(matInv)) {
    message("getting cached data.")
    return(matInv)
  }
  data <- x$get()
  matInv <- solve(data)
  x$setInv(matInv)
  matInv
}
