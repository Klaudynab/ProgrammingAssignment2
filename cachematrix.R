## These functions allow you to cache the inverse matrix to a given matrix. 
## The code checks if the operation has already be done for a matrix with the same content
## and if so, it doesn't repeat the calculation, but takes the inversed matrix from cache memory.

## The function makeCacheMatrix creates a special list which is containing a function to:
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inversed matrix
## 4. get the value of the inversed matrix

makeCacheMatrix <- function(x = matrix()) {
  
## setting the value of a vector:
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  
## getting the value of a vector:
  get <- function() x

## setting the value of the inversed matrix:
  setinverse <- function(inverse) i <<- inverse

## getting the value of the inversed matrix:
  getinverse <- function() i

## creating a list:
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## The function cacheSolve checks if the inversed matrix has already been calculated.
## If so, it gets the inversed matrix from the cache and omits the calculation.
## Otherwise it calculates the inversed matrix and sets this matrix in cache by function setinverse.

cacheSolve <- function(x, ...) {

## Checking if the inverse matrix has already been calculated. If so, returning this matrix.
 
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  
## If the inverse matrix hasn't been calculated, it is being done now with the function solve. 
## The inversed matrix is set in a cache memory with the function setinverse.

  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}

