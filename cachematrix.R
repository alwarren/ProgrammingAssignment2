## This function creates a matrix that can cache its inverse.
## Previous Hash - 20b87a8000d8af643eb946a5d4cc343572a16c58

makeCacheMatrix <- function(original = matrix(c(-1, -2, 1, 1), 2,2)) {
  
  # initialize the inverse matrix
  inverse <- NULL
  
  # function that sets a new original matrix 
  # and reinitilizes the inverse
  set <- function(newMatrix) {
    original <<- newMatrix
    inverse <<- NULL
  }
  
  # function that returns the original matrix
  get <- function() original
  
  # function that sets a new inverse matrix
  setinverse <- function(newInverse) inverse <<- newInverse
  
  # function that gets the inverse matrix
  getinverse <- function() inverse
  
  ## return the matrix as a list
  list(set = set, get = get, setinverse = setinverse, 
       getinverse = getinverse)
  
}

## This function computes the inverse of the matrix created in
## makeCacheMatrix(). If the inverse has already been calculated,
## it is retrieved from the input matrix object.

cacheSolve <- function(matrixIn, ...) {
  
  # set the inverse using the input object's inverse
  inverse <- matrixIn$getinverse()
  
  # return the cached inverse if it's not NULL
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  
  # calculate the inverse
  inverse <- solve(matrixIn$get())
  
  # set the input object's inverse
  matrixIn$setinverse(inverse)
  
  # return the calculated inverse
  inverse
}

## Tests

## > m <- matrix(c(-1, -2, 1, 1), 2,2)
## > x <- makeCacheMatrix(m)
## > x$get()
##      [,1] [,2]
## [1,]   -1    1
## [2,]   -2    1

## > inv <- cacheSolve(x)
## > inv
##      [,1] [,2]
## [1,]    1   -1
## [2,]    2   -1

## > inv <- cacheSolve(x)
## getting cached data
## > inv
##      [,1] [,2]
## [1,]    1   -1
## [2,]    2   -1
