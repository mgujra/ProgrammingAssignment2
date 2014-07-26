## The objective of the functions in this file is to store inverted matrix in a cache
## provided the matrix does not exists in the cache. If it exists, the function would 
## return cached matrix otherwise it will create a new matrix, invert it and cache it.


## makeCacheMatrix will accept matrix object as an argument and cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    
  matrix_inverse <- NULL
  
  # set function to assign matrix
  setmatrix <- function(matrix_org){
    x <<- matrix_org
    matrix_inverse <<- NULL
  }
  
  #get function to return the matrix
  getmatrix <- function() x
  
  #set the inverse
  setinverse <- function(matrix) matrix_inverse <<- matrix
  
  #get function
  getinverse <- function() matrix_inverse
  
  list(setmatrix = setmatrix, getmatrix = getmatrix,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


## cacheSolve function will compute the inverse of the matrix returned from makeCacheMatrix
## function. If inverse has already been calculated, then cacheSolve function would retrieve
## the inverse from the cache

cacheSolve <- function(x, ...) {
  
  #retrieve the inverse using inverse function
  matrix_inverse <- x$getinverse()
  
  #check if the inverse of the matrix exists in cache
  if(!is.null(matrix_inverse)) {
    message("getting cached quitdata")
    return(matrix_inverse)
  }
  
  matrix_org <- x$getmatrix()
  #inverse the matrix
  matrix_inverse <- solve(matrix_org)
  #set the matrix
  x$setinverse(matrix_inverse)
  #return the inverse of the matrix 'x'
  return(matrix_inverse)
        
}
