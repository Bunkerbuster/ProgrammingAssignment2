## Put comments here that give an overall description of what your
## functions do

## The function "makeCacheMatrix" creates a special "matrix" object, that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  
  # Set Variable "SpecialMatrix" to NULL
  SpecialMatrix <- NULL
  
  # Setter 
  # Set the Value of the matrix
  set <- function(y)
  {
    x <<- y
    SpecialMatrix <<- NULL
  }
  
  # Getter
  # Get the value of the matrix
  get <- function() x
  
  # Set the inverse of the SpecialMatrix
  # using the function solve to computing the inverse of a square matrix
  setInverse <- function(i) SpecialMatrix <<- solve(x)
  
  # Get the inverse of the SpecialMatrix
  getInverse <- function() SpecialMatrix
  
  # The list containing the function
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

cacheSolve <- function(x, ...) {
  
  ## get the inverse of the specialmatrix
  SpecialMatrix <- x$getInverse()
  
  # if the SpecialMatrix is not null then return the specialMatrix (because it has already been computed and cached)
  if(!is.null(SpecialMatrix))
  {
    return(SpecialMatrix)
  }
  
  # because the specialMatrix is not yet computed we now can use the function Solve to compute the inverse of a square matrix
  SpecialMatrix <- solve(x$get())
  
  # The computed result we cache using the setinverse
  x$setInverse(SpecialMatrix)
  
  # return the SpecialMatrix
  SpecialMatrix
}