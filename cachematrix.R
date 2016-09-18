## Put comments here that give an overall description of what your
## functions do

## A type "Matrix" object that saves it's inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv = NULL
  SetMatrix = function(y){
    x <<- y
    inv <<- NULL
  }
  GetMatrix = function()x
  SetInverse = function(inverse) inv <<- inverse
  GetInverse = function() inv
  list(SetMatrix = SetMatrix, GetMatrix = GetMatrix, SetInverse = SetInverse, GetInverse = GetInverse)

}


## Calculates the inverse of a matrix. If the matrix's inverse was already calculated
## it will return the same result otherwise if it is a different matrix, it will 
## calculate it's inverse.

cacheSolve <- function(x, ...) {
        inv = x$GetInverse()
        if(!is.null(inv)){
          message("Getting cached data")
          return(inv)
        }
        mat.data = x$GetMatrix()
        inv = solve(mat.data, ...)
        x$SetInverse(inv)
        return(inv)
}
