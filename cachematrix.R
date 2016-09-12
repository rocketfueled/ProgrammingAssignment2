## Programming Assignment2: Lexical Scoping
## Randall Tan

## makeCacheMatrix stores and keeps a Matrix and its inverse with the following:
## SetMatrix      assigns the matrix data
## GetMatrix      retrives the matrix data
## SetInverse     assigns the inverse matrix data
## GetInverse     retrieves the inverse matrix data

makeCacheMatrix <- function(x = matrix()) {
      MakeMatrix <- NULL
      SetMatrix <- function(y) {
            x <<- y
            MakeMatrix <<- NULL
      }
      GetMatrix <- function() x
      SetInverse <- function(inverse) MakeMatrix <<- inverse
      GetInverse <- function() MakeMatrix
      list(
            SetMatrix = SetMatrix,
            GetMatrix = GetMatrix,
            SetInverse = SetInverse,
            GetInverse = GetInverse
            )
}

## cacheSolve calculates the inverse of the the original matrix created with
## makeCacheMatrix.
## If the inverse of the matrix has been created, it will return cached data
## from MakeMatrix.

cacheSolve <- function(x, ...) {
      MakeMatrix <- x$GetInverse()
      if (!is.null(MakeMatrix)) {
            message("Retrieving cached data")
            return(MakeMatrix)
      }
      matdata <- x$GetMatrix()
      MakeMatrix <- solve(matdata, ...)
      x$SetInverse(MakeMatrix)
      MakeMatrix
}
