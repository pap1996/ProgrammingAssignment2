## The functions below is used to cache the inverse of a matrix

## The first function is used to make a list of functions
## including the one to set the value of a matrix, get the value of a matrix, 
## set the inversed matrix and get the inversed matrix

makeCacheMatrix <- function(x = matrix()) {
  inverseMatrix <- NULL
  set <- function(y){
    x <<- y
    inverseMatrix <<- NULL
  }
  get <- function() x
  setinverseMatrix <- function(matrix) inverseMatrix <<- matrix
  getinverseMatrix <- function() inverseMatrix
  list(set = set, get = get, setinverseMatrix = setinverseMatrix, getinverseMatrix=getinverseMatrix)
}


## The second function is used to retrieve the inverse of the matrix created from the first function

cacheSolve <- function(x, ...) {
  inverseMatrix <- x$getinverseMatrix()
  if (!is.null(inverseMatrix)) {
      message("getting cached data")
      return(inverseMatrix)
  }
  data <- x$get()
  inverseMatrix <- solve(data)
  x$setinverseMatrix(inverseMatrix)
  inverseMatrix
}

# Test them!
m <- makeCacheMatrix(matrix(1:4,2,2))
m$set(matrix(c(1,2,4,6,8,2,9,3,3),3,3))
cacheSolve(m)
m$getinverseMatrix()
