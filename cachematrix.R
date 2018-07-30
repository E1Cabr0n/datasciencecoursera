## makeCacheMatrix creates a list of fuctions with functions for:
## setting the matrix: set
## getting the matrix: get
## setting the inverse matrix with help the solce() function: setinverse
## getting the inverse matrix: getinverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get, setinverse = setinverse, 
       getinverse = getinverse)

}


## cacheSolve function calculates the inverse matrix with help
## of the makeCacheMatrix functions. First of all it chechs if
## the inverse matrix has been calculated already. In that case
## it takes the inverse matrix from cache and returns it. If no,
## cacheSolcve function calculates the inverse matrix and puts
## it in the cache

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
  
}

## List commands for the code testing
#source("ProgrammingAssignment2-master/cacheMatrix.R")
#mtrx <- makeCacheMatrix(matrix(rnorm(36), nrow = 6, ncol = 6))
#mtrx$get()
#get matrix
#mtrx$getinverse()
##Output: NULL
#cacheSolve(mtrx)
##Ouptput: matrix
#mtrx$getinverse()
#cacheSolve(mtrx)
##Output "getting cached data & matrix