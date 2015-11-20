## makeCachematrix and cachesolve are a pair of functions that cache the inverse of a matrix.

## makeCachematrix is a function that creates a matrix "object" that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setMatrix <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get, setMatrix = setMatrix, 
     getinverse = getinverse)
}


## cacheSolve is a function that computes the inverse of the matrix returned by makeCachematrix.
## If the inverse is already caclulated, 
## then the cacheSolve function will retrieve it from the cache.

cacheSolve <- function(x, ...) {
m <- x$getinverse()

if (!is.null(m)) {
  message("getting cached data")
  
  # display matrix in console
  return(m)
}

# create matrix since it does not exist
matrix <- x$get()
tryCatch( {
  m <- solve(matrix, ...)
},
error = function(e) {
  message("Some error:")
  message(e)
  return(NA)
},
warning = function(e) { message("Another Error:")  
  message(e)
  return(NA)
},
setMI = { x$setMatrix(m) } 
)

# display matrix in console
return (m)
} ## End of cachSolve
