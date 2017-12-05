# K. Devlin
# Coursera R Programming
# Week 3, Programming Assignment 2
# Peer Reviewed Assignment, Lexical Scoping

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmatrix <- function(solve) m <<- solve
  getmatrix <- function() m
  list(set = set, get = get,
       setmatrix = setmatrix,
       getmatrix = getmatrix)
}

cacheSolve <- function(x, ...) {
  m <- x$getmatrix()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setmatrix(m)
  m
}



## Test of above code
B <- matrix(c(1,2,3,4),2,2)
B1 <- makeCacheMatrix(B)
cacheSolve(B1) # computed
cacheSolve(B1) # returned from cache
