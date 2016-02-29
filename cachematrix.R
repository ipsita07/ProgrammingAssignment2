# Assignment: Caching the Inverse of a Matrix

# Matrix inversion is usually a costly computation. 
# The assignment is to write a pair of functions that cache the inverse of a matrix.

# Write the following functions:

# makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
# cacheSolve: This function computes the inverse. 

# If the inverse has already been calculated, 
# then the cachesolve should retrieve the inverse from the cache.

# 1. set the value of the matrix
# 2. get the value of the matrix

makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  get<-function() x
  setmatrix<-function(solve) m<<- solve
  getmatrix<-function() m
  list(set=set, get=get,
       setmatrix=setmatrix,
       getmatrix=getmatrix)
}


# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix

cacheSolve <- function(x, ...) {
  m<-x$getmatrix()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  matrix<-x$get()
  m<-solve(matrix, ...)
  x$setmatrix(m)
  m
}
