## This program creates a special matrix, calculates the inversion of that matrix, then 
## saves the calculation in cache

## This function creates a special matrix object and caches its inverse

makeCacheMatrix <- function(x = matrix()) {
      invobs<-NULL
      set<-function(y) {
        x<<- y
        invobs<<- NULL
      }
      get<-function() x
      setinverse <- function(inverse) invobs<<-inverse
      getinverse <- function() invobs
      list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## This function computes the inverse of the matrix created above, and retreives
## it from cache if it has already been computed.

cacheSolve <- function(x, ...) {
        invobs<-x$getinverse()
        if(!is.null(invobs)) {
          message("getting cached data")
          return(invobs)
        }
        data<-x$get()
        invobs<-solve(data, ...)
        x$setinverse(invobs)
        invobs
}
