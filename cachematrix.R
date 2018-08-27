## The functions are generally the same as the ones in the example.


## This function makes tha cashed matrix.

makeCacheMatrix <- function(x = matrix()){
      inv <- NULL
      set <- function(y){
            x <<- y
            inv <<- NULL
      }
      get <- function() x
      setinverse <- function(inverse) inv <<- inverse
      getinverse <- function() inv
      list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## This function either solves for the inverse or returns a solved inverse.

cacheSolve <- function(x, ...){
      inv <- x$getinverse()
      if(!is.null(inv)){
            message("returning cached data")
            return(inv)
      }
      data <- x$get()
      inv <- solve(data)
      x$setinverse(inv)
      message("calculating ...")
      inv
}
