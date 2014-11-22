## A pair of functions that cache the inverse of a matrix when it is
## absent and create the inverse of a matrix when it is present


## Create a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
	i <<- NULL

	set<- function(y){
		x <<- y
		i <<- NULL
	}

	get <- function() x
	setInverse <- function(inverse) i <<- inverse
	getInverse <- function() i
	list(get = get, set=set, getInverse = getInverse, 
	setInverse = setInverse)
}

## Return a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
	i <- x$getInverse()

      if (!is.null(i)) {
		message("getting cached inverse")
            return(i)
      }

      y <- x$get()
      i <- solve(y)
      x$setInverse(i)
      i
}
