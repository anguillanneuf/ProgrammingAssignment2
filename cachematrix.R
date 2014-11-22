## A pair of functions that cache the inverse of a matrix when the inverse
## is present and create the inverse of a matrix when the inverse is absent


## makeCacheMatrix creates a special "matrix" object that can cache its inverse

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

## cacheSolve returns a matrix that is the inverse of 'x'

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
