## Put comments here that give an overall description of what your
## functions do

## Creates a matrix to be cache its inverse

makeCacheMatrix <- function(x = matrix()) {
	i <- NULL

	set <- function(matrix) {
		x <<- matrix
		i <<- NULL
	}

	get <- function() x

	setInverse <- function(inverse) i <<- inverse

	getInverse <- function() i
	
	list(set = set, get = get, 
	     setInverse = setInverse, 
	     getInverse = getInverse)
}


## Calculates inverse of matrix from makeCacheMatrix
## if inverse is already calculated, method retrieves inverse
## from cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	m <- x$getInverse()
	if(!is.null(m)) {
		message("getting cached data")
		return(m)
	}
	data <- x$get()
	m <- solve(data)
	x$setInverse(m)
	m
}
