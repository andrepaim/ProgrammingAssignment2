# These functions implement a cached strategy to compute a matrix inverse:
# * function 'makeCacheMatrix' creates a special matrix object that can 
#   store its inverse
# * function 'cacheSolve' receives a special matrix object and returns its
#   inverse. If the matrix inverse has already been computed, it returns the
#   cached value

# Creates a special matrix (represented as a list) containing functions able to
# * set the value of the matrix
# * get the value of the matrix
# * set the cached value of the inverse matrix
# * get the cached value of the inverse matrix
makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y) {
		x <<- y
		inv <<- NULL
	}
	get <- function() x
	setinverse <- function(i) inv <<- i
	getinverse <- function() inv
	list(set = set,
	     get = get,
	     setinverse = setinverse,
	     getinverse = getinverse)
}

# Receives a sepcial matrix x (created by the function 'makeCacheMatrix') as a 
# argument and checks if its inverse has been already computed. If so, returns 
# the cached value of the inverse. Otherwise, computes the matrix inverse
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	inv <- x$getinverse()
	if (!is.null(inv)) {
		message("getting cached data")
		return(inverse)
	}
	data <- x$get()
	inv <- solve(data,...)
	x$setinverse(inv)
	inv
}
