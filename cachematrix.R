## Courser ProgrammingAssignment2 
## Make and Caching the inverse of a Matrix

## This function creates a special matrix object that cache it's inverse

makeCacheMatrix <- function(x = matrix()) {

	inv <- NULL
	set <- function ( y) {
		x <<- y
		inv <<- NULL
	}

	get <- function () x

	setInverse <- function ( inverse ) inv <<- inverse

	getInverse <- function () inv

	list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
	
}


## This function computes the inverse of the matrix returned by makeCacheMatrix.
## If the inverse has already been calculated and the matrix has not changed then retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {

	inv <- x$getInverse()

	if (!is.null(inv)) {
		message ("getting cached inverse")
		return(inv)
	}
	
	data <- x$get()
	inv <- solve(data, ...)
	x$setInverse(inv)
	inv

}
