# cachematrix.R

# makeCacheMatrix(x = matrix())
# This function creates a special 'matrix' object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL				# The inverse
	set <- function(y) {	# Set function
		x <<- y
		inv <<- NULL
	}
	get <- function() x		# Get the matrix

	setinv <- function(inverse) inv <<- inverse		# Set the inverse
	getinv <- function() inv						# Get the inverse
	list(set = set, get = get, setinv = setinv, getinv = getinv)
}


# cacheSolve(x, ...)
# This function computes the inverse of the special 'matrix' returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
	# Gets the inverse of x
	inv <- x$getinv()

	# If it isn't NULL, it must have been computed before, and return that
	if(!is.null(inv)) {
		message('Getting cached data')
		return(inv)
	}

	# If it is, get the matrix x
	data <- x$get()

	# Solve (invert) it
	inv <- solve(data, ...)

	# Set the solved (inverted) matrix in memory (cache)
	x$setinv(inv)

	# Return solved value
	inv
}
