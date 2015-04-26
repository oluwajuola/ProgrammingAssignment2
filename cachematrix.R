## The following two functions creates a Matrix object that can cache its own inverse
## and computes the inverse of a Matrix returned by the first function.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
		inverse <- NULL
		
		## Function to set value of the matrix
		set <- function(y) {
			x <<- y
			m <<- NULL
		}
		get <- function() x
		setinverse <- function(inv) inverse <<- inv
		getinverse <- function() inverse
		#returns inverse list
		return(list(set = set, get = get, setinverse = setinverse, getinverse = getinverse))
		
}


## This function computes the inverse of the special "matrix" returned by the first function (makeCacheMatrix) above
## If the inverse has already been calculated (and the matrix has not changed), then this function retrieves the 
## inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		inverse <- x$getinverse()
		if(!is.null(inverse)){
			message("getting cached data")
			return(inverse)
		}
		data <- x$get()
		inverse <- solve(data, ...)
		x$setinverse(inverse)
		return(inverse)		
}
