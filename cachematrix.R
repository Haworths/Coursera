## This pair of functions are designed to 1 ) calculate and store the
## inverse of a numeric matrix and 2) attempt retrieve the stored value
## before calculating the inverse again


## This function takes a matrix as an argument (or cretes an empty matrix)
## The cached solution is set to NULL. There are subfunctions, set and get
## which set and get the value of the matrix, and setinverse and getinverse,
## which set and get the value of the solved matrix. At the end of this function
## the set and get values should be identical (i.e. overwrites the cache and interal
## objects when a new matrix is passed)

makeCacheMatrix <- function(x = matrix()) {	
	solvedx <- NULL					
	set <- function(y) {				
		x <<- y
		solvedx <<- NULL
	}
	get <- function() x
	setinverse <- function(solve) solvedx <<- solve
	getinverse <- function() solvedx
	list(set = set, get = get,
		setinverse = setinverse,
		getinverse = getinverse)
}

## This function takes a matrix as an argument, and any additional arguments can
## be passed with the ... function. If the cached value is not null (i.e. there
## is data in the cache) then this value is returned as the solution. If there is
## no data in the cache, then data is retrieved using the get function, and solved
## the final line returns the solved matrix

cacheSolve <- function(x, ..) {
	solvedx <- x$getmean()
	if(!is.null(solvedx)) {
		message("getting cached data")
		return(solvedx)
	}
	data <- x$get()
	solvedx <- solve(data, ...)
	x$setinverse(solvedx)
	solvedx
}

