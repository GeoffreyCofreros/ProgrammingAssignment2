## The functions below are a pair of functions that cache 
## the inverse of a matrix

## makeCacheMatrix creates a special matrix tat can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
	
	inv <- NULL

	## function that would set the matrix
	set <- function(matrix){
		x <<- matrix
		inv <<- NULL
	}

	## function to get the matrix
	get <- function(){
		x
	}	
	
	## sets the inverse of matrix
	setInverse <- function(inverse){
		inv <<- inverse
	}

	## gets the inverse of matrix
	getInverse <- function(){
		inv
	}
	
	list(set = set, get=get,
		setInverse = setInverse,
		getInverse = getInverse)
}


## computes the inverse of the matrix made by "makeCacheMatrix"
## if inverse was already calculated, 
## "cacheSolve" would retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
	## Return a matrix that is the inverse of 'x'
	inv <- x$getInverse()

	## Returns the inverse if it was already calculated
	if(!is.null(inv)){
		message("getting cached data")
		return(inv)
	}

	data <- x$get()
	
	## calculate the inverse
	inv <- solve(data)

	## set the inverse to inv
	x$setInverse(inv)

	## return the inverse
	inv
}
