## Both functions aim to cache the inverse of a matrix

## The function creates special matrix and outputs its inverse

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y){
		x <<- y
		inv <<- NULL
	}
	get <- function() x
	setinverse <- function(inverse) inv <<- inverse
	getinverse <- function() inv
	list(set = set, get = get, setinverse=setinverse, getinverse=getinverse)
}


## The following function checks whether the inverse of the function is 
## saved somewhere in the memory. If not, it calculates it.
## the function returns a matrix that is the inverse of 'x' 

cacheSolve <- function(x, ...) {
	inv <- x$getinverse()
	if(!is.null(inv)){
		message("getting data from cache")
		return(inv)
	}
	data <- x$get()
	inv <- solve(data,...)
	x$setinverse(inv)

	inv
}
