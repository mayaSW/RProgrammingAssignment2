## This function creates a special "matrix" object
## that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
			I <- NULL
			set <- function(y)
				x <<- y
				I <<- NULL
}
			get <- function() x
			setInverse <- function(solveMatrix) I <<- solveMatrix
			getInverse <- function() I
			list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        I <- x$getInverse()
        if(!is.null(I)){
        		message("getting cached data")
        		return(I)
        }
		data <- x$get()
		I <- solve(data)
		x$setInverse(I)
		I
}