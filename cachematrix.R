## Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse 
## of a matrix rather than compute it repeatedly
## The following two functions are used to cache the inverse of a matrix.

## makeCacheMatrix creates a special "matrix" object that can cache its inverse. 
## A list of available function are
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of inverse of the matrix
## 4. get the value of inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	
	## set the value of the matrix
	set <- function(y)
	{
		x <<- y
        inv <<- NULL
	}
	
	## get the value of the matrix
	get <- function() x
	
	## set the value of inverse of the matrix
    setsolve <- function(solve) inv <<- solve
	
	## get the value of inverse of the matrix
    getsolve <- function() inv
    list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {

		## get inverse
        inv <- x$getsolve()
		
		## check if inverse is already calculated
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
		
		## Reach here if inverse has not been calculated.
		## Get the matrix and calculate its inverse
        data <- x$get()
        inv <- solve(data, ...)
        x$setsolve(inv)
        inv
}
