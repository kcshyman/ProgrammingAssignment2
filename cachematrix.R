## Matrix inversion is usually a costly computation and there may be 
## some benefit to caching the inverse of a matrix rather than compute it repeatedly
## Function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {      
	inv = NULL
	set = function(y) {
		#  <<- operator which can be used to assign a value to an object in an environment that is different from the current environment
		x <<- y
		inv <<- NULL
	}
	get = function() x
	setinv = function(inverse) inv <<- inverse 
	getinv = function() inv
	list(set=set, get=get, setinv=setinv, getinv=getinv)
}

## Computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache
cacheSolve <- function(x, ...) {    
	inv = x$getinv()

	if (!is.null(inv)){
		# inverse already calculated, get value from cache
		message("Using cached data")
	} else {
		# no cache, calc inverse and cache result
		mat.data = x$get()
		inv = solve(mat.data, ...)

		# set value of cache
		x$setinv(inv)
	}
	
	return(inv)		
}