## Matrix inversion is usually a costly computation and their may be 
## some benefit to caching the inverse of a matrix rather than compute
## it repeatedly.  
## The folowwing two functions shows how the concept of using cached
## values for heavy computation, in this particular case Matrix Inversion

## makeCacheMatrix provides the means to set/get a  matrix object and its inverse ## calculates the inverse of the matrix.
## In conjunction with the function cacheSolve it provides the mechanism to reuse
## the cached value of matrix and return it, without recalculating.



makeCacheMatrix <- function(mx = matrix()) {
 	#intialize 
	mx_inv <- NULL
	# set/get matrix functions
	set <- function(y) {
	mx <<- y
	mx_inv <<- NULL
	}
	get <- function() mx
	
	# set/get matrix inverse functions
	setmxinverse<- function(mx_inverse) mx_inv <<-mx_inverse
	getmxinverse <- function() mx_inv
	list(set = set, get = get,
	setmxinverse = setmxinverse,
	getmxinverse = getmxinverse)


}


## The function cacheSolve provides the inverse of  a matrix either by
## using the cached value or when not available by creating the inverse
## and storing it for future use in conjunction with the makeCacheMatrix 
## function.

cacheSolve <- function(mx, ...) {
        ## Return a matrix that is the inverse of 'mx'
 	mx_inv <- mx$getmxinverse()
	if (!is.null(mx_inv)) {
	message("getting cached value of matrix inverse")
	return(mx_inv)
	} else {
		mx_inv <- solve(mx$get())
		mx$setmxinverse(mx_inv)
		mx_inv
		}
}



