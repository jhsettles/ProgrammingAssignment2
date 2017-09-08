## Put comments here that give an overall description of what your
## functions do

# makeCacheMatrix function creates a matrix and caches it.
# cacheSolve function uses makeCacheMatrix to solve inverse of the 
# matrix. If not cached, it calculates the inverse.

## Write a short comment describing this function
# makeCacheMatrix caches the matrix, it has a function to set and get
# the value of the matrix.

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
	set <- function(y){
		x <<- y
		inverse <<- NULL
        }
        get <- function() x
	setinverse <- function(inv) inverse <<- inv
	getinverse <- function() inverse
	list(set = set,
		get = get,
		setinverse = setinverse
		getinverse = getinverse)     
        }


## Write a short comment describing this function
# cacheSolve uses the makeCacheMatrixto calculate inverse. If result
# is cached  then it returns cached data.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$getinverse()
	if(!is.null(inverse)){
		message("Getting cached data.")
		return(inverse)

        }
        data <- x$get()
	inverse <- solve(data)
	x$setinverse(inverse)
	inverse

        }
