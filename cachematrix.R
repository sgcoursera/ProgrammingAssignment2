## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than compute it 
## repeatedly 

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        inverted <- NULL   ## we will use this variable to store cached value
	# this member function sets source matrix data
        set <- function(y) {
                x <<- y
                inverted <<- NULL
        }
	# this member function is used to get source matrix data
        get <- function() x
	# this member function is used to remember inverted matrix in inverted variable
        set_inverted <- function(inv) inverted <<- inv
	# this member function is used to get stored value for inverted matrix
        get_inverted <- function() inverted
        list(set = set, get = get,
             set_inverted = set_inverted,
             get_inverted = get_inverted)
}


## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. 

cacheSolve <- function(x, ...) {
	# get stored value first
        inverted <- x$get_inverted()
        if(!is.null(inverted)) { # if stored value is not null then just return it 
                message("getting cached data")
                return(inverted)
        }
	# if stored value is null, then calc it and store before exit to use it in next calls
        data <- x$get()
        inverted <- solve(data, ...)
        x$set_inverted(inverted)
        inverted
}
