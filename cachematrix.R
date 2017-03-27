# Since calculating the inverse of a matrix is a time consuming process, the following functions
# will first determine the inverse of a given matrix has been recently calculated and stored in the
# cache, where it can be obtained in an efficient manner.  If the inverse of a matrix has not
# recently been saved to the cache, the function will calculate its inverse.


# Creates a special "matrix" object and stores its' inverse in the cache

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {							# set value of matrix
                x <<- y
                m <<- NULL
        }
        get <- function() x							# get value of matrix
        setinvmatrix <- function(invmatrix) m <<- invmatrix		# set value of inverted matrix
        getinvmatrix <- function() m					# get value of inverted matrix
        list(set = set, get = get,
             setinvmatrix = setinvmatrix,
             getinvmatrix = getinvmatrix)
}


# Computes the inverse of the special "matrix" returned by makeCacheMatrix
# above.  If the inverse has already been calcuated (and the matrix has 
# not changed), then this function will retrieve the inverse matrix from
# the cache as opposed to recalcuting the inverse matrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinvmatrix()
        if(!is.null(m)) {		# if matrix is stored in the cache, then returns stored value
                message("getting cached inverse matrix")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)		# 'solve()' calculates the inverse matrix
        x$setinvmatrix(m)
        m
}
