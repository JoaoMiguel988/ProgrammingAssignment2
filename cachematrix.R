## Assigment 2:  Compute a special matrix that is able to cache its computed inverse matrix
## Author: João Miguel

## Creates a special Matrix that is able to cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        
        # Function to set the global matrix data and clean cached inverse matrix
        set <- function(y) {
                x <<- y
                # Clean cached inverse matrix, as data changed
                inv <<- NULL
        }
        
        # Function to get the global matrix data
        get <- function() x
        
        # Function to set cached inverse matrix
        setinv <- function(inv_matrix) {
                inv <<- inv_matrix
        }
        
        # Function to get cached inverse matrix
        getinv <- function() inv
        
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## Receives a special matrix to compute its inverse matrix in case 
## its data was changed and the new inverse was not cached yet

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        
        if (is.null(inv)) {
                # Inverse matrix needs to be computed
                data <- x$get()
                inv <- solve(data)
                # Set inverse matrix in cache
                x$setinv(inv)
                return(inv)
        }
        
        # Already has a chached inverse matrix
        inv
}
