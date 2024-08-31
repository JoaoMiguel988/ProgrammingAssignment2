## Assigment 2:  Compute a special matrix that is able to cache is computed inverse matrix
## Author: João Miguel

## Creates a special Matrix that is able to cache its inverse
## set, get: functions to set or get the global matrix data
## setinv, getinv:: functions to set or get cached inverse matrix
## haschanged: function that returns TRUE in case the Matrix was set (changed) after its inverse was cached

makeCacheMatrix <- function(x = matrix()) {
        changed <- TRUE
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
                changed <<- TRUE
        }
        
        get <- function() x
        
        haschanged <- function() changed
        
        setinv <- function(inv_matrix) {
                inv <<- inv
                changed <<- FALSE
        }
        
        getinv <- function() inv
        
        list(set = set, get = get,
             haschanged = haschanged,
             setinv = setinv,
             getinv = getinv)
}


## Receives a special matrix to compute its inverse matrix in case 
## its data was changed and the new ivnerse wasn't cached yet

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        
        if (is.null(inv) || x$haschanged()) {
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
