## The following script contains two functions that together can be used to cache the inverse of a matrix.
## By caching the inverse of the matrix, the inverse should not be computed repeatedly if the matrix is the same.



## This function creates a matrix of which the inverse can be cached.


makeCashMatrix <- function(x=matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) inv <<- solve
        getsolve <- function() inv
        list(set=set, get=get, setsolve=setsolve, getsolve=getsolve)
}


## This functions computes the inverse of a matrix, unless the inverse was already computed before 
##and cached by the 'makeCashMatrix' function.

cacheSolve <- function(x, ...) {
        inv <- x$getsolve()
        if(!is.null(inv)){
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setsolve(inv)
        inv
}
