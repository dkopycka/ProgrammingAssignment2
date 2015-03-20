## the makeCacheMatrix creates list of functions to
## set the values of the matrix, get the matrix,
## set the inverse matrix and get it

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    
    get <- function() x
    
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv
    
    list(set = set,
         get = get,
         setinv = setinv,
         getinv = getinv)
}

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    ## firstly it verifies whether the inverse has been already calculated
    ## if yes, the function gets it from the cache
    ## if not, the inverse is calculated, printed and sent to the cache
    
    inv <- x$getinv()
    
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    
    matrix <- x$get()
    inv <- solve(matrix, ...)
    x$setinv(inv)
    inv
}