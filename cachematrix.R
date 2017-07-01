## R Programming assignment 2: Creating R functions that cache the inverse of
## a matrix.


## A function that creates a special "vector", or rather a list containing 
## functions that will set the matrix, get the matrix, set the inverse and get
## the inverse. 

makeCacheMatrix <- function(x = matrix()) {
        inv = NULL
        set = function(y) {
                x <<- y
                inv <<- NULL
        }
        get = function() x
        setinv = function(inverse) inv <<- inverse 
        getinv = function() inv
        list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## A function that creates the inverse matrix from the makeCacheMatrix data. 
## If cached data exists, this data will be used and a comment "getting cached 
## data" together with the cached inverse matrix will appear. If not, inverse  
## matrix will be calculated and printed.

cacheSolve <- function(x, ...) {
        inv = x$getinv()
        if (!is.null(inv)){
                message("getting cached data")
                return(inv)
        }
        matdata = x$get()
        inv = solve(matdata, ...)
        x$setinv(inv)
        return(inv)
}
