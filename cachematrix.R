
#This R function is able to cache potentially time-consuming computation for calculating
# inverse of a Matrix.

# makeCacheMatrix  
# This function creates a special "matrix" object that can cache 
# its inverse.
# It Sets and Gets the Martrix Object
# And Gets and sets the inverse of the Matrix Object
makeCacheMatrix <- function(x = matrix()) {
    
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


#This function computes the inverse of the special "matrix" 
#returned by makeCacheMatrix above. If the inverse has already 
#been calculated (and the matrix has not changed), 
#then the cachesolve should retrieve the inverse from the cache
#(that was set by setinverse).
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setinverse(inv)
    inv
}
## Example Test Data

##> x = rbind(c(4, 7), c(2, 6))
##> m = makeCacheMatrix(x)
##> m$get()
##[,1]  [,2]
##[1,]     4   7
##[2,]     2   6

## No cache in the first run
##> cacheSolve(m)
##[,1]      [,2]
##[1,]       0.6         -0.7
##[2,]       -0.2        0.4

## Retrieving from the cache in the second run
##> cacheSolve(m)
##getting cached data.
##[,1]      [,2]
##[1,]        0.6        -0.7
##[2,]        -0.2       0.4