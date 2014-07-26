## This R code is used to calculate and cache the inverse of a matrix. 

## Generate a matrix consist of  1 field 'inv' and  4 methods(get and set the value)
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL       
        set <- function(y) {
                x   <<- y
                inv <<- NULL
        }       
        get <- function() x        
        setinverse <- function(inverse) inv <<- inverse      
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## Get a matrix(an object of 'makeCacheMatrix') and check the whther the inverse has
## been calculated.
cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if (!is.null(inv)) {
                message("getting cached data!")
                return(inv)
        }
        matrix <- x$get()
        inv <- solve(matrix)
        x$setinverse(inv)
        inv
}
