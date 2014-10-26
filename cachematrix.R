## makeCacheMatrix and cacheSolve are two functions that work in coordination
## to set a matrix and calculate its inverse. The user can see what are the 
## contents of the original matrix and its inverse. 

## The makeCacheMatrix function is a container for several functions to set or
## get the original matrix and to set or get its inverse; and returns a list
## with this information.

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        get <- function(){
                x
        } 
        setInverse <- function(inv) {
                inverse <<- inv
        }
        getInverse <- function() {
                inverse
        }
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}

## The cashSolve function calculates de inverse of the original matrix; checks 
## if the inverse has been calculated already, and if so it doesn't 
## calculate it again; it just presents the recorded information.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$getInverse()
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        data <- x$get()
        inverse <- solve(data)
        x$setInverse(inverse)
        inverse
}
