## Coursera R Programming - Programming Assignment 2

## First create variable x
x <- rbind(c(1, -1/4), c(-1/4, 1))

## makeCacheMatrix function will create a vector that caches the calc of its inverse
## Using a list to creat an object as a list with 2 functions - `set` and `get`
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        ## 1st set the object, assign/store the new argument value and reset the
        ## inverse calculation
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }

        get <- function() x

        ## Steps to write get and set functions for the inverse
        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() inv
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## Next use this function to calc the inverse of the received square matrix, caching
## the result in the CacheMatrix object.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        if(!is.null(inv)) {
                message("Patience please I am finding cached data")
                return(inv)
        }
        data <- x$get()     ## Getting data
        inv <- solve(data)  ## inverse calc
        x$setInverse(inv)
        inv
}

## Check your results by using x <- rbind(c(1, -1/4), c(-1/4, 1))

## Note: Using the "IF" above to check to see if cache already exists
## If cache exists then return the cached result