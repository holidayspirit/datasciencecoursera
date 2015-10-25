## Two functions below:
## makeCacheMatrix() creates a vector, which is a list of four functions: set, get, setInverse and getInverse
## cacheSolve() takes the vector created by makeCacheMatrix and returns its inverse.
## cacheSolve() will either calculate the inverse, or will fetch the cached inverse of the matrix if it has been calculated before


## Takes a matrix as its argument, and returns a list of 4 functions, to be used as the argument for cacheSolve()
makeCacheMatrix <- function(x = matrix()) {
        
        m <- NULL  ## Initialise m locally
        
        ## Create the set function
        set <- function(y) {
                x <<- y  ##x gets the argument to the set function in the main makeCacheMatrix function
                m <<- NULL  ##Initialise m in the main function
        }
        
        ## Create the get function - merely returns the original matrix
        get <- function() x
        
        ## Create the setInverse function - m gets its argument (which is going to be the inverse of the matrix)
        setInverse <- function(inverse) m <<- inverse
        
        ## Create the getInverse function - merely returns m, the inversed matrix
        getInverse <- function() m
        
        ## Return the list of 4 functions
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Takes the list of 4 functions created by makeCacheMatrix(), checks if the inverse is in the cache
## If so, returns the cached inverse; if not, calculates, stores and returns the inverse
cacheSolve <- function(x, ...) {
        
        ## Use the function getInverse (created in makeCacheMatrix()) to see if there is a cached inverse for x
        m <- x$getInverse()
        
        if(!is.null(m)){     ## If inverse exists in cache...
                message("getting cached data") 
                return(m)  ## ...then return it, ...
        }
        
        ## ...else calculate the inverse
        
        data <- x$get()   ## Get the matrix to be inversed
        m <- solve(data, ...)   ## Invert it
        x$setInverse(m)  ## Save the inverse to the cache
        m  ##Return the inverse
}
