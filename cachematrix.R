## 
## 

## This creates a Matrix
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        
        ## This will create 2 cached vectors and assign a 
        ## value to each object in an environment that is different from 
        ## the current environment
        x <<- y
        m <<- NULL
    }
    
    get <- function() x
    
    setsolve <- function(solve) m <<- solve
    getsolve <- function() m
    
    ## Return a list where ("name of column in the inverse matrix" =
    ## "name of the function to be returned")
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
    
}

## This will check if "solve" has been calculated. If so, it returns the
## previous value and skips the computations
cacheSolve <- function(x, ...) {
    
    ## Return a matrix that is the inverse of 'x'
    m <- x$getsolve()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setsolve(m)
    m
    
}
