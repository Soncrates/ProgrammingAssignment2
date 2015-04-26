## Using the <<- operator which can be used to assign a value to an object 
## in an environment that is different from the current environment. 
## Below are two functions that are used to create a special object 
## that stores a matrix and caches its inverse.

## makeCacheMatrix is a factory function that takes the matrix supplied
## as an argument and returns the matrix with a wrapper of extra functionality
##

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setsolve <- function(inv) m <<- inv
        getsolve <- function() m
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## cacheSolve calculates inverse of the 'wrapped matrices' produced by the
## the previous function.  If the matrix cached value of  inverse evaluates
## to null, the inverse is calculated, stored as a cached instance and returned
## otherwise  the cached value is returned

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data)
        x$setsolve(m)
        m
}

testCache <- function() {
   temp=rbind(c(1, -1/4), c(-1/4, 1))  
   wrappedTemp = makeCacheMatrix(temp)
   inverseTemp = cacheSolve(wrappedTemp)
   ret <- temp %*% inverseTemp
   print(ret)
   inverseTemp = cacheSolve(wrappedTemp)
   ret <- temp %*% inverseTemp
   print(ret)

}
