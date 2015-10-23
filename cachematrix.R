## Inversing a matrix is usually a costly computation so it's better to 
## cache the result. We would do that with two function: the 1st one 
## is where we store the matrix we want to inverse and its inversion(if any)
## The 2nd one is the computing function: it would 1st check if the inversion is 
## cached yet, if so, it would call that from the cache, if not, it would 
## compute the inversion.
## This function is where we can set can store the matrix and its inversion
## This is essentially a list of 4 sub-functions: set, get, setinverse, getinverse

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

## This function will 1st check to see if the inversion is cached yet
## If so, it will get the inversion from the cache (in the 1st function)
## If not, it will compute the inversion
## The input of this function should be the variable which you assign the 1st 
## function to

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data.")
                return(inv)
        }
        inv <- solve(x$get())
        x$setinverse(inv)
        inv
}

## We can create a function which is a list of other sub-functions and
## then just call the individual sub-functions where it is neccessary