## These two functions, respectively, use a matrix argument to:

## 1) create a special object, (i.e., a list containing four functions) which 
## sets & gets the values of a matrix, and sets & gets the value of that matrix's inverse; and 

## 2) pull the inverse matrix from the cache if already computed in step 1, and if not,
## pull a matrix if already cached in step 1, compute its inverse matrix, and assign that inverse matrix to the cache.



## makeCacheMatrix takes a matrix as an argument and caches it and its inverse.

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) i <<- solve
        getinverse <- function() i
        list(set = set,get = get, setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve takes a matrix as an argument and pulls its already cached inverse OR calculates its inverse from an already cached matrix.

cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}
