## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## Creates a special matrix object that can cache its inverse
makeCacheMatrix <- function( m = matrix() ) {

	## Initialize 
    i <- NULL

    ## Method to set matrix
    set <- function( matrix ) {
            m <<- matrix
            i <<- NULL
    }

    ## Method to get matrix
    get <- function() {
    	## Return the matrix
    	m
    }

    ## Method to set  inverse of the matrix
    setInverse <- function(inverse) {
        i <<- inverse
    }

    ## Method to get inverse of the matrix
    getInverse <- function() {
        ## Return inverse 
        i
    }

    ## Return a list of the methods
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## Write a short comment describing this function

## Compute the inverse of the special matrix returned by "makeCacheMatrix" above.
## If the inverse has already been calculated 
## then the "cachesolve" should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {

    ## Return a matrix that is the inverse of 'x'
    m <- x$getInverse()

    ## return the inverse if its already set
    if( !is.null(m) ) {
            message("getting cached data")
            return(m)
    }

    ## Get the matrix from  object
    data <- x$get()

    ## Calculate inverse using matrix multiplication
    m <- solve(data) %*% data

    ## Set the inverse to the object
    x$setInverse(m)

    ## Return the matrix
    m
}
