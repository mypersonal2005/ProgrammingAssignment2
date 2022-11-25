## Caching the inverse of the matrix using a pair of fucntions
## Example
##m = makeCacheMatrix(matrix(rnorm(16), 4, 4))
##cacheSolve(m) --> Computes, first time
##cacheSolve(m) --> Gets from cache

## Function to create a special matrix object that can cache its inverse

makeCacheMatrix <- function(m = matrix()) {
## Initialize the inverse property
    i <- NULL
## Method to set the matrix
    set <- function(matrix) {
            m <<- matrix
            i <<- NULL
    }
## Method the get the matrix
    get <- function() {
    	m
    }
## Method to set the inverse of the matrix
    setInverse <- function(inverse) {
      i <<- inverse
    }
## Method to get the inverse of the matrix
    getInverse <- function() {
      i
    }
## Return a list of the methods
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}

## Function to compute the inverse of the special matrix returned by
## makeCacheMatrix above. 
## Note: If the inverse has already been calculated and the matrix has
## not changed, then the function cacheSolve should retrieve the inverse
## from the cache instead of computing again.

cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
    m <- x$getInverse()
## Return the inverse if it exists already
    if( !is.null(m) ) {
            message("getting cached data")
            return(m)
    }
## Get the matrix from the object
    data <- x$get()
## Compute the inverse using solve function
    m <- solve(data) %*% data
## Set the inverse to the object
    x$setInverse(m)
## Return the matrix
    m
}