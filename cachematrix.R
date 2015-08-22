## Carly Stoughton, August 2015

## Matrix inversion is usually a costly computation and there may be some benefit to caching the
## inverse of a matrix rather than computing it repeatedly. A pair of functions are defined that 
## cache the inverse of a matrix. The following functions are defined and described below:
## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
## cacheSolve: This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated -or set- (and the matrix has  
## not changed), then cacheSolve retrieves the inverse from the cache. Assumes that the input is
## a matrix object, and that the matrix supplied as input is always invertible.

## Creates a matrix object and defines and returns a list of functions that
## can be performed on objects of this type related to matrix inversion

makeCacheMatrix <- function(x = matrix()) 
{
    ## reset inverse in this function's environmet
    inverse <- NULL
    
    ## set matrix object to input matrix
    set <- function(newMatrix)
    {
        ## set x to new input matrix
        x <<- newMatrix
        
        ## reset inverse since matrix has changed
        inverse <<- NULL
    }
    
    ## return current matrix 'x'
    get <- function() x
    
    ## manually set value of inverse to input (does NOT check if input == inverse('x'))
    setinverse <- function(newInverse) inverse <<- newInverse
    
    ## return current inverse of matrix 'x'
    getinverse <- function() inverse
    
    ## return a list of functions that can be performed on an 
    ## instance of matrix x object(s) defined in this function,
    ## kinda like object-oriented programming! :)
    
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cachsSolve accepts a matrix 'x', and sets inverse to value of getinverse().
## If inverse exists, cachesolve returns the cached inverse and a message 
## noting cached data returned (versus a new calculation, saving time).
## If inverse does not exist, cachesolve gets the matrix from input 'x',
## inverts the matrix, sets (caches) the inverse of the matrix 'x' object, and
## returns the calculated (and cached for subsequent use) inverse of matrix 'x'.

cacheSolve <- function(x, ...) 
{
    ## set inverse to inverse of input matrix object 'x'   
    inverse <- x$getinverse()
    
    ## if inverse of 'x' has already been calculated, return
    ## message noting value is cached, and return inverse.
    
    if( !is.null(inverse) )
    {
        message("getting cached data")
        return(inverse)
    }
    
    ## ELSE, inverse has NOT yet been calculated on input matrix 'x'
    ## set data to value of input matrix 'x' from makeCacheMatrix function
    data <- x$get()
    
    ## caculate and set (cache) inverse of input matrix 'x' object
    inverse <- solve(data, ...)
    x$setinverse(inverse)
    
    ## return calculated inverse of input matrix 'x' object
    inverse
}