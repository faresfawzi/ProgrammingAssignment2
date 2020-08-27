## Two functions to display the cached matrix inverse instead of computing it.
## If the matrix inverse is not cached it can be computed by the 
## cacheSolve function.

##makeCacheMatrix function takes a matrix and caches its inverse.
##makeCacheMatrix function creates a list which contains the a function to
##(1)set the value of a matrix
##(2)get the value of the vector
##(3)set the value of the inverse
##(4)get the value of the inverse
makeCacheMatrix <- function(x = matrix()) {
        
        I <- NULL
        set <- function(y) {
                x <<- y
                I <<- NULL
        }
        get <- function() x
        setinverse <- function(inv) I <<- inv
        getinverse <- function() I
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
        
        
        
}


## The cacheSolve function checks if the inverse has already been computed and
##gets the inverted matrix from the cache.
##If the inverse was not previously computed the function will compute it.
##The function displays the matrix inverse.
 
cacheSolve <- function(x, ...) {
        
        I <- x$getinverse()
        if(!is.null(I)) {
                message("getting cached data")
                return(I)
        }
        matrix <- x$get()
        I <- solve(matrix, ...)
        x$setinverse(I)
        
        I
        
        
}