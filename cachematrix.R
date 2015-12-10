## The following functions are used to create a matrix object that 
## can cache it's inverse matrix, once it was calculated.


## Create a matrix object that can cache it's inverse.
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        
        ## Set the matrix data.
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        
        ## Get the matrix data.
        get <- function() { x }
        
        ## Store the inverse matrix.
        setinv <- function(inv) {
                m <<- inv
        }
        
        ## Get the inverse matrix.
        getinv <- function() { m }
        
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## Returns a matrix that is the inverse of 'x'. 
## If the inverse was already calculated once, than the cached
## inverse matrix is returned.
cacheSolve <- function(x, ...) {
        
        m <- x$getinv()
        
        ## If the inverse matrix was never calculated until now.
        if(is.null(m)) {
                ## Get the data
                data <- x$get()
                ## Find the inverse matrix
                m <- solve(data, ...)
                ## Cache the inverse matrix
                x$setinv(m)
        } else {
                message('returning cached data')
        }
        
        return (m)
}

test <- function() {
        ## This matrix should almost always be invertible
        myMatrix <- matrix(rnorm(25), 5, 5)
        
        ## Store our inverted matrix
        myInv <- solve(myMatrix)
        
        ## Create our cache matrix
        cMat <- makeCacheMatrix(myMatrix)
        
        ## Get our inverse matrix using the cache matrix.
        cInv <- cacheSolve(cMat)
        
        ## Get our inverse matrix using the cache matrix, this time
        ## a message should be printed stating that the return value 
        ## is the cached result.
        cInv <- cacheSolve(cMat)
        
        print('regular inverse matrix')
        print(myInv)
        print('cached inverse matrix')
        print(cInv)
}