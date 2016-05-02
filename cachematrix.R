## function:    makeCacheMatrix
##
##              cache the inverse of a matrix
## 
## usage:       makeCacheMatrix(x, ...) - x = matrix (which can be inverted )
##              mat <- matrix(c(1,2,3,4), nrow=2, ncol=2)
##              cacheMat <- makeCacheMatrix(mat)
## 
## changelog:
##              20160502 MF intial version
##
makeCacheMatrix <- function(x = matrix()) {
        
        m <- NULL
        
        #set the matrix
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        
        #get the matrix
        get <- function() x
        
        #set the inverse of the matrix
        setsolve <- function(solve) m <<- solve
        
        #get the inverse of the matrix
        getsolve <- function() m
        
        #functions list
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
        

}


## function:    cacheSolve
##
##              based on the makeCacheMatrix object, 
##              the inverse of the matrix will be returned
##              with the use of a cache
## 
## usage:       cacheSolve(x, ...) - x = makeCacheMatrix Object
##              cacheSolve(cacheMat)
##
## changelog:
##              20160502 MF intial version
##
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        #get from cache
        m <- x$getsolve() 
        
        if(!is.null(m)) {
                #return data from cache
                message("getting cached data")
                return(m)
        }

        #caching part
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        
        #return
        m
        
}
