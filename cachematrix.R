# "makeCacheMatrix" function accepts a "nxn" matrix. 

makeCacheMatrix <- function(x = matrix()) {
        m<-NULL
################################################################################
#######   Function "set" is used to set matrix values or override an     #######
#######   matrix! NOTE: When "set" function is used, cache is RESET!!    #######
################################################################################

        set <- function(y) {
                x <<- y
                m <<- NULL
        }
################################################################################
#######   Function "get" is used to retrieve set matrix values           #######
################################################################################
        
        get <- function() x
        
################################################################################
#######   Function "setinv" is used to set matrix inverse values.        #######
#######   For theoretical purposes. Typically inverses are not set.      #######
################################################################################
        
        setinv <- function(inv) m <<- inv
        
################################################################################
#######   Function "getinv" is used to set matrix inverse values.        #######
################################################################################
        
        getinv <- function() m
        
################################################################################
#######   "set", "get", setinv" & "getinv" are the functions that can be #######
#######   accessed using $ sign.                                         #######
################################################################################
        
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)        
        
###########################End of makeCacheMatrix function #####################
}

# "cacheSolve" function below returns the inverse of the matrix created in 
# "makeCacheMatrix" function above if not calculated before AND cached. If 
#  inverse of same matrix is called for, its returned from its cache "getinv" 
#  function.

cacheSolve <- function(x, ...) {
################################################################################
#######             Return the cached inverse of matrix                  #######
################################################################################
        
        m <- x$getinv()
        if(!is.null(m)) {
                message("getting cached inverse")
                return(m)
        }
################################################################################
#######   Matrix inverse is calculated below using solve function if not #######
#######   calculated already.  .                                         #######
################################################################################
        
        data <- x$get()
        m <- solve(data, ...)
        x$setinv(m)
        m
        
############################ End of cacheSolve function ########################
}