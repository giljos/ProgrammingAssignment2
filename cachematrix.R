## makeCacheMatrix: This function creates a special "matrix" object that can
## cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
        # fontion input x : a square invertible matrix
        # return a list with the following functions (methode):
        #       1. set the matrix
        #       2. get the matrix
        #       3. set the inverse matrix
        #       4. get the inverse matrix
        
        inv <- NULL 
        set <- function(y){
                x <<- y
                inv <<- NULL
        }
        get <- function() x # return the input matrix
        setinvmat <- function(minv) inv <<- minv # set the inversed matrix
        getinvmat <- function() inv # return the inversed matrix
        list(set=set, get=get, setinvmat=setinvmat, getinvmat=getinvmat)
        
}

##
## cacheSolve: This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already been
## calculated (and the matrix has not changed), then the cachesolve 
## retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## input 'x'  : a list created with the fonction makeCacheMatrix
        ## Return a inverse matrix  of 'x'
        ## compute and cache inverse matrix to the makeCacheMatrix list
        
        inv <- x$getinvmat()    # get the inverse matrix from object x
        if (!is.null(inv)){     # This fonction returns inverse matrix if already 
                #calculated and cached
                message("getting cached data")
                return(inv) #
        }
        matdata <- x$get()
        inv <- solve(matdata) # compute inverse matrix
        x$setinvmat(inv)     # set  inverse matrix in cache  
        return(inv)
        
}
