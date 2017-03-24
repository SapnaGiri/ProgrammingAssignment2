## Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix 
## rather than compute it repeatedly. 
## Below are two functions that cache the inverse of a matrix.




## The below function 'makeCacheMatrix' takes a matrix 'x' as input, computes the inverse of the matrix, 
## stores the inverse in object 'i' and returs a list of four functions which can then be used to cache the 
## inverse of the matrix as and when required.

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL

        set <- function(y) {    ##function to set x to input matrix for which inverse must be computed. i which contains the inverse is set to NULL here.
                x <<- y
                i <<- NULL
        }

        get <- function() x		## function to get the input matrix from x

        setinverse <- function(inv) i <<- inv			## function to set i to the inverse of matrix x

        getinverse <- function() i						## function to get inverse of matrix x from i

        list(set = set, get=get, setinverse = setinverse, getinverse = getinverse)       
}



## The below function 'cacheSolve' takes the output of above function as input and returns the inverse of matrix 
## directly from cache if computed previously. Else the inverse is computed and cached so that it can be used next time its required.

cacheSolve <- function(x, ...) {
        i <- x[[4]]()  ##call getinverse() and assign it to i

        if(!is.null(i)) {     		## if i is not null then return i from the cache
                message("getting cached data") 
                return(i)
        }

        data <- x[[2]]()         ## call get() and assign the matrix to data

        i <- solve(data, ...)    ## calculate inverse of matrix in data using 'solve()'

        x[[3]](i)				## call setinverse() and cache the inverse of matrix

        i						## return inverse of matrix
}
