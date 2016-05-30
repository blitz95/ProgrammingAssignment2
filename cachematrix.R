###############################################################################
## The combintation of two functions:
##      makeCacheMatrix()
##      cacheSolve()
## can be used to calculate and store the inverse of a matrix.  If the inverse
## is already calculated and stored for a given Matrix, the stored value will 
## be returned rather than recalculating the inverse again. 
## 
## The output list from makeCacheMatrix() is used as the input to cacheSolve()
##
## To run the functions, assign the output from makeCacheMatrix() to a 
## variable and use that variable as input to cache Solve()
## Example:
## mkMat = makeCacheMatrix(matrix(C(2, 6, 4, 3), nrow = 2, ncol = 3))
## cacheSolve(mkMat)
###############################################################################

## Function: makeCacheMatrix()
## This function creates a special "matrix" object/list containg functions to:
##  1.set the the matrix
##  2.get the the matrix
##  3.set the value of the inverse matrix
##  4.get the value of the inverse matrix
## This output is used as an input to the cacheSolve() function

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setInv <- function(solve) m <<- solve
    getInv <- function() m
    
    list(set = set, get = get,
         setInv = setInv,
         getInv = getInv)
}

## cacheSolve() 
## The input to cacheSolve, x, is the output "matrix" of makeCacheMatrix()
## The output of cacheSolve() is the inverse of the matrix entered in 
## `makeCacheMatrix`
## Note: cacheSolve() assumes all matrices entered are invertible.

cacheSolve <- function(x, ...) {
    m <- x$getInv()
    
    ## If the inverse matrix, m, has been calculated and stored, retreive it
    ## from cache, return it,  and exit function.  No recalculation performed.
    
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    
    ## else, calculate, store, and return inverse matrix.
    else{
        data <- x$get()
        m <- solve(data, ...)
        x$setInv(m)
        m
    }
}