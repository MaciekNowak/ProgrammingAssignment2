## There are two functions: makeCacheMatrix takes a martix as its parameter
## and returns a list consisting of get/set functions
## and cacheSolve that caluclates this martix inverse when needed
## or retrieves it from the cache.
##
## The sample session in RStudio would look like this:
## 
## source("cachematrix.R")
## m <- matrix(ncol=2, nrow=2)
## m[1, 1] = 2
## m[1, 2] = 3
## m[2, 1] = 2
## m[2, 2] = 2
## o <- makeCacheMatrix(m)
## io <- cacheSolve(o) # calculates and returns the inverse
## io <- cacheSolve(o) # returns the inverse from the cache
## m[1, 1] = 1
## m[1, 2] = 1
## m[2, 1] = -1
## m[2, 2] = 2
## o$set(m)
## io <- cacheSolve(o) # calculates and returns the inverse
## io <- cacheSolve(o) # returns the inverse from the cache

## Takes the matrix to be inversed and returns a list with four functions:
## set - sets the martix to be invsered
## get - gets the martix to be inversed
## setInverse - caches the martix inverse
## getInverse - returns the cached inverse or NULL if not calculation 
##              has been done
makeCacheMatrix <- function(x = matrix())
{
    mtrxInverse <- NULL
    set <- function(m)
    {
        x <<- m # the matrix to be inversed
        mtrxInverse <<- NULL
    }
    get <- function() { x }
    setInverse <- function(im) { mtrxInverse <<- im }
    getInverse <- function() { mtrxInverse }
    list(set = set, get = get,
         setInverse = setInverse, getInverse = getInverse)
}


## Takes as its parameters
## x - a list that includes the matrix to be inversed and set/get
##     functions
## ... - the other parameters to be passed to the solve() function
## then retrieves the matrix inverse from the cache and if not yet calculated
## calculates the inverse.
## Returns the inverse, if the inverse was retrieved from the cache
## prints ths information message.
cacheSolve <- function(x, ...)
{
    mtrxInverse <- x$getInverse() # get the inverse from the cache
    if(! is.null(mtrxInverse))
    {
        message("getting the matrix inverse from the cache")
        return(mtrxInverse)
    }
    mtrx <- x$get()
    mtrxInverse <- solve(mtrx, ...) # calculate the inverse
    x$setInverse(mtrxInverse) # save the inverse in the cache
    mtrxInverse # return the inverse
}
