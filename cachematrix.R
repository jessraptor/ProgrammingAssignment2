## These two functions reduce the computational load of calculating
## the inverse of a matrix by allowing it to be cached.

## This function creates an object/matrix that can store/cache
##  the inverse.

makeCacheMatrix <- function(x = matrix()) {
        i<-NULL
        set<-function(y){
                x<<-y
                i<<-NULL
        }
        get<-function()x
        setinverse<-function(inverse) i<<-inverse 
        getinverse<-function()i
        list(set=set,get=get,
             setinverse=setinverse,
             getinverse=getinverse)
}

## This function calculates the inverse of the matrix if it has
## not already been solved/stored in the makeCacheMatrix function.

cacheSolve <- function(x, ...) {
        i<-x$getinverse()
        if(!is.null(i)){
                message("getting cached data")
                return(i)
        }
        data<-x$get()
        i<-solve(data,...)
        x$setinverse(i)
        i
        ## Returns a matrix that is the inverse of x.
}
