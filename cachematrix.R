## This R script calculates the Inverse of a matrix and then caches the 
## calculated inverse. The cached matrix inverse can be accessed whenever it is required
## instead of recalculating it each time, hence we save computing time.
## The functions assume the matrix provided is invertible

## The function below creates a cache matrix i.e. the resulting matrix has object functions 

makeCacheMatrix <- function(x = matrix()) {
        
        ## Check to see if Matrix Inverse exists
        ## If exists then return else calculate
        ## have get and set functions.
        
        x_inv<-NULL  ## Assign NULL to the Matrix inverse when the cache matrix is created
        
        set<- function(y)
        {       ## Function assigns a matrix to the cache matrix
               
                x<<-y
                x_inv<<-NULL
                
        }
        
        get<-function() x ## Function returns the matrix 
        setinv<-function(inv) x_inv<<-inv ## Function assigns the matrix inverse to x_inv variable 
        getinv<-function() x_inv ## Function returns the matrix inverse 
        
        list(set=set,get=get,setinv=setinv,getinv=getinv)
        
        
}


## The function below computes the matrix inverse and the caches it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

        
        x_inv<-x$getinv()
        
        if(!is.null(x_inv))
        {
                message("getting cached data")
                return(x_inv)
                
        }
        
        data<-x$get() 
        x_inv<-solve(data) ## Computes the inverse of a matrix
        x$setinv(x_inv) ## call functions setinv to Assign the matrix inverse
        x_inv
        
}


