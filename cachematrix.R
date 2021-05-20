## Put comments here that give an overall description of what your
## functions do
## There are two functions makeCacheMatrix,cachesolve
## makeCacheMatrix consists of set,get,setinv,getinv
## library(MASS) is used to calculate inverse for squared as well as non square
##  matrices.
library(MASS)
makeCacheMatrix <- function(x = matrix()) {
         inv<-NULL        ##Initializing inverse as NULL
         set<-function(y){
           x<<-y
           inv<<-NULL
         }
         get<-function()x    ##Function to get matrix x
         setinv<-function(inverse)inv<<-inverse
         getinv<-function(){
                 inver<-ginv(x)
                 inver%*%x   ## Function to obtain inverse of the matrix
         }
         list(set=set,get=get,setinv=setinv,getinv=getinv)

}


## Write a short comment describing this function
## This function is used to get the cache data
cacheSolve <- function(x, ...) {          ## Gets cache data
            inv<-x$getinv()
            if(!is.null(inv)){            ## Checking whether inverse is null
                     message("getting cached data!")
                     return(inv)          ## Returns inverse value
            }
            data<-x$get()
            inv<-solve(data,...)          ## Calculates inverse value
            x$setinv(inv)
            inv                           ## Returns an inverse matrix of x 
            
}
