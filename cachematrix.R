## The functions creates a special matrix which can cache its inverse. 
## When an invertible matrix is provided as input, it checks 
## whether the value is already cached and can be used, else calculates 
## the value and caches it for future use  

## 'makeCacheMatrix' takes as input an invertible matrix(creates a new empty matrix if nothing is provided)
## It returns a list of functions to set and get the matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
        inv<-NULL
        set<-function(m){
                x<<-m
                inv<<-NULL
        }

        get<-function(){
                x
        }
        
        setinverse<-function(minv){
                inv<<-minv                
        }
        
        getinverse<-function(){
                inv
        }
        
       list(set=set,get=get,setinverse=setinverse,getinverse=getinverse) 
        
}


## Takes as argument a special matrix(created using function 'makeCacheMatrix'),
## and returns the inverse.If the inverse of the matrix already exists, 
## it a uses that from cache, else it calculates the inverse and returns that.

cacheSolve <- function(x, ...) {
        if(is.null(x$getinverse())){
                t=x$get()
                print("calculating inverse")
                id=diag(1,nrow(t),ncol(t))
                inv<-solve(t,id)
                x$setinverse(inv)
        }
       
        x$getinverse()
}
