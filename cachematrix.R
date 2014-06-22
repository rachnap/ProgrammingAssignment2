## To calculate the inverse of an invertible matrix. 
## The funtions check whether the inverse of a matrix already exists and use it 
## from Cache if it does, else calculates the inverse and stores it for future use
 

## The function creates a special matrix on which functions like 
##set,get,setinverse and getinverse can be called to  

makeCacheMatrix <- function(x = matrix()) {
        inv=NULL
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


## ## Takes as argument a special matrix(x),and returns the inverse. 
##If the inverse of the matrix already exists, it a uses that from cache, 
## else it calculates the inverse and returns that.

cacheSolve <- function(x, ...) {
        if(is.null(x$getinverse())){
                t=x$get()
                print("calculating inverse")
                id=diag(1,nrow(t),ncol(t))
                inv<-solve(t,id)
                x$setinverse(inv)
        }
        else{
                print("using cache")
                
        }
        inv
}
