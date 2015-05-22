## These functions cache the inverse of a matrix

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  i<-NULL
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  get<-function(){
    x
  }
  setinverse<-function(solve){
    i<<-solve
  }
  getinverse<-function(){
    i
  }
  list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}


## The cacheSolve function computes the inverse of the special "matrix" 
## returned by the makeCacheMatrix function

cacheSolve <- function(x, ...) {
        i<-x$getinverse()
        if(!is.null(i)){
          message("getting cached data")
          return (i)
        }
        data<-x$get()
        i<-solve(data,...)
        x$setinverse(i)
        i
}
