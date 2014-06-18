## These functions store a matrix and cache its inverse
## to ensure for subsequent calls that the inverse
## does not have to be calculated each time

## Function creates a vector to get/set the value of
## a matrix and get/set the value of its inverse

makeCacheMatrix <- function(x = matrix()) {
  i<-NULL
  set<-function(y){
    x<<-y
    i<<-NULL
  }
  
  get<-function() x
  setinverse<-function(inverse)i<<-inverse
  getinverse<-function() i
  list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}


## returns the inverse if already calculate else
## calculates and stores it for future calls

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv<-x$getinverse()
  if(!is.null(inv)){
    message("returning cached inverse")
    return(inv)
  }
  data<-x$get()
  #solve() calculates the inverse
  inv<-solve(data)
  x$setinverse(inv)
  inv
}
