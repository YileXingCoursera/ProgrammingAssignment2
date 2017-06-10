## This function calculates the inverse of a square matrix

## This function returns a list that contains a matrix
makeCacheMatrix <- function(x = matrix()) {
  m<-Null
  set<-function(y){
          x<<-y
          m<<-Null
  }
  get<-function(){
          x
  }
  setinverse<-function(solve){
          m<<-solve
  }
  getinverse<-function(){
          m
  }
  list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}

## This function returns the inverse of a square matrix x
## If x is not a square matrix, then a message and number 0 return

cacheSolve <- function(x, ...) {
        m<-x$getinverse()
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        }
        data<-x$get()
        if(nrow(data)==ncol(data)){
                m<-solve(data,...)
                x$setinverse(m)
                m
        }else{
                message("wrong matrix")
                return(0)
        }        
}
