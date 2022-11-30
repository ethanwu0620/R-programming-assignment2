## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()){  inv<-NULL # cached inverse of matrix
get<-function ()x
set<-function(y){
  x<<-y
  inv<<-NULL
}
getinv<-function()inv
setinv<-function(inverse)inv<<-inverse   ## getter/setter for matrix inverse
list(get=get, set=set, getinv=getinv, setinv=setinv)
}



## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    
    if (!is.null(inv)) {  # return cached matrix inverse if it's been already computed
      message("inverse is cached")
      return(inv)
    }
    m <- x$get()# compute inverse of matrix 
    inv <- solve(m, ...)
    x$setinv(inv) # cache inverse
    return(inv)
  }

