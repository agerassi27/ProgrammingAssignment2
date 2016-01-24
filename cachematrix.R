## Programming Assignment 2 - Introduction to R (Asaf Gerassi 24/01/2016)
## Function that creates a matrix object that can cache its inverse


makeCacheMatrix <- function(x = matrix()) {

  m<-NULL
  ## set function
  set<-function(y){
     x<<-y
     m<<-NULL
  }
  
  ##get function
  get<-function() x
  
  ## setmatrix (solve function inverts the matrix called into it)
  setinvmatrix<-function(solve) m<<- solve
  
  ##getmatrix function 
  getinvmatrix<-function() m
  
  ## list of all the functions defined
  list(set=set, get=get,
       setinvmatrix=setinvmatrix,
       getinvmatrix=getinvmatrix)
  
}


## Function that computes inverse of a function

cacheSolve <- function(x, ...) {
        
  m<-x$getinvmatrix()
  
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  
  matrix<-x$get()
  m<-solve(matrix, ...)
  x$setinvmatrix(m)
  m 
  
}
