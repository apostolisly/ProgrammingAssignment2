## It's a pair of functions that cache the inverse of a matrix.

## This function creates a special "matrix" object that can cache its inverse.


makeCacheMatrix <- function(x = matrix()) {
      
            m<-NULL
            set<-function(y){
                  x<<-y
                  m<<-NULL
            }
            get<-function() x
            setmatrix<-function(solve) m<<- solve
            getmatrix<-function() m
            list(set=set, get=get,
                 setmatrix=setmatrix,
                 getmatrix=getmatrix)
      }



## calculates the mean of the special "vector" created with the above function, if its not already
##calculated. If it's already calculated ut appears "getting cached data"

cacheSolve <- function(x, ...) {
       
      m<-x$getmatrix()
      if(!is.null(m)){
            message("getting cached data")
            return(m)
      }
      matrix<-x$get()
      m<-solve(matrix, ...)
      x$setmatrix(m)
      m
}
