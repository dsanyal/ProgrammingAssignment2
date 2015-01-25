## Put comments here that give an overall description of what your
## functions do

#The code below takes a matrix as an input 
#and caches its inverse using the superassignment operator. 
#For e.g., if the inverse of a given (invertible) matrix has already been computed
#once, then the inverse is retrieved from the cache 
#when the user asks for it to be computed again. 



## Write a short comment describing this function

#The makeCacheMatrix takes a matrix as its argument and stores its inverse 
#in the cache (i.e in the containing environment which is global in this case). 
#It returns a special 'matrix' which is basically a list containing the  
#functions to get information about the input matrix 'x'. 
# It is understood that if the input matrix needs to be changed, 
#it can be done only via the 'set' function which then 
#clears the cache and computes the inverse afresh.


makeCacheMatrix <- function(x = matrix()) {
  inv_cache<-NULL
  set_matrix<-function(y){
    x<<-y
    inv_cache<<-NULL
  }
  get_matrix<-function() x
  set_inverse<-function(inverse){
    inv_cache<<-inverse
  }
  get_inverse <- function() inv_cache
  list( set_matrix = set_matrix, get_matrix = get_matrix, set_inverse = set_inverse, get_inverse = get_inverse)
}


## Write a short comment describing this function

#The cacheSolve function computes the inverse of its argument (the special 'matrix') 
#using solve() if the corresponding inverse does not already exist 
#in the cache. If the inverse already exists in the cache, then 
#it is retrieved from the cache by the get_inverse function 
# with an appropriate message.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverse<-x$get_inverse()
  if(!is.null(inverse)){
    message("Retrieving the inverse from cache")
    return(inverse)
  }
  input_matrix<-x$get_matrix()
  inverse<-solve(input_matrix,...)
  x$set_inverse(inverse)
  inverse
}
