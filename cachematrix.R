## This file contains two functions, makeCacheMatrix and cacheSolve
## makeCacheMatrix takes an invertible matrix, and returns a list containing functions to set and get the matrix and its inverse.
##cacheSolve takes the output of makeCacheMatrix and returns the inverse in an efficient way using cached results to avoid re-computation.


## Takes a matrix x and returns a list containing functions set,get,getInv,and setInt, to get and set the matrix and its inverse. The matrix is assumed to be invertible.

makeCacheMatrix <- function(x = matrix()) {
		inv<-NULL
		set<-function(amatrix){
		x<<-amatrix
		inv<<-NULL
		}
		get<-function(){return(x)}
		getInv<-function(){return(inv)}
		setInv<-function(newinv){inv<<-newinv}
return(
list(set=set,get=get,getInv=getInv,setInv=setInv)
)
}

## Takes a list output by makeCacheMatrix, returns the stored inverse if there is one, otherwise computes and sets the inverse before returning it. Additional arguments are passed to 'solve'. 

cacheSolve <- function(x, ...) {
	   inv<-x$getInv()
	   if(is.null(inv)){
	  #message("Setting inverse")
		inv<-x$get()
		inv<-solve(inv, ...)
		x$setInv(inv)
		return(inv)
	}
	else {
	#message("Using cache")
	return(inv)
	}
}