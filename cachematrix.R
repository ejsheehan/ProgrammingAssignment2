## These two functions help set and store a matrix and calculate
## and cache its inverse.

## This function creates a function named makeCacheMatrix that is
## really a list of functions. These functions help set and store 
## the original matrix (x) and the inverse matrix (m).

makeCacheMatrix <- function(x = matrix()) {
	m<-NULL
	set<-function(y){
		x<<-y
		m<<-NULL
	}
	get<-function()x
	setminv<-function(minv) m<<-minv
	getminv<-function() m
	list(set=set, get=get, setminv=setminv, getminv=getminv)
}


## This function recalls a cached inverse matrix result, or, in the absence of
## a cached result, solves for the inverse and then caches the result.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	m<-x$getminv()
	if(!is.null(m)) {
		message("getting cached data")
		return(m)
	}
	data<-x$get()
	m<-solve(data)
	x$setminv(m)
	m
}