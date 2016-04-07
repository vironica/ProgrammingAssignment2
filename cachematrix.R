## computing the inverse of a matrix often requires a lot of work
##these functions allows us to cache the inverse of a matrix rather than calculate it repeatedly

## makeCacheMatrix creates a special matrix that allows it to cache the inverse

makeCacheMatrix <- function(x = matrix()) {
        m<-NULL
	set<-function(y){
		x<<-y
		m<<-NULL
	}
	get<-function()x
	setInv<-function(inv)m
	getInv<-function()m
	list(set=set,get=get,
		setInv=setInv,
		getInv=getInv)
}


## this function checks if the inverse is in the cache, if so, it gets the
##inverse from the cache, otherwise, it calculate the inverse and sets 
##the inverse in the cache via the setInv function

cacheSolve <- function(x, ...) {
        m<-x$getInv()
	if(!is.null(m)){
		message("getting cached data")
		return(m)
	}
	data<-x$get()
	m<-solve(data)
	x$setInv(m)
	m
}
