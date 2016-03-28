## Put comments here that give an overall description of what your
## functions do: 
# Assignment 2, Lexical Scoping

# Write a short comment describing this function
## The idea of this fn is to obtain the inverse of a matrix. 
## In this case it will be a square matrix, 
## since we will be using the solve command and we are under the assumption that it is always invertible.

## Put comments here that give an overall description of what your
## functions do: 
# Assignment 2, Lexical Scoping

# Write a short comment describing this function
## The idea of this fn is to obtain the inverse of a matrix. 
## In this case it will be a square matrix, 
## since we will be using the solve command and we are under the assumption that it is always invertible.
##following the example of caching the mean of a vector. 
##set the value of the MATRIX
##get the value of the MATRIX
##set the value of the INVERSE
##get the value of the INVERSE

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL   
        set <- function(y) {
                x <<- y
                inverse <<- NULL      
        }
        get <- function() x
        setinverse <- function(inverse) inverse <<- inverse
        getinverse <- function() inverse
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

#As the cachemean, this function calculates the INVERSE of the special (2x2) matrix.
## Also, it checks if the INVERSE has already been calculated, 
## so it won't repeat the computation.

cacheSolve <- function(x, ...) {
        inverse <- x$getinverse()
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        data <- x$get()
        inverse <- solve(data,...)
        x$setinverse(inverse)
        inverse
}

test<-makeCacheMatrix(matrix(10:7,2,2))
test$get()
test$getinverse()
cachesolve(test)


##This are the results under diferent definitions of the test matrix
Browse[1]> test<-makeCacheMatrix(matrix(1:9,2,2))
Browse[1]> test$get()
     [,1] [,2]
[1,]    1    3
[2,]    2    4
Browse[1]> test$getinverse()
NULL
Browse[1]> cachesolve(test)
     [,1] [,2]
[1,]   -2  1.5
[2,]    1 -0.5
Browse[1]> test<-makeCacheMatrix(matrix(4:7,2,2))
Browse[1]> test$get()
     [,1] [,2]
[1,]    4    6
[2,]    5    7
Browse[1]> test$getinverse()
NULL
Browse[1]> cachesolve(test)
     [,1] [,2]
[1,] -3.5    3
[2,]  2.5   -2
Browse[1]> test<-makeCacheMatrix(matrix(10:7,2,2))
Browse[1]> test$get()
     [,1] [,2]
[1,]   10    8
[2,]    9    7
Browse[1]> test$getinverse()
NULL
Browse[1]> cachesolve(test)
     [,1] [,2]
[1,] -3.5    4
[2,]  4.5   -5
Browse[1]> 
