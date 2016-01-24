## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix handles the matrix in input and its inverse 
## while cacheSolve returns the inverse if exists or solve the matrix returning it 'solved' and setting it
## to the variable inverseMatrix in the makeCacheMatrix function using the setInverseMatrix setter method of makeCacheMatrix 

## Write a short comment describing this function
## getters and setters of the nMatrix parameter and of the inverseMatrix variable initialized to null are defined and implemented
## look at the comments in the function

makeCacheMatrix <- function(mMatrix = matrix()) {
        getMatrix <- function() mMatrix
        inverseMatrix <- NULL
        setMatrix <- function(newMatrix) {
                ## setMatrix is called outside the scope of this function we need the <<- operator ...
                ## check if the matrix has changed and if so the inverse has to be set to null because in cacheSolve inverseMatrix is checked to be null or not ...
                if (length(which((mMatrix==newMatrix)==FALSE))>0) {
                        mMatrix <<- newMatrix
                        inverseMatrix <<- NULL
                }
        }
        getInverseMatrix <- function() inverseMatrix
        ## in setInverseMatrix there has to be <<- operator and not <- operator else the inverseMatrix will never be cached ...
        setInverseMatrix <- function(inverseMtx) inverseMatrix <<- inverseMtx
        list(getMatrix = getMatrix,
             getInverseMatrix = getInverseMatrix,
             setMatrix = setMatrix,
             setInverseMatrix = setInverseMatrix)
} 


## Write a short comment describing this function
## look at the comments in the function

cacheSolve <- function(fMakeCacheMatrix, ...) {
        ## if inverse matrix has already been cached (it is not null) it will be returned ...
        inverseMatrix <- fMakeCacheMatrix$getInverseMatrix()
        if (!is.null(inverseMatrix)) {
                message("cached inverseMatrix gotten")
                return(inverseMatrix)
        }
        ## otherwise inverse matrix is calculated and cached using fMakeCacheMatrix$setInverse ...
        message("cached inverseMatrix not found so now it is 'solved' and cached ... ")
        inverseMatrix <- solve(fMakeCacheMatrix$getMatrix(), ...)
        fMakeCacheMatrix$setInverseMatrix(inverseMatrix)
        inverseMatrix
}
