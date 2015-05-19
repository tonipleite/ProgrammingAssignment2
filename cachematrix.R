# I tried to use some self-explanatory names for the identifiers, to make it
# easier. On the bottom of the page you can find an example.


makeCacheMatrix <- function (x = matrix()){
    # This function creates an object (a list) whose elements are functions.
    # Since these functions are defined in THIS enviroment, they are able to
    # use the values of the free variables of THIS enviroment.
    inverseMatrix <- NULL;
    f.setNewMatrix <- function (newMatrix){
        x <<- newMatrix;
        inverseMatrix <<- NULL; # Unknown inverse, since it is a new matrix.
    };
    f.getCurrentMatrix <- function (){x};
    f.setInverse <- function (newInverse){inverseMatrix <<- newInverse};
    f.getCurrentInverse <- function (){inverseMatrix};
    return(list(set = f.setNewMatrix,
                get = f.getCurrentMatrix,
                setI = f.setInverse,
                getI = f.getCurrentInverse));
};
cacheSolve <- function (x = list(),...){
    # This function returns the inverse of the matrix that is saved into
    # the object provided by the previous funtion. Also, if it hadn't been
    # calculated before, it solves the matrix to its inverse and saves the
    # result in the cache memory.
    inverse <- x$getI(); #Get the current inverse.
    if (!is.null(inverse)){
        message("Getting inverse from memory.");
        return (inverse);
    } else {
        currentMatrix <- x$get();
        inverse <- solve (currentMatrix);
        x$setI(inverse);
        return(inverse);
    };
};
# Example:
# myMatrix <- makeCacheMatrix(matrix(c(c(2:9)*10,1),3,3));
# myMatrix$get();
# myInverse <- cacheSolve (myMatrix);
# myInverse;
# myMatrix2 <- makeCacheMatrix(myInverse);
# cacheSolve(myMatrix2);