## These functions collectively calculate the inverse of a given matrix and then help
## improve performance by caching and retrieving the inverse 

## This main function includes 4 sub-functions that can store and retrieve a matrix
## and its inverse

makeCacheMatrix <- function (x=numeric()) { 
        i <- NULL                           #Object holding the inverse of a given matrix
        set <- function (y){                #In case of a new matrix, this function updates matrix data and clears the cached inverse
                x <<- y                     #x is the location holding the matrix. y is the user input.
                i <<- NULL
        }
        get <- function () x                #Retrieves the matrix given as input to makeCacheMatrix and stored in location x
        setinverse <- function(inverse) i <<- inverse #Function that stores "any" given input to the object i
        getinverse <- function() i          #Retrieves the vector stored in location i
        list (set = set, get = get, setinverse = setinverse, getinverse = getinverse) #Stores the listed functions within the main makeCacheMatrix function
}


## Function to check if an inverse is stored in the object given as input. If so, it 
## retrieves the inverse. Else, it calculates the inverse, updates the cache and returns 
## the inverse.

cacheSolve <- function (x, ...){            
        i <- x$getinverse()                #Uses a function getinverse() that is within the main function makeCaseMatrix using subsetting
        if(!is.null(i)){                   #Simple check for null and if not null, returning the cached inverse
                message("getting cached data") #On screen update to the user that cached information is being displayed
                return(i)
        }
        data <- x$get()                    #This is where the recalculation of the inverse begins, where needed.The new matrix is stored in object "data"
        i <- solve (data, ...)             #Simple inverse calculation.
        x$setinverse (i)                   #Another access to a function within the main function to update the cache
        i                                  #Returns the newly calculated inverse
}
