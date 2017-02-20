## The below functions all you to create a numeric vector object with getter and setter methods for
## the vectors value and calculated mean. They also allow you to cache the vectors mean for later use. 

# The makeVector method creates a vector object with the following funtions
#   1. Set the value of the numeric vector
#   2. Get the value of the numeric vector
#   3. Set the value of the numeric vectors mean
#   4. Get the value of the numeric vectors mean

makeVector <- function(x = numeric()) {
  m <-NULL                                # instantiate the vectors mean (m) to null
  set <- function(y) {                    # function to set the value for the numeric vector
    x <<- y
    m <<- NULL
  }
  get <- function() x                     # function to return the value of the numeric vector 
  setmean <- function(mean) m <<- mean    # function to set the value of the numeric vectors mean
  getmean <- function() m                 # function to return the value of the numeric vectors mean
  list(set = set, get = get, setmean = setmean, getmean = getmean) # list to hold the objects functions
}


# The cachemean method returns the mean a numeric vector object if it has been assigned to the vector.  If the 
# mean has not been defined or instantiated yet, the function will calculate and store the mean in the vector object. 

cachemean <- function(x, ...) {
  m <- x$getmean()                       # attempt to retrieve numeric vectors stored mean
  if(!is.null(m)){                       # check if m is null 
    message("getting cached mean")       # if m is not null, return vectors stored mean
    return(m)
  }
  data <- x$get()                        # if m is null, retrieve vector objects data and caculate mean
  m <- mean(data, ...)
  x$setmean(m)                           # set calculated mean and return it
  m
}