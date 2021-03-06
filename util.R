### Notes:
### reactivePoll and reactiveFileReader contain an observer leak which keeps polling the file even after the 
### observer that depends on reactive poll is unsubscribed.
### Therefore we create a simplified custom reactive file reader which needs just one observer and can be safely unsubscribed.

mytempdir <- function() {
    
    ## Just for case Windows would need to define specific temporary directory
    # if (.Platform$OS.type == "windows") {
    #     personal <- strsplit(tempdir(),"[\\/]")[[1]][length(strsplit(tempdir(),"[\\/]")[[1]])]
    #     dir <- paste("C://Temp", personal, sep="//")
    #     if(!dir.exists(dir)) dir.create(dir, recursive=T, mode="0777")
    #     return(dir)
    # } else
        return(tempdir())
}


myCoerceToFunc <- function(x) {
  force(x);
  if (is.function(x))
    return(x)
  else
    return(function() x)
}


myReactivePoll <- function(intervalMillis, session, checkFunc, valueFunc) {

  intervalMillis <- myCoerceToFunc(intervalMillis)

  rv <- reactiveValues(cookie = isolate(checkFunc()))

  observe({
    print("Poll!")
    rv$cookie <- checkFunc()
    invalidateLater(intervalMillis(), session)
  })

  # TODO: what to use for a label?
  re <- reactive({
    rv$cookie

    valueFunc()

  }, label = NULL)

  return(re)
}

myReactiveFileReaderOld <- function(intervalMillis, session, filePath, readFunc, ...) {  
  filePath <- myCoerceToFunc(filePath)
  extraArgs <- list(...)

  myReactivePoll(
    intervalMillis, session,
    function() {
      path <- filePath()
      info <- file.info(path)
      return(paste(path, info$mtime, info$size))
    },
    function() {
      do.call(readFunc, c(filePath(), extraArgs))
    }
  )
}

# TODO - add extra fixed check interval for progress files.
myReactiveFileReader <- function(intervalMillis, session, path, action) {
  clock <- createCounter(1)
  
  checkFunc <- function() {
    info <- file.info(path)
    return(paste(path, info$mtime, info$size))
  }

  cookie <- checkFunc()
  observe({
    latest <- checkFunc()
    if (latest != cookie || clock() %% 10 == 0) {
      cookie <- latest
      action(readLines(path))
    }
    invalidateLater(intervalMillis, session)
  })
}


createCounter <- function(value) {
  function() {
    value <<- value + 1
  }
}

unwrapOr <- function(nullable, value) {
  if (is.null(nullable)) {
    value
  } else {
    nullable
  }
}

printInterval <- function(a, b) {
  paste0("[", round(a, digits = 3), ", ", round(b, digits = 3), "]")
}

let <- function(value, action) {
  if (is.null(value)) {
    NULL
  } else {
    action(value)
  }
}


# Remove one specific row(column?) from given dimension without affecting rest of the array
dropRow <- function(arr, dim, row) {    
  # arr[,,-row,,]
  mask <- lapply(1:length(dim(arr)), function(i) if (i == dim) -row else TRUE)
  do.call("[", append(list(arr, drop = FALSE), mask))   
}
# Update one specific row(column?) from given dimension without affecting rest of the array
assignRow <- function(arr, dim, row, value) {
  # arr[,,row,,] <- value
  mask <- append(lapply(1:length(dim(arr)), function(i) if (i == dim) row else TRUE), value)    
  do.call("[<-", append(list(arr), mask))       
}

rowProjection <- function(arr, dim, row) {
  mask <- lapply(1:length(dim(arr)), function(i) if (i == dim) row else TRUE)
  do.call("[", append(list(arr, drop = FALSE), mask))
}

explodeArray <- function(thresholds, d, dimensionSizes) {
    # create permutation vector
    dimensions <- 1:length(dimensionSizes)
    dimensions[c(1,d)] <- dimensions[c(d,1)]
    sizes <- sapply(dimensions, function(i) dimensionSizes[i])
    aperm(array(thresholds, sizes), dimensions)
}

stateIndexFromValue <- function(t, x) {
  for (i in 2:length(t)) {
    if (t[i-1] <= x && x <= t[i]) {
      return(i-1)
    }
  }
  length(t-1)
}