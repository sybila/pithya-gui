### Notes:
### reactivePoll and reactiveFileReader contain an observer leak which keeps polling the file even after the 
### observer that depends on reactive poll is unsubscribed.
### Therefore we create a simplified custom reactive file reader which needs just one observer and can be safely unsubscribed.

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

myReactiveFileReader <- function(intervalMillis, session, path, action) {
  checkFunc <- function() {
    info <- file.info(path)
    return(paste(path, info$mtime, info$size))
  }
  #rv <- reactiveValues(cookie = checkFunc())
  cookie <- checkFunc()
  observe({
    print("poll")
    latest <- checkFunc()
    if (latest != cookie) {
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