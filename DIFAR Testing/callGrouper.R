callGrouper <- function(difar, threshold=.5) {
  callId <- 0
  difar$callId <- callId
  do.call(rbind, by(difar, difar$Channel, function(x) {
    x <- arrange(x, UTC)
    startTime <- x$UTC[1]
    callId <<- callId + 1
    for(i in 1:nrow(x)) {
      thisTime <- x$UTC[i]
      if(thisTime - startTime > threshold) {
        callId <<- callId + 1
        startTime <- thisTime
      }
      x$callId[i] <- callId
    }
    x
  }))
}
