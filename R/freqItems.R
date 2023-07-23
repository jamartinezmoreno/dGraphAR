#' Title
#'
#' @param datTrans An object of type transaction
#'
#' @return to be written
#' @export
#'
#' @examples
#' to be written
#'
freqItems <- function(datTrans){

  nTrans <- length(datTrans@itemsetInfo$transactionID)
  freq <- sort(arules::itemFrequency(datTrans), decreasing=TRUE)
  listCols <- list(item = names(freq),
                   freq = as.numeric(freq))
  dat_freq <- data.table::copy(listCols)
  data.table::setDT(dat_freq)
  dat_freq[, cases:= freq * nTrans]
  return(dat_freq)
}



