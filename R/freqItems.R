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
                   freq = as.numeric(freq),
                   cases = as.numeric(freq) * nTrans)
  dat_freq <- data.table::copy(listCols)
  data.table::setDT(dat_freq)
  output = list(freqTable = dat_freq, nTransactions = nTrans)
  class(output) <- "dGraphAR_freq"
  return(output)
}






