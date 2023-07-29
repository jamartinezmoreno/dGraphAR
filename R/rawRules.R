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
rawRules <- function(datTrans, minNSupport, confidence, minLen, maxLen){

  setDTthreads(1)
  options(digits = 10)
  set.seed(1234)

  freqSupport <- minNSupport / dim(datTrans)[1]

  rules <- arules::apriori(datTrans, parameter = list(supp = freqSupport,
                                                      conf = confidence,
                                                      minlen = minLen,
                                                      maxlen = maxLen,
                                                      maxtime = 840))

  output <- data.table::data.table(arules::inspect(rules))
  return(output)
}
