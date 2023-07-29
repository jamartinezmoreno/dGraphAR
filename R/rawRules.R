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

  rules_dt <- data.table::data.table(arules::inspect(rules))
  rules_dt$items <- gsub("[[:punct:]]", "", paste(rules_dt$lhs, rules_dt$rhs, sep = " "))

  output = list(datRawRules = rules_dt,
                transactions = datTrans,
                rules = rules,
                params = list(minNSupport = minNSupport, confidence = confidence, minLen = minLen, maxLen = maxLen))
  class(output) <- "dGraphAR_rawRules"

  return(output)
}
