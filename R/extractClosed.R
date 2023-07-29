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
extractClosed <- function(rawRulesObj){
  par_A <- rawRulesObj$params$minNSupport
  par_B <- rawRulesObj$params$confidence
  par_C <- rawRulesObj$params$minLen
  par_D <- rawRulesObj$params$maxLen

  freqSupport <- par_A / dim(rawRulesObj$transactions)[1]

  close_itemsets <- arules::apriori(rawRulesObj$transactions,
                                    parameter = list(target = "closed",
                                                     supp = freqSupport,
                                                     conf = par_B,
                                                     minlen = par_C,
                                                     maxlen = par_D,
                                                     maxtime = 840))

  dt_closed_itemsets <- data.table::data.table(arules::inspect(close_itemsets))

  idx_closed_rules <- rawRulesObj$datRawRules$items %in% gsub("[[:punct:]]", "", dt_closed_itemsets$items)

  rules_closed <- subset(rawRulesObj$rules , subset = idx_closed_rules)

  output = list(closeRules = rules_closed,
                transactions = rawRulesObj$transaction)
  class(output) <- "dGraphAR_closeRules"
  return(output)
}
