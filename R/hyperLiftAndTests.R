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
hyperLiftAndTests <- function(closedRulesObj){

  dt_rules <- data.table::data.table(arules::inspect(closedRulesObj$closeRules))

  dt_rules[, fisher_test_pvalue:= arules::interestMeasure(closedRulesObj$closeRules,
                                                          measure = "fishersExactTest",
                                                          transactions = closedRulesObj$transactions)]

  dt_rules[, hyperLift:= arules::interestMeasure(closedRulesObj$closeRules,
                                                 measure = "hyperLift",
                                                 transactions = closedRulesObj$transactions)]

  dt_rules[, bonferrCorrected_signif:= arules::is.significant(closedRulesObj$closeRules,
                                                              transactions = closedRulesObj$transactions,
                                                              method = "fisher",
                                                              alpha = 0.01,
                                                              adjust = "bonferroni")]

  return(dt_rules)
}

