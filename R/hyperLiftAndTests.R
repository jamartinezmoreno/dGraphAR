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
hyperLiftAndTests <- function(closedRulesObj, signifLevel){

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
                                                              alpha = signifLevel,
                                                              adjust = "bonferroni")]
  output = list(rulesSet = closedRulesObj$closeRules, dt_rulesStats = dt_rules, signifLevel = signifLevel)
  class(output) <- "dGraphAR_hyperlifSign"

  return(output)
}

