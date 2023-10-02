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
filterHochbergHLift <- function(hyperlifSignObj, minHLift){

  signRules <- hyperlifSignObj$dt_rulesStats[fisher_test_pvalue < hyperlifSignObj$signifLevel & bonferrCorrected_signif == TRUE]
  idx_signRules <- hyperlifSignObj$dt_rulesStats$fisher_test_pvalue < hyperlifSignObj$signifLevel &
                   hyperlifSignObj$dt_rulesStats$bonferrCorrected_signif == TRUE

  setSignRules <- subset(hyperlifSignObj$rulesSet, subset = idx_signRules)

  setorder(signRules, fisher_test_pvalue)
  signRules[, j_Hochberg:=1:.N]

  n_rules <- nrow(signRules)

  signRules[, value_j_Hochberg:=(j_Hochberg*hyperlifSignObj$signifLevel )/n_rules, by=c("j_Hochberg")]
  signRules[, idx_Hochberg:= fisher_test_pvalue <= value_j_Hochberg, by=c("j_Hochberg")]

  filtRules <- signRules[signRules$idx_Hochberg & hyperLift >= minHLift, !c("bonferrCorrected_signif", "j_Hochberg", "idx_Hochberg"), with = FALSE]

  idxHochber_hyperLift <- data.table::data.table(arules::inspect(setSignRules))$lhs %in% filtRules$lhs &
                          data.table::data.table(arules::inspect(setSignRules))$rhs %in% filtRules$rhs

  rulesSetFilt = subset(setSignRules, subset = idxHochber_hyperLift)


  output = list(rules = rulesSetFilt, dt_filtRules = filtRules, minHLift = minHLift, numRules = nrow(filtRules))
  class(output) <- "dGraphAR_filtRules"

  return(output)
}

