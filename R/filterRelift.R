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
filterRelift <- function(filtRulesObj, w){

  options(digits = 4)

  target_rules <- copy(filtRulesObj$dt_filtRules)

  rule_level <- lapply(target_rules$lhs, function(ttext){

    split_string_ante <- strsplit(as.character(ttext), ",")[[1]]
    output_level <- length(split_string_ante) #paste("level =", length(split_string_ante))
    return(output_level)
  })

  target_rules[, level := unlist(rule_level)]

  target_rules[, aux:=1:.N]
  target_rules[, xy := combine_xy(lhs, rhs), by=c("aux")]

  rules_items <- lapply( target_rules$xy, function(x){strsplit( x, ",")[[1]] })

  list_rules_redund <- list()
  list_pairs_redund_RR <- list()
  k <- 1
  for(i in 1:nrow(target_rules)){
    # i = 1
    rule_objective <- copy( target_rules[aux==i] )
    rule_objective_level <- rule_objective$level
    idx_rest_larger <- !(target_rules$aux == 1) & (target_rules$level > rule_objective_level)
    idx_rest_equal <- !(target_rules$aux == 1) & (target_rules$level == rule_objective_level)

    xy_objective <- strsplit( rule_objective$xy, "," )[[1]]
    hyperlift_objective <- rule_objective$hyperLift
    support_objective <- rule_objective$support
    support_objective_w <- w * rule_objective$support
    confidence_objective <- rule_objective$confidence

    # redundancy on higher level rules =========================================
    idx_larger_candidates <- unlist( lapply(rules_items[idx_rest_larger], function(xy_items){
      is_contained <- as.numeric( sum(xy_objective %in% xy_items) == rule_objective_level + 1 )
      return(is_contained)
    } ) )

    idx_LargRules_scope <- which(idx_larger_candidates==1)
    target_LargRules_scope <- target_rules[idx_rest_larger][idx_LargRules_scope]

    idx_LargRules_higher_HLift <- target_LargRules_scope$hyperLift > hyperlift_objective
    idx_LargRules_higher_SupportW <- target_LargRules_scope$support > support_objective_w

    idx_ruleLarge_redund <- idx_LargRules_higher_HLift + idx_LargRules_higher_SupportW
    idx_selRLarge_redund <- which(idx_ruleLarge_redund == 2)
    isRedun_toLargeRule <- sum(idx_ruleLarge_redund == 2) > 0

    # redundancy on same level rules ===========================================
    idx_equal_candidates <- unlist( lapply(rules_items[idx_rest_equal], function(xy_items){
      is_contained <- as.numeric( sum(xy_objective %in% xy_items) == rule_objective_level + 1 )
      return(is_contained)
    } ) )

    idx_EquaRules_scope <- which(idx_equal_candidates==1)
    target_EquaRules_scope <- target_rules[idx_rest_equal][idx_EquaRules_scope]

    idx_EquaRules_hiEqu_HLift <- target_EquaRules_scope$hyperLift >= hyperlift_objective
    idx_EquaRules_hiEqu_Support <- target_EquaRules_scope$support >= support_objective
    idx_EquaRules_highe_Confide <- target_EquaRules_scope$confidence > confidence_objective

    idx_ruleEqua_redund <- idx_EquaRules_hiEqu_HLift + idx_EquaRules_hiEqu_Support + idx_EquaRules_highe_Confide
    idx_selREqua_redund <- which(idx_ruleLarge_redund == 3)
    isRedun_toEquaRule <- sum(idx_ruleEqua_redund == 3) > 0

    output_isRed <- isRedun_toLargeRule | isRedun_toEquaRule
    if(output_isRed){

      overrulingRules <- rbindlist(list(target_rules[idx_rest_larger][idx_LargRules_scope][idx_selRLarge_redund],
                                        target_rules[idx_rest_equal][idx_EquaRules_scope][idx_selREqua_redund]))

      list_pairs_redund_RR[[k]] <- list(redunRul <- rule_objective, overllingRul <- overrulingRules, i <- i)
      k <- k + 1
    }

    list_rules_redund[[i]] <- output_isRed
  }

  idx_withRedund <- unlist(list_rules_redund)

  output <- list(dt_filtRules = filtRulesObj$dt_filtRules[!idx_withRedund],
                 dt_redunRules = filtRulesObj$dt_filtRules[idx_withRedund],
                 redundInspect = list_pairs_redund_RR,
                 w = w)

  class(output) <- "dGraphAR_filtRulesReLift"
  return(output)
}

