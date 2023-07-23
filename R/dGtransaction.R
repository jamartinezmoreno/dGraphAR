#' Convert a data.frame to transaction format
#'
#' @param datA A data.frame which contain a column with the transaction IDs and a column with the items per transaction
#' @param transCol An integer denoting the column's index that contains the transactions IDs
#' @param itemCol An integer denoting the column's index that contains the items
#'
#' @return An S4 class in transaction format
#' @export
#'
#' @examples
#' to be written
dGtransaction <- function(datA, transCol, itemCol){

  datATransact <- arules::transactions(datA,
                                       format="long",
                                       cols=c(transCol, itemCol))
}
