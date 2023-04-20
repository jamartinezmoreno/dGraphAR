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
#' dat <- data.table::data.table(rbind(c(1, "milk"), c(1, "bread"), c(2, "onions"), c(3, "onions"), c(3, "beer")))
#' transDat <- dGtransaction(dat, 1, 2)
#' transDat
#'
#' transactions in sparse format with
#' 3 transactions (rows) and
#' 4 items (columns)
#'
dGtransaction <- function(datA, transCol, itemCol){

  datATransact <- arules::transactions(datA,
                                       format="long",
                                       cols=c(transCol, itemCol))
}
