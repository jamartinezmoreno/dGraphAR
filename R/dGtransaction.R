dGtransaction <- function(datA, transCol, itemCol){

  datATransact <- arules::transactions(datA,
                                       format="long",
                                       cols=c(transCol, itemCol))
}
