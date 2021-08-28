# Time-stamp: <Samstag, 10. Mai 2014, 21:06, [HT.R], Gerhard Sch�n>

# H�ufigkeitstablelle

# H�ufigkeitstabelle 
HT <- function(x, y = NULL, digits = 1, miss = NULL, label = FALSE, print = TRUE) {
  is.miss <- sum(is.na(x)) != 0
  if ((is.null(miss) & is.miss == TRUE) | (ifelse(is.logical(miss), miss == TRUE, FALSE))) M <- TRUE
  if ((is.null(miss) & is.miss == FALSE) | (ifelse(is.logical(miss), miss == FALSE, FALSE))) M <- FALSE
  if (M == TRUE) {
    TABv <- table(x, exclude = NULL)
    TAB <- c(table(x), 0)
  }
  if (M == FALSE) {
    TABv <- table(x)
    TAB <- table(x)
  }
  TAB <- cbind("valid n" = TAB, "valid %" = prop.table(TAB) * 100)
  if (M == TRUE) {
    TAB <- addmargins(cbind(TAB, "n" = TABv, "%" = prop.table(TABv) * 100), 1)
    TAB <- data.frame(" " = rownames(TAB), TAB[, 1:4], check.names = FALSE, row.names = NULL, stringsAsFactors
      = FALSE)
  }
  if (M == FALSE) {
    TAB <- addmargins(cbind(TAB), 1)
    TAB <- data.frame(" " = rownames(TAB), TAB[, 1:2], check.names = FALSE, row.names = NULL, stringsAsFactors
      = FALSE)
  }
  if (M == TRUE) {
    F.proz <- c(3, 5)
    F.n <- c(2, 4)
  }
  if (M == FALSE) {
    F.proz <- 3
    F.n <- 2
  }
  for (i in F.proz) TAB[, i] <- formatC(TAB[, i], format = "f", digits = digits)
  for (i in F.n)    TAB[, i] <- formatC(TAB[, i], format = "f", digits = 0)
  if (M == TRUE) {
    TAB[nrow(TAB) - 1, 1:2] <- c("", "")
    TAB[nrow(TAB) - 1, 1] <- "- missing -"
  }
  if (label != FALSE) cat("--- ", attr(x, "label"), " ---\n", sep = "")
  if (print == TRUE) {
    print(TAB, row.names = FALSE)
  }
  if (print == FALSE) {
    TAB
  }
}

