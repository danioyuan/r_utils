currency2Num <- function(x, na.string = "N/A") {
  # Extract value and currency symbol from a currency string.
  # Eg. "-CND$0.08", "$-1.89B"
  
  if (is.na(x) || x == na.string) { return(rep(NA, 2)) }
  m <- regexpr("^([+-]*)([^\\d+-]+)([\\d.+-]+)(\\D*)", x, perl=T)
  if (m < 0) { return(rep(NA, 2)) }
  
  ss <- attr(m, "capture.start")
  ll <- attr(m, "capture.length")
  
  val <- paste(substr(x, ss[1], ss[1] + ll[1] - 1),
               substr(x, ss[3], ss[3] + ll[3] - 1), sep = "")
  unit <- substr(x, ss[4], ss[4] + ll[4] - 1)
  currency <- substr(x, ss[2], ss[2] + ll[2] - 1)
  
  val <- as.numeric(val)
  if (unit == "K") {
    val <- val * 1e3
  } else if (unit == "M") {
    val <- val * 1e6
  } else if (unit == "B") {
    val <- val * 1e9
  }
  return(c(val, currency))
}
