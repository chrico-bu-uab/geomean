geomean <- function(x,
                    zero.rm = TRUE,
                    na.rm = TRUE,
                    nan.rm = TRUE,
                    eta = NA_real_) {
    nan.count <- sum(is.nan(x))
     na.count <- sum(is.na(x))
  value.count <- if(zero.rm) sum(x[!is.na(x)] > 0) else sum(!is.na(x))

  #Handle cases when there are negative values, all values are missing, or
  #missing values are not tolerated.
  if ((nan.count > 0 & !nan.rm) | any(x < 0, na.rm = TRUE)) {
    return(NaN)
  }
  if ((na.count > 0 & !na.rm) | value.count == 0) {
    return(NA_real_)
  }

  #Handle cases when non-missing values are either all positive or all zero.
  #In these cases the eta parameter is irrelevant and therefore ignored.
  if (all(x > 0, na.rm = TRUE)) {
    return(exp(mean(log(x), na.rm = TRUE)))
  }
  if (all(x == 0, na.rm = TRUE)) {
    return(0)
  }

  #All remaining cases are cases when there are a mix of positive and zero
  #values.
  #By default, we do not use an artificial constant or propagate zeros.
  if (is.na(eta)) {
    return(exp(sum(log(x[x > 0]), na.rm = TRUE) / value.count))
  }
  if (eta > 0) {
    return(exp(mean(log(x + eta), na.rm = TRUE)) - eta)
  }
  return(0) #only propagate zeroes when eta is set to 0 (or less than 0)
}
