linguistic_variable <- function(...){
  lv <- list(...)
  class(lv) <- 'linguistic_variable'
  lv
}
