linguistic_variable <- function(name = NULL, xlim = NULL, fuzzy_set_list = NULL, ...){
  fs_list <- if(is.null(fuzzy_set_list)){
    list(...)
  }else{
    fuzzy_set_list
  }
  
  lv <- list(
    name = name,
    xlim = xlim,
    fuzzy_set_list = fs_list
  )
  class(lv) <- 'linguistic_variable'
  lv
}
