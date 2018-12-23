
get_var_gam <- function(x, i, rename = NULL){
  
  fem <- extract_fit(x[[1]][[i]], rename) %>%
    dplyr::mutate(sex = "F")
  mal <- extract_fit(x[[2]][[i]], rename) %>%
    dplyr::mutate(sex = "M")
  
  dplyr::bind_rows(fem, mal)
}

extract_fit <- function(y, rename = NULL){
  if("y" %in% names(y)){
    d <- expand.grid(x = y$x,
                     y = y$y) %>%
      dplyr::tbl_df()
  } else {
    d <- dplyr::data_frame(x = y$x)
  }
  d %<>%
    dplyr::mutate(fit = y$fit[, 1],
                  fitmax = fit + y$se,
                  fitmin = fit - y$se)
  
  if(!is.null(rename)) {
    names(d)[names(d) == "fit"] <- paste(y$xlab, "fit", sep = "_")
    names(d)[names(d) == "x"] <- rename
  }
  
  return(d)
}
