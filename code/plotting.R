
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
  fit <- y$fit[, 1]
  se <- y$se
  d %<>%
    dplyr::mutate(fit = fit,
                  fitmax = fit + se,
                  fitmin = fit - se)
  
  if(!is.null(rename)) {
    names(d)[names(d) == "fit"] <- paste(y$xlab, "fit", sep = "_")
    names(d)[names(d) == "x"] <- rename
  }
  
  return(d)
}

pub_theme <- function(){
  theme_bw() +
    theme(strip.background = element_blank(),
          strip.text = element_text(size = 8),
          plot.margin = grid::unit(c(0.4,0.4, 0.4, 0.4), "lines"),
          text = element_text(size = 8),
          legend.title = element_text(size = 14),
          plot.title = element_text(face = "bold", size = 8, margin = margin(b = 1)),
          plot.subtitle = element_text(size = 8),
          panel.grid = element_blank(),
          legend.position = c(0,0),
          legend.direction = "horizontal",
          legend.key.size = grid::unit(0.7, "lines"),
          legend.margin = margin(),
          legend.justification = c(0,0),
          legend.background = element_rect(fill = alpha('white', 0.5)))
}

colour_scale <- function(){
  scales::viridis_pal(begin = 0.2, end = 0.7, option = "D")(2) 
}
