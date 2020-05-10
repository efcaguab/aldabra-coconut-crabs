
plot_seasonality_counts_size <- function(count_models, size_models){
  m_c <- count_models
  m_s <- size_models
  viridis_option <- "D"
  require(ggplot2)
  require(data.table)
  require(grid)
  
  pl <- function(x){
    x %>% 
      ggplot(aes(x = x, y = y, z = fit)) +
      # geom_tile() +
      geom_contour_fill() +
      geom_contour2() +
      scale_x_date(date_labels = "%b", expand = c(0,0), name = "month", date_breaks = "2 month") +
      scale_y_continuous(expand = c(0,0), name = "dist. to shore [m]")
  }
  
  theme_contour <- function(){
    theme(legend.position = "right", 
          legend.direction = "vertical",
          legend.title = element_text(size = 7),
          axis.title.x = element_blank(), 
          legend.key.height = unit(5, "mm"), 
          strip.text = element_text(hjust = 0)) 
  }
  
  # month
  intercept_c <- data.frame(sex = c("F", "M"),
                            intercept = c(m_c[[1]][[1]]$coefficients[1],
                                          m_c[[1]][[2]]$coefficients[1]))
  dc <- get_var_gam(m_c[[2]], 3) %>%
    dplyr::inner_join(dplyr::select(get_var_gam(m_c[[2]], 1, "x"), -3, -4)) %>%
    dplyr::inner_join(dplyr::select(get_var_gam(m_c[[2]], 2, "y"), -3, -4)) %>%
    dplyr::inner_join(intercept_c) %>%
    dplyr::mutate(fit = fit + month_fit + dist_shore_fit, 
                  fit = fit + intercept, 
                  fit = exp(fit),
                  x = as.Date("2016-01-01") + x - min(x), 
                  sex = dplyr::if_else(sex == "F", 
                                       "Female crabs", 
                                       "Male crabs")) 
  
  pc <- dc %>% 
    pl() +
    facet_wrap(~sex) +
    scale_fill_viridis_c(option = viridis_option, name = "crabs per\ntransect\nsection") +
    geom_text_contour(stroke = 0.5, size = 2.3) +
    pub_theme() +
    theme_contour() +
    labs(tag = "A.")
  
  
  intercept_s <- data.frame(sex = c("F", "M"),
                            intercept = c(m_s[[1]][[1]]$coefficients[1],
                                          m_s[[1]][[2]]$coefficients[1]))
  ds <- get_var_gam(m_s[[2]], 3) %>%
    dplyr::inner_join(dplyr::select(get_var_gam(m_s[[2]], 1, "x"), -3, -4)) %>%
    dplyr::inner_join(dplyr::select(get_var_gam(m_s[[2]], 2, "y"), -3, -4)) %>%
    dplyr::inner_join(intercept_s) %>%
    dplyr::mutate(fit = fit + month_fit + dist_shore_fit, 
                  fit = fit + intercept, 
                  x = as.Date("2016-01-01") + x - min(x), 
                  sex = dplyr::if_else(sex == "F", 
                                       "Female crabs", 
                                       "Male crabs")) 
  
  ps <- ds %>% 
    # (sex == "M") %>%
    pl() +
    facet_wrap(~sex) +
    scale_fill_viridis_c(option = viridis_option, name = "thoracic\nlength\n(mm)") +
    geom_text_contour(stroke = 0.5, size = 2.3) +
    pub_theme() +
    theme_contour()+
    labs(tag = "B.")
  
  cowplot::plot_grid(pc, ps, ncol = 1, align = "hv")
}

