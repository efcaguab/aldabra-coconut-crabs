
model_moults <- function(crab_tbl, collection_event, dist_shore, n = 362){
  
  sexes <- c("female", "male")
  
  moult_models <- sexes %>%
    plyr::llply(function(x){
      data <- crab_tbl %>%
        dplyr::filter(t_length > 30, 
                      sex == x) %>%
        dplyr::group_by(col_id) %>% 
        dplyr::full_join(collection_event) %>%
        dplyr::mutate(month = lubridate::yday(date)) %>%
        dplyr::filter(area %in% c("BP", "CP")) %>%
        dplyr::filter(!is.na(locality)) %>%
        dplyr::full_join(dist_shore) %>%
        dplyr::filter(!(area == "CP" & as.numeric(locality) <= 12),
                      date > as.Date("2010-01-01"))
      
      mgcv::gam(moult ~ 
                  # s(as.numeric(date), k = 5) +
                  # ti(month, dist_shore, bs = c("cc", "cr"), k = c(5, 3)) +
                  # s(dist_shore, k = 2, bs = "cr") +
                  # s(moon_ph, bs = "cc", k = 3) +
                  s(month, k = 12, bs = "cc"),
                data = data , 
                gamma = 1.4)
    })
  
  moult_model_plots <- lapply(moult_models, plot, pers = T, n = n, n2 = n, pages = 1)
  
  
  list(mod = moult_models, 
       pred = moult_model_plots) 
}


plot_moulting <- function(moult_models){
  require(ggplot2)
  m_m <- moult_models
  widths1 <- c(0, 30, 16, 22)
  
  p <- get_var_gam(m_m[[2]], 1)  %>%
    dplyr::mutate(x = as.Date("2016-01-01") + x - min(x),
                  sex = plyr::mapvalues(sex, c("F", "M"), c("♀", "♂"))) %>% 
    ggplot(aes(x = x, y = fit)) +
    geom_hline(yintercept = 0, linetype = 2, colour = "grey25", size = 0.25) +
    geom_ribbon(aes(ymax = fitmax, ymin = fitmin, fill = sex), alpha = 0.2) +
    geom_line(aes(colour = sex)) +
    scale_x_date(date_labels = "%b", name = "month", date_breaks = "2 month", expand = c(0,0)) +
    # scale_y_continuous(name = "probability", expand = c(0,0), limits = c(-0.4, 0.4)) +
    scale_color_manual(values = colour_scale(), name = "") +
    scale_fill_manual(values = colour_scale(), name = "") + 
    scale_linetype_manual(values = c(2, 1), name = "moon\nphase") + 
    pub_theme() +
    theme(legend.position = "none", 
          axis.title.x = element_blank()) +
    labs(y = "pleonal index deviation", 
         title = "Pleonal index deviation over the year", 
         subtitle = "Female and male crabs have distinct moulting periods")
  
  # colorful subtitle
  pc3grob <- ggplotGrob(p)
  strings <- c("Female", "and", "male", "crabs moult at different times")
  pc3grob[[1]][[15]]$children[[1]]$label <- strings
  pc3grob[[1]][[15]]$children[[1]]$x <- unit(cumsum(widths1), "pt")
  pc3grob[[1]][[15]]$children[[1]]$gp$col <- c(colour_scale()[1], "black", colour_scale()[2], "black")
  pc3grob[[1]][[15]]$children[[1]]$gp$font <- c("plain" = c(2L, 1L, 2L, 1L))
  
  pc3grob
}

plot_moulting_data <- function(moult_models){
  
  colour_scale <- colour_scale()
  require(ggplot2)
  
  dplyr::mutate(moult_models$mod[[1]]$model, sex = "Female crabs") %>%
    dplyr::bind_rows(dplyr::mutate(moult_models$mod[[2]]$model, sex = "Male crabs")) %>%
    dplyr::rename(moult = 1, yday = 2) %>%
    dplyr::mutate(yday = as.Date("2016-01-01") + yday) %>% 
    ggplot(aes(x = yday, y = moult, colour = sex))  +
    scale_color_manual(values = colour_scale) + 
    scale_x_date(date_labels = "%b", expand = c(0,0), name = "month", date_breaks = "2 month") +
    geom_point(size = 0.25, alpha = 0.3) +
    facet_grid(cols = vars(sex)) +
    pub_theme() +
    theme(legend.position = "none") +
    labs(y = "pleonal index")
}