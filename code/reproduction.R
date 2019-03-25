model_reproduction <- function(crab_tbl, collection_event){
  cc_m <- crab_tbl %>%
    dplyr::filter(sex == "female") %>%
    dplyr::mutate(egg = grepl("gg", comments) & !grepl("Dragging", comments)) %>%
    dplyr::group_by(col_id) %>%
    dplyr::summarise(egg = any(egg)) %>%
    dplyr::full_join(collection_event) %>%
    dplyr::mutate(month = lubridate::yday(date)) %>%
    dplyr::filter(area %in% c("BP", "CP"))
  
  m_e <- mgcv::gam(egg ~ 
                     # s(as.numeric(date), k = 5) +
                     # ti(month, dist_shore, bs = c("cc", "cr"), k = c(12, 3)) +
                     s(month, k = 12, bs = "cc") +
                     # s(dist_shore, k = 2, bs = "cr") +
                     s(moon_ph, bs = "cc", k = 3), 
                   data = cc_m, 
                   family = "binomial", 
                   gamma = 1.4)
  
  
  n <- 362
  m_e_p <- plot(m_e, pers = T, n = n, n2 = n, pages = 1)
  
  list(mod = m_e, 
       pred = m_e_p) 
}

plot_reproduction <- function(reproduction_models){
  require(ggplot2)
  m_e <- reproduction_models
  intercept <- m_e[[1]]$coefficients[1]
  expand.grid(moon_ph_fit = c(0, range(extract_fit(m_e[[2]][[2]])$fit)),
              x = extract_fit(m_e[[2]][[1]])$x) %>%
    dplyr::full_join(extract_fit(m_e[[2]][[1]])) %>% 
    dplyr::inner_join(dplyr::select(extract_fit(m_e[[2]][[2]], "moon_ph"), -3, -4)) %>%
    dplyr::mutate(fit = plogis(intercept + fit + moon_ph_fit), 
                  fitmin = plogis(intercept + fitmin), 
                  fitmax = plogis(intercept + fitmax),
                  x = as.Date("2016-01-01") + x - min(x)) %>% 
    ggplot(aes(x = x, y = fit)) +
    geom_ribbon(aes(ymax = fitmax, ymin = fitmin), alpha = 0.2, fill = colour_scale()[1]) +
    geom_line(aes(linetype = as.factor(round(moon_ph, 2))), colour = colour_scale()[1]) +
    scale_x_date(date_labels = "%b", name = "month", date_breaks = "2 month", expand = c(0,0)) +
    scale_y_continuous(name = "probability", expand = c(0,0), limits = c(0,1)) +
    scale_linetype_manual(values = c(2, 1), name = "moon\nphase", 
                          labels = c(" near full moon    ", " near new moon  ")) + 
    pub_theme() +
    theme(strip.background = element_blank(),
          # plot.margin = grid::unit(c(0,-0.7, 0.4, 0.4), "lines"),
          legend.position = "top", 
          legend.title = element_blank(), 
          legend.key.width = grid::unit(1, "lines"), 
          legend.key.height = grid::unit(0.1, "lines"), 
          axis.title.x = element_blank()) +
    labs(title = "Probability of encountering a female carring eggs", 
         subtitle = "Higer probability between Dec. and Mar.")
  
  # ggplot2::ggsave("figs/reproduction.pdf", width = 3.18, height = 4.5/2)
}
