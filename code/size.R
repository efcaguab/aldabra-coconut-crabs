
model_size <- function(crab_master_df, n = 362/2){
  crab_master_df %<>%
    dplyr::filter(!(area == "CP" & as.numeric(locality) <= 12))
  
  sexes <- c("female", "male")
  
  size_models <- plyr::llply(sexes, function(x){
    data <- dplyr::filter(crab_master_df, sex == x)
    mgcv::gam(t_length ~ 
                # s(as.numeric(date), k = 5) +
                s(month, bs = c("cc"), k = 12) +
                s(dist_shore, k = 3, bs = "tp") + 
                ti(month, dist_shore, k = c(12, 3), bs = c("cc", "tp")) +
                s(moon_ph, bs = "cc", k = 5),
              # s(moon_ph, bs = "cc", k = 3, by = dist_shore_l) +
              # rain, 
              data = data, 
              gamma = 1.4)
  })
  
  n <- 362/2
  size_model_plots <- lapply(size_models, plot, pers = T, n = n, n2 = n, pages = 1)
  
  list(mod = size_models, 
       pred = size_model_plots)
}

plot_size_distribution <- function(crab_tbl){
  require(ggplot2)
  colour_scale <- colour_scale()
  
  p1 <- crab_tbl %>%
    dplyr::select(sex, t_length) %>% 
    dplyr::filter(!is.na(sex)) %>% 
    dplyr::mutate(row_n = 1:nrow(.)) %>%
    tidyr::spread(sex, t_length) %>%
    ggplot() +
    stat_density(aes(x = male), alpha = 1, fill = colour_scale[2], colour = colour_scale[2], geom = "area", trim = T) +
    stat_density(aes(x = female, y = -(..density..)), alpha = 1, fill = colour_scale[1], colour = colour_scale[1], geom = "area", trim = T) +
    scale_x_continuous(name = "thoracic length [mm]") +
    pub_theme() +
    coord_flip() +
    labs(title = "Size distribution of observed coconut crabs", 
         subtitle = "Female crabs are smaller than males")
  
  # colorful subtitle
  p1grob <- ggplotGrob(p1)
  strings <- c("Female","crabs are smaller than", "males")
  widths <- c(0, 30, 81)
  # widths <- c(0, 4.5, 6, 3, 4)
  p1grob[[1]][[15]]$children[[1]]$label <- strings
  p1grob[[1]][[15]]$children[[1]]$x <- unit(cumsum(widths), "pt")
  p1grob[[1]][[15]]$children[[1]]$gp$col <- c(colour_scale[1], "black", colour_scale[2])
  p1grob[[1]][[15]]$children[[1]]$gp$font <- c("plain" = c(2L, 1L, 2L))
  
  p2 <- crab_tbl %>%
    dplyr::filter(!is.na(sex)) %>%
    ggplot(aes(x = sex, y = t_length)) +
    geom_boxplot(aes(colour = sex), 
                 alpha = 0.2, outlier.size = 1, 
                 outlier.shape = 21, size = 0.5) +
    # geom_boxplot(aes(fill = sex),
    #              fill = "transparent",
    #              outlier.size = 1,
    #              outlier.shape = 21, size = 0.5) +
    theme_bw() +
    scale_x_discrete(labels = c("females", "males")) +
    scale_y_continuous(position = "right") +
    scale_fill_manual(values = colour_scale) +
    scale_color_manual(values = colour_scale) +
    pub_theme() +
    theme(legend.position = "none", 
          axis.title.y = element_blank(), 
          plot.background = element_blank()) +
    ylab("thoracic length [mm]") + xlab("")
  
  cowplot::plot_grid(p1grob, p2, ncol = 2, rel_widths = c(2, 1), align = "h")
  # ggplot2::ggsave(drake::file_out("figs/size_distrubution.pdf"), width = 3.18, height = 4.5/2)
}
