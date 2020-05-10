
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
  
  size_data <- crab_tbl %>%
    dplyr::select(sex, t_length) %>% 
    dplyr::filter(!is.na(sex)) %>% 
    dplyr::mutate(row_n = 1:nrow(.), 
                  sex = if_else(sex == "male", 
                                true = "Male crabs", false = "Female crabs")) 
  
  size_means <- size_data %>%
    dplyr::group_by(sex) %>%
    dplyr::summarise(mean = mean(t_length), 
                     sd = sd(t_length),
                     n = dplyr::n()) %>%
    dplyr::mutate(se = sd/sqrt(n))
  
  p1 <- size_data %>%
    ggplot(aes(fill = sex, colour = sex)) +
    geom_histogram(aes(x = t_length), binwidth = 1) +
    geom_vline(data = size_means, aes(xintercept = mean, colour = sex), 
               linetype = 2) +
    scale_x_continuous(name = "thoracic length [mm]") +
    scale_fill_manual(values = colour_scale, aesthetics = c("fill", "colour")) +
    facet_grid(sex~ .) +
    pub_theme() +
    # coord_flip() +
    labs() +
    theme(legend.position = "none")
  
  p1
  
  # ggplot2::ggsave(drake::file_out("figs/size_distrubution.pdf"), width = 3.18, height = 4.5/2)
}
