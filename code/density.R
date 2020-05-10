model_detectability_abundance <- function(crab_tbl, collection_event, habitat_pca_components, dist_shore){
  
  counts <- crab_tbl
  col_event <- collection_event
  hab <- habitat_pca_components
  loc_dist <- dist_shore
  
  counts %<>% 
    dplyr::inner_join(col_event, by = "col_id") %>%
    dplyr::filter(!is.na(date), !is.na(area), !is.na(locality), !is.na(distance)) %>%
    dplyr::group_by(date, area, locality, distance) %>%
    dplyr::summarise(n = n())
  
  y <- expand.grid(date = unique(counts$date),
                   area = unique(counts$area),
                   locality = unique(counts$locality),
                   distance = 0:4) %>% 
    dplyr::left_join(counts) %>%
    dplyr::mutate(n = replace(n, is.na(n), 0), 
       distance = distance , 
       distance = paste("d", distance, sep = ""))
  
  unmarkedFrames <- plyr::dlply(y, "date", function(x){
    d <- x %>%
      tidyr::spread(distance, n) %>% 
      dplyr::inner_join(hab, by = c("area", "locality")) %>% 
      dplyr::inner_join(col_event, by = c("date", "area")) %>%
      dplyr::inner_join(loc_dist, by = c("area", "locality")) %>%
      dplyr::mutate(locality = as.numeric(locality), 
                    dist_shore = dist_shore, 
                    length = length) %>%
      dplyr::filter(!(area == "CP" & locality <= 12))
    
    unmarked::unmarkedFrameDS(
      y = as.matrix(dplyr::select(d, 4:8)),
      siteCovs = dplyr::select(d, area, dist_shore, habitat),
      dist.breaks = 0:5,
      tlength = dplyr::select(d, length)[[1]],
      survey = "line",
      unitsIn = "m"
    )
  }, .progress = "text")
  
  abu0 <- plyr::llply(unmarkedFrames, 
                      function(x) try(unmarked::distsamp(~ habitat ~ habitat, x)),
                      .progress = "text")
  abu1 <- plyr::llply(unmarkedFrames, 
                      function(x) unmarked::distsamp(~ habitat ~ 1, x),
                      .progress = "text")
  abu2 <- plyr::llply(unmarkedFrames, 
                      function(x) unmarked::distsamp(~ 1 ~ habitat, x),
                      .progress = "text")
  abu3 <- plyr::llply(unmarkedFrames, 
                      function(x) unmarked::distsamp(~ 1 ~ 1, x),
                      .progress = "text")
  # abu4 <- plyr::llply(unmarkedFrames, 
  #                     function(x) unmarked::distsamp(~ PC1 + PC2 ~ 1, x),
  #                     .progress = "text")
  # abu5 <- plyr::llply(unmarkedFrames, 
  #                     function(x) unmarked::distsamp(~ PC1 ~ 1, x),
  #                     .progress = "text")
  # abu6 <- plyr::llply(unmarkedFrames, 
  #                     function(x) unmarked::distsamp(~ PC2 ~ 1, x),
  #                     .progress = "text")
  # abu7 <- plyr::llply(unmarkedFrames, 
  #                     function(x) unmarked::distsamp(~ dist_shore ~ 1, x),
  #                     .progress = "text")
  abu8 <- plyr::llply(unmarkedFrames,
                      function(x) unmarked::distsamp(~ 1 ~ 1, x),
                      .progress = "text")
  abu9 <- plyr::llply(unmarkedFrames,
                      function(x) unmarked::distsamp(~ 1 ~ 1, x, keyfun = "exp"),
                      .progress = "text")
  abu10 <- plyr::llply(unmarkedFrames,
                       function(x) unmarked::distsamp(~ 1 ~ 1, x, keyfun = "hazard"),
                       .progress = "text")
  abu11 <- plyr::llply(unmarkedFrames,
                       function(x) unmarked::distsamp(~ 1 ~ 1, x, keyfun = "uniform"),
                       .progress = "text")
  
  list(abu0 = abu0, 
       abu1 = abu1, 
       abu2 = abu2, 
       abu3 = abu3,
       # abu4 = abu4, 
       # abu5 = abu5, 
       # abu6 = abu6, 
       # abu7 = abu7, 
       abu8 = abu8,
       abu9 = abu9,
       abu10 = abu10,
       abu11 = abu11
       )
  
}

model_detectability_abundance_global <- function(crab_tbl, collection_event, habitat_pca_components, dist_shore){
  
  counts <- crab_tbl
  col_event <- collection_event
  hab <- habitat_pca_components
  loc_dist <- dist_shore
  
  counts %<>% 
    dplyr::inner_join(col_event, by = "col_id") %>%
    dplyr::filter(!is.na(date), !is.na(area), !is.na(locality), !is.na(distance)) %>%
    dplyr::group_by(date, area, locality, distance) %>%
    dplyr::summarise(n = n())
  
  y <- expand.grid(date = unique(counts$date),
                   area = unique(counts$area),
                   locality = unique(counts$locality),
                   distance = 0:4) %>% 
    dplyr::left_join(counts) %>%
    dplyr::mutate(n = replace(n, is.na(n), 0), 
                  distance = distance , 
                  distance = paste("d", distance, sep = ""))

  d <- y %>%
    tidyr::spread(distance, n) %>%
    dplyr::inner_join(hab, by = c("area", "locality")) %>%
    dplyr::inner_join(col_event, by = c("date", "area")) %>%
    dplyr::inner_join(loc_dist, by = c("area", "locality")) %>%
    dplyr::mutate(locality = as.numeric(locality),
                  dist_shore = dist_shore,
                  length = length) %>%
    dplyr::filter(!(area == "CP" & locality <= 12))

  umkf <- unmarked::unmarkedFrameDS(
    y = as.matrix(dplyr::select(d, 4:8)),
    siteCovs = dplyr::select(d, area, dist_shore, habitat),
    dist.breaks = 0:5,
    tlength = dplyr::select(d, length)[[1]],
    survey = "line",
    unitsIn = "m"
  )

  abu0 <- unmarked::distsamp(~ habitat ~ habitat, umkf)
  abu1 <- unmarked::distsamp(~ 1 ~ habitat, umkf)
  abu2 <- unmarked::distsamp(~ habitat ~ 1, umkf)
  abu3 <- unmarked::distsamp(~ 1 ~ 1, umkf)
  
  list(abu0 = abu0, 
       abu1 = abu1, 
       abu2 = abu2, 
       abu3 = abu3)
}


determine_best_detectability_abundance_model <- function(detectability_abundance_model){
  require(foreach)
  attach(detectability_abundance_model)
  
  extract_aics <- . %>%
    purrr::map_dfr(~dplyr::tibble(AIC = .@AIC), .id = "date")
  
  aic_table <- detectability_abundance_model %>%
    purrr::map_dfr(extract_aics, .id = "model") 
  
  summary_delta_aic <- . %>%
    dplyr::group_by(date) %>%
    dplyr::mutate(delta_AIC = AIC - min(AIC)) %>%
    dplyr::select(-AIC) %>%
    dplyr::group_by(model) %>%
    dplyr::summarise_if(is.numeric, .funs = list(mean = mean, median = median, min = min, max = max))
  
  aic_table %>%
    dplyr::filter(model %in% c("abu0", "abu1", "abu2", "abu3")) %>%
    summary_delta_aic()
}

extract_coeficients <- function(detectability_abundance_model, habitat_simple){
  
  # baseline_habitat <- unique(habitat_simple$habitat)[!unique(habitat_simple$habitat) %in% habitat_names]
  library(unmarked)
  # loadd(detectability_abundance_model)
  extract_detectability_coef <- . %>%
    coef(type = "det") %>%
    as.data.frame() %>%
    tibble::rownames_to_column() %>%
    dplyr::rename(habitat = rowname, 
                  sigma = ".") %>%
    dplyr::mutate(habitat = stringr::str_extract(habitat, "[A-Z]+"),
                  habitat = dplyr::if_else(habitat == "I", 
                                           unique(habitat_simple$habitat)[1], 
                                           habitat),
                  sigma = dplyr::if_else(sigma == dplyr::first(sigma), 
                                         sigma, 
                                         sigma + dplyr::first(sigma)),
                  sigma = exp(sigma)) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(eshw = integrate(unmarked::gxhn, 0, 5, sigma)$value, 
                  det_probability = eshw / 5) %>%
    dplyr::select(-sigma)
                  
  extract_density_coef <- . %>%  
    coef(type = "state") %>%
    as.data.frame() %>%
    tibble::rownames_to_column() %>%
    dplyr::rename(habitat = rowname, 
                  sigma = ".") %>%
    dplyr::mutate(habitat = stringr::str_extract(habitat, "[A-Z]+"),
                  habitat = dplyr::if_else(habitat == "I", 
                                           unique(habitat_simple$habitat)[1], 
                                           habitat),
                  sigma = dplyr::if_else(sigma == dplyr::first(sigma), 
                                         sigma, 
                                         sigma + dplyr::first(sigma)))
  
  
  detectability_abundance_model$abu1 %>%
    purrr::map_dfr(extract_detectability_coef, .id = "date") %>%
    dplyr::group_by(habitat) %>%
    dplyr::summarise_if(is.numeric, .funs = list(mean =mean))
                        # , se = ~ sd(.)/sqrt(dplyr::n()))
  

  detectability_abundance_model$abu2 %>%
    purrr::map_dfr(extract_density_coef, .id = "date") %>%
    dplyr::group_by(habitat) %>%
    dplyr::summarise_if(is.numeric, .funs = list(mean =mean)) %>%
    dplyr::mutate(mean = exp(mean))
  
  
}

calculate_abundance_per_day <- function(detectability_abundance_model){
  
  ab <- detectability_abundance_model$abu3 %>%
    lapply(function(x) {
      xx <- x@estimates@estimates$state
      ests <- xx@estimates
      SEs <- unmarked::SE(xx)
      Z <- ests/SEs
      p <- 2 * pnorm(abs(Z), lower.tail = FALSE)
      # print(c(ests, SEs, Z,p))
      if(!is.na(p)){
        if(p < 0.05){
          # print(backTransform(x, type = "state")@estimate)
          return(unmarked::backTransform(x, type = "state")@estimate)
        }
      } else return(NA)
    }) %>% unlist %>% as.data.frame.vector() %>%
    dplyr::add_rownames()
  names(ab) <- c("date", "density")
  
  return(ab)
}

calculate_detectability_per_day <- function(detectability_abundance_model, habitat_simple) {
  
  require(unmarked)
  
  extract_detectability_coef <- . %>%
      coef(type = "det") %>%
      as.data.frame() %>%
      tibble::rownames_to_column() %>%
      dplyr::rename(habitat = rowname, 
                    sigma = ".") %>%
      dplyr::mutate(habitat = stringr::str_extract(habitat, "[A-Z]+"),
                    habitat = dplyr::if_else(habitat == "I", 
                                             unique(habitat_simple$habitat)[1], 
                                             habitat),
                    sigma = dplyr::if_else(sigma == dplyr::first(sigma), 
                                           sigma, 
                                           sigma + dplyr::first(sigma)), 
                    sigma = exp(sigma)) %>%
      dplyr::rowwise() %>%
      dplyr::mutate(eshw = integrate(unmarked::gxhn, 0, 5, sigma)$value, 
                    det_probability = eshw / 5) %>%
    dplyr::select(-sigma)
  
  detectability_abundance_model$abu3 %>%
    purrr::map_dfr(extract_detectability_coef, .id = "date") %>%
    dplyr::ungroup() %>%
    dplyr::summarise_if(is.numeric, .funs = list(mean = mean, max = max, min = min))
}

check_env_effect_detectability <- function(detectability_abundance_model){
  library(ggplot2)
  attach(detectability_abundance_model)
  # no significant effect of environment on the detection estimates:
  
  extract_distance_coeff <- function(y, which = "det"){
    plyr::ldply(y, function(x){
      if (class(x) == "unmarkedFitDS"){
        e <- x@estimates@estimates[[which]]@estimates %>% as.data.frame() %>%
          dplyr::add_rownames()
        names(e) <- c("coeficient", "estimate")
        e
      }
    })
  }
  det_pvalue_hist <- abu0 %>%
    extract_distance_coeff() %>%
    dplyr::filter(coeficient != "sigma(Intercept)") %>%
    ggplot(aes(x = plogis(estimate))) +
    geom_density() + facet_wrap(~coeficient) +
    xlab("p-value") + theme_bw()
  
  # detection estimates 
  # ggsave("./paper/supp_figures/detection-coovariates.pdf", width = 4.7,height = 1.65, scale = 1.5)
  
  
  det_pvalue_yday <- abu0 %>%
    extract_distance_coeff() %>%
    dplyr::mutate(date = as.Date(date),
       yday = lubridate::yday(date)) %>%
    ggplot(aes(x = yday, y = plogis(estimate))) +
    geom_point() + facet_wrap(~coeficient) + geom_smooth()
  

  det_pvalue_moonph <- abu0 %>%
    extract_distance_coeff() %>%
    dplyr::mutate(date = as.Date(date),
       date_utc = as.POSIXct(as.POSIXlt(date, tz = "UTC")),
       moon_ph = oce::moonAngle(date_utc, lon = 46.2, lat = -9.4)$phase,
       moon_ph = moon_ph - floor(moon_ph)) %>%
    ggplot(aes(x = moon_ph, y = plogis(estimate))) +
    geom_point() + facet_wrap(~coeficient) + geom_smooth()
  
  # no significant effect of environment on the abundance estimates:
  abu_pvalue_hist <- abu0 %>%
    extract_distance_coeff("state") %>%
    dplyr::filter(coeficient != "(Intercept)") %>%
    ggplot(aes(x = plogis(estimate))) +
    geom_density() + facet_wrap(~coeficient) +
    xlab("p-value") + theme_bw()
  
  # detection estimates 
  # ggsave("./paper/supp_figures/density-coovariates.pdf", width = 4.7,height = 1.65, scale = 1.5)
  
  list(det_pvalue_hist = det_pvalue_hist, 
       det_pvalue_yday = det_pvalue_yday, 
       det_pvalue_moonph = det_pvalue_moonph, 
       abu_pvalue_hist = abu_pvalue_hist)
}

plot_abundance_from_density_model <- function(abundance_per_day){
  require(ggplot2)
  p1 <- abundance_per_day %>%
    dplyr::mutate(date = as.Date(date)) %>%
    ggplot(aes(x = date, y = density)) +
    geom_smooth(method = "glm", method.args = list(family = "poisson")) +
    geom_point() + ylim(c(0, 125))

  p2 <- abundance_per_day %>%
    dplyr::mutate(date = as.Date(date),
                  yday = lubridate::yday(date)) %>%
    ggplot(aes(x = yday, y = density)) +
    geom_point() + geom_smooth()
  
  list(p1, p2)
}

model_density <- function(abundance_per_day, collection_event){
  
  dens <- abundance_per_day
  col_event <- collection_event
  
  col_event_date <- col_event %>%
    # fi(!is.na(rain)) %>%
    dplyr::group_by(date) %>%
    dplyr::mutate(moon_ph = mean(moon_ph),
       rain = rain[1])
  
  m_d_d <- dens %>%
    dplyr::mutate(date = as.Date(date),
       yday = lubridate::yday(date),
       year = lubridate::year(date)) %>%
    dplyr::inner_join(col_event_date)
  
  m_d <- mgcv::gam(density ~ s(yday, bs = "cc")  + s(as.numeric(date), k = 4), data = m_d_d)
  
  # summary(m_d)
  n <- 362
  m_d_p <- plot(m_d, n = n, pages = 1)
  
  
  list(mod = m_d, 
       pred = m_d_p) 
}

plot_density <- function(density_models){
  require(ggplot2)
  m_d <- density_models
  ylim <- c(0,44)
  intercept_d <- c(m_d[[1]]$coefficients[1])
  
  data_points <- m_d[[1]]$model %>%
    dplyr::rename(density = 1, yday = 2, date = 3) %>%
    dplyr::mutate(date = as.Date(date, origin = as.Date("1970-01-01")), 
                  yday = as.Date("2016-01-01") + yday)
  
  pd1 <- extract_fit(m_d[[2]][[1]]) %>%
    dplyr::mutate(fit = fit + intercept_d,
       fitmin = fitmin + intercept_d,
       fitmax = fitmax + intercept_d,
       x = as.Date("2016-01-01") + x) %>%
    ggplot(aes(x = x, y = fit)) +
    geom_point(data = data_points, 
               mapping = aes(x = yday, y = density), 
               size = 0.25, 
               colour = "grey70") +
    geom_hline(yintercept = intercept_d, linetype = 2, colour = "grey25", size = 0.25) +
    geom_ribbon(aes(ymin = fitmin, ymax = fitmax), alpha = 0.25, fill = "grey50") +
    geom_line() + 
    scale_x_date(date_labels = "%b", expand = c(0,0), name = "month", date_breaks = "2 month") +
    scale_y_continuous(limits = ylim, name = "crabs / hectare") +
    pub_theme() +
    labs(tag = "B.")

  pd2 <- extract_fit(m_d[[2]][[2]]) %>%
    dplyr::mutate(fit = fit + intercept_d,
       fitmin = fitmin + intercept_d,
       fitmax = fitmax + intercept_d,
       x = as.Date("1970-01-01") + x) %>%
    ggplot(aes(x = x, y = fit)) +
    geom_point(data = data_points, 
               mapping = aes(x = date, y = density), 
               size = 0.25, 
               colour = "grey70") +
    geom_hline(yintercept = intercept_d, linetype = 2, colour = "grey25", size = 0.25) +
    # geom_point(data = m_d[[1]]$model, aes(y = density, x = as.Date("2016-01-01") + yday), size = 1, shape = 21, alpha = 0.5) + 
    geom_ribbon(aes(ymin = fitmin, ymax = fitmax), alpha = 0.25, fill = "grey50") +
    geom_line() + 
    scale_x_date(expand = c(0,0), name = "date") +
    scale_y_continuous(limits = ylim, name = "crabs / hectare") +
    pub_theme() +
    labs(tag = "A.")

  cowplot::plot_grid(pd2, pd1, ncol = 1, align = "hv")
}
