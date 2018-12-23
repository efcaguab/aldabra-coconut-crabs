model_detectability_abundance <- function(crab_tbl, collection_event, habitat_pca_components, dist_shore){
  drake::loadd(crab_tbl, collection_event)
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
                    dist_shore = dist_shore.x, 
                    length = length.x) %>%
      dplyr::filter(!(area == "CP" & locality <= 12))
    
    unmarked::unmarkedFrameDS(
      y = as.matrix(dplyr::select(d, 4:8)),
      siteCovs = dplyr::select(d, area, dist_shore, PC1, PC2),
      dist.breaks = 0:5,
      tlength = dplyr::select(d, length)[[1]],
      survey = "line",
      unitsIn = "m"
    )
  }, .progress = "text")
  
  abu0 <- plyr::llply(unmarkedFrames, 
                      function(x) try(unmarked::distsamp(~ dist_shore + PC1 + PC2 ~ dist_shore + PC1 + PC2, x)),
                      .progress = "text")
  abu1 <- plyr::llply(unmarkedFrames, 
                      function(x) unmarked::distsamp(~ dist_shore + PC1 + PC2 ~ 1, x),
                      .progress = "text")
  abu2 <- plyr::llply(unmarkedFrames, 
                      function(x) unmarked::distsamp(~ dist_shore + PC1 ~ 1, x),
                      .progress = "text")
  abu3 <- plyr::llply(unmarkedFrames, 
                      function(x) unmarked::distsamp(~ dist_shore + PC2 ~ 1, x),
                      .progress = "text")
  abu4 <- plyr::llply(unmarkedFrames, 
                      function(x) unmarked::distsamp(~ PC1 + PC2 ~ 1, x),
                      .progress = "text")
  abu5 <- plyr::llply(unmarkedFrames, 
                      function(x) unmarked::distsamp(~ PC1 ~ 1, x),
                      .progress = "text")
  abu6 <- plyr::llply(unmarkedFrames, 
                      function(x) unmarked::distsamp(~ PC2 ~ 1, x),
                      .progress = "text")
  abu7 <- plyr::llply(unmarkedFrames, 
                      function(x) unmarked::distsamp(~ dist_shore ~ 1, x),
                      .progress = "text")
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
       abu4 = abu4, 
       abu5 = abu5, 
       abu6 = abu6, 
       abu7 = abu7, 
       abu8 = abu8, 
       abu9 = abu9, 
       abu10 = abu10, 
       abu11 = abu11)
  
}


determine_best_detectability_abundance_model <- function(detectability_abundance_model){
  require(foreach)
  attach(detectability_abundance_model)
  
  habitat_abundance <- foreach (i = 1:length(abu1), .combine = c) %do% {
    if (class(abu0[[i]]) == "unmarkedFitDS"){
      which.min(c(abu1[[i]]@AIC, abu2[[i]]@AIC, abu3[[i]]@AIC, abu4[[i]]@AIC,
                  abu5[[i]]@AIC, abu6[[i]]@AIC, abu7[[i]]@AIC, abu8[[i]]@AIC))
    }
  } %>% table
  
  detectability_function <- foreach (i = 1:length(abu1), .combine = c) %do% {
    which.min(c(abu8[[i]]@AIC, abu9[[i]]@AIC, abu10[[i]]@AIC, abu11[[i]]@AIC))
  } %>% table
  
  list(habitat_abundance, detectability_function)
}

calculate_abundance_per_day <- function(detectability_abundance_model){
  
  ab <- detectability_abundance_model$abu8 %>%
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