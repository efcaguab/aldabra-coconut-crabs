get_habitat_pca <- function(hab){
  pc <- hab %>%
    dplyr::select(3, 5:11) %>%
    prcomp() 
}

plot_habitat_pca <- function(pc, hab){
  require(ggfortify)
  autoplot(pc, loadings = T, 
           loadings.label = T, 
           data = hab, 
           colour = 'dist_shore',
           # frame = T,
           loadings.label.repel = T,
           loadings.label.size = 3.5,
           loadings.label.label = c("champignon", "exposed surface", "mangrove", "open mixed scrub", "standard mixed scrub", "pemphis", "sand", "grasses")) +
    scale_color_continuous(name = "distance\nfrom shore") +
    theme_bw()
}