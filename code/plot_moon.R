
plot_moon_counts_size <- function(count_models, size_models){
  ##### 
  m_c <- count_models
  m_s <- size_models
  moons_in_axis <- 50
  colour_scale <- colour_scale()
  require(ggplot2)
  fq <- grImport::readPicture("data/fig_annotations/FQ.xml") %>% grImport::pictureGrob()
  f <- grImport::readPicture("data/fig_annotations/F.xml") %>% grImport::pictureGrob()
  n <- grImport::readPicture("data/fig_annotations/N.xml") %>% grImport::pictureGrob()
  lq <- grImport::readPicture("data/fig_annotations/LQ.xml") %>% grImport::pictureGrob()
  # # moon
  yc <- -0.099
  ys <- -3.25
  widths1 <- c(0, 30, 16, 22)
  female_width_coords <- 0.16
  male_width_coords <- 0.11
  widths2 <- c(0, 22)
  alphas <- 0.15
  
  intercept_c <- data.frame(sex = c("F", "M"),
                            intercept = c(m_c[[1]][[1]]$coefficients[1],
                                          m_c[[1]][[2]]$coefficients[1]))
  # Base graph
  pc3 <- get_var_gam(m_c[[2]], 4) %>%
    dplyr::inner_join(intercept_c) %>%
    dplyr::mutate(fit = exp(fit + intercept) - exp(intercept) ,
                  fitmax = exp(fitmax + intercept) - exp(intercept),
                  fitmin = exp(fitmin + intercept) - exp(intercept),
                  sex = plyr::mapvalues(sex, c("F", "M"), c("♀", "♂"))) %>%
    ggplot(aes(x = x, y = fit)) +
    annotation_custom(grid::rectGrob(gp = grid::gpar(col = NA, fill = colour_scale[1], alpha = alphas)), xmin = -0.005, xmax = -0.005 + female_width_coords, ymin = 0.108, ymax = 0.108 + 0.0175) +
    annotation_custom(grid::rectGrob(gp = grid::gpar(col = NA, fill = colour_scale[2], alpha = alphas)), xmin = 0.24, xmax = 0.24 + male_width_coords, ymin = 0.108, ymax = 0.108 + 0.0175) +
    geom_hline(yintercept = 0, linetype = 2, size = 0.25, color = "grey20") +
    geom_line(aes(colour = sex)) +
    geom_ribbon(aes(ymax = fitmax, ymin = fitmin, fill = sex), alpha = alphas) +
    scale_fill_manual(values = colour_scale) + 
    scale_color_manual(values = colour_scale) + 
    coord_cartesian(clip = "off") +
    # geom_text(x = 0.5, y = 0, label = emo::ji("waxing_crescent_moon"), family = "EmojiOne") +
    scale_x_continuous(expand = c(0,0), breaks = seq(0,1, by = 0.25), labels = scales::number_format(accuracy = 0.01)) +
    geom_point(data = data.frame(x=c(0,1), y = 0), aes(x, y), alpha = 0) 
  
  # moon annotations
  s <- size_balls(pc3, moons_in_axis)
  pc3moon <- draw_moons(pc3, ys = yc, size_balls = s, n, fq, f, lq, offset_x = 0)
  
  # other theme stuff
  pc3moon <- pc3moon + 
    xlab("moon phase") + ylab("count deviation") +
    pub_theme() +
    theme(legend.position = "none", 
          # plot.subtitle = element_text(margin = margin(t = 0, b = 0)), 
          # plot.title = element_text(margin = margin(b = 0)), 
          axis.text.x = element_text(hjust = 0.5, margin = margin(t = 10)),
          plot.margin = grid::unit(c(0.4,0.5, 0.4, 0.4), "lines")) +
    labs(title = "A. Crab counts vs. moon phase", 
         subtitle = "asdasd")
  
  # colorful subtitle
  pc3grob <- ggplotGrob(pc3moon)
  strings <- c("Female", "and", "male", "counts peak at oppossite phases")
  pc3grob[[1]][[15]]$children[[1]]$label <- strings
  pc3grob[[1]][[15]]$children[[1]]$x <- unit(cumsum(widths1), "pt")
  pc3grob[[1]][[15]]$children[[1]]$gp$col <- c(colour_scale[1], "black", colour_scale[2], "black")
  pc3grob[[1]][[15]]$children[[1]]$gp$font <- c("plain" = c(2L, 1L, 2L, 1L))
  
  ##### Size plot
  
  intercept_s <- data.frame(sex = c("F", "M"),
                            intercept = c(m_s[[1]][[1]]$coefficients[1],
                                          m_s[[1]][[2]]$coefficients[1]))
  ps3 <- get_var_gam(m_s[[2]], 4) %>%
    dplyr::inner_join(intercept_s) %>% 
    dplyr::mutate(sex = plyr::mapvalues(sex, c("F", "M"), c("♀", "♂"))) %>%
    ggplot(aes(x = x, y = fit)) +
    # annotation_custom(grid::rectGrob(gp = grid::gpar(col = NA, fill = colour_scale[1], alpha = alphas)), xmin = 0.174, xmax = 0.33, ymin = 3.8, ymax = 4.32) +
    annotation_custom(grid::rectGrob(gp = grid::gpar(col = NA, fill = colour_scale[2], alpha = alphas)), xmin = -0.0075, xmax = -0.0075 + male_width_coords, ymin = 3.25, ymax = 3.25 + 0.52) +
    geom_hline(yintercept = 0, linetype = 2, size = 0.25, color = "grey20") +
    geom_line(aes(colour = sex)) +
    geom_ribbon(aes(ymax = fitmax, ymin = fitmin, fill = sex), alpha = alphas) +
    scale_fill_manual(values = colour_scale) +
    scale_color_manual(values = colour_scale) + 
    scale_x_continuous(expand = c(0,0), breaks = seq(0,1, by = 0.25), labels = scales::number_format(accuracy = 0.01)) +
    geom_point(data = data.frame(x=c(0,1), y = 0), aes(x, y), alpha = 0) +
    coord_cartesian(clip = "off")
  
  s <- size_balls(ps3, moons_in_axis)
  ps3moon <- draw_moons(ps3, ys, s, n, fq, f, lq)
  
  ps3moon <- ps3moon +
    xlab("moon phase") + ylab("size deviation") +
    pub_theme() +
    theme(legend.position = "none", 
          axis.text.x = element_text(hjust = 0.5, margin = margin(t = 10)),
          plot.margin = grid::unit(c(0.4,0.5, 0.4, 0.4), "lines")) +
    labs(title = "B. Crab size vs. moon phase", 
         subtitle = "Csadasdas", 
         x = "moon phase", 
         y = "size deviation [mm]")
  
  # colorful subtitle
  ps3grob <- ggplotGrob(ps3moon)
  strings <- c("Male", "size peaks on new moon")
  # widths <- c(0, 4.5, 6, 3, 4)
  ps3grob[[1]][[15]]$children[[1]]$label <- strings
  ps3grob[[1]][[15]]$children[[1]]$x <- unit(cumsum(widths2), "pt")
  ps3grob[[1]][[15]]$children[[1]]$gp$col <- c(colour_scale[2], "black")
  ps3grob[[1]][[15]]$children[[1]]$gp$font <- c("plain" = c(2L, 1L))
  
  
  cowplot::plot_grid(pc3grob, ps3grob, ncol = 1, align = "hv")
  
  # ggplot2::ggsave(drake::file_out("figs/moon_count_size.pdf"), width = 3.18, height = 4.5)
}


size_balls <- function(x, divs = 20){
  range_y <- ggplot_build(x)$layout$panel_scales_y[[1]]$range$range
  range_x <- ggplot_build(x)$layout$panel_scales_x[[1]]$range$range
  range_y <- range_y[2] - range_y[1]
  range_x <- range_x[2] - range_x[1]
  range_x/divs
}

annotate_moon <- function(image, x, y, padding_x, size_y){
  annotation_custom(image, xmin = x - size_y, xmax = x + size_y, ymin = y - padding_x, ymax = y + padding_x)
}


draw_moons <- function(plot, ys, size_balls, n, fq, f, lq, padding_x = 10, offset_x = 0){
  o <- plot + 
    annotate_moon(n, offset_x + 0, ys, padding_x, size_balls) +
    annotate_moon(fq, offset_x + 0.25, ys, padding_x, size_balls) + 
    annotate_moon(f, offset_x + 0.5, ys, padding_x, size_balls) +
    annotate_moon(lq, offset_x + 0.75, ys, padding_x, size_balls) +
    annotate_moon(n, offset_x + 1, ys, padding_x, size_balls)
  
  return(o)
}