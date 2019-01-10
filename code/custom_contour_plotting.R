
#' @importFrom grid is.unit unit
.unit_ifnot <- function(x, unit) {
  if (!is.unit(x)) {
    x <- unit(x, unit)
  }
  x
}


.derv <- function(x, y, order = 1, cyclical = FALSE, fill = FALSE) {
  N <- length(x)
  d <- y[2] - y[1]
  if (order >= 3) {
    dxdy <- .derv(.derv(x, y, order = 2, cyclical = cyclical, fill = fill),
                  y, order = order - 2, cyclical = cyclical, fill = fill)
  } else {
    if (order == 1) {
      dxdy <- (x[c(2:N, 1)] - x[c(N, 1:(N-1))])/(2*d)
    } else if (order == 2) {
      dxdy <- (x[c(2:N, 1)] + x[c(N, 1:(N-1))] - 2*x)/(d)^2
    }
    if (!cyclical) {
      if (!fill) {
        dxdy[c(1, N)] <- NA
      }
      if (fill) {
        dxdy[1] <- (-11/6*x[1] + 3*x[2] - 3/2*x[3] + 1/3*x[4])/d
        dxdy[N] <- (11/6*x[N] - 3*x[N-1] + 3/2*x[N-2] - 1/3*x[N-3])/d
      }
    }
    
  }
  return(dxdy)
}



#' @rdname geom_text_contour
#' @export
geom_label_contour <- function(mapping = NULL, data = NULL,
                               stat = "text_contour", position = "identity",
                               ...,
                               min.size = 5,
                               skip = 0,
                               parse = FALSE,
                               nudge_x = 0,
                               nudge_y = 0,
                               label.padding = unit(0.25, "lines"),
                               label.r = unit(0.15, "lines"),
                               label.size = 0.25,
                               na.rm = FALSE,
                               show.legend = NA,
                               inherit.aes = TRUE) {
  if (!missing(nudge_x) || !missing(nudge_y)) {
    if (!missing(position)) {
      stop("Specify either `position` or `nudge_x`/`nudge_y`",
           call. = FALSE)
    }
    
    position <- position_nudge(nudge_x, nudge_y)
  }
  
  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomLabelContour,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      min.size = min.size,
      skip = skip,
      parse = parse,
      label.padding = label.padding,
      label.r = label.r,
      label.size = label.size,
      na.rm = na.rm,
      ...
    )
  )
}

#' @rdname geom_text_contour
#' @usage NULL
#' @format NULL
#' @export
GeomLabelContour <- ggplot2::ggproto("GeomLabelContour", ggplot2::Geom,
                                     required_aes = c("x", "y", "label"),
                                     default_aes = ggplot2::aes(
                                       colour = "black", fill = "white", size = 3.88, angle = 0,
                                       hjust = 0.5, vjust = 0.5, alpha = NA, family = "", fontface = 1,
                                       lineheight = 1.2
                                     ),
                                     
                                     draw_panel = function(self, data, panel_params, coord, parse = FALSE,
                                                           na.rm = FALSE,
                                                           label.padding = unit(0.25, "lines"),
                                                           label.r = unit(0.15, "lines"),
                                                           label.size = 0.25, min.size = 20,
                                                           skip = 1, gap = 0) {
                                       data <- data.table::as.data.table(coord$transform(data, panel_params))
                                       min.size <- ceiling(min.size)
                                       if (min.size %% 2 == 0) {
                                         min.size <- min.size - 1
                                       }
                                       # Get points of labels
                                       data <- .label.position(copy(data), min.size, skip, rotate = FALSE)
                                       
                                       lab <- data$label
                                       if (parse) {
                                         lab <- parse(text = as.character(lab))
                                       }
                                       
                                       if (is.character(data$vjust)) {
                                         data$vjust <- ggplot2:::compute_just(data$vjust, data$y)
                                       }
                                       if (is.character(data$hjust)) {
                                         data$hjust <- ggplot2:::compute_just(data$hjust, data$x)
                                       }
                                       
                                       grobs <- lapply(seq_len(nrow(data)), function(i) {
                                         row <- data[i, , drop = FALSE]
                                         ggplot2:::labelGrob(lab[i],
                                                             x = unit(row$x, "native"),
                                                             y = unit(row$y, "native"),
                                                             just = c(row$hjust, row$vjust),
                                                             padding = label.padding,
                                                             r = label.r,
                                                             text.gp = grid::gpar(
                                                               col = row$colour,
                                                               fontsize = row$size * .pt,
                                                               fontfamily = row$family,
                                                               fontface = row$fontface,
                                                               lineheight = row$lineheight
                                                             ),
                                                             rect.gp = grid::gpar(
                                                               col = row$colour,
                                                               fill = alpha(row$fill, row$alpha),
                                                               lwd = label.size * .pt
                                                             )
                                         )
                                       })
                                       class(grobs) <- "gList"
                                       
                                       ggplot2:::ggname("geom_label_contour", grid::grobTree(children = grobs))
                                     },
                                     
                                     draw_key = ggplot2::draw_key_label
)

#' Label contours
#'
#' Draws labels on contours built with [ggplot2::stat_contour].
#'
#' @inheritParams ggplot2::geom_text
#' @inheritParams ggplot2::geom_label
#' @inheritParams geom_contour2
#' @param min.size minimum number of points for a contour to be labeled.
#' @param skip number of contours to skip
#' @param rotate logical indicating whether to rotate text following the contour.
#' @param stroke numerical indicating width of stroke relative to the size of
#' the text. Ignored if less than zero.
#' @param stroke.color any valid color.
#'
#' @details
#' Is best used with a previous call to [ggplot2::stat_contour] with the same
#' parameters.
#' Note that while `geom_text_contour()` can angle itself to follow the contour,
#' this is not the case with `geom_label_contour()`.
#'
#' @examples
#' library(ggplot2)
#' v <- data.table::melt(volcano)
#' g <- ggplot(v, aes(Var1, Var2)) +
#'        geom_contour(aes(z = value))
#' g + geom_text_contour(aes(z = value))
#'
#' g + geom_text_contour(aes(z = value), stroke = 0.2)
#'
#' @section Aesthetics:
#' \code{geom_text_contour} understands the following aesthetics (required aesthetics are in bold):
#'
#' \itemize{
#' \item \strong{x}
#' \item \strong{y}
#' \item \strong{label}
#' \item \code{alpha}
#' \item \code{angle}
#' \item \code{colour}
#' \item \code{family}
#' \item \code{fontface}
#' \item \code{group}
#' \item \code{hjust}
#' \item \code{lineheight}
#' \item \code{size}
#' \item \code{vjust}
#'}
#'
#'
#' @export
#' @import ggplot2 data.table
#' @family ggplot2 helpers
geom_text_contour <- function(mapping = NULL, data = NULL,
                              stat = "text_contour",
                              position = "identity",
                              ...,
                              min.size = 5,
                              skip = 0,
                              rotate = TRUE,
                              parse = FALSE,
                              nudge_x = 0,
                              nudge_y = 0,
                              stroke = 0,
                              stroke.color = "white",
                              check_overlap = FALSE,
                              # xwrap = NULL,
                              # ywrap = NULL,
                              na.rm = FALSE,
                              show.legend = NA,
                              inherit.aes = TRUE)
{
  if (!missing(nudge_x) || !missing(nudge_y)) {
    if (!missing(position)) {
      stop("Specify either `position` or `nudge_x`/`nudge_y`",
           call. = FALSE)
    }
    
    position <- position_nudge(nudge_x, nudge_y)
  }
  
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomTextContour,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      skip = skip,
      min.size = min.size,
      rotate = rotate,
      parse = parse,
      check_overlap = check_overlap,
      stroke = stroke,
      stroke.color = stroke.color,
      # xwrap = xwrap,
      # ywrap = ywrap,
      na.rm = na.rm,
      ...
    )
  )
}

#' @rdname geom_text_contour
#' @usage NULL
#' @format NULL
#' @export
# #' @importFrom shadowtext shadowtextGrob
GeomTextContour <- ggplot2::ggproto("GeomTextContour", ggplot2::Geom,
                                    required_aes = c("x", "y", "label"),
                                    default_aes = ggplot2::aes(colour = "black", size = 3.88, angle = 0,
                                                               hjust = 0.5, vjust = 0.5, alpha = NA, family = "",
                                                               fontface = 1, lineheight = 1.2),
                                    
                                    draw_panel = function(data, panel_params, coord, parse = FALSE,
                                                          na.rm = FALSE, check_overlap = FALSE, min.size = 20,
                                                          skip = 1, rotate = FALSE, gap = NULL,
                                                          stroke = 0, stroke.colour = "white") {
                                      data <- data.table::as.data.table(coord$transform(data, panel_params))
                                      min.size <- ceiling(min.size)
                                      if (min.size %% 2 == 0) {
                                        min.size <- min.size - 1
                                      }
                                      # Get points of labels
                                      data <- .label.position(copy(data), min.size, skip, rotate)
                                      
                                      ## Original ggplot2 here.
                                      lab <- data$label
                                      if (parse) {
                                        lab <- parse(text = as.character(lab))
                                      }
                                      
                                      if (is.character(data$vjust)) {
                                        data$vjust <- ggplot2:::compute_just(data$vjust, data$y)
                                      }
                                      if (is.character(data$hjust)) {
                                        data$hjust <- ggplot2:::compute_just(data$hjust, data$x)
                                      }
                                      
                                      shadowtext(
                                        lab,
                                        data$x, data$y, default.units = "native",
                                        hjust = data$hjust, vjust = data$vjust,
                                        dx = data$dx, dy = data$dy,
                                        bg.r = stroke, bg.color = stroke.colour,
                                        gp = grid::gpar(
                                          col = alpha(data$colour, data$alpha),
                                          fontsize = data$size * .pt,
                                          fontfamily = data$family,
                                          fontface = data$fontface,
                                          lineheight = data$lineheight
                                        ),
                                        check.overlap = check_overlap
                                      )
                                      
                                    },
                                    
                                    draw_key = ggplot2::draw_key_text
)


.cont.angle <- function(x, y, gap) {
  N <- length(x)
  gap <- ceiling((gap-1)/2)
  fill <- rep(NA, gap/2)
  dx <- c(fill, diff(x, gap), fill)
  dy <- c(fill, diff(y, gap), fill)
  angle <- atan2(dy, dx)*180/pi
  angle <- ifelse(angle > 180, angle - 180, angle)
  angle <- ifelse(angle > 90, angle - 180, angle)
  angle <- ifelse(angle < -90, angle + 180, angle)
  angle
}

.label.position <- function(data, min.size, skip, rotate) {
  data <- as.data.table(data)
  breaks <- unique(data$level)
  breaks.cut <- breaks[seq(1, length(breaks), by = skip + 1)]
  data <- data[level %in% breaks.cut]
  data[, id := seq_len(.N), by = piece]
  data[, c("dx", "dy") := .(.derv(x, id), .derv(y, id)),
       by = .(piece)]
  
  # Safety strip around the edges (10%)
  safe <- c(0, 1) + 0.1*c(+1, -1)
  data <- data[x %between% safe &
                 y %between% safe]
  
  data[, N := .N, by = piece]
  data <- data[N >= min.size]
  
  # if (rotate == TRUE) {
  data[, angle := .cont.angle(x, y, min.size), by = piece]
  # }
  
  # Check if point has minimum variance
  # data[, var := minvar(x, y), by = .(piece)]
  # data[, curvature := .curvature(x, y), by = piece]
  # data <- data[data[, .I[which.min(abs(curvature))], by = piece]$V1]
  # data[is.na(var), var := FALSE]
  # data <- data[var == TRUE][, head(.SD, 1), by = piece]
  
  data[, min := straight(x, y, min.size), by = piece]
  # data <- data[min == TRUE]
  # data[, min := angle == min(angle, na.rm = T), by = piece]
  # data[, min := atan2(dy, dx) == min(atan2(dy, dx), na.rm = TRUE), by = piece]
  data <- data[!is.na(dx + dy)]
  if (rotate == FALSE) data[, c("dx", "dy") := .(0, 0)]
  
  return(data[min == TRUE, head(.SD, 1), by = piece])
}

straight <- function(x, y, gap) {
  N <- length(x)
  gap <- ceiling((gap-1)/2)
  if (N < gap) {
    gap <- N
  }
  if (is_closed(x, y)) {
    x <- c(x[(N-gap):(N-1)], x, x[2:(gap+1)])
    y <- c(y[(N-gap):(N-1)], y, y[2:(gap+1)])
    var <- vapply((gap+1):(N+gap), function(i) {
      dx <- diff(x[(i-gap):(i+gap)])
      dy <- diff(y[(i-gap):(i+gap)])
      a <- dy/dx
      var(a)
    }, FUN.VALUE = 1)
  } else {
    var <- c(rep(NA, gap), vapply((gap+1):(N-gap), function(i) {
      dx <- diff(x[(i-gap):(i+gap)])
      dy <- diff(y[(i-gap):(i+gap)])
      a <- dy/dx
      var(a)
    }, FUN.VALUE = 1), rep(NA, gap))
  }
  return(var == min(var, na.rm = TRUE))
}

# from https://stackoverflow.com/questions/21868353/drawing-labels-on-flat-section-of-contour-lines-in-ggplot2
minvar <- function (x, y){
  if (length(x) < 4) return(rep(FALSE, length(x)))
  N <- length(x)
  xdiffs <- c(NA, diff(x)) #c(NA, x[3:N] - x[1:(N-2)], NA)
  ydiffs <- c(NA, diff(y)) #c(NA, y[3:N] - y[1:(N-2)], NA)
  avgGradient <- ydiffs/xdiffs
  variance <- abs(avgGradient)
  # squareSum <- avgGradient * avgGradient
  # variance <- (squareSum - (avgGradient * avgGradient) / N) / N
  # change!! this causes problems if length(variance) < 4
  variance <- c(NA, variance[2:(N-1)], NA)
  return(variance == min(variance, na.rm = TRUE))
}

#' Impute missing values by linear or constant interpolation
#'
#' Provides methods for (soft) imputation of missing values.
#'
#' @inheritParams Interpolate
#' @param method "interpolate" for interpolation, a numeric for constant imputation
#' or a function that takes a vector and returns a number (like [mean])
#'
#' @details
#' This is "soft" imputation because the imputed values are not supposed to be
#' representative of the missing data but just filling for algorithms that need
#' complete data (in particular, contouring). The method used if
#' `method = "interpolate"` is to do simple linear interpolation in both the x and y
#' direction and then average the result.
#'
#' This is the imputation method used by [geom_contour_fill()].
#'
#' @export
#' @import data.table
Impute2D <- function(formula, data = NULL, method = "interpolate") {
  checks <- makeAssertCollection()
  assertClass(formula, "formula", add = checks)
  assertDataFrame(data, null.ok = TRUE, add = checks)
  assert(
    checkClass(method, "character"),
    checkClass(method, "numeric"),
    checkClass(method, "function"))
  
  if (!is.function(method)) {
    assertVector(method, len = 1, add = checks)
  }
  
  reportAssertions(checks)
  
  dep.names <- formula.tools::lhs.vars(formula)
  if (length(dep.names) == 0) stop("LHS of formula must have at least one variable")
  
  ind.names <- formula.tools::rhs.vars(formula)
  if (length(ind.names) > 2) {
    stop("RHS of formula must be of the form x + y")
  }
  
  formula <- Formula::as.Formula(formula)
  data <- data.table::as.data.table(eval(quote(model.frame(formula, data = data,
                                                           na.action = NULL))))
  
  if (method == "interpolate") method <- TRUE
  
  for (var in dep.names) {
    data_sub <- data[, c(ind.names, var), with = FALSE]
    data.table::setnames(data_sub, c(ind.names, var), c("x", "y", "z"))
    set(data, NULL, var, .impute_data(data_sub, na.fill = method, verbose = FALSE)$z)
  }
  
  return(as.list(data[, dep.names, with = FALSE]))
}


soft_approx <- function(x, y = NULL, xout = x) {
  if (sum(!is.na(y)) < 2) {
    return(rep(NA_real_, length(xout)))
  }
  approx(x, y, xout = xout, rule = 2)$y
}

soft_approx2d <- function(x, y, z) {
  dt <- data.table(x, y, z)
  dt[, z1 := soft_approx(x, z), by = y][, z2 := soft_approx(y, z), by = x]
  dt[, mean(c(z1, z2), na.rm = TRUE), by = .(x, y)]$V1
}

.impute_data <- function(data, na.fill = TRUE, verbose = TRUE) {
  nas <- nrow(data[is.na(z)])
  if (nas != 0) {
    if (isTRUE(na.fill)) {
      if(isTRUE(verbose)) warning("imputing missing values", call. = FALSE)
      data <- copy(data)[, z := soft_approx2d(x, y, z)]
    } else if (is.numeric(na.fill)) {
      if(isTRUE(verbose)) warning("imputing missing values", call. = FALSE)
      data[is.na(z), z := na.fill[1]]
    } else if (is.function(na.fill)) {
      if(isTRUE(verbose)) warning("imputing missing values", call. = FALSE)
      z.fill <- data[is.finite(z), na.fill(z)]
      data[is.na(z), z := z.fill]
    }
  }
  return(data)
}

.impute_data.m <- memoise::memoise(.impute_data)

#' Functions for making breaks
#'
#' Functions that return functions suitable to use as the `breaks` argument in
#' ggplot2's continuous scales and in [geom_contour_fill].
#'
#' @param binwidth width of breaks
#' @param bins number of bins, used if `binwidth = NULL`
#' @param exclude a vector of breaks to exclude
#' @param anchor anchor value
#'
#' @return
#' A function that takes a range as argument and a binwidth as an optional argument
#' and returns a sequence of equally spaced intervals covering the range.
#'
#' @details
#' `MakeBreaks` is essentially an export of the default way
#' [ggplot2::stat_contour] makes breaks.
#'
#' `AnchorBreaks` makes breaks starting from an `anchor` value and covering
#' the range of the data according to `binwidth`.
#'
#' @examples
#'
#' my_breaks <- MakeBreaks(10)
#' my_breaks(c(1, 100))
#' my_breaks(c(1, 100), 20)    # optional new binwidth argument ignored
#'
#' MakeBreaks()(c(1, 100), 20)  # but is not ignored if initial binwidth is NULL
#'
#' # One to one mapping between contours and breaks
#' library(ggplot2)
#' binwidth <- 20
#' ggplot(reshape2::melt(volcano), aes(Var1, Var2, z = value)) +
#'     geom_contour(aes(color = ..level..), binwidth = binwidth) +
#'     scale_color_continuous(breaks = MakeBreaks(binwidth))
#'
#' #Two ways of getting the same contours. Better use the second one.
#' ggplot(reshape2::melt(volcano), aes(Var1, Var2, z = value)) +
#'     geom_contour2(aes(color = ..level..), breaks = AnchorBreaks(132),
#'                   binwidth = binwidth) +
#'     geom_contour2(aes(color = ..level..), breaks = AnchorBreaks(132, binwidth)) +
#'     scale_color_continuous(breaks = AnchorBreaks(132, binwidth))
#'
#' @export
#' @family ggplot2 helpers
MakeBreaks <- function(binwidth = NULL, bins = 10, exclude = NULL) {
  function(range, binwidth2) {
    if (!is.null(binwidth)) binwidth2 <- binwidth
    
    # If no parameters set, use pretty bins
    if (is.null(binwidth2)) {
      b <- pretty(range, bins)
      return(b[!(b %in% exclude)])
    } else {
      b <- scales::fullseq(range, binwidth2)
      b[!(b %in% exclude)]
    }
  }
}

#' @rdname MakeBreaks
#' @export
AnchorBreaks <- function(anchor = 0, binwidth = NULL, exclude = NULL) {
  function(x, binwidth2) {
    if (!is.null(binwidth)) binwidth2 <- binwidth
    mult <- ceiling((x[1] - anchor)/binwidth2) - 1L
    start <- anchor + mult*binwidth2
    b <- seq(start, x[2] + binwidth2, binwidth2)
    b[!(b %in% exclude)]
  }
}


# InScale <- function(fun, binwidith, ...) {
#     function(range){
#         fun(range, binwidth, ...)
#     }
# }
#
# InContour <- function(fun, ...) {
#     function(range, binwidth) {
#         fun(range, ...)
#     }
# }


#' Extended logical operators
#'
#' Extended binary operators for easy subsetting.
#'
#' @param x,target numeric vectors
#' @param tol tolerance for similarity
#'
#'
#' @return
#' A logical vector of the same length of x.
#'
#' @details
#' \code{\%~\%} can be thought as a "similar" operator. It's a fuzzy version of
#' \code{\link{\%in\%}} in that returns \code{TRUE} for the element of \code{x}
#' which is the (first) closest to any element of \code{target}.
#'
#' \code{Similar} is a functional version of \code{\%~\%} that also has a
#' \code{tol} parameter that indicates the maximum allowed tolerance.
#'
#' @examples
#' set.seed(198)
#' x <- rnorm(100)
#' x[x %~% c(0.3, 0.5, 1)]
#'
#' # Practical use case: vertical cross-section at
#' # approximately 36W between 50S and 50N.
#' cross.lon <- -34 + 360
#' library(ggplot2)
#' library(data.table)
#' ggplot(temperature[lon %~% cross.lon & lat %between% c(-50, 50)],
#'        aes(lat, lev)) +
#'     geom_contour(aes(z = air))
#'
#' @family utilities
#' @name logic
#' @export
`%~%` <- function(x, target) {
  r <- rep(FALSE, length(x))
  for (i in seq_along(target)) {
    dif <- abs(as.numeric(x - target[i]))
    x.select <- x[which.min(dif)]
    r <- r | (x == x.select)
  }
  return(r)
}

# from shadowtext
#' @importFrom grid gList gTree
shadowtext <- function(label, x = unit(0.5, "npc"), y = unit(0.5, "npc"),
                       just = "centre", hjust = NULL, vjust = NULL,
                       dx = 0, dy = 0,
                       check.overlap = FALSE,
                       default.units = "npc", name = NULL, gp = gpar(col = "white"),
                       vp = NULL, bg.color = "black", bg.r = 0.1) {
  upperGrob <- textContourGrob(label = label, x = x, y = y, just = just,
                               hjust = hjust, vjust = vjust,
                               dx = dx, dy = dy,
                               default.units = default.units,
                               check.overlap = check.overlap, name = name, gp = gp,
                               vp = vp)
  if (is.null(bg.color))
    return(upperGrob)
  gp$col <- bg.color
  theta <- seq(pi/8, 2 * pi, length.out = 16)
  char <- substring(label[1], 1, 1)
  r <- bg.r[1]
  if (r != 0) {
    bgList <- lapply(theta, function(i) {
      if (!is.unit(x))
        x <- unit(x, default.units)
      if (!is.unit(y))
        y <- unit(y, default.units)
      x <- x + unit(cos(i) * r, "strwidth", data = char)
      y <- y + unit(sin(i) * r, "strheight", data = char)
      textContourGrob(label = label, x = x, y = y, just = just, hjust = hjust,
                      vjust = vjust,
                      dx = dx, dy = dy,
                      default.units = default.units,
                      check.overlap = check.overlap, name = name, gp = gp,
                      vp = vp)
    })
  } else {
    bgList <- list()
  }
  bgGrob <- do.call(gList, bgList)
  grobs <- gList(bgGrob, upperGrob)
  gTree(children = grobs)
}


#' @importFrom grid grob
textContourGrob <- function(label, x = unit(0.5, "npc"), y = unit(0.5, "npc"),
                            just = "centre", hjust = NULL, vjust = NULL,
                            dx = 0, dy = 0,
                            bg.r = 0.1, bg.color = "black",
                            check.overlap = FALSE, default.units = "npc",
                            name = NULL, gp = gpar(), vp = NULL) {
  x <- .unit_ifnot(x, default.units)
  y <- .unit_ifnot(y, default.units)
  
  dx <- .unit_ifnot(dx, default.units)
  dy <- .unit_ifnot(dy, default.units)
  
  grob(label = label, x = x, y = y, just = just, hjust = hjust,
       vjust = vjust,
       dx = dx, dy = dy,
       bg.r = bg.r, bg.color = bg.color,
       check.overlap = check.overlap,
       name = name, gp = gp, vp = vp, cl = "textContour")
}

#' @importFrom grid convertX convertY convertWidth convertHeight unit.c
#' @export
makeContent.textContour <- function(x) {
  require(grid)
  x$x <- convertX(x$x, 'mm')
  x$y <- convertY(x$y, 'mm')
  x$dx <- convertWidth(x$dx, 'mm')
  x$dy <- convertHeight(x$dy, 'mm')
  
  x$rot <- atan2(as.numeric(x$dy), as.numeric(x$dx))*180/pi
  
  x$rot <- ifelse(x$rot > 180, x$rot - 180, x$rot)
  x$rot <- ifelse(x$rot > 90, x$rot - 180, x$rot)
  x$rot <- ifelse(x$rot < -90, x$rot + 180, x$rot)
  
  x$cl <- "text"
  class(x)[1] <- "text"
  x
}
#' @rdname geom_contour_fill
#' @export
#' @import sp
#' @import ggplot2
stat_contour_fill <- function(mapping = NULL, data = NULL,
                              geom = "polygon", position = "identity",
                              ...,
                              breaks = MakeBreaks(),
                              bins = NULL,
                              binwidth = NULL,
                              # na.rm = FALSE,
                              na.fill = FALSE,
                              # xwrap = NULL,
                              # ywrap = NULL,
                              show.legend = NA,
                              inherit.aes = TRUE) {
  .check_wrap_param(list(...))
  layer(
    data = data,
    mapping = mapping,
    stat = StatContourFill,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = FALSE,
      na.fill = na.fill,
      breaks = breaks,
      bins = bins,
      binwidth = binwidth,
      # xwrap = xwrap,
      # ywrap = ywrap,
      ...
    )
  )
}

#' @import ggplot2
#' @import scales
#' @rdname geom_contour_fill
#' @usage NULL
#' @format NULL
#' @export
StatContourFill <- ggplot2::ggproto("StatContourFill", ggplot2::Stat,
                                    required_aes = c("x", "y", "z"),
                                    default_aes = ggplot2::aes(fill = ..int.level..),
                                    setup_params = function(data, params) {
                                      # Check is.null(breaks) for backwards compatibility
                                      if (is.null(params$breaks)) {
                                        params$breaks <- scales::fullseq
                                      }
                                      if (is.function(params$breaks)) {
                                        # If no parameters set, use pretty bins to calculate binwidth
                                        if (is.null(params$bins) && is.null(params$binwidth)) {
                                          params$binwidth <- diff(pretty(range(data$z, na.rm = TRUE), 10))[1]
                                        }
                                        # If provided, use bins to calculate binwidth
                                        if (!is.null(params$bins)) {
                                          params$binwidth <- diff(range(data$z, na.rm = TRUE)) / params$bins
                                        }
                                        
                                        params$breaks <- params$breaks(range(data$z, na.rm = TRUE), params$binwidth)
                                      }
                                      return(params)
                                    },
                                    compute_layer = function(self, data, params, layout) {
                                      ggplot2:::check_required_aesthetics(
                                        self$required_aes,
                                        c(names(data), names(params)),
                                        ggplot2:::snake_class(self)
                                      )
                                      
                                      # Trim off extra parameters
                                      params <- params[intersect(names(params), self$parameters())]
                                      
                                      args <- c(list(data = quote(data), scales = quote(scales)), params)
                                      plyr::ddply(data, "PANEL", function(data) {
                                        scales <- layout$get_scales(data$PANEL[1])
                                        tryCatch(do.call(self$compute_panel, args), error = function(e) {
                                          warning("Computation failed in `", ggplot2:::snake_class(self),
                                                  "()`:\n",
                                                  e$message, call. = FALSE)
                                          data.frame()
                                        })
                                      })
                                    },
                                    compute_group = function(data, scales, bins = NULL, binwidth = NULL,
                                                             breaks = scales::fullseq, complete = TRUE,
                                                             na.rm = FALSE, xwrap = NULL,
                                                             ywrap = NULL, na.fill = FALSE) {
                                      setDT(data)
                                      data <- data[!(is.na(y) | is.na(x)), ]
                                      
                                      if (isFALSE(na.fill)) {
                                        data <- data[!is.na(z), ]
                                      }
                                      
                                      # Check if is a complete grid
                                      nx <- data[, uniqueN(x), by = y]$V1
                                      ny <- data[, uniqueN(y), by = x]$V1
                                      
                                      complete.grid <- abs(max(nx) - min(nx)) == 0 & abs(max(ny) - min(ny)) == 0
                                      
                                      if (complete.grid == FALSE) {
                                        if (complete == FALSE) {
                                          warning("data must be a complete regular grid", call. = FALSE)
                                          return(data.frame())
                                        } else {
                                          data <- setDT(tidyr::complete(data, x, y, fill = list(z = NA)))
                                        }
                                      }
                                      
                                      data <- .impute_data(data, na.fill)
                                      
                                      nas <- nrow(data[is.na(z)])
                                      if (nas != 0) {
                                        warning("data must not have missing values. Use na.fill = TRUE or impute them before plotting.", call. = FALSE)
                                        return(data.frame())
                                      }
                                      
                                      if (!is.null(xwrap)) {
                                        data <- suppressWarnings(WrapCircular(data, "x", xwrap))
                                      }
                                      if (!is.null(ywrap)) {
                                        data <- suppressWarnings(WrapCircular(data, "y", ywrap))
                                      }
                                      
                                      # jitter <- diff(range(data$z))*0.000000
                                      # data$z <- data$z + rnorm(nrow(data))*jitter
                                      setDF(data)
                                      mean.z <- data$z[data$z %~% mean(data$z)][1]
                                      
                                      # mean.z <- min(breaks) - 100*resolution(breaks, zero = FALSE)
                                      range.data <- as.data.frame(vapply(data[c("x", "y", "z")], range, c(1, 2)))
                                      
                                      # Expand data by 1 unit in all directions.
                                      data <- .expand_data(data, mean.z)
                                      
                                      # Make contours
                                      cont <- data.table::setDT(.contour_lines(data, breaks, complete = complete))
                                      
                                      if (length(cont) == 0) {
                                        warning("Not possible to generate contour data", call. = FALSE)
                                        return(data.frame())
                                      }
                                      
                                      # Ugly hack for joining disjointed contours
                                      cont <- .join_contours.m(cont)
                                      
                                      # Calculate inner fill
                                      cont <- .inner_fill.m(cont, data, breaks)
                                      
                                      # Add bounding contour, if necessary.
                                      if (data.table::between(mean.z, min(breaks), max(breaks))) {
                                        mean.level <- breaks[breaks %~% mean.z]
                                        
                                        i <-  which(breaks == mean.level)
                                        correction <- sign(mean.z - mean.level)
                                        if (correction == 0) correction <- 1
                                        correction <- (breaks[i + correction] - mean.level)/2
                                        
                                        # Adds bounding contour
                                        setDT(data)
                                        Nx <- data.table::uniqueN(data$x)
                                        Ny <- data.table::uniqueN(data$y)
                                        
                                        x <- c(rep(range.data$x[1], Ny),
                                               sort(data[y == range.data$y[2], x]),
                                               rep(range.data$x[2], Ny),
                                               sort(data[y == range.data$y[1], x], decreasing = TRUE))
                                        y <- c(sort(data[x == range.data$x[1], y]),
                                               rep(range.data$y[2], Nx),
                                               sort(data[x == range.data$x[2], y], decreasing = TRUE),
                                               rep(range.data$y[1], Nx))
                                        
                                        mean.cont  <- data.frame(
                                          level = mean.level,
                                          x = x,
                                          y = y,
                                          piece = max(cont$piece) + 1,
                                          int.level = mean.level + correction)
                                        mean.cont$group <- factor(paste("", sprintf("%03d", mean.cont$piece), sep = "-"))
                                        cont <- rbind(cont, mean.cont)
                                      }
                                      
                                      # Move contours to range of original data
                                      cont$x[cont$x > range.data$x[2]] <- range.data$x[2]
                                      cont$x[cont$x < range.data$x[1]] <- range.data$x[1]
                                      cont$y[cont$y < range.data$y[1]] <- range.data$y[1]
                                      cont$y[cont$y > range.data$y[2]] <- range.data$y[2]
                                      
                                      # Order pieces according to area.
                                      cont <- .order_fill(cont)
                                      cont
                                    }
)

.order_fill <- function(cont) {
  areas <- cont[, .(area = abs(area(x, y))), by = .(piece)][
    , rank := frank(-area, ties.method = "first")]
  
  cont <- cont[areas, on = "piece"]
  area.back <- areas[piece == max(cont$piece), area]
  if (any(areas[piece != max(cont$piece), area] == area.back)) {
    cont <- cont[piece != max(cont$piece)]
  }
  cont[, piece := rank]
  cont[, group := factor(paste("", sprintf("%03d", piece), sep = "-"))]
  cont
}

.expand_data <- function(data, fill) {
  dx <- ggplot2::resolution(subset(data, y == data$y[1])$x)
  dy <- ggplot2::resolution(subset(data, x == data$x[1])$y)
  
  #Extender para grilla rectangular.
  range.data <- as.data.frame(vapply(data[c("x", "y", "z")], range, c(1,2)))
  extra <- rbind(expand.grid(y = c(range.data$y[2] + dy,
                                   range.data$y[1] - dy),
                             x = unique(data$x)),
                 expand.grid(y = c(unique(data$y), range.data$y[2] + dy,
                                   range.data$y[1] - dy),
                             x = c(range.data$x[1] - dx, range.data$x[2] + dx)))
  
  extra$z <- fill
  
  rbind(data[c("x", "y", "z")], extra)
}

# area
# clockwise > 0, counterclockwise < 0
# From http://stackoverflow.com/questions/1165647
area <- function(x, y) {
  xdiff <- c(x[-1], x[1]) - x
  ysum <- c(y[-1], y[1]) + y
  sum(xdiff * ysum)/2
}


#' @import data.table
.inner_fill <- function(cont, data, breaks) {
  # Add one more to breaks extremes
  m.level <- -breaks[2] + 2*breaks[1]
  M.level <- 2*breaks[length(breaks)] - breaks[length(breaks) - 1]
  breaks <- c(m.level, breaks, M.level)
  
  cont[, int.level := 0]
  pieces <- unique(cont$piece)
  
  # Data properties
  data <- data.table::as.data.table(data)
  x.data <- unique(data$x)
  x.data <- x.data[order(x.data)]
  x.N <- length(x.data)
  y.data <- unique(data$y)
  y.data <- y.data[order(y.data)]
  y.N <- length(y.data)
  
  # Make correction for each piece
  for (p in pieces) {
    level <- cont[piece == p, level[1]]
    
    i <- which(breaks == level)
    cur.piece <- cont[piece == p]
    
    # Get one point in piece
    p0 <- cur.piece[x >= x.data[2] & x <= x.data[x.N-1]
                    & y >= y.data[2] & y <= y.data[y.N-1]][1]
    
    if (nrow(p0[!is.na(x)]) == 0) {
      inside.z <- level
    } else {
      # If p0 is in the x grid of the data
      if (p0$x %in% x.data) {
        # Search for the closest data points up and down
        y1 <- Closest(y.data, p0$y, sign = 1)
        y2 <- Closest(y.data, p0$y, sign = -1)
        p1 <- data[x == p0$x & y == y1]
        p2 <- data[x == p0$x & y == y2]
        # If p0 is in the y grid of the data
      } else {
        # Search for the closest data points left and right
        x1 <- Closest(x.data, p0$x, sign = 1)
        x2 <- Closest(x.data, p0$x, sign = -1)
        p1 <- data[x == x1 & y == p0$y]
        p2 <- data[x == x2 & y == p0$y]
      }
      
      points <- rbind(p1, p2)
      # Get one point whose value is NOT equal to the contour level.
      points[, equal := z == level]
      points <- points[equal == FALSE][1]
      points[, inside := IsInside(x, y, cur.piece$x, cur.piece$y)]
    }
    
    # Change sign of correction if point is outside.
    corr.sign <- as.numeric(points$inside)*2 - 1
    
    correction <- (breaks[i + sign(points$z - level)*corr.sign] - level)/2
    
    cont[piece == p, int.level := level + correction]
  }
  return(cont)
}

.inner_fill.m <- memoise::memoise( .inner_fill)

Closest <- function(x, target, sign = c(1, -1)) {
  tmp <- (x - target)*sign[1]
  tmp[tmp < 0] <- NA
  x[which.min(abs(tmp))]
}


IsInside <- function(xp, yp, x, y) {
  !(sp::point.in.polygon(xp, yp, x, y) == 0)
}

is_closed <- function(x, y) {
  (x[length(x)] == x[1]) & (y[length(x)] == y[1])
}

# nocov start
close_path <- function(x, y, range_x, range_y) {
  L <- length(x)
  if ((x[1] == x[L] & x[1] %in% range_x) |
      (y[1] == y[L] & y[1] %in% range_y)) {
    
  } else if (sum(x %in% range_x)*sum(y %in% range_y) != 0) {
    x[L + 1] <- x[x %in% range_x]
    y[L + 1] <- y[y %in% range_y]
  } else if (sum(y %in% range_y) == 0) {
    x[L + 1] <- x[L]
    y[L + 1] <- range_y[1]    # arbitrary
    
    y[L + 2] <- y[L + 1]
    x[L + 2] <- x[1]
  } else if (sum(x %in% range_x) == 0) {
    y[L + 1] <- y[L]
    x[L + 1] <- range_x[1]    # arbitrary
    
    y[L + 2] <- y[1]
    x[L + 2] <- x[L + 1]
  }
  L <- length(x)
  x[L + 1] <- x[1]
  y[L + 1] <- y[1]
  return(list(x = x, y = y))
}
# nocov end


.join_contours <- function(contours) {
  #join small segments
  # For now, lets asume that the problem occurs with only 1 contour.
  # If the case arises that more than one contour is separated into pieces,
  # it will be a problem for my future self.
  # (Oh, hi, future self!)
  
  contours[, close := is_closed(x, y), by = piece]
  contours2 <- contours[close == FALSE]
  contours[, close := NULL]
  if (nrow(contours2) == 0) return(contours)
  contours2[, close := NULL]
  contours2[, first := (1 == 1:.N), by = piece]
  pieces <- unique(contours2[, piece])
  
  cont <- contours2[piece == pieces[1]]
  p <- pieces[1]
  for(i in seq_along(pieces)) {
    last <- cont[, .(x = x[.N], y = y[.N])]
    # search for next piece
    next.point <- contours2[x == last$x & y == last$y & piece != p]
    
    if (nrow(next.point) == 0) {
      warning("some contours may not have closed correctly", call. = FALSE)
      return(contours)
    }
    
    # if (nrow(next.point) == 0) break
    next.piece <- contours2[piece == next.point$piece]
    
    if (next.piece$piece[1] == pieces[1]) break
    
    cont <- rbindlist(list(cont, .reverse(next.piece, next.point)))
    p <- next.piece[1, piece]
  }
  cont[, piece := max(contours$piece) + 1]
  cont <- rbindlist(list(contours[close == TRUE], cont))
  cont[, close := NULL]
  cont
}

.join_contours.m <- memoise::memoise(.join_contours)


.reverse <- function(cont, point) {
  if (identical(cont[1], point)) {
    return(cont)
  } else {
    return(cont[.N:1])
  }
}

#' @inheritParams ggplot2::stat_identity
#' @inheritParams geom_contour_fill
#' @param breaks One of:
#'   - A numeric vector of breaks
#'   - A function that takes the range of the data and binwidth as input
#'   and returns breaks as output
#' @param bins Number of evenly spaced breaks.
#' @param binwidth Distance between breaks.
# #' @param xwrap,ywrap vector of length two used to wrap the circular dimension
#'
#' @export
#' @section Computed variables:
#' \describe{
#'  \item{level}{height of contour}
#' }
#' @rdname geom_contour2
#' @family ggplot2 helpers
stat_contour2 <- function(mapping = NULL, data = NULL,
                          geom = "contour2", position = "identity",
                          ...,
                          breaks = MakeBreaks(),
                          bins = NULL,
                          binwidth = NULL,
                          na.rm = FALSE,
                          na.fill = FALSE,
                          # fill.linear = TRUE,
                          # xwrap = NULL,
                          # ywrap = NULL,
                          show.legend = NA,
                          inherit.aes = TRUE) {
  .check_wrap_param(list(...))
  layer(
    data = data,
    mapping = mapping,
    stat = StatContour2,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      na.fill = na.fill,
      # fill.linear = fill.linear,
      breaks = breaks,
      bins = bins,
      binwidth = binwidth,
      # xwrap = xwrap,
      # ywrap = ywrap,
      ...
    )
  )
}

#' @rdname geom_contour2
#' @usage NULL
#' @format NULL
#' @export
StatContour2 <- ggplot2::ggproto("StatContour2", ggplot2::Stat,
                                 required_aes = c("x", "y", "z"),
                                 default_aes = ggplot2::aes(order = ..level..),
                                 setup_params = function(data, params) {
                                   # Check is.null(breaks) for backwards compatibility
                                   if (is.null(params$breaks)) {
                                     params$breaks <- scales::fullseq
                                   }
                                   
                                   if (is.function(params$breaks)) {
                                     # If no parameters set, use pretty bins to calculate binwidth
                                     if (is.null(params$bins) && is.null(params$binwidth)) {
                                       params$binwidth <- diff(pretty(range(data$z, na.rm = TRUE), 10))[1]
                                     }
                                     # If provided, use bins to calculate binwidth
                                     if (!is.null(params$bins)) {
                                       params$binwidth <- diff(range(data$z, na.rm = TRUE)) / params$bins
                                     }
                                     
                                     params$breaks <- params$breaks(range(data$z, na.rm = TRUE), params$binwidth)
                                   }
                                   return(params)
                                 },
                                 compute_layer = function(self, data, params, layout) {
                                   ggplot2:::check_required_aesthetics(
                                     self$required_aes,
                                     c(names(data), names(params)),
                                     ggplot2:::snake_class(self)
                                   )
                                   
                                   # Trim off extra parameters
                                   params <- params[intersect(names(params), self$parameters())]
                                   
                                   args <- c(list(data = quote(data), scales = quote(scales)), params)
                                   plyr::ddply(data, "PANEL", function(data) {
                                     scales <- layout$get_scales(data$PANEL[1])
                                     tryCatch(do.call(self$compute_panel, args), error = function(e) {
                                       warning("Computation failed in `", ggplot2:::snake_class(self),
                                               "()`:\n",
                                               e$message, call. = FALSE)
                                       data.frame()
                                     })
                                   })
                                 },
                                 compute_group = function(data, scales, bins = NULL, binwidth = NULL,
                                                          breaks = scales::fullseq, complete = TRUE,
                                                          na.rm = FALSE, circular = NULL, xwrap = NULL,
                                                          ywrap = NULL, na.fill = FALSE) {
                                   setDT(data)
                                   
                                   data <- data[!(is.na(y) | is.na(x)), ]
                                   
                                   if (isFALSE(na.fill)) {
                                     data <- data[!is.na(z), ]
                                   }
                                   
                                   nx <- data[, uniqueN(x), by = y]$V1
                                   ny <- data[, uniqueN(y), by = x]$V1
                                   
                                   complete.grid <- abs(max(nx) - min(nx)) == 0 & abs(max(ny) - min(ny)) == 0
                                   
                                   if (complete.grid == FALSE) {
                                     if (complete == FALSE) {
                                       warning("data must be a complete regular grid", call. = FALSE)
                                       return(data.frame())
                                     } else {
                                       data <- setDT(tidyr::complete(data, x, y, fill = list(z = NA)))
                                     }
                                   }
                                   
                                   data <- .impute_data.m(data, na.fill)
                                   
                                   if (!is.null(xwrap)) {
                                     data <- suppressWarnings(WrapCircular(data, "x", xwrap))
                                   }
                                   if (!is.null(ywrap)) {
                                     data <- suppressWarnings(WrapCircular(data, "y", ywrap))
                                   }
                                   setDF(data)
                                   contours <- as.data.table(.contour_lines(data, breaks, complete = complete))
                                   
                                   if (length(contours) == 0) {
                                     warning("Not possible to generate contour data", call. = FALSE)
                                     return(data.frame())
                                   }
                                   contours <- .order_contour.m(contours, setDT(data))
                                   
                                   return(contours)
                                 }
)



.order_contour <- function(contours, data) {
  data <- copy(data)
  contours <- copy(contours)
  x.data <- unique(data$x)
  x.data <- x.data[order(x.data)]
  x.N <- length(x.data)
  y.data <- unique(data$y)
  y.data <- y.data[order(y.data)]
  y.N <- length(y.data)
  
  contours[, c("dx", "dy") := .(c(diff(x), NA), c(diff(y), NA)), by = group]
  
  segments <- contours[dx != 0 & dy != 0]
  
  segments[, c("x.axis", "y.axis") := .(x %in% x.data, y %in% y.data), by = group]
  
  # x axis
  x.axis <- segments[x.axis == TRUE]
  x.axis[, x.axis := NULL]   # remove annoying column
  x.axis[, y.d := .second(y.data, y), by = .(group, y)]  # select 2nd closest data point
  x.axis[, m := y - y.d]
  
  x.axis <- data[, .(x, y.d = y, z)][x.axis, on = c("x", "y.d")]  # get z column
  x.axis <- x.axis[level != z]
  x.axis <- x.axis[x.axis[, .I[1], by = group]$V1]   # select the first one.
  
  # Rotation...
  x.axis[, rotate := FALSE]
  x.axis[dx > 0, rotate := (sign(level - z) == sign(m))]
  x.axis[dx < 0, rotate := (sign(level - z) != sign(m))]
  
  # x axisd
  y.axis <- segments[y.axis == TRUE]
  y.axis[, y.axis := NULL]
  y.axis[, x.d := .second(x.data, x), by = .(x, group)]
  y.axis[, m := x - x.d]
  
  y.axis <- data[, .(x.d = x, y, z)][y.axis, on = c("x.d", "y")]
  y.axis <- y.axis[level != z]
  y.axis <- y.axis[y.axis[, .I[1], by = group]$V1]
  
  y.axis[, rotate := FALSE]
  y.axis[dy > 0, rotate := (sign(level - z) != sign(m))]
  y.axis[dy < 0, rotate := (sign(level - z) == sign(m))]
  
  rot.groups <- c(y.axis[rotate == TRUE]$group,
                  x.axis[rotate == TRUE]$group)
  
  # rot.groups <- c(as.character(y.axis$group), as.character(x.axis$group))
  
  contours[, rotate := as.numeric(group[1]) %in% rot.groups, by = group]
  contours <- contours[contours[, ifelse(rotate == TRUE, .I[.N:1], .I), by = group]$V1]
  
  # Congratulations, your contours all have the same direction.
  return(contours)
}

.order_contour.m <- memoise::memoise(.order_contour)

.second <- function(x, target) {
  tmp <- (x - target)
  x[order(abs(tmp))][2]
}

.contour_lines <- memoise::memoise(function(data, breaks, complete = FALSE) {
  z <- tapply(data$z, data[c("x", "y")], identity)
  
  if (is.list(z)) {
    stop("Contour requires single `z` at each combination of `x` and `y`.",
         call. = FALSE)
  }
  
  cl <- grDevices::contourLines(
    x = sort(unique(data$x)), y = sort(unique(data$y)), z = z,
    levels = breaks)
  
  if (length(cl) == 0) {
    warning("Not possible to generate contour data", call. = FALSE)
    return(data.frame())
  }
  
  # Convert list of lists into single data frame
  lengths <- vapply(cl, function(x) length(x$x), integer(1))
  levels <- vapply(cl, "[[", "level", FUN.VALUE = double(1))
  xs <- unlist(lapply(cl, "[[", "x"), use.names = FALSE)
  ys <- unlist(lapply(cl, "[[", "y"), use.names = FALSE)
  pieces <- rep(seq_along(cl), lengths)
  # Add leading zeros so that groups can be properly sorted later
  groups <- paste(data$group[1], sprintf("%03d", pieces), sep = "-")
  
  data.frame(
    level = rep(levels, lengths),
    x = xs,
    y = ys,
    piece = pieces,
    group = groups
  )
})

# .contour_lines_irregular <- function(data, breaks, complete = FALSE) {
#
#     cl <- setDT(contoureR::getContourLines(
#         x = data$x, y = data$y, z = data$z, levels = breaks))
#
#     if (length(cl) == 0) {
#         warning("Not possible to generate contour data", call. = FALSE)
#         return(data.frame())
#     }
#     setnames(cl, c("z", "Group", "PID"), c("level", "group", "piece"))
#     return(cl)
# }


#' @rdname geom_text_contour
#' @usage NULL
#' @format NULL
#' @export
StatTextContour <- ggplot2::ggproto("StatTextContour", StatContour2,
                                    required_aes = c("x", "y", "z"),
                                    default_aes = ggplot2::aes(order = ..level.., label = ..level..)
)

# nocov start

.tidy2matrix <- function(data, formula, value.var, fill = NULL, ...) {
  row.vars <- all.vars(formula[[2]])
  col.vars <- all.vars(formula[[3]])
  data <- as.data.table(data)
  data[, row__ := .GRP, by = c(row.vars)]
  data[, col__ := .GRP, by = c(col.vars)]
  if (is.null(fill)){
    fill <- 0
    rowdims <- data[col__ == 1, (row.vars), with = FALSE]
    coldims <- data[row__ == 1, (col.vars), with = FALSE]
  } else {
    rowdims <- unique(data[, (row.vars), with = FALSE])
    coldims <- unique(data[, (col.vars), with = FALSE])
  }
  
  data.m <- matrix(fill[1], nrow = max(data[["row__"]]),
                   ncol = max(data[["col__"]]))
  data.m[cbind(data[["row__"]], data[["col__"]])] <- data[[value.var]]
  
  return(list(matrix = data.m,
              coldims = coldims,
              rowdims = rowdims))
}


seq_range <- function(x, by = ggplot2::resolution(x, zero = FALSE),...) {
  r <- range(x)
  seq.int(r[1], r[2], by = by, ...)
}

is.error <- function(x) inherits(x, "try-error")



# from data.table
guess <- function (x)
{
  if ("value" %chin% names(x))
    return("value")
  if ("(all)" %chin% names(x))
    return("(all)")
  var <- names(x)[ncol(x)]
  message("Using '", var, "' as value column. Use 'value.var' to override")
  return(var)
}

# from ggplot2
`%||%` <- function(a, b) {
  if (!is.null(a)) a else b
}
# from ggplot2
is.waive <- function (x) {
  inherits(x, "waiver")
}

element_render <- function(theme, element, ..., name = NULL) {
  
  # Get the element from the theme, calculating inheritance
  el <- ggplot2::calc_element(element, theme)
  if (is.null(el)) {
    message("Theme element ", element, " missing")
    return(ggplot2::zeroGrob())
  }
  
  grob <- element_grob(el, ...)
  ggname(paste(element, name, sep = "."), grob)
}

ggname <- function(prefix, grob) {
  grob$name <- grid::grobName(grob, prefix)
  grob
}


width_cm <- function(x) {
  if (is.grob(x)) {
    grid::convertWidth(grid::grobWidth(x), "cm", TRUE)
  } else if (is.unit(x)) {
    grid::convertWidth(x, "cm", TRUE)
  } else if (is.list(x)) {
    vapply(x, width_cm, numeric(1))
  } else {
    stop("Unknown input")
  }
}
height_cm <- function(x) {
  if (is.grob(x)) {
    grid::convertHeight(grid::grobHeight(x), "cm", TRUE)
  } else if (is.unit(x)) {
    grid::convertHeight(x, "cm", TRUE)
  } else if (is.list(x)) {
    vapply(x, height_cm, numeric(1))
  } else {
    stop("Unknown input")
  }
}

message_wrap <- function(...) {
  msg <- paste(..., collapse = "", sep = "")
  wrapped <- strwrap(msg, width = getOption("width") - 2)
  message(paste0(wrapped, collapse = "\n"))
}

.seq_range <- function(x, by = ggplot2::resolution(x, zero = FALSE),...) {
  r <- range(x)
  seq.int(r[1], r[2], by = by, ...)
}



# Interleave (or zip) multiple units into one vector
interleave <- function(...) UseMethod("interleave")
#' @export
interleave.unit <- function(...) {
  do.call("unit.c", do.call("interleave.default", plyr::llply(list(...), as.list)))
}
#' @export
interleave.default <- function(...) {
  vectors <- list(...)
  
  # Check lengths
  lengths <- unique(setdiff(plyr::laply(vectors, length), 1))
  if (length(lengths) == 0) lengths <- 1
  stopifnot(length(lengths) <= 1)
  
  # Replicate elements of length one up to correct length
  singletons <- plyr::laply(vectors, length) == 1
  vectors[singletons] <- plyr::llply(vectors[singletons], rep, lengths)
  
  # Interleave vectors
  n <- lengths
  p <- length(vectors)
  interleave <- rep(1:n, each = p) + seq(0, p - 1) * n
  unlist(vectors, recursive = FALSE)[interleave]
}

matched_aes <- function(layer, guide, defaults) {
  all <- names(c(layer$mapping, if (layer$inherit.aes) defaults, layer$stat$default_aes))
  geom <- c(layer$geom$required_aes, names(layer$geom$default_aes))
  matched <- intersect(intersect(all, geom), names(guide$key))
  matched <- setdiff(matched, names(layer$geom_params))
  setdiff(matched, names(layer$aes_params))
}

rename_aes <- function(x) {
  names(x) <- ggplot2::standardise_aes_names(names(x))
  duplicated_names <- names(x)[duplicated(names(x))]
  if (length(duplicated_names) > 0L) {
    duplicated_message <- paste0(unique(duplicated_names), collapse = ", ")
    warning(
      "Duplicated aesthetics after name standardisation: ", duplicated_message, call. = FALSE
    )
  }
  x
}

#' @importFrom stats line runif var
#' @importFrom utils head
if(getRversion() >= "2.15.1") {
  utils::globalVariables(
    c("as", "dep.names", "ecdf", "equal", "fft", "hasArg", "id",
      "ind.names", "inside", "int.level", "land", "latrad", "lon", "lonrad",
      "piece", "psi", "psi.dx", "psi.dxx", "psi.dxy", "psi.dy", "psi.dyy",
      "r2", "sd", "setTxtProgressBar", "time", "txtProgressBar",
      "u.mean", "v.mean", "write.csv", "x", "y", "z", ".", "time2",
      "group", "step", "point", "change", "end", "level", "m", "rotate",
      "x.d", "y.d", "PC", "step2", "runif", "N", "angle", "var", "head",
      "col__", "row__", "t1", "z1", "z2"))
}


`%>%` <- dplyr::`%>%`

.is.reggrid <- function(data, coords) {
  lengths <- data[, .N, by = coords]$N
  !any(lengths > 1)
}


.simple.scale <- function(x) {
  r <- range(x)
  (x - r[1])/diff(r)
}

isFALSE <- function (x) {
  is.logical(x) && length(x) == 1L && !is.na(x) && !x
}

.pasteand <- function(x) {
  l <- length(x)
  paste0(paste0(x[-l], collapse = ", ")," and ", x[l])
}

checkListSameLengh <- function(x, names = "x") {
  l <- lengths(x)
  if (slow_equal(l)) {
    return(TRUE)
  }
  
  if (isTRUE(names)) {
    names <- .pasteand(names(x))
  } else {
    names <- paste0("Elements of ", names)
  }
  
  return(paste0(names, " must have the same length"))
}

assertListSameLength <- checkmate::makeAssertionFunction(checkListSameLengh)

# fast_equal = inline::cxxfunction(signature(x = 'numeric', y = 'numeric'), '
#   NumericVector var(x);
#   double precision = as<double>(y);
#
#   for (int i = 0, size = var.size(); i < size; ++i) {
#     if (var[i] - var[0] > precision || var[0] - var[i] > precision)
#       return Rcpp::wrap(false);
#   }
#
#   return Rcpp::wrap(true);
# ', plugin = 'Rcpp')

slow_equal <- function(x) diff(range(x)) < sqrt(.Machine$double.eps)

checkSameLength <- function(x) {
  if (!slow_equal(x)) {
    return(paste0(.pasteand(names(x)), " must have the same length"))
  }
  return(TRUE)
}

assertSameLength <- checkmate::makeAssertionFunction(checkSameLength)

checkDateish <- function(x, ...) {
  x <- try(as.Date(x), TRUE)
  if (is.error(x)) {
    return("Must be coercible to date")
  }
  checkDate(x, ...)
}

assertDateish <- checkmate::makeAssertionFunction(checkDateish)
# nocov end

#' Label contours
#'
#' Draws labels on contours built with [ggplot2::stat_contour].
#'
#' @inheritParams ggplot2::geom_text
#' @inheritParams ggplot2::geom_label
#' @inheritParams geom_contour2
#' @param min.size minimum number of points for a contour to be labeled.
#' @param skip number of contours to skip
#' @param rotate logical indicating whether to rotate text following the contour.
#' @param stroke numerical indicating width of stroke relative to the size of
#' the text. Ignored if less than zero.
#' @param stroke.color any valid color.
#'
#' @details
#' Is best used with a previous call to [ggplot2::stat_contour] with the same
#' parameters.
#' Note that while `geom_text_contour()` can angle itself to follow the contour,
#' this is not the case with `geom_label_contour()`.
#'
#' @examples
#' library(ggplot2)
#' v <- data.table::melt(volcano)
#' g <- ggplot(v, aes(Var1, Var2)) +
#'        geom_contour(aes(z = value))
#' g + geom_text_contour(aes(z = value))
#'
#' g + geom_text_contour(aes(z = value), stroke = 0.2)
#'
#' @section Aesthetics:
#' \code{geom_text_contour} understands the following aesthetics (required aesthetics are in bold):
#'
#' \itemize{
#' \item \strong{x}
#' \item \strong{y}
#' \item \strong{label}
#' \item \code{alpha}
#' \item \code{angle}
#' \item \code{colour}
#' \item \code{family}
#' \item \code{fontface}
#' \item \code{group}
#' \item \code{hjust}
#' \item \code{lineheight}
#' \item \code{size}
#' \item \code{vjust}
#'}
#'
#'
#' @export
#' @import ggplot2 data.table
#' @family ggplot2 helpers
geom_text_contour <- function(mapping = NULL, data = NULL,
                              stat = "text_contour",
                              position = "identity",
                              ...,
                              min.size = 5,
                              skip = 0,
                              rotate = TRUE,
                              parse = FALSE,
                              nudge_x = 0,
                              nudge_y = 0,
                              stroke = 0,
                              stroke.color = "white",
                              check_overlap = FALSE,
                              # xwrap = NULL,
                              # ywrap = NULL,
                              na.rm = FALSE,
                              show.legend = NA,
                              inherit.aes = TRUE)
{
  if (!missing(nudge_x) || !missing(nudge_y)) {
    if (!missing(position)) {
      stop("Specify either `position` or `nudge_x`/`nudge_y`",
           call. = FALSE)
    }
    
    position <- position_nudge(nudge_x, nudge_y)
  }
  
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomTextContour,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      skip = skip,
      min.size = min.size,
      rotate = rotate,
      parse = parse,
      check_overlap = check_overlap,
      stroke = stroke,
      stroke.color = stroke.color,
      # xwrap = xwrap,
      # ywrap = ywrap,
      na.rm = na.rm,
      ...
    )
  )
}

#' @rdname geom_text_contour
#' @usage NULL
#' @format NULL
#' @export
# #' @importFrom shadowtext shadowtextGrob
GeomTextContour <- ggplot2::ggproto("GeomTextContour", ggplot2::Geom,
                                    required_aes = c("x", "y", "label"),
                                    default_aes = ggplot2::aes(colour = "black", size = 3.88, angle = 0,
                                                               hjust = 0.5, vjust = 0.5, alpha = NA, family = "",
                                                               fontface = 1, lineheight = 1.2),
                                    
                                    draw_panel = function(data, panel_params, coord, parse = FALSE,
                                                          na.rm = FALSE, check_overlap = FALSE, min.size = 20,
                                                          skip = 1, rotate = FALSE, gap = NULL,
                                                          stroke = 0, stroke.colour = "white") {
                                      data <- data.table::as.data.table(coord$transform(data, panel_params))
                                      min.size <- ceiling(min.size)
                                      if (min.size %% 2 == 0) {
                                        min.size <- min.size - 1
                                      }
                                      # Get points of labels
                                      data <- .label.position(copy(data), min.size, skip, rotate)
                                      
                                      ## Original ggplot2 here.
                                      lab <- data$label
                                      if (parse) {
                                        lab <- parse(text = as.character(lab))
                                      }
                                      
                                      if (is.character(data$vjust)) {
                                        data$vjust <- ggplot2:::compute_just(data$vjust, data$y)
                                      }
                                      if (is.character(data$hjust)) {
                                        data$hjust <- ggplot2:::compute_just(data$hjust, data$x)
                                      }
                                      
                                      shadowtext(
                                        lab,
                                        data$x, data$y, default.units = "native",
                                        hjust = data$hjust, vjust = data$vjust,
                                        dx = data$dx, dy = data$dy,
                                        bg.r = stroke, bg.color = stroke.colour,
                                        gp = grid::gpar(
                                          col = alpha(data$colour, data$alpha),
                                          fontsize = data$size * .pt,
                                          fontfamily = data$family,
                                          fontface = data$fontface,
                                          lineheight = data$lineheight
                                        ),
                                        check.overlap = check_overlap
                                      )
                                      
                                    },
                                    
                                    draw_key = ggplot2::draw_key_text
)


.cont.angle <- function(x, y, gap) {
  N <- length(x)
  gap <- ceiling((gap-1)/2)
  fill <- rep(NA, gap/2)
  dx <- c(fill, diff(x, gap), fill)
  dy <- c(fill, diff(y, gap), fill)
  angle <- atan2(dy, dx)*180/pi
  angle <- ifelse(angle > 180, angle - 180, angle)
  angle <- ifelse(angle > 90, angle - 180, angle)
  angle <- ifelse(angle < -90, angle + 180, angle)
  angle
}

.label.position <- function(data, min.size, skip, rotate) {
  data <- as.data.table(data)
  breaks <- unique(data$level)
  breaks.cut <- breaks[seq(1, length(breaks), by = skip + 1)]
  data <- data[level %in% breaks.cut]
  data[, id := seq_len(.N), by = piece]
  data[, c("dx", "dy") := .(.derv(x, id), .derv(y, id)),
       by = .(piece)]
  
  # Safety strip around the edges (10%)
  safe <- c(0, 1) + 0.1*c(+1, -1)
  data <- data[x %between% safe &
                 y %between% safe]
  
  data[, N := .N, by = piece]
  data <- data[N >= min.size]
  
  # if (rotate == TRUE) {
  data[, angle := .cont.angle(x, y, min.size), by = piece]
  # }
  
  # Check if point has minimum variance
  # data[, var := minvar(x, y), by = .(piece)]
  # data[, curvature := .curvature(x, y), by = piece]
  # data <- data[data[, .I[which.min(abs(curvature))], by = piece]$V1]
  # data[is.na(var), var := FALSE]
  # data <- data[var == TRUE][, head(.SD, 1), by = piece]
  
  data[, min := straight(x, y, min.size), by = piece]
  # data <- data[min == TRUE]
  # data[, min := angle == min(angle, na.rm = T), by = piece]
  # data[, min := atan2(dy, dx) == min(atan2(dy, dx), na.rm = TRUE), by = piece]
  data <- data[!is.na(dx + dy)]
  if (rotate == FALSE) data[, c("dx", "dy") := .(0, 0)]
  
  return(data[min == TRUE, head(.SD, 1), by = piece])
}

straight <- function(x, y, gap) {
  N <- length(x)
  gap <- ceiling((gap-1)/2)
  if (N < gap) {
    gap <- N
  }
  if (is_closed(x, y)) {
    x <- c(x[(N-gap):(N-1)], x, x[2:(gap+1)])
    y <- c(y[(N-gap):(N-1)], y, y[2:(gap+1)])
    var <- vapply((gap+1):(N+gap), function(i) {
      dx <- diff(x[(i-gap):(i+gap)])
      dy <- diff(y[(i-gap):(i+gap)])
      a <- dy/dx
      var(a)
    }, FUN.VALUE = 1)
  } else {
    var <- c(rep(NA, gap), vapply((gap+1):(N-gap), function(i) {
      dx <- diff(x[(i-gap):(i+gap)])
      dy <- diff(y[(i-gap):(i+gap)])
      a <- dy/dx
      var(a)
    }, FUN.VALUE = 1), rep(NA, gap))
  }
  return(var == min(var, na.rm = TRUE))
}

# from https://stackoverflow.com/questions/21868353/drawing-labels-on-flat-section-of-contour-lines-in-ggplot2
minvar <- function (x, y){
  if (length(x) < 4) return(rep(FALSE, length(x)))
  N <- length(x)
  xdiffs <- c(NA, diff(x)) #c(NA, x[3:N] - x[1:(N-2)], NA)
  ydiffs <- c(NA, diff(y)) #c(NA, y[3:N] - y[1:(N-2)], NA)
  avgGradient <- ydiffs/xdiffs
  variance <- abs(avgGradient)
  # squareSum <- avgGradient * avgGradient
  # variance <- (squareSum - (avgGradient * avgGradient) / N) / N
  # change!! this causes problems if length(variance) < 4
  variance <- c(NA, variance[2:(N-1)], NA)
  return(variance == min(variance, na.rm = TRUE))
}
#' @rdname geom_text_contour
#' @export
geom_label_contour <- function(mapping = NULL, data = NULL,
                               stat = "text_contour", position = "identity",
                               ...,
                               min.size = 5,
                               skip = 0,
                               parse = FALSE,
                               nudge_x = 0,
                               nudge_y = 0,
                               label.padding = unit(0.25, "lines"),
                               label.r = unit(0.15, "lines"),
                               label.size = 0.25,
                               na.rm = FALSE,
                               show.legend = NA,
                               inherit.aes = TRUE) {
  if (!missing(nudge_x) || !missing(nudge_y)) {
    if (!missing(position)) {
      stop("Specify either `position` or `nudge_x`/`nudge_y`",
           call. = FALSE)
    }
    
    position <- position_nudge(nudge_x, nudge_y)
  }
  
  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomLabelContour,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      min.size = min.size,
      skip = skip,
      parse = parse,
      label.padding = label.padding,
      label.r = label.r,
      label.size = label.size,
      na.rm = na.rm,
      ...
    )
  )
}

#' @rdname geom_text_contour
#' @usage NULL
#' @format NULL
#' @export
GeomLabelContour <- ggplot2::ggproto("GeomLabelContour", ggplot2::Geom,
                                     required_aes = c("x", "y", "label"),
                                     default_aes = ggplot2::aes(
                                       colour = "black", fill = "white", size = 3.88, angle = 0,
                                       hjust = 0.5, vjust = 0.5, alpha = NA, family = "", fontface = 1,
                                       lineheight = 1.2
                                     ),
                                     
                                     draw_panel = function(self, data, panel_params, coord, parse = FALSE,
                                                           na.rm = FALSE,
                                                           label.padding = unit(0.25, "lines"),
                                                           label.r = unit(0.15, "lines"),
                                                           label.size = 0.25, min.size = 20,
                                                           skip = 1, gap = 0) {
                                       data <- data.table::as.data.table(coord$transform(data, panel_params))
                                       min.size <- ceiling(min.size)
                                       if (min.size %% 2 == 0) {
                                         min.size <- min.size - 1
                                       }
                                       # Get points of labels
                                       data <- .label.position(copy(data), min.size, skip, rotate = FALSE)
                                       
                                       lab <- data$label
                                       if (parse) {
                                         lab <- parse(text = as.character(lab))
                                       }
                                       
                                       if (is.character(data$vjust)) {
                                         data$vjust <- ggplot2:::compute_just(data$vjust, data$y)
                                       }
                                       if (is.character(data$hjust)) {
                                         data$hjust <- ggplot2:::compute_just(data$hjust, data$x)
                                       }
                                       
                                       grobs <- lapply(seq_len(nrow(data)), function(i) {
                                         row <- data[i, , drop = FALSE]
                                         ggplot2:::labelGrob(lab[i],
                                                             x = unit(row$x, "native"),
                                                             y = unit(row$y, "native"),
                                                             just = c(row$hjust, row$vjust),
                                                             padding = label.padding,
                                                             r = label.r,
                                                             text.gp = grid::gpar(
                                                               col = row$colour,
                                                               fontsize = row$size * .pt,
                                                               fontfamily = row$family,
                                                               fontface = row$fontface,
                                                               lineheight = row$lineheight
                                                             ),
                                                             rect.gp = grid::gpar(
                                                               col = row$colour,
                                                               fill = alpha(row$fill, row$alpha),
                                                               lwd = label.size * .pt
                                                             )
                                         )
                                       })
                                       class(grobs) <- "gList"
                                       
                                       ggplot2:::ggname("geom_label_contour", grid::grobTree(children = grobs))
                                     },
                                     
                                     draw_key = ggplot2::draw_key_label
)
#' Filled 2d contours of a 3d surface
#'
#' While ggplot2's \code{\link[ggplot2]{geom_contour}} can plot nice contours, it
#' doesn't work with the polygon geom. This stat makes some small manipulation
#' of the data to ensure that all contours are closed and also computes a new
#' aesthetic \code{int.level}, which differs from \code{level} (computed by
#' [ggplot2::geom_contour]) in that represents
#' the value of the \code{z} aesthetic *inside* the contour instead of at the edge.
#' It also computes breaks globally instead of per panel, so that faceted plots
#' have all the same binwidth.
#'
#' @inheritParams ggplot2::geom_contour
#' @inheritParams geom_contour2
#' @param breaks numeric vector of breaks
#' @param bins Number of evenly spaced breaks.
#' @param binwidth Distance between breaks.
#' @param na.fill How to fill missing values.
# #' @param fill.linear
# #'    - `FALSE` for leting the computation fail with no interpolation
# #'    - `TRUE` for imputing missing values with [approx]
# #'    - A numeric value for constant imputation
# #'    - A function that takes a vector and returns a numeric (e.g. `mean`)
# #' @param xwrap,ywrap vector of length two used to wrap the circular dimension.
#'
#' @section Aesthetics:
#' \code{geom_contour_fill} understands the following aesthetics (required aesthetics are in bold):
#'
#' \itemize{
#' \item \strong{x}
#' \item \strong{y}
#'  \item \code{alpha}
#'  \item \code{colour}
#'  \item \code{group}
#'  \item \code{linetype}
#'  \item \code{size}
#'  \item \code{weight}
#'}
#'
#'
#' @section Computed variables:
#' \describe{
#'  \item{int.level}{value of the interior contour}
#'  }
#'
#' @examples
#' library(ggplot2)
#' surface <- reshape2::melt(volcano)
#' ggplot(surface, aes(Var1, Var2, z = value)) +
#'   geom_contour_fill() +
#'   geom_contour(color = "black", size = 0.1)
#'
#' # If one uses level instead of int.level, one of the small
#' # contours near the crater disapears
#' ggplot(surface, aes(Var1, Var2, z = value)) +
#'   geom_contour_fill(aes(fill = ..level..))
#'
#' @family ggplot2 helpers
#' @export
#' @import sp
#' @import ggplot2
geom_contour_fill <- function(mapping = NULL, data = NULL,
                              stat = "ContourFill", position = "identity",
                              ...,
                              breaks = MakeBreaks(),
                              bins = NULL,
                              binwidth = NULL,
                              # na.rm = FALSE,
                              na.fill = FALSE,
                              # fill.linear = TRUE,
                              # xwrap = NULL,
                              # ywrap = NULL,
                              show.legend = NA,
                              inherit.aes = TRUE) {
  .check_wrap_param(list(...))
  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomPolygon,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      breaks = breaks,
      bins = bins,
      binwidth = binwidth,
      na.rm = FALSE,
      na.fill = na.fill,
      # fill.linear = fill.linear,
      # xwrap = xwrap,
      # ywrap = ywrap,
      ...
    )
  )
}


#' 2d contours of a 3d surface
#'
#' A copy of [ggplot2::geom_contour] that accepts a function as the `breaks`
#' argument and makes gaps for labels and computes breaks globally instead of
#' per panel.
#'
#' @inheritParams ggplot2::geom_contour
#'
#' @section Aesthetics:
#' \code{geom_contour2} understands the following aesthetics (required aesthetics are in bold):
#'
#' \itemize{
#' \item \strong{x}
#' \item \strong{y}
#'  \item \code{alpha}
#'  \item \code{colour}
#'  \item \code{group}
#'  \item \code{linetype}
#'  \item \code{size}
#'  \item \code{weight}
#'}
#'
#' @examples
#' library(ggplot2)
#' ggplot(reshape2::melt(volcano), aes(Var1, Var2)) +
#'     geom_contour2(aes(z = value, color = ..level..),
#'                   breaks = AnchorBreaks(130, binwidth = 11))
#'
#' @family ggplot2 helpers
#' @export
geom_contour2 <- function(mapping = NULL, data = NULL,
                          stat = "contour2", position = "identity",
                          ...,
                          lineend = "butt",
                          linejoin = "round",
                          linemitre = 1,
                          breaks = MakeBreaks(),
                          bins = NULL,
                          binwidth = NULL,
                          na.rm = FALSE,
                          na.fill = FALSE,
                          # fill.linear = TRUE,
                          show.legend = NA,
                          inherit.aes = TRUE) {
  .check_wrap_param(list(...))
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomContour2,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      lineend = lineend,
      linejoin = linejoin,
      linemitre = linemitre,
      breaks = breaks,
      bins = bins,
      binwidth = binwidth,
      na.rm = na.rm,
      na.fill = na.fill,
      # fill.linear = fill.linear,
      ...
    )
  )
}


#' @rdname geom_contour2
#' @usage NULL
#' @format NULL
#' @export
GeomContour2 <- ggplot2::ggproto("GeomContour2", ggplot2::GeomContour,
                                 default_aes = ggplot2::aes(weight = 1, colour = "black", size = 0.5, linetype = 1,
                                                            alpha = NA))

.check_wrap_param <- function(params) {
  if (!is.null(params$xwrap) | !is.null(params$ywrap)) {
    warning("'xwrap' and 'ywrap' will be deprecated.",
            " Use ggperiodic::periodic insead.")
  }
}
#' Discretized continuous color guide
#'
#' A version of [ggplot2::guide_colourbar] that displays discretized values and,
#' by default, puts labels in between values.
#'
#' @inheritParams ggplot2::guide_colourbar
#' @param inside logical indicating where to position labels (see examples).
#'
#' @details
#' The default behaviour works fell for geom_contour_fill in which the colors
#' represent the value between contour surfaces.
#'
#' `inside = TRUE`` works better for geom_tile where the color represents
#' the value of the data and is very similar to [ggplot2::guide_legend].
#'
#' @examples
#' # In this example the lowest color represents an area of the data with values
#' # between 80 and 100.
#' library(ggplot2)
#' binwidth <- 20
#' data(volcano)
#' ggplot(reshape2::melt(volcano), aes(Var1, Var2, z = value)) +
#'     geom_contour_fill(binwidth = binwidth) +
#'     scale_fill_continuous(guide = guide_colourstrip(),
#'                          breaks = MakeBreaks(binwidth))
#'
#' # Difference between guide_legend() and guide_colorbar2(inside = T)
#' df <- reshape2::melt(outer(1:4, 1:4), varnames = c("X1", "X2"))
#' g <- ggplot(df, aes(X1, X2)) +
#'         geom_tile(aes(fill = value)) +
#'         theme(legend.position = "bottom")
#'
#' # Tick labels are to the side
#' g + scale_fill_continuous(guide = guide_legend())
#' # Tick labels are at the bottom
#' g + scale_fill_continuous(guide = guide_colourstrip(inside = TRUE))
#'
#' @return
#' A guide object.
#' @family ggplot2 helpers
#' @export
#' @importFrom grid is.unit
guide_colourstrip <- function(
  # title
  title = waiver(),
  title.position = NULL,
  title.theme = NULL,
  title.hjust = NULL,
  title.vjust = NULL,
  
  # label
  label = TRUE,
  label.position = NULL,
  label.theme = NULL,
  label.hjust = NULL,
  label.vjust = NULL,
  
  # bar
  barwidth = NULL,
  barheight = NULL,
  
  # ticks
  ticks = FALSE,
  draw.ulim= TRUE,
  draw.llim = TRUE,
  inside = FALSE,
  
  # general
  direction = NULL,
  default.unit = "line",
  reverse = FALSE,
  order = 0,
  
  available_aes = c("colour", "color", "fill"),
  ...) {
  
  if (!is.null(barwidth) && !is.unit(barwidth)) barwidth <- unit(barwidth, default.unit)
  if (!is.null(barheight) && !is.unit(barheight)) barheight <- unit(barheight, default.unit)
  
  structure(list(
    # title
    title = title,
    title.position = title.position,
    title.theme = title.theme,
    title.hjust = title.hjust,
    title.vjust = title.vjust,
    
    # label
    label = label,
    label.position = label.position,
    label.theme = label.theme,
    label.hjust = label.hjust,
    label.vjust = label.vjust,
    
    # bar
    barwidth = barwidth,
    barheight = barheight,
    raster = FALSE,
    
    # ticks
    ticks = ticks,
    draw.ulim = draw.ulim,
    draw.llim = draw.llim,
    inside = inside,
    
    # general
    direction = direction,
    default.unit = default.unit,
    reverse = reverse,
    order = order,
    
    # parameter
    available_aes = available_aes, ..., name = "colorbar"),
    class = c("guide", "colorstrip", "colorbar")
  )
}

#' @usage NULL
#' @format NULL
#' @importFrom stats setNames
#' @import gtable
#' @export
#' @rdname guide_colourstrip
#' @keywords internal
guide_train.colorstrip <- function(guide, scale, aesthetic = NULL) {
  # do nothing if scale are inappropriate
  if (length(intersect(scale$aesthetics, guide$available_aes)) == 0) {
    warning("colorstrip guide needs colour or fill scales.")
    return(NULL)
  }
  if (scale$is_discrete()) {
    warning("colorstrip guide needs continuous scales.")
    return(NULL)
  }
  
  # bar specification (number of divs etc)
  .limits <- scale$get_limits()
  
  # create data frame for tick display
  
  if (guide$inside) {
    breaks <- scale$get_breaks()
    breaks <- breaks[!is.na(breaks)]
    guide$nbin <- length(breaks)
    .bar <- breaks
  } else {
    breaks <- .get_breaks(scale)
    .bar <- .inside(breaks[!is.na(breaks)])
    guide$nbin <- length(.bar)
  }
  if (length(breaks) == 0 || all(is.na(breaks)))
    return()
  
  if (length(.bar) == 0) {
    .bar = unique(.limits)
  }
  
  guide$bar <- data.frame(colour = scale$map(.bar), value = .bar,
                          stringsAsFactors = FALSE)
  
  ticks <- as.data.frame(setNames(list(scale$map(breaks)), scale$aesthetics[1]))
  ticks$.value <- breaks
  ticks$.label <- scale$get_labels(breaks)
  
  guide$key <- ticks
  
  if (guide$reverse) {
    guide$key <- guide$key[nrow(guide$key):1, ]
    guide$bar <- guide$bar[nrow(guide$bar):1, ]
  }
  
  guide$hash <- with(guide, digest::digest(list(title, key$.label, bar, name)))
  guide
}


#' @export
#' @rdname guide_colourstrip
guide_colorstrip <- guide_colourstrip

.inside <- function(x) {
  N <- length(x)
  x1 <- x[-N] + diff(x)[-N]/2
  # x1[N] <- x[N]
  x1
}



.get_breaks = function(scale, limits = scale$get_limits()) {
  if (scale$is_empty()) return(numeric())
  
  # Limits in transformed space need to be converted back to data space
  limits <- scale$trans$inverse(limits)
  
  if (is.null(scale$breaks)) {
    return(NULL)
  } else if (identical(scale$breaks, NA)) {
    stop("Invalid breaks specification. Use NULL, not NA")
  } else if (zero_range(as.numeric(limits))) {
    breaks <- limits[1]
  } else if (is.waive(scale$breaks)) {
    breaks <- scale$trans$breaks(limits)
  } else if (is.function(scale$breaks)) {
    breaks <- scale$breaks(limits)
  } else {
    breaks <- scale$breaks
  }
  
  breaks <- scale$trans$transform(breaks)
  breaks
}


#' @usage NULL
#' @format NULL
#' @import grid gtable
#' @export
#' @rdname guide_colourstrip
#' @keywords internal
guide_gengrob.colorstrip <- function(guide, theme) {
  # settings of location and size
  switch(guide$direction,
         "horizontal" = {
           label.position <- guide$label.position %||% "bottom"
           if (!label.position %in% c("top", "bottom")) stop("label position \"", label.position, "\" is invalid")
           
           barwidth <- grid::convertWidth(guide$barwidth %||% (theme$legend.key.width * 5), "mm")
           barheight <- grid::convertHeight(guide$barheight %||% theme$legend.key.height, "mm")
         },
         "vertical" = {
           label.position <- guide$label.position %||% "right"
           if (!label.position %in% c("left", "right")) stop("label position \"", label.position, "\" is invalid")
           
           barwidth <- grid::convertWidth(guide$barwidth %||% theme$legend.key.width, "mm")
           barheight <- grid::convertHeight(guide$barheight %||% (theme$legend.key.height * 5), "mm")
         })
  
  barwidth.c <- c(barwidth)
  barheight.c <- c(barheight)
  barlength.c <- switch(guide$direction, "horizontal" = barwidth.c, "vertical" = barheight.c)
  nbreak <- nrow(guide$key)
  
  tic_pos.c <- rescale(guide$key$.value, c(0.5, guide$nbin - 0.5), guide$bar$value[c(1, nrow(guide$bar))]) * barlength.c / guide$nbin
  grob.bar <-
    switch(guide$direction,
           horizontal = {
             if (guide$inside) {
               bx <- .inside(tic_pos.c)
               bx <- c(2*tic_pos.c[1] - bx[1], bx)
               bw <- c(diff(bx), 2*(tic_pos.c[length(tic_pos.c)] - bx[length(bx)]))
             } else {
               bx <- tic_pos.c[-length(tic_pos.c)]
               bw <- diff(tic_pos.c)
             }
             
             grid::rectGrob(x = bx, y = 0, vjust = 0, hjust = 0, width = bw, height = barheight.c, default.units = "mm",
                            gp = gpar(col = NA, fill = guide$bar$colour))
           },
           vertical = {
             if (guide$inside) {
               by <- .inside(tic_pos.c)
               by <- c(2*tic_pos.c[1] - by[1], by)
               bh <- c(diff(by), 2*(tic_pos.c[length(tic_pos.c)] - by[length(by)]))
             } else {
               by <- tic_pos.c[-length(tic_pos.c)]
               bh <- diff(tic_pos.c)
             }
             
             grid::rectGrob(x = 0, y = by, vjust = 0, hjust = 0, width = barwidth.c, height = bh, default.units = "mm",
                            gp = gpar(col = NA, fill = guide$bar$colour))
           })
  
  
  # tick and label position
  
  label_pos <- unit(tic_pos.c, "mm")
  if (!guide$draw.ulim) tic_pos.c <- tic_pos.c[-1]
  if (!guide$draw.llim) tic_pos.c <- tic_pos.c[-length(tic_pos.c)]
  
  # title
  grob.title <- ggname("guide.title",
                       ggplot2::element_grob(
                         guide$title.theme %||% calc_element("legend.title", theme),
                         label = guide$title,
                         hjust = guide$title.hjust %||% theme$legend.title.align %||% 0,
                         vjust = guide$title.vjust %||% 0.5
                       )
  )
  
  
  title_width <- grid::convertWidth(grobWidth(grob.title), "mm")
  title_width.c <- c(title_width)
  title_height <- grid::convertHeight(grobHeight(grob.title), "mm")
  title_height.c <- c(title_height)
  
  # gap between keys etc
  hgap <- width_cm(theme$legend.spacing.x  %||% unit(0.3, "line"))
  vgap <- height_cm(theme$legend.spacing.y %||% (0.5 * unit(title_height, "cm")))
  
  # label
  label.theme <- guide$label.theme %||% ggplot2::calc_element("legend.text", theme)
  grob.label <- {
    if (!guide$label)
      ggplot2::zeroGrob()
    else {
      hjust <- x <- guide$label.hjust %||% theme$legend.text.align %||%
        if (any(is.expression(guide$key$.label))) 1 else switch(guide$direction, horizontal = 0.5, vertical = 0)
      vjust <- y <- guide$label.vjust %||% 0.5
      switch(guide$direction, horizontal = {x <- label_pos; y <- vjust}, "vertical" = {x <- hjust; y <- label_pos})
      
      label <- guide$key$.label
      
      # If any of the labels are quoted language objects, convert them
      # to expressions. Labels from formatter functions can return these
      if (any(vapply(label, is.call, logical(1)))) {
        label <- lapply(label, function(l) {
          if (is.call(l)) substitute(expression(x), list(x = l))
          else l
        })
        label <- do.call(c, label)
      }
      g <- ggplot2::element_grob(element = label.theme, label = label,
                                 x = x, y = y, hjust = hjust, vjust = vjust)
      ggname("guide.label", g)
    }
  }
  
  label_width <- grid::convertWidth(grobWidth(grob.label), "mm")
  label_width.c <- c(label_width)
  label_height <- grid::convertHeight(grobHeight(grob.label), "mm")
  label_height.c <- c(label_height)
  
  # ticks
  grob.ticks <-
    if (!guide$ticks) zeroGrob()
  else {
    switch(guide$direction,
           "horizontal" = {
             x0 = rep(tic_pos.c, 2)
             y0 = c(rep(0, nbreak), rep(barheight.c * (4/5), nbreak))
             x1 = rep(tic_pos.c, 2)
             y1 = c(rep(barheight.c * (1/5), nbreak), rep(barheight.c, nbreak))
           },
           "vertical" = {
             x0 = c(rep(0, nbreak), rep(barwidth.c * (4/5), nbreak))
             y0 = rep(tic_pos.c, 2)
             x1 = c(rep(barwidth.c * (1/5), nbreak), rep(barwidth.c, nbreak))
             y1 = rep(tic_pos.c, 2)
           })
    segmentsGrob(x0 = x0, y0 = y0, x1 = x1, y1 = y1,
                 default.units = "mm", gp = gpar(col = "white", lwd = 0.5, lineend = "butt"))
  }
  
  # layout of bar and label
  switch(guide$direction,
         "horizontal" = {
           switch(label.position,
                  "top" = {
                    bl_widths <- barwidth.c
                    bl_heights <- c(label_height.c, vgap, barheight.c)
                    vps <- list(bar.row = 3, bar.col = 1,
                                label.row = 1, label.col = 1)
                  },
                  "bottom" = {
                    bl_widths <- barwidth.c
                    bl_heights <- c(barheight.c, vgap, label_height.c)
                    vps <- list(bar.row = 1, bar.col = 1,
                                label.row = 3, label.col = 1)
                  })
         },
         "vertical" = {
           switch(label.position,
                  "left" = {
                    bl_widths <- c(label_width.c, vgap, barwidth.c)
                    bl_heights <- barheight.c
                    vps <- list(bar.row = 1, bar.col = 3,
                                label.row = 1, label.col = 1)
                  },
                  "right" = {
                    bl_widths <- c(barwidth.c, vgap, label_width.c)
                    bl_heights <- barheight.c
                    vps <- list(bar.row = 1, bar.col = 1,
                                label.row = 1, label.col = 3)
                  })
         })
  
  # layout of title and bar+label
  switch(guide$title.position,
         "top" = {
           widths <- c(bl_widths, max(0, title_width.c - sum(bl_widths)))
           heights <- c(title_height.c, vgap, bl_heights)
           vps <- with(vps,
                       list(bar.row = bar.row + 2, bar.col = bar.col,
                            label.row = label.row + 2, label.col = label.col,
                            title.row = 1, title.col = seq_along(widths)))
         },
         "bottom" = {
           widths <- c(bl_widths, max(0, title_width.c - sum(bl_widths)))
           heights <- c(bl_heights, vgap, title_height.c)
           vps <- with(vps,
                       list(bar.row = bar.row, bar.col = bar.col,
                            label.row = label.row, label.col = label.col,
                            title.row = length(heights), title.col = seq_along(widths)))
         },
         "left" = {
           widths <- c(title_width.c, hgap, bl_widths)
           heights <- c(bl_heights, max(0, title_height.c - sum(bl_heights)))
           vps <- with(vps,
                       list(bar.row = bar.row, bar.col = bar.col + 2,
                            label.row = label.row, label.col = label.col + 2,
                            title.row = seq_along(heights), title.col = 1))
         },
         "right" = {
           widths <- c(bl_widths, hgap, title_width.c)
           heights <- c(bl_heights, max(0, title_height.c - sum(bl_heights)))
           vps <- with(vps,
                       list(bar.row = bar.row, bar.col = bar.col,
                            label.row = label.row, label.col = label.col,
                            title.row = seq_along(heights), title.col = length(widths)))
         })
  
  # background
  grob.background <- element_render(theme, "legend.background")
  
  # padding
  padding <- convertUnit(theme$legend.margin %||% margin(), "mm")
  widths <- c(padding[4], widths, padding[2])
  heights <- c(padding[1], heights, padding[3])
  
  gt <- gtable(widths = unit(widths, "mm"), heights = unit(heights, "mm"))
  gt <- gtable_add_grob(gt, grob.background, name = "background", clip = "off",
                        t = 1, r = -1, b = -1, l = 1)
  gt <- gtable_add_grob(gt, grob.bar, name = "bar", clip = "off",
                        t = 1 + min(vps$bar.row), r = 1 + max(vps$bar.col),
                        b = 1 + max(vps$bar.row), l = 1 + min(vps$bar.col))
  gt <- gtable_add_grob(gt, grob.label, name = "label", clip = "off",
                        t = 1 + min(vps$label.row), r = 1 + max(vps$label.col),
                        b = 1 + max(vps$label.row), l = 1 + min(vps$label.col))
  gt <- gtable_add_grob(gt, grob.title, name = "title", clip = "off",
                        t = 1 + min(vps$title.row), r = 1 + max(vps$title.col),
                        b = 1 + max(vps$title.row), l = 1 + min(vps$title.col))
  gt <- gtable_add_grob(gt, grob.ticks, name = "ticks", clip = "off",
                        t = 1 + min(vps$bar.row), r = 1 + max(vps$bar.col),
                        b = 1 + max(vps$bar.row), l = 1 + min(vps$bar.col))
  
  gt
}
