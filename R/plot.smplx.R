#' Draws a two dimentional plot of the vertexes in a simplex
#'
#' The function generates a 2D plot of the vertexes in a simplex optimization
#' when simplex dimensionality is at least 2. When dimensionality is higher
#' than 2, the plot produced is a projection of the selected variables.
#'
#' For 3D representations of simplexes with dimensionality higher than 2 you
#' can use \code{\link{plotSimplex3D}}.
#'
#' @param  x       object of class \code{smplx}.
#' @param  sel.dim \code{numeric} or \code{char} vector for variables to be
#'                 considered when simplex dimensionality is higher than 2.
#'                 By default, the first two are chosen. If the vector is
#'                 \code{numeric}, it must contain the ordinal numbers
#'                 corresponding to the desired variables. If the vector is of
#'                 class \code{char}, it must contain the names of such
#'                 dimensions.
#' @param  all.ver logical default to \code{TRUE}. Should all vertexes be
#'                 plotted? If \code{FALSE}, the function draws only the
#'                 vertexes of the current simplex.
#' @param  all.lin logical default to \code{TRUE}. Should all lines be drawn?
#'                 If \code{FALSE}, the function draws only the lines of the
#'                 last simplex.
#' @param  expand  logical default to \code{TRUE}. Should the plot scales be
#'                 expanded?
#' @param  exp.fac expansion factor used when \code{expand = TRUE}.
#' @param  ...     other graphical parameters used in
#'                 \code{\link[graphics]{plot}}
#' @return 2D plot of the simplex coordinates.
#' @seealso \code{\link{plotSimplex3D}}
#' @examples
#'   plot(x = labsimplex(n = 2, centroid = c(7, 340), stepsize = c(1.2, 15)))
#'
#'   ## Several options are posible when visualizing higher order simplexes
#'   plot(x = labsimplex(n = 3))
#'   plot(x = labsimplex(n = 3), sel.dim = c(2, 3))
#'
#'   ## Simplex movements can be visualized after some experiments has been
#'   ## performed
#'   simplex <- exampleOptimization(surface = exampleSurfaceR2,
#'                                  centroid = c(7, 340),
#'                                  stepsize = c(1.2, 15), experiments = 16)
#'   plot(x = simplex)
#' @importFrom graphics lines par plot segments title
#' @importFrom grDevices dev.off pdf
#' @method plot smplx
#' @author Cristhian Paredes, \email{craparedesca@@unal.edu.co}
#' @author Jesús Ágreda, \email{jagreda@@unal.edu.co}
#' @export
# S3 method for smplx class object

plot.smplx <- function(x, sel.dim = NULL, all.ver = TRUE, all.lin = TRUE,
                       expand = TRUE, exp.fac = 1.5, ...){
  # Error handling
  checkMain(simplex = x)
  if (x$dim < 2){
    stop("Simplex dimension must be at least 2")
  }
  if (!missing(sel.dim)) {
    if (length(sel.dim) != 2) {
      stop("Only 2 coordinates can be plotted. Length of sel.dim vector ",
           "differs from 2: ", length(sel.dim))
      }
  } else {
    var.plt <- 1:2
  }

  if (x$dim > 2) {
    if (missing(sel.dim)) {
      message("No selected dimensions for ploting. Default is the first two",
              "ones.")
    } else {
      if (is.numeric(sel.dim)) {
        var.plt <- sel.dim
        if (any(var.plt > x$dim)){
          stop("At least one ingresed variable is not available in data")
        }
      } else {
        var.plt <- (1:ncol(x$coords))[!is.na(match(colnames(x$coords),
                                                   sel.dim))]
      }
      if (length(var.plt) != 2) {
        stop("At least one ingresed variable is not available in data")
      }
    }
  }

  opar <- par(no.readonly = TRUE)
  on.exit(par(opar))

  par(mar = c(2.36, 2.9, 1, 1), mgp = c(1.7, 0.35, 0), las = 1,
      tcl = -0.25, cex.axis = 1, cex.lab = 1)

  xlab <- dimnames(x$coords)[[2]][var.plt[1]]
  ylab <- dimnames(x$coords)[[2]][var.plt[2]]
  if (all.ver) {
    pdf(file = NULL)
    plot(x$coord[, var.plt[1]], x$coord[, var.plt[2]], pch = 16, ...)
    if (expand) {
      xexp <- c(par("usr")[1] - 0.5 * diff(par("usr")[1:2]) * exp.fac,
                par("usr")[2] + 0.5 * diff(par("usr")[1:2]) * exp.fac)
      yexp <- c(par("usr")[3] - 0.5 * diff(par("usr")[3:4]) * exp.fac,
                par("usr")[4] + 0.5 * diff(par("usr")[3:4]) * exp.fac)
      dev.off()
      plot(x$coord[, var.plt[1]], x$coord[, var.plt[2]], xlim = xexp,
           ylim = yexp, pch = 16, xlab = "", ylab = ylab, ...)
    } else {
      dev.off()
      plot(x$coord[, var.plt[1]], x$coord[, var.plt[2]], pch = 16,
           xlab = "", ylab = ylab, ...)
    }
    #title(xlab = dimnames(x$coords)[[2]][var.plt[1]],
    #      ylab = dimnames(x$coords)[[2]][var.plt[2]],
    #      outer = TRUE, line = 3)
  } else {
    plot(x$coord[(nrow(x$coords) - x$dim):nrow(x$coords), var.plt[1]],
         x$coord[(nrow(x$coords) - x$dim):nrow(x$coords), var.plt[2]],
         pch = 16, xlab = "", ylab = ylab, ...)
    if (expand) {
      xexp <- c(par("usr")[1] - 0.5 * diff(par("usr")[1:2]) * exp.fac,
                par("usr")[2] + 0.5 * diff(par("usr")[1:2]) * exp.fac)
      yexp <- c(par("usr")[3] - 0.5 * diff(par("usr")[3:4]) * exp.fac,
                par("usr")[4] + 0.5 * diff(par("usr")[3:4]) * exp.fac)
      plot(x$coord[(nrow(x$coords) - x$dim):nrow(x$coords), var.plt[1]],
           x$coord[(nrow(x$coords) - x$dim):nrow(x$coords), var.plt[2]],
           xlim = xexp, ylim = yexp, pch = 16,
           xlab = "", ylab = ylab, ...)
    }
  }
  title(xlab = xlab, mgp = c(1.2, 0.7, 0))

  if (all.lin) {
    V.pos <- as.numeric(gsub("Vertex.", "", row.names(x$coords)))
    for (ii in 1:length(x$families)) {
      for (jj in 1:(length(x$families[[ii]]) - 1)) {
        for (kk in (jj + 1):length(x$families[[ii]])) {
          jj. <- which(x$families[[ii]][jj] == V.pos)
          kk. <- which(x$families[[ii]][kk] == V.pos)
          lines(x$coords[c(jj., kk.),  var.plt[1]],
                x$coords[c(jj., kk.),  var.plt[2]], col = "grey")
        }
      }
    }
  }
  for (ii in (nrow(x$coords) - x$dim):nrow(x$coords)){
    for (jj in ii:nrow(x$coords)){
      lines(x$coords[c(ii, jj),  var.plt[1]],
            x$coords[c(ii, jj),  var.plt[2]], col = "blue")
    }
  }
  #par(oma = opar$oma, mar = opar$mar)
}

