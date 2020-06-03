library(labsimplex)

rm(list = ls())
#The surface in Figure~XX represents the yield of a hypotetical chemical reaction that depends on pH and temperature. The contour
# plot of such surface is shown in Figure~XX. The figure includes the vertexes of a regular simplex with a pH and temperature
# step-size of 1.2 and 15, respectively, with a centroid at 7 and 340, respectively.
prspctv(length = 45, noise = 0, surface = exampleSurfaceR2, par = list(mar = c(1.2, 1, 0, 0)), phi = 30, theta = 30,
        ltheta = -120, shade = 0.2, expand = 0.6, xlab = 'Temperature (K)', ylab = 'pH', zlab = 'Yield (%)', ticktype = "detailed")

simplex <- labsimplex(n =2, centroid = c(7, 340), stepsize = c(1.2, 15))
p0 <- cntr(surface = exampleSurfaceR2, length = 250)
print(addSimplex2Surface(p = p0, simplex = simplex))


#In a real situation ... perform the experiments and obtain a response
generateVertex(simplex = simplex,
               qflv = exampleSurfaceR2(x1 = simplex$coords[, 2],
                                       x2 = simplex$coords[, 1]),
               overwrite = TRUE)
print(addSimplex2Surface(p = p0, simplex = simplex))
n <- nrow(simplex$coords)
generateVertex(simplex = simplex,
               qflv = exampleSurfaceR2(x1 = simplex$coords[n, 2],
                                       x2 = simplex$coords[n, 1]),
               overwrite = TRUE)
print(addSimplex2Surface(p = p0, simplex = simplex))

simplex <- exampleOptimization(surface = exampleSurfaceR2,
                               centroid = c(7, 340), stepsize = c(1.2, 15),
                               experiments = 16)
print(addSimplex2Surface(p = p0, simplex = simplex))

# Variable-size
simplex <- labsimplex(n = 2, centroid = c(7, 340), stepsize = c(1.2, 15))
simplex$families; simplex$coords
generateVertex(simplex = simplex, qflv = exampleSurfaceR2(x1 = simplex$coords[, 2],
                                                          x2 = simplex$coords[, 1]),
               overwrite = TRUE, algor = 'variable')
print(addSimplex2Surface(p = p0, simplex = simplex))
for (i in 1:2) {
  n <- nrow(simplex$coords)
  generateVertex(simplex = simplex,
                 qflv = exampleSurfaceR2(x1 = simplex$coords[n, 2],
                                         x2 = simplex$coords[n, 1]),
                 overwrite = TRUE, algor = 'variable')
  simplex$families; simplex$coords
  print(addSimplex2Surface(p = p0, simplex = simplex))
}
simplex <- exampleOptimization(simplex = simplex, surface = exampleSurfaceR2,
                               centroid = c(7, 340), stepsize = c(1.2, 15),
                               experiments = 12, algor = 'variable')
print(addSimplex2Surface(p = p0, simplex = simplex))


#finding the local maxima
prspctv(length = 45, noise = 0, surface = exampleSurfaceR2.2pks, par = list(mar = c(1.2, 1, 0, 0)),
        ltheta = -120, shade = 0.2, expand = 0.6, xlab = 'Temperature (K)', ylab = 'pH', zlab = 'Yield (%)', ticktype = "detailed")

print(addSimplex2Surface(p = cntr(length = 200, noise = 0, surface = exampleSurfaceR2.2pks),
                         simplex = exampleOptimization(surface = exampleSurfaceR2.2pks,
                                                       centroid = c(5.5, 315),
                                                       stepsize = c(-1.5, 15),
                                                       experiments = 13)))
print(addSimplex2Surface(p = cntr(length = 200, noise = 0, surface = exampleSurfaceR2.2pks),
                         simplex = exampleOptimization(surface = exampleSurfaceR2.2pks,
                                                       centroid = c(1.5, 310),
                                                       stepsize = c(1.5, 15),
                                                       experiments = 14)))
print(addSimplex2Surface(p = cntr(length = 200, noise = 0, surface = exampleSurfaceR2.2pks),
                         simplex = exampleOptimization(surface = exampleSurfaceR2.2pks,
                                                       centroid = c(5.5, 315),
                                                       stepsize = c(-1.5, 15),
                                                       experiments = 13,
                                                       algor = 'variable')))
print(addSimplex2Surface(p = cntr(length = 200, noise = 0, surface = exampleSurfaceR2.2pks),
                         simplex = exampleOptimization(surface = exampleSurfaceR2.2pks,
                                                       centroid = c(1.5, 310),
                                                       stepsize = c(1.5, 15),
                                                       experiments = 14,
                                                       algor = 'variable')))
#noisy surface

prspctv(length = 45, noise = 0, surface = exampleSurfaceR2, par = list(mar = c(1.2, 1, 0, 0)),
        ltheta = -120, shade = 0.2, expand = 0.6, xlab = 'Temperature (K)', ylab = 'pH', zlab = 'Yield (%)', ticktype = "detailed")
prspctv(length = 45, noise = 2, surface = exampleSurfaceR2, par = list(mar = c(1.2, 1, 0, 0)),
        ltheta = -120, shade = 0.2, expand = 0.6, xlab = 'Temperature (K)', ylab = 'pH', zlab = 'Yield (%)', ticktype = "detailed")
prspctv(length = 45, noise = 7, surface = exampleSurfaceR2, par = list(mar = c(1.2, 1, 0, 0)),
        ltheta = -120, shade = 0.2, expand = 0.6, xlab = 'Temperature (K)', ylab = 'pH', zlab = 'Yield (%)', ticktype = "detailed")
prspctv(length = 45, noise = 12, surface = exampleSurfaceR2, par = list(mar = c(1.2, 1, 0, 0)),
        ltheta = -120, shade = 0.2, expand = 0.6, xlab = 'Temperature (K)', ylab = 'pH', zlab = 'Yield (%)', ticktype = "detailed")

if (T) {
print(addSimplex2Surface(p = cntr(length = 120, noise = 0, surface = exampleSurfaceR2),
                         simplex = exampleOptimization(surface = exampleSurfaceR2,
                                                       centroid = c(5, 335),
                                                       stepsize = c(-1.5, 15),
                                                       experiments = 15)))
set.seed(35)
print(addSimplex2Surface(p = cntr(length = 120, noise = 2, surface = exampleSurfaceR2),
                         simplex = exampleOptimization(surface = exampleSurfaceR2,
                                                       centroid = c(5, 335),
                                                       stepsize = c(-1.5, 15),
                                                       experiments = 17,
                                                       noise = 2)))

set.seed(6)
print(addSimplex2Surface(p = cntr(length = 120, noise = 7, surface = exampleSurfaceR2),
                         simplex = exampleOptimization(surface = exampleSurfaceR2,
                                                       centroid = c(5, 335),
                                                       stepsize = c(-1.5, 15),
                                                       experiments = 16,
                                                       noise = 7)))

print(addSimplex2Surface(p = cntr(length = 120, noise = 0, surface = exampleSurfaceR2, x1lim = c(274, 365)),
                         simplex = exampleOptimization(surface = exampleSurfaceR2,
                                                       centroid = c(5, 335),
                                                       stepsize = c(-1.5, 15),
                                                       experiments = 17, algor = 'variable')))
set.seed(35)
print(addSimplex2Surface(p = cntr(length = 120, noise = 2, surface = exampleSurfaceR2),
                         simplex = exampleOptimization(surface = exampleSurfaceR2,
                                                       centroid = c(5, 335),
                                                       stepsize = c(-1.5, 15),
                                                       experiments = 17,
                                                       noise = 2, algor = 'variable')))

set.seed(6)
print(addSimplex2Surface(p = cntr(length = 120, noise = 7, surface = exampleSurfaceR2),
                         simplex = exampleOptimization(surface = exampleSurfaceR2,
                                                       centroid = c(5, 335),
                                                       stepsize = c(-1.5, 15),
                                                       experiments = 16,
                                                       noise = 7, algor = 'variable')))
}
