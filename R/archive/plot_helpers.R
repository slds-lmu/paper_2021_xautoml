# Plotting functionality 

library("plot3D")
library("ggplot2")
library("GGally")
library("gridExtra")

plot_function = function(obj) {
    ps = getParamSet(obj)
    x1 <- seq(from = ps$pars$x$lower[1], to = ps$pars$x$upper[1], length.out = 100)
    x2 <- seq(from = ps$pars$x$lower[2], to = ps$pars$x$upper[2], length.out = 100)
    gg <- expand.grid(x1 = x1, x2 = x2)
    gg$y = apply(gg, 1, obj)

    p = ggplot() + geom_tile(data = gg, aes(x = x1, y = x2, fill = y))
    p = p + scale_fill_distiller(palette = "Spectral")
    p
}