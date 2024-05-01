## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>",
  fig.width = 6, fig.height = 4.5, out.width = "95%")
library(chart)

## -----------------------------------------------------------------------------
data(trees)
trees_lm <- lm(Volume ~ Girth, data = trees)
summary(trees_lm)

## -----------------------------------------------------------------------------
library(ggplot2)
ggplot(trees, aes(x = Girth, y = Volume)) +
  geom_point() +
  geom_smooth(method = "lm", formula = 'y ~ x')

## -----------------------------------------------------------------------------
library(chart)
chart(trees, Volume ~ Girth) +
  geom_point() +
  geom_smooth(method = "lm", formula = 'y ~ x')

## -----------------------------------------------------------------------------
chart(trees, Volume ~ Girth %col=% Height) +
  geom_point()

## -----------------------------------------------------------------------------
chart(function() {
  plot(Volume ~ Girth, data = trees, pch = 19, col = "black")
  grid(lty = "solid")
})

## -----------------------------------------------------------------------------
trees <- data.io::labelise(trees, label = list(
    Volume = "Volume of timber",
    Girth  = "Diameter at 1.4m",
    Height = "Height"
  ), units = list(
    Volume = "cubic feet",
    Girth  = "inches",
    Height = "feet"))
chart(trees, Volume ~ Girth %col=% Height) +
  geom_point()

