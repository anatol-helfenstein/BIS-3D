#------------------------------------------------------------------------------
# Name:     color_schemes.R
#
# Content:  - explore different color schemes for maps / raster files
#           - e.g. colors for continuous vs. categorical covariates
#
# Project:  BIS+
# Author:   Anatol Helfenstein
# Updated:  December 2020
#-------------------------------------------------------------------------------



# Load libraries ----------------------------------------------------------
library(RColorBrewer)
library(viridis)
library(tidyverse)



# Gradual color schemes (for continuous variables) ------------------------

n = 200

# set plotting layout
par(mfrow = c(5, 1))

# viridis colors
image(1:n, 1, as.matrix(1:n),
      col = viridis(n, option = "viridis"),
      xlab = "viridis", ylab = "", xaxt = "n", yaxt = "n", bty = "n")
image(1:n, 1, as.matrix(1:n),
      col = viridis(n, option = "magma"),
      xlab = "magma", ylab = "", xaxt = "n", yaxt = "n", bty = "n")
image(1:n, 1, as.matrix(1:n),
      col = viridis(n, option = "plasma"),
      xlab = "plasma", ylab = "", xaxt = "n", yaxt = "n", bty = "n")
image(1:n, 1, as.matrix(1:n),
      col = viridis(n, option = "inferno"),
      xlab = "inferno", ylab = "", xaxt = "n", yaxt = "n", bty = "n")
image(1:n, 1, as.matrix(1:n),
      col = viridis(n, option = "cividis"),
      xlab = "cividis", ylab = "", xaxt = "n", yaxt = "n", bty = "n")


# other commonly used colors

# set plotting layout
par(mfrow = c(4, 1))

image(1:n, 1, as.matrix(1:n),
      col = rainbow(n),
      xlab = "rainbow", ylab = "", xaxt = "n", yaxt = "n", bty = "n")
image(1:n, 1, as.matrix(1:n),
      col = heat.colors(n),
      xlab = "heat", ylab = "", xaxt = "n", yaxt = "n", bty = "n")
image(1:n, 1, as.matrix(1:n),
      col = terrain.colors(n),
      xlab = "terrain", ylab = "", xaxt = "n", yaxt = "n", bty = "n")
image(1:n, 1, as.matrix(1:n),
      col = sp::bpy.colors(n),
      xlab = "bpy", ylab = "", xaxt = "n", yaxt = "n", bty = "n")



# Discrete colors, as different as possible (for categorical variables) --------

# reset plotting layout
par(mfrow=c(1,1))

# Paired color palette works up to 12 colors
image(1:n, 1, as.matrix(1:n),
      col = brewer.pal(n = 12, name = "Paired"),
      xlab = "paired", ylab = "", xaxt = "n", yaxt = "n", bty = "n")

# with more than 12 categories/classes it becomes more difficult
n = 60
qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]

v_colors = unlist(mapply(brewer.pal,
                           qual_col_pals$maxcolors,
                           rownames(qual_col_pals)))

pie(rep(1,n), col=sample(v_colors, n))

color = grDevices::colors()[grep('gr(a|e)y', grDevices::colors(), invert = T)]

pie(rep(1,n), col=sample(color, n))

pie(rep(1,n), col=sample(color, n))


