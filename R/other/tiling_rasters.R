# Tiling rasters

# https://stackoverflow.com/questions/52484216/split-a-raster-into-5-pixel-x-5-pixel-tiles-in-r

library(raster)
library(purrr)
library(furrr)

# define tiling function (sequential)
split_raster_seq <- function(r, nx, ny, buffer = c(0,0)) {
  
  ext <- extent(r)
  
  tiles <- vector("list", length = nx * ny)
  
  n <- 1L
  
  for (i in seq_len(nx) - 1L) {
    for (j in seq_len(ny) - 1L) {
      x0 <- ext@xmin + i * ((ext@xmax - ext@xmin) / nx) - buffer[1] * xres(r) 
      x1 <- ext@xmin + (i + 1L) * ((ext@xmax - ext@xmin) / nx) + buffer[1] * xres(r) # nolint
      y0 <- ext@ymin + j * ((ext@ymax - ext@ymin) / ny) - buffer[2] * yres(r) # nolint
      y1 <- ext@ymin + (j + 1L) * ((ext@ymax - ext@ymin) / ny) + buffer[2] * yres(r) # nolint
      tiles[[n]] <- extent(x0, x1, y0, y1)
      n <- n + 1L
    }
  }
  
  crop_tiles <- function(i, e, r) {
    ri <- crop(r, e[[i]])
    crs(ri) <- crs(r)
    return(ri)
  }
  
  tiles <- purrr::map(seq_along(tiles), function(i) crop_tiles(i, tiles, r)) 
  
  return(tiles)
  
}



# exchange purrr fncs to furrr to run in parallel

# define tiling function using parallel backends
split_raster_par <- function(r, nx, ny, buffer = c(0,0)) {
  
  ext <- extent(r)
  
  tiles <- vector("list", length = nx * ny)
  
  n <- 1L
  
  for (i in seq_len(nx) - 1L) {
    for (j in seq_len(ny) - 1L) {
      x0 <- ext@xmin + i * ((ext@xmax - ext@xmin) / nx) - buffer[1] * xres(r) 
      x1 <- ext@xmin + (i + 1L) * ((ext@xmax - ext@xmin) / nx) + buffer[1] * xres(r) # nolint
      y0 <- ext@ymin + j * ((ext@ymax - ext@ymin) / ny) - buffer[2] * yres(r) # nolint
      y1 <- ext@ymin + (j + 1L) * ((ext@ymax - ext@ymin) / ny) + buffer[2] * yres(r) # nolint
      tiles[[n]] <- extent(x0, x1, y0, y1)
      n <- n + 1L
    }
  }
  
  crop_tiles <- function(i, e, r) {
    ri <- crop(r, e[[i]])
    crs(ri) <- crs(r)
    return(ri)
  }
  
  tiles <- furrr::future_map(seq_along(tiles), function(i) crop_tiles(i, tiles, r)) 
  
  return(tiles)
  
}


