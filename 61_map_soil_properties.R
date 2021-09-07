#------------------------------------------------------------------------------
# Name:     61_map_soil_properties.R
#
# Content:  - Load stack of predicted target soil property rasters and visualize
#             results as plots using "rasterVis" pkg:
#               - 5th, 50th (median) and 95th quantile of QRF
#               - 90% prediction interval (PI90)
#               - GSM accuracy thresholds
#           
# Inputs:   - out/maps/target/[TARGET]/GeoTIFFs/
#
# Output:   - out/maps/target/[TARGET]/pdf/
#
# Project:  BIS+
# Author:   Anatol Helfenstein
# Updated:  June 2020
#------------------------------------------------------------------------------



### empty memory and workspace; load required packages -------------------------
gc()
rm(list=ls())

pkgs <- c("raster", "rasterVis", "viridisLite", "grid", "gridExtra", "foreach",
          "RColorBrewer", "tidyverse")
lapply(pkgs, library, character.only = TRUE)



### Designate script parameters and load data ----------------------------------

# Specify DSM target soil property:
TARGET = "pH_KCl"

# GlobalSoilMap (GSM) depth layers
# (see out/data/covariates/target_GSM_depths):
D_MID = c("d_0_5_mid", "d_5_15_mid", "d_15_30_mid",
          "d_30_60_mid", "d_60_100_mid", "d_100_200_mid")

# locate, read in and stack rasters of response soil properties
# prediction mean and quantiles
v_response_names_pred <- dir(paste0("out/maps/target/", TARGET, "/GeoTIFFs"),
                             pattern = "pred[059_mean]{,5}.tif$", recursive = FALSE)

ls_r_response_pred <- foreach(r = 1:length(v_response_names_pred)) %do%
  raster(paste0("out/maps/target/", TARGET, "/GeoTIFFs/", v_response_names_pred[[r]]))

r_stack_response_pred <- stack(ls_r_response_pred)

# PI90
v_response_names_PI90 <- dir(paste0("out/maps/target/", TARGET, "/GeoTIFFs"),
                           pattern = "\\PI90.tif$", recursive = FALSE)

ls_r_response_PI90 <- foreach(r = 1:length(v_response_names_PI90)) %do%
  raster(paste0("out/maps/target/", TARGET, "/GeoTIFFs/", v_response_names_PI90[[r]]))

r_stack_response_PI90 <- stack(ls_r_response_PI90)

# thresholds
v_response_names_thresh <- dir(paste0("out/maps/target/", TARGET, "/GeoTIFFs"),
                               pattern = "\\holds.tif$", recursive = FALSE)

ls_r_response_thresh <- foreach(r = 1:length(v_response_names_thresh)) %do%
  raster(paste0("out/maps/target/", TARGET, "/GeoTIFFs/", v_response_names_thresh[[r]]))

r_stack_response_thresh <- stack(ls_r_response_thresh)

# extract min and max values so we can use same color legend for all maps
response_min = round(min(minValue(r_stack_response_pred)))
response_max = round(max(maxValue(r_stack_response_pred)))

# extract min and max values so we can use same color legend for all maps
PI90_min = round(min(minValue(r_stack_response_PI90)))
PI90_max = round(max(maxValue(r_stack_response_PI90)))

# define interval (smallest step cm to visualize on map and in color scheme)
interval = 0.1

# vector that will define global color scheme for prediction quantiles and PI90
v_col_pred <- seq(response_min, response_max, interval)
v_col_PI90 <- seq(PI90_min, PI90_max, interval)



### Maps of mean & median (50th quantile) predictions over all depth layers ----

# mean predictions over all GSM depth layers
m_pred_mean <- levelplot(stack(r_stack_response_pred$pH_KCl_d_0_5_mid_QRF_pred_mean,
                               r_stack_response_pred$pH_KCl_d_5_15_mid_QRF_pred_mean,
                               r_stack_response_pred$pH_KCl_d_15_30_mid_QRF_pred_mean,
                               r_stack_response_pred$pH_KCl_d_30_60_mid_QRF_pred_mean,
                               r_stack_response_pred$pH_KCl_d_60_100_mid_QRF_pred_mean,
                               r_stack_response_pred$pH_KCl_d_100_200_mid_QRF_pred_mean),
                         margin = FALSE,
                         # main = expression(paste("Soil pH [KCl] (mean)")),
                         scales = list(draw = FALSE),
                         at = v_col_pred,
                         col.regions = viridis(n = length(v_col_pred),
                                               option = "magma"),
                         par.settings = list(axis.line = list(col = 0),
                                             strip.background = list(col = "white")),
                         names.attr = c(expression("0 cm to 5 cm"),
                                        expression("5 cm to 15 cm"),
                                        expression("15 cm to 30 cm"),
                                        expression("30 cm to 60 cm"),
                                        expression("60 cm to 100 cm"),
                                        expression("100 cm to 200 cm"))
                         # colorkey = list(title = expression("pH [KCl] 100-200cm"),
                         #                 row = 1, column = 1, vjust = 2))
)

# save to disk
# pdf(paste0("out/maps/target/", TARGET, "/pdf/m_", TARGET, "_pred_mean_all_depths.pdf"),
#     height = 10, width = 10)
# m_pred_mean
# dev.off()

# median predictions (50th quantile) over all GSM depth layers
m_pred50 <- levelplot(stack(r_stack_response_pred$pH_KCl_d_0_5_mid_QRF_pred50,
                            r_stack_response_pred$pH_KCl_d_5_15_mid_QRF_pred50,
                            r_stack_response_pred$pH_KCl_d_15_30_mid_QRF_pred50,
                            r_stack_response_pred$pH_KCl_d_30_60_mid_QRF_pred50,
                            r_stack_response_pred$pH_KCl_d_60_100_mid_QRF_pred50,
                            r_stack_response_pred$pH_KCl_d_100_200_mid_QRF_pred50),
                      margin = FALSE,
                      # main = expression(paste("Soil pH [KCl] (median)")),
                      scales = list(draw = FALSE),
                      at = v_col_pred,
                      col.regions = viridis(n = length(v_col_pred),
                                            option = "magma"),
                      par.settings = list(axis.line = list(col = 0),
                                          strip.background = list(col = "white")),
                      names.attr = c(expression("0 cm to 5 cm"),
                                     expression("5 cm to 15 cm"),
                                     expression("15 cm to 30 cm"),
                                     expression("30 cm to 60 cm"),
                                     expression("60 cm to 100 cm"),
                                     expression("100 cm to 200 cm"))
                         # colorkey = list(title = expression("pH [KCl] 100-200cm"),
                         #                 row = 1, column = 1, vjust = 2))
)

# save to disk
# pdf(paste0("out/maps/target/", TARGET, "/pdf/m_", TARGET, "_pred50_all_depths.pdf"),
#     height = 10, width = 10)
# m_pred50
# dev.off()

# out of curiosity, map of mean - median values (subtract the two maps)
# 0-5cm
r_mean_minus_median_0_5 <- r_stack_response_pred$pH_KCl_d_0_5_mid_QRF_pred_mean -
  r_stack_response_pred$pH_KCl_d_0_5_mid_QRF_pred50
names(r_mean_minus_median_0_5) <- "r_mean_minus_median_0_5"
# 5-15cm
r_mean_minus_median_5_15 <- r_stack_response_pred$pH_KCl_d_5_15_mid_QRF_pred_mean -
  r_stack_response_pred$pH_KCl_d_5_15_mid_QRF_pred50
names(r_mean_minus_median_5_15) <- "r_mean_minus_median_5_15"
# 15-30cm
r_mean_minus_median_15_30 <- r_stack_response_pred$pH_KCl_d_15_30_mid_QRF_pred_mean -
  r_stack_response_pred$pH_KCl_d_15_30_mid_QRF_pred50
names(r_mean_minus_median_15_30) <- "r_mean_minus_median_15_30"
# 30-60cm
r_mean_minus_median_30_60 <- r_stack_response_pred$pH_KCl_d_30_60_mid_QRF_pred_mean -
  r_stack_response_pred$pH_KCl_d_30_60_mid_QRF_pred50
names(r_mean_minus_median_30_60) <- "r_mean_minus_median_30_60"
# 60-100cm
r_mean_minus_median_60_100 <- r_stack_response_pred$pH_KCl_d_60_100_mid_QRF_pred_mean -
  r_stack_response_pred$pH_KCl_d_60_100_mid_QRF_pred50
names(r_mean_minus_median_60_100) <- "r_mean_minus_median_60_100"
# 100-200cm
r_mean_minus_median_100_200 <- r_stack_response_pred$pH_KCl_d_100_200_mid_QRF_pred_mean -
  r_stack_response_pred$pH_KCl_d_100_200_mid_QRF_pred50
names(r_mean_minus_median_100_200) <- "r_mean_minus_median_100_200"

# mean - median over all depth layers
r_stack_mean_minus_median <- stack(r_mean_minus_median_0_5,
                                   r_mean_minus_median_5_15,
                                   r_mean_minus_median_15_30,
                                   r_mean_minus_median_30_60,
                                   r_mean_minus_median_60_100,
                                   r_mean_minus_median_100_200)

# subtracted maps (mean - median) over all GSM depth layers
m_mean_minus_median <- levelplot(
  r_stack_mean_minus_median,
  margin = FALSE,
  main = expression(paste("Subtracted Maps of Mean - Median for Soil pH [KCl]")),
  scales = list(draw = FALSE),
  col.regions = colorRampPalette(colors = c("#a50026", "#ffffbf", "#313695")),
  par.settings = list(axis.line = list(col = 0),
                      strip.background = list(col = "white")),
                      names.attr = c(expression("0 cm to 5 cm"),
                                     expression("5 cm to 15 cm"),
                                     expression("15 cm to 30 cm"),
                                     expression("30 cm to 60 cm"),
                                     expression("60 cm to 100 cm"),
                                     expression("100 cm to 200 cm"))
                      # colorkey = list(title = expression("pH [KCl] 100-200cm"),
                      #                 row = 1, column = 1, vjust = 2))
)

# save to disk
# pdf(paste0("out/maps/target/", TARGET, "/pdf/m_", TARGET,
#            "_mean_minus_median_all_depths.pdf"),
#     height = 10, width = 10)
# m_mean_minus_median
# dev.off()

# save prediction maps as GeoTIFFs to disk
system.time(
  foreach(n = 1:nlayers(r_stack_mean_minus_median)) %do%
    writeRaster(r_stack_mean_minus_median[[n]],
                paste0("out/maps/target/", TARGET, "/GeoTIFFs/", TARGET, "_",
                       D_MID[n], "_QRF_mean_minus_median.tif"),
                overwrite = TRUE)
) # time elapse sequential: 2.5 min



### Maps of 5th, 50th & 95th prediction quantiles at each GSM depth layer ------

# 0-5 cm
m_pred_0_5 <- levelplot(stack(r_stack_response_pred$pH_KCl_d_0_5_mid_QRF_pred5,
                              r_stack_response_pred$pH_KCl_d_0_5_mid_QRF_pred50,
                              r_stack_response_pred$pH_KCl_d_0_5_mid_QRF_pred95),
                        margin = FALSE,
                        # main = expression("Soil pH [KCl]: 0 cm to 5 cm depth"),
                        scales = list(draw = FALSE),
                        at = v_col_pred,
                        col.regions = viridis(n = length(v_col_pred),
                                              option = "magma"),
                        par.settings = list(axis.line = list(col = 0),
                                            strip.background = list(col = "white")),
                        names.attr = c(expression(paste("q"[0.05], " (0.05 quantile)")),
                                       expression(paste("q"[0.50], " (median)")),
                                       expression(paste("q"[0.95], " (0.95 quantile)")))
                        #colorkey = list(title = expression("pH [KCl] 0-5cm"),
                        #                row = 1, column = 1, vjust = 2))
)

# save to disk
# pdf(paste0("out/maps/target/", TARGET, "/pdf/m_", TARGET, "_d_0_5_pred.pdf"),
#     height = 6, width = 12)
# m_pred_0_5
# dev.off()


# 5-15 cm
m_pred_5_15 <- levelplot(stack(r_stack_response_pred$pH_KCl_d_5_15_mid_QRF_pred5,
                               r_stack_response_pred$pH_KCl_d_5_15_mid_QRF_pred50,
                               r_stack_response_pred$pH_KCl_d_5_15_mid_QRF_pred95),
                         margin = FALSE,
                         # main = expression("Soil pH [KCl]: 5 cm to 15 cm depth"),
                         scales = list(draw = FALSE),
                         at = v_col_pred,
                         col.regions = viridis(n = length(v_col_pred),
                                               option = "magma"),
                         par.settings = list(axis.line = list(col = 0),
                                             strip.background = list(col = "white")),
                         names.attr = c(expression(paste("q"[0.05], " (0.05 quantile)")),
                                        expression(paste("q"[0.50], " (median)")),
                                        expression(paste("q"[0.95], " (0.95 quantile)")))
                         #colorkey = list(title = expression("pH [KCl] 0-5cm"),
                         #                row = 1, column = 1, vjust = 2))
)

# save to disk
# pdf(paste0("out/maps/target/", TARGET, "/pdf/m_", TARGET, "_d_5_15_pred.pdf"),
#     height = 6, width = 12)
# m_pred_5_15
# dev.off()


# 15-30 cm
m_pred_15_30 <- levelplot(stack(r_stack_response_pred$pH_KCl_d_15_30_mid_QRF_pred5,
                                r_stack_response_pred$pH_KCl_d_15_30_mid_QRF_pred50,
                                r_stack_response_pred$pH_KCl_d_15_30_mid_QRF_pred95),
                          margin = FALSE,
                          # main = expression("Soil pH [KCl]: 15 cm to 30 cm depth"),
                          scales = list(draw = FALSE),
                          at = v_col_pred,
                          col.regions = viridis(n = length(v_col_pred),
                                                option = "magma"),
                          par.settings = list(axis.line = list(col = 0),
                                              strip.background = list(col = "white")),
                          names.attr = c(expression(paste("q"[0.05], " (0.05 quantile)")),
                                         expression(paste("q"[0.50], " (median)")),
                                         expression(paste("q"[0.95], " (0.95 quantile)")))
                          #colorkey = list(title = expression("pH [KCl] 0-5cm"),
                          #                row = 1, column = 1, vjust = 2))
)

# save to disk
# pdf(paste0("out/maps/target/", TARGET, "/pdf/m_", TARGET, "_d_15_30_pred.pdf"),
#     height = 6, width = 12)
# m_pred_15_30
# dev.off()


# 30-60 cm
m_pred_30_60 <- levelplot(stack(r_stack_response_pred$pH_KCl_d_30_60_mid_QRF_pred5,
                                r_stack_response_pred$pH_KCl_d_30_60_mid_QRF_pred50,
                                r_stack_response_pred$pH_KCl_d_30_60_mid_QRF_pred95),
                          margin = FALSE,
                          # main = expression("Soil pH [KCl]: 30 cm to 60 cm depth"),
                          scales = list(draw = FALSE),
                          at = v_col_pred,
                          col.regions = viridis(n = length(v_col_pred),
                                                option = "magma"),
                          par.settings = list(axis.line = list(col = 0),
                                              strip.background = list(col = "white")),
                          names.attr = c(expression(paste("q"[0.05], " (0.05 quantile)")),
                                         expression(paste("q"[0.50], " (median)")),
                                         expression(paste("q"[0.95], " (0.95 quantile)")))
                          #colorkey = list(title = expression("pH [KCl] 0-5cm"),
                          #                row = 1, column = 1, vjust = 2))
)

# save to disk
# pdf(paste0("out/maps/target/", TARGET, "/pdf/m_", TARGET, "_d_30_60_pred.pdf"),
#     height = 6, width = 12)
# m_pred_30_60
# dev.off()


# 60-100 cm
m_pred_60_100 <- levelplot(stack(r_stack_response_pred$pH_KCl_d_60_100_mid_QRF_pred5,
                                 r_stack_response_pred$pH_KCl_d_60_100_mid_QRF_pred50,
                                 r_stack_response_pred$pH_KCl_d_60_100_mid_QRF_pred95),
                           margin = FALSE,
                           # main = expression("Soil pH [KCl]: 60 cm to 100 cm depth"),
                           scales = list(draw = FALSE),
                           at = v_col_pred,
                           col.regions = viridis(n = length(v_col_pred),
                                                 option = "magma"),
                           par.settings = list(axis.line = list(col = 0),
                                               strip.background = list(col = "white")),
                           names.attr = c(expression(paste("q"[0.05], " (0.05 quantile)")),
                                          expression(paste("q"[0.50], " (median)")),
                                          expression(paste("q"[0.95], " (0.95 quantile)")))
                           #colorkey = list(title = expression("pH [KCl] 0-5cm"),
                           #                row = 1, column = 1, vjust = 2))
)

# save to disk
# pdf(paste0("out/maps/target/", TARGET, "/pdf/m_", TARGET, "_d_60_100_pred.pdf"),
#     height = 6, width = 12)
# m_pred_60_100
# dev.off()


# 100-200 cm
m_pred_100_200 <- levelplot(stack(r_stack_response_pred$pH_KCl_d_100_200_mid_QRF_pred5,
                                  r_stack_response_pred$pH_KCl_d_100_200_mid_QRF_pred50,
                                  r_stack_response_pred$pH_KCl_d_100_200_mid_QRF_pred95),
                            margin = FALSE,
                            # main = expression("Soil pH [KCl]: 100 cm to 200 cm depth"),
                            scales = list(draw = FALSE),
                            at = v_col_pred,
                            col.regions = viridis(n = length(v_col_pred),
                                                  option = "magma"),
                            par.settings = list(axis.line = list(col = 0),
                                                strip.background = list(col = "white")),
                            names.attr = c(expression(paste("q"[0.05], " (0.05 quantile)")),
                                           expression(paste("q"[0.50], " (median)")),
                                           expression(paste("q"[0.95], " (0.95 quantile)")))
                            #colorkey = list(title = expression("pH [KCl] 0-5cm"),
                            #                row = 1, column = 1, vjust = 2))
)

# save to disk
# pdf(paste0("out/maps/target/", TARGET, "/pdf/m_", TARGET, "_d_100_200_pred.pdf"),
#     height = 6, width = 12)
# m_pred_100_200
# dev.off()



### Maps of PI90 over all GSM depth layers -----------------------------------

# PI90 over all depths
m_PI90 <- levelplot(stack(r_stack_response_PI90$pH_KCl_d_0_5_mid_QRF_PI90,
                        r_stack_response_PI90$pH_KCl_d_5_15_mid_QRF_PI90,
                        r_stack_response_PI90$pH_KCl_d_15_30_mid_QRF_PI90,
                        r_stack_response_PI90$pH_KCl_d_30_60_mid_QRF_PI90,
                        r_stack_response_PI90$pH_KCl_d_60_100_mid_QRF_PI90,
                        r_stack_response_PI90$pH_KCl_d_100_200_mid_QRF_PI90),
                  margin = FALSE,
                  # main = expression("PI90 of pH [KCl]"),
                  scales = list(draw = FALSE),
                  at = v_col_PI90,
                  col.regions = viridis(n = length(v_col_PI90),
                                        option = "viridis"),
                  par.settings = list(axis.line = list(col = 0),
                                      strip.background = list(col = "white")),
                  names.attr = c(expression("0 cm to 5 cm"),
                                 expression("5 cm to 15 cm"),
                                 expression("15 cm to 30 cm"),
                                 expression("30 cm to 60 cm"),
                                 expression("60 cm to 100 cm"),
                                 expression("100 cm to 200 cm"))
                  #colorkey = list(title = expression("pH [KCl] 100-200cm"),
                  #                row = 1, column = 1, vjust = 2))
)

# save to disk
# pdf(paste0("out/maps/target/", TARGET, "/pdf/m_", TARGET, "_PI90_all_depths.pdf"),
#     height = 10, width = 10)
# m_PI90
# dev.off()



### Maps of GSM accuracy thresholds over all GSM depth layers ------------------

# Accuracy thresholds over all depths
m_thresholds <- levelplot(stack(r_stack_response_thresh$pH_KCl_d_0_5_mid_QRF_PI90_thresholds,
                                r_stack_response_thresh$pH_KCl_d_5_15_mid_QRF_PI90_thresholds,
                                r_stack_response_thresh$pH_KCl_d_15_30_mid_QRF_PI90_thresholds,
                                r_stack_response_thresh$pH_KCl_d_30_60_mid_QRF_PI90_thresholds,
                                r_stack_response_thresh$pH_KCl_d_60_100_mid_QRF_PI90_thresholds,
                                r_stack_response_thresh$pH_KCl_d_100_200_mid_QRF_PI90_thresholds),
                          att = "category",
                          margin = FALSE,
                          # main = expression("Accuracy thresholds of PI90"),
                          scales = list(draw = FALSE),
                          col.regions = c("#db4325", "#eda247", "#e6e1bc", "#006164"),
                          par.settings = list(axis.line = list(col = 0),
                                              strip.background = list(col = "white")),
                          names.attr = c(expression("0 cm to 5 cm"),
                                         expression("5 cm to 15 cm"),
                                         expression("15 cm to 30 cm"),
                                         expression("30 cm to 60 cm"),
                                         expression("60 cm to 100 cm"),
                                         expression("100 cm to 200 cm"))
)

# save to disk
# pdf(paste0("out/maps/target/", TARGET, "/pdf/m_", TARGET, "_thresh_all_depths.pdf"),
#     height = 10, width = 10)
# m_thresholds
# dev.off()



### Maps of PI90 and GSM accuracy thresholds -----------------------------------

# 0-5 cm PI90
m_PI90_0_5 <- levelplot(r_stack_response_PI90$pH_KCl_d_0_5_mid_QRF_PI90,
                      margin = FALSE,
                      # main = expression("PI90"),
                      scales = list(draw = FALSE),
                      at = v_col_PI90,
                      col.regions = viridis(n = length(v_col_PI90),
                                            option = "viridis"),
                      par.settings = list(axis.line = list(col = 0),
                                          strip.background = list(col = "white"))
)

# 0-5 cm thresholds
m_thresh_0_5 <- levelplot(r_stack_response_thresh$pH_KCl_d_0_5_mid_QRF_PI90_thresholds,
                              att = "category",
                              margin = FALSE,
                              # main = expression("Accuracy thresholds"),
                              scales = list(draw = FALSE),
                              col.regions = c("#db4325", "#eda247", "#e6e1bc", "#006164"),
                              par.settings = list(axis.line = list(col = 0),
                                                  strip.background = list(col = "white"))
)

# save to disk; combine using gridExtra pkg
# pdf(paste0("out/maps/target/", TARGET, "/pdf/m_", TARGET, "_d_0_5_PI90_thresh.pdf"),
#     height = 6, width = 12)
# grid.arrange(m_PI90_0_5, m_thresh_0_5, ncol = 2)
#              # top = textGrob("pH [KCl]: 0 cm to 5 cm depth",
#              #                gp = gpar(fontsize = 16)))
# dev.off()


# 5-15 cm PI90
m_PI90_5_15 <- levelplot(r_stack_response_PI90$pH_KCl_d_5_15_mid_QRF_PI90,
                      margin = FALSE,
                      # main = expression("PI90"),
                      scales = list(draw = FALSE),
                      at = v_col_PI90,
                      col.regions = viridis(n = length(v_col_PI90),
                                            option = "viridis"),
                      par.settings = list(axis.line = list(col = 0),
                                          strip.background = list(col = "white"))
)

# 5-15 cm thresholds
m_thresh_5_15 <- levelplot(r_stack_response_thresh$pH_KCl_d_5_15_mid_QRF_PI90_thresholds,
                              att = "category",
                              margin = FALSE,
                              # main = expression("Accuracy thresholds"),
                              scales = list(draw = FALSE),
                              col.regions = c("#db4325", "#eda247", "#e6e1bc", "#006164"),
                              par.settings = list(axis.line = list(col = 0),
                                                  strip.background = list(col = "white"))
)

# save to disk; combine using gridExtra pkg
# pdf(paste0("out/maps/target/", TARGET, "/pdf/m_", TARGET, "_d_5_15_PI90_thresh.pdf"),
#     height = 6, width = 12)
# grid.arrange(m_PI90_5_15, m_thresh_5_15, ncol = 2)
#              # top = textGrob("pH [KCl]: 5 cm to 15 cm depth",
#              #                gp = gpar(fontsize = 16)))
# dev.off()


# 15-30 cm PI90
m_PI90_15_30 <- levelplot(r_stack_response_PI90$pH_KCl_d_15_30_mid_QRF_PI90,
                      margin = FALSE,
                      # main = expression("PI90"),
                      scales = list(draw = FALSE),
                      at = v_col_PI90,
                      col.regions = viridis(n = length(v_col_PI90),
                                            option = "viridis"),
                      par.settings = list(axis.line = list(col = 0),
                                          strip.background = list(col = "white"))
)

# 15-30 cm thresholds
m_thresh_15_30 <- levelplot(r_stack_response_thresh$pH_KCl_d_15_30_mid_QRF_PI90_thresholds,
                              att = "category",
                              margin = FALSE,
                              # main = expression("Accuracy thresholds"),
                              scales = list(draw = FALSE),
                              col.regions = c("#db4325", "#eda247", "#e6e1bc", "#006164"),
                              par.settings = list(axis.line = list(col = 0),
                                                  strip.background = list(col = "white"))
)

# save to disk; combine using gridExtra pkg
# pdf(paste0("out/maps/target/", TARGET, "/pdf/m_", TARGET, "_d_15_30_PI90_thresh.pdf"),
#     height = 6, width = 12)
# grid.arrange(m_PI90_15_30, m_thresh_15_30, ncol = 2)
#              # top = textGrob("pH [KCl]: 15 cm to 30 cm depth",
#              #                gp = gpar(fontsize = 16)))
# dev.off()


# 30-60 cm PI90
m_PI90_30_60 <- levelplot(r_stack_response_PI90$pH_KCl_d_30_60_mid_QRF_PI90,
                      margin = FALSE,
                      # main = expression("PI90"),
                      scales = list(draw = FALSE),
                      at = v_col_PI90,
                      col.regions = viridis(n = length(v_col_PI90),
                                            option = "viridis"),
                      par.settings = list(axis.line = list(col = 0),
                                          strip.background = list(col = "white"))
)

# 30-60 cm thresholds
m_thresh_30_60 <- levelplot(r_stack_response_thresh$pH_KCl_d_30_60_mid_QRF_PI90_thresholds,
                              att = "category",
                              margin = FALSE,
                              # main = expression("Accuracy thresholds"),
                              scales = list(draw = FALSE),
                              col.regions = c("#db4325", "#eda247", "#e6e1bc", "#006164"),
                              par.settings = list(axis.line = list(col = 0),
                                                  strip.background = list(col = "white"))
)

# save to disk; combine using gridExtra pkg
# pdf(paste0("out/maps/target/", TARGET, "/pdf/m_", TARGET, "_d_30_60_PI90_thresh.pdf"),
#     height = 6, width = 12)
# grid.arrange(m_PI90_30_60, m_thresh_30_60, ncol = 2)
#              # top = textGrob("pH [KCl]: 30 cm to 60 cm depth",
#              #                gp = gpar(fontsize = 16)))
# dev.off()


# 60-100 cm PI90
m_PI90_60_100 <- levelplot(r_stack_response_PI90$pH_KCl_d_60_100_mid_QRF_PI90,
                      margin = FALSE,
                      # main = expression("PI90"),
                      scales = list(draw = FALSE),
                      at = v_col_PI90,
                      col.regions = viridis(n = length(v_col_PI90),
                                            option = "viridis"),
                      par.settings = list(axis.line = list(col = 0),
                                          strip.background = list(col = "white"))
)

# 60-100 cm thresholds
m_thresh_60_100 <- levelplot(r_stack_response_thresh$pH_KCl_d_60_100_mid_QRF_PI90_thresholds,
                              att = "category",
                              margin = FALSE,
                              # main = expression("Accuracy thresholds"),
                              scales = list(draw = FALSE),
                              col.regions = c("#db4325", "#eda247", "#e6e1bc", "#006164"),
                              par.settings = list(axis.line = list(col = 0),
                                                  strip.background = list(col = "white"))
)

# save to disk; combine using gridExtra pkg
# pdf(paste0("out/maps/target/", TARGET, "/pdf/m_", TARGET, "_d_60_100_PI90_thresh.pdf"),
#     height = 6, width = 12)
# grid.arrange(m_PI90_60_100, m_thresh_60_100, ncol = 2)
#              # top = textGrob("pH [KCl]: 60 cm to 100 cm depth",
#              #                gp = gpar(fontsize = 16)))
# dev.off()


# 100-200 cm PI90
m_PI90_100_200 <- levelplot(r_stack_response_PI90$pH_KCl_d_100_200_mid_QRF_PI90,
                      margin = FALSE,
                      # main = expression("PI90"),
                      scales = list(draw = FALSE),
                      at = v_col_PI90,
                      col.regions = viridis(n = length(v_col_PI90),
                                            option = "viridis"),
                      par.settings = list(axis.line = list(col = 0),
                                          strip.background = list(col = "white"))
)

# 100-200 cm thresholds
m_thresh_100_200 <- levelplot(r_stack_response_thresh$pH_KCl_d_100_200_mid_QRF_PI90_thresholds,
                              att = "category",
                              margin = FALSE,
                              # main = expression("Accuracy thresholds"),
                              scales = list(draw = FALSE),
                              col.regions = c("#db4325", "#eda247", "#e6e1bc", "#006164"),
                              par.settings = list(axis.line = list(col = 0),
                                                  strip.background = list(col = "white"))
)

# save to disk; combine using gridExtra pkg
# pdf(paste0("out/maps/target/", TARGET, "/pdf/m_", TARGET, "_d_100_200_PI90_thresh.pdf"),
#     height = 6, width = 12)
# grid.arrange(m_PI90_100_200, m_thresh_100_200, ncol = 2)
#              # top = textGrob("pH [KCl]: 100 cm to 200 cm depth",
#              #                gp = gpar(fontsize = 16)))
# dev.off()



### Retrieve % of pixels for each accuracy thresholds per depth layer ----------

# list of percentages of each accuracy threshold for each depth layer
system.time(
  ls_thresh_per <- map(ls_r_response_thresh,
                     ~prop.table(table(as.vector(.x)))) %>% 
  map(., ~as_tibble(as.data.frame(.x)))
) # time elapse:  min


