##### capstone ---------------------------------
library(dplyr)

# these values are taken from histograms in script 5
# https://code.earthengine.google.com/d25862a76dfa67e03b3533c377825dc2

evi_bp_slm <- c(0.546, 0.44, 0.507, 0.67, 0.668, 0.755, 0.851, 0.883, 0.949, 0.987, 0.918, 1.152, 1.096, 1.008, 1.688, NA, NA, NA)
evi_bp <- c(5.340, 4.385, 5.612, 5.710, 5.754, 6.616, 7.278, 6.46, 6.27, 6.311, 6.58, 8.795, 6.659, 7.58, 14.758, NA, NA, NA)

ndvi_bp_slm <- c(0.72, 0.55, 0.63, 0.778, 0.82, 0.879, 0.936, 0.968, 1.012, 1.071, 0.956, 1.29, 1.192, 1.127, 1.745, NA, NA, NA)
ndvi_bp <- c(6.298, 5.612, 6.454, 7.055, 7.334, 7.721, 9.018, 7.825, 7.818, 7.286, 7.939, 10.289, 8.180, 8.618, 16.355, NA, NA, NA)

breakpoints <- data.frame(
    year = 2004:2021,
    evi_slm = evi_bp_slm,
    evi = evi_bp,
    ndvi_slm = ndvi_bp_slm,
    ndvi = ndvi_bp
  )

bp <- breakpoints %>% 
  mutate(share_slm_evi = evi_slm / evi,
         share_slm_ndvi = ndvi_slm / ndvi
         )
par(mfrow = c(1,3), cex = 1.1, mar = c(4,4,4,2))
plot(ndvi_slm ~ year, data = bp, type = "l", lwd = 2, ylim = c(.45, 1.9),
     main = "", xlab = "", ylab = "freq[.]", las = 2) #Breakpoints SLM
lines(evi_slm ~ year, data = bp, type = "l", lwd = 2, col = "red")
legend("top", legend = c("ndvi", "evi"), col = c("black", "red"), lwd = 2, bty = "n")
plot(ndvi ~ year, data = bp, type = "l", lwd = 2, ylim = c(4,17),
     main = "", ylab = "freq[.]", las = 2) #Breakpoints Tot
lines(evi ~ year, data = bp, type = "l", lwd = 2, col = "red")
plot(share_slm_ndvi ~ year, data = bp, type = "l", lwd = 2, col = "red",
     lty = 1 , ylim = c(.09, .17), main = "", xlab = "",
     ylab = "share[.]", las = 2) #Ratio BP-SLM / BP-Tot 
lines(share_slm_evi ~ year, data = bp, type = "l", lwd = 2, lty = 1)

# MODIS 061 --------------------------------------------------------------------

evi_bp_slm_q61 <- c(0.472, 0.366, 0.454, 0.549, 0.549, 0.629, 0.703, 0.789, 0.834, 0.914, 0.939, 1.059, 0.982, 0.931, 1.107, 1.489, 1.355, 1.565)

evi_bp_q61 <- c(5.754, 4.470, 5.805, 5.811, 6.181, 7.267, 7.364, 7.125, 6.753, 6.608, 7.273, 9.299, 7.239, 7.545, 10.436, 10.993, 11.956, 14.286)

ndvi_bp_slm_q61 <- c(0.535, 0.421, 0.483, 0.604, 0.626, 0.682, 0.729, 0.788, 0.869, 0.903, 0.911, 1.087, 1.053, 0.895, 1.061, 1.426, 1.324, 1.513)

ndvi_bp_q61 <- c(6.965, 5.515, 6.990, 7.061, 7.236, 8.698, 9.378, 8.375, 8.256, 7.983, 8.781, 10.867, 9.284, 8.988, 12.031, 12.276, 12.854, 15.668)

breakpoints_q61 <- data.frame(
  evi_slm_q61 = evi_bp_slm_q61,
  evi_q61 = evi_bp_q61,
  ndvi_slm_q61 = ndvi_bp_slm_q61,
  ndvi_q61 = ndvi_bp_q61
)
bp <- cbind(breakpoints, breakpoints_q61)

bp <- bp %>% 
  mutate(share_slm_evi = evi_slm / evi,
         share_slm_ndvi = ndvi_slm / ndvi,
         share_slm_evi_q61 = evi_slm_q61 / evi_q61,
         share_slm_ndvi_q61 = ndvi_slm_q61 / ndvi_q61,
  )

par(mfrow = c(2,3), cex = 1.1, mar = c(3,4,1,1), las = 2, oma = c(0,0,0,0))

# Plot 2
plot(ndvi ~ year, data = bp, type = "l", lwd = 2, ylim = c(4,22),
     main = "", ylab = "freq[.]", las = 2, xaxt = "n") # Breakpoints Tot
lines(evi ~ year, data = bp, type = "l", lwd = 2, col = "red")
legend("topleft", legend = c( "MOD13Q1.006", "NDVI", "EVI"), col = c("black", "black", "red"), lwd = c(NA,2,2), bty = "n")

# Plot 1
plot(ndvi_slm ~ year, data = bp, type = "l", lwd = 2, ylim = c(.45, 3),
     main = "", xlab = "", ylab = "freq[.]", las = 2, xaxt = "n") # Breakpoints SLM
lines(evi_slm ~ year, data = bp, type = "l", lwd = 2, col = "red")


# Plot 3
plot(share_slm_ndvi ~ year, data = bp, type = "l", lwd = 2, col = "red",
     lty = 1, ylim = c(.07, .17), main = "", xlab = "",
     ylab = "share[.]", las = 2, xaxt = "n") # Ratio BP-SLM / BP-Tot 
lines(share_slm_evi ~ year, data = bp, type = "l", lwd = 2, lty = 1)

# Plot 5
plot(ndvi_q61 ~ year, data = bp, type = "l", lwd = 2, ylim = c(4,22),
     main = "", ylab = "freq[.]", las = 2, xaxt = "n") # Breakpoints Tot
axis(1, at = bp$year, labels = bp$year)
lines(evi_q61 ~ year, data = bp, type = "l", lwd = 2, col = "red")
legend("topleft", legend = "MOD13Q1.061", bty = "n")

# Plot 4
plot(ndvi_slm_q61 ~ year, data = bp, type = "l", lwd = 2, ylim = c(.45, 3),
     main = "", xlab = "", ylab = "freq[.]", las = 2, xaxt = "n") # Breakpoints SLM
axis(1, at = bp$year, labels = bp$year)
lines(evi_slm_q61 ~ year, data = bp, type = "l", lwd = 2, col = "red")


# Plot 6
plot(share_slm_ndvi_q61 ~ year, data = bp, type = "l", lwd = 2, col = "red",
     lty = 1, ylim = c(.07, .17), main = "", xlab = "",
     ylab = "share[.]", las = 2, xaxt = "n") # Ratio BP-SLM / BP-Tot 
axis(1, at = bp$year, labels = bp$year)
lines(share_slm_evi_q61 ~ year, data = bp, type = "l", lwd = 2, lty = 1)
