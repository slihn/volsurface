library("plot3D")
library("plyr")

setwd("~/git/volsurface")

start_close <- 1848.36
start_date <- as.Date("12/31/2013", format="%m/%d/%Y")

spx_data0 <- read.csv("data/SPX_options1_2013_1231.csv")
spx_data1 <- transform(spx_data0, Expiration = as.Date(Expiration, format="%m/%d/%Y"))
spx_data1$exp_days <- as.numeric(spx_data1[["Expiration"]] - start_date)

spx_data <- spx_data1

spx_dates <- unique(spx_data[["Expiration"]])
spx_strikes <- unique(spx_data[["Strike"]])
spx_days <- unique(spx_data[["exp_days"]])

imp_vol_data <- function(in_days, in_strike) {
  d <- subset(spx_data, exp_days == in_days & Strike == in_strike & substring(OptionLongName,1,3) == "SPX")
  return(d)
}
imp_vol <- function(in_days, in_strike) {
  d <- imp_vol_data(in_days, in_strike)
  return(mean(d[["ImpliedVolatility"]], na.rm=TRUE))
}
imp_vol_mat <- function(days, strike) {
  m2 <- days
  for (r in seq(nrow(m2)))
    for (c in seq(ncol(m2)))
      m2[[r, c]] <- imp_vol(days[r,c], strike[r,c])
  return(m2)
}

x <- spx_days[spx_days <= 100]
y <- spx_strikes[abs(spx_strikes-start_close) <= 200]
M <- mesh(x, y)

x3d <- M$x
y3d <- M$y
vol3d <- imp_vol_mat(x3d, y3d)

surf3D(x = x3d, y = y3d, z = vol3d,
       colkey=FALSE, contour=TRUE, border = "black", 
       facets=FALSE, # turn this on and off
       bty="b2", theta=-45, phi=45,
       main="Volatility Surface 1", xlab="Days", ylab="Strike", zlab="Imp Vol")




