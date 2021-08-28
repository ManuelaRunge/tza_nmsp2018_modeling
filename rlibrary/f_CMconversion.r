## Converted using VecNet, for comparison see http://static-content.springer.com/esm/art%3A10.1186%2Fs12936-015-0864-3/MediaObjects/12936_2015_864_MOESM1_ESM.pdf

f_CMconversion <- function(cminputvalues, fromTo = "E14toE5") {
  CMprop14days <- (c(0, 0.1, 1, 5, 10, 12, 15, 18, 20, 22, 24, 25, 28, 30, 32, 35, 36, 38, 40, 42, 45, 48, 49, 50, 53, 55, 59, 60, 62, 65, 68, 70, 73, 75, 78, 80, 82, 85, 88, 90, 95, 99, 100)) / 100
  CMxml5days <- c(0, 0.0004, 0.0037, 0.0182, 0.0356, 0.0418, 0.0516, 0.0635, 0.0725, 0.0821, 0.0921, 0.0972, 0.1125, 0.1227, 0.1329, 0.1488, 0.1544, 0.1661, 0.1782, 0.1905, 0.2093, 0.2284, 0.2348, 0.2412, 0.2598, 0.2715, 0.2957, 0.3030, 0.3210, 0.3567, 0.3949, 0.4165, 0.4449, 0.4646, 0.5010, 0.5319, 0.5644, 0.6057, 0.6466, 0.6813, 0.7934, 0.9580, 1)

  x = CMprop14days
  y = CMxml5days

  modelE14toE5 <- lm(y ~ poly(x, 3))  ##poly
  modelE5toE14 <- lm(x ~ poly(y, 3))  ##poly

  if (fromTo == "E14toE5") {
    predicted.intervals <- predict(modelE14toE5, data.frame(x = cminputvalues))
  }

  if (fromTo == "E5toE14") {
    predicted.intervals <- predict(modelE5toE14, data.frame(y = cminputvalues))
  }

  return(predicted.intervals)
}


testing = FALSE
if (testing) {
  CMprop14days <- (c(0, 0.1, 1, 5, 10, 12, 15, 18, 20, 22, 24, 25, 28, 30, 32, 35, 36, 38, 40, 42, 45, 48, 49, 50, 53, 55, 59, 60, 62, 65, 68, 70, 73, 75, 78, 80, 82, 85, 88, 90, 95, 99, 100)) / 100
  CMxml5days <- c(0, 0.0004, 0.0037, 0.0182, 0.0356, 0.0418, 0.0516, 0.0635, 0.0725, 0.0821, 0.0921, 0.0972, 0.1125, 0.1227, 0.1329, 0.1488, 0.1544, 0.1661, 0.1782, 0.1905, 0.2093, 0.2284, 0.2348, 0.2412, 0.2598, 0.2715, 0.2957, 0.3030, 0.3210, 0.3567, 0.3949, 0.4165, 0.4449, 0.4646, 0.5010, 0.5319, 0.5644, 0.6057, 0.6466, 0.6813, 0.7934, 0.9580, 1)


  y <- f_CMconversion(CMprop14days, "E14toE5")

  plot(CMprop14days, CMxml5days)
  lines(CMprop14days, y)

  CMcov <- seq(0, 1, 0.01)
  y <- f_CMconversion(CMcov, "E14toE5")
  plot(CMprop14days, CMxml5days)
  lines(CMcov, y)

  CMcov <- seq(0, 1, 0.01)
  y <- f_CMconversion(CMcov, "E5toE14")
  plot(CMxml5days, CMprop14days)
  lines(CMcov, y)

}

