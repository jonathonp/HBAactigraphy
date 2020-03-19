#' Calculate cosinor measures from HBA actigraphy data
#' Always refer to https://rpubs.com/Jonathon_Pye for full explanation and package dependencies
#' After setting your working directory, assign your .csv's to 'files' and run the loop.
#' _cosinor and _cosinor_plots.csv will be saved in your working directory
#' @keywords process, actigraphy, cosinor
#' @export
#' @examples
#'
#'
#'
#'
#' process_cosinor()


cosinor_batch <- lapply(files, function(f) {

  library(tidyverse)
  library(lubridate)
  library(psych)

  data <- read.csv(f, header = T, sep = ",")

  data$Date <- dmy(data$Date)
  split_summary <- data.frame(split(data$data.Activity, data$Date))
  split_summary <- split_summary %>%
    transmute(split_means = rowMeans(split_summary))

  data$Time <- as.POSIXct(data$Time, format = "%I:%M:%S %p", tz = "GMT")
  data$Time <- strftime(data$Time, format = "%H:%M:%S", tz = "GMT")
  data$Time <- sapply(strsplit(data$Time,":"),
                      function(x) {
                        x <- as.numeric(x)
                        x[1]+x[2]/60
                      })

  split_time <- data.frame(split(data$Time, data$Date))

  split_time <- split_time %>%
    transmute(time_day = rowMeans(split_time))

  Activity_Rhythm <- split_summary$split_means
  Angle <- split_time$time_day
  cosinor_df <- data.frame(Angle, Activity_Rhythm)

  cosinor_results <- cosinor(cosinor_df, hours = T, na.rm = F, period = 24)


  # Save them to a .csv with filename as filename
  write.csv(cosinor_results, paste0(f, "_Cosinor.csv"))

  pdf(file = paste0(f, "_Cosinor_plots.pdf"))

  print(cosinor.plot(cosinor_df, hours = T, na.rm = F, period = 24, xlab = "Dual-day plot of averaged 24-hour activity rhythm with fitted curve",
                     ylim = c(0, 300), ylab = "Activity intensity", cex = 0.1, col = "brown4"))

  print(cosinor.plot(cosinor_df, hours = T, na.rm = F, period = 24, xlab = "Dual-day plot of averaged 24-hour activity rhythm",
                     ylim = c(0, 300), ylab = "Activity intensity", cex = 0.8, col = "brown4"))

  dev.off()

  print(paste0(f, "    has been processed"))

})
