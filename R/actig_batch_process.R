#' Process and impute HBA actigraphy data
#'
#' After setting your working directory, assign your .csv's to 'files' and run the loop
#' @param
#' @keywords impute, process, actigraphy
#' @export
#' @param files list of .csv actigraphy files in your WD.
#' @examples
#' files(.csv) # list of files in wd.
#'
#'
#'
#'
#' actig_batch_process()

files <- Sys.glob("*.csv")
actig_batch_process <- lapply(files, function(f) {

  # Locate and load the file
  data <- read.csv(f, header = T, sep = ",")

  # Examine the dimensions of the data to make sure it contains
  # all the relevant statistics of your current file.
  str(data)

  # Load the required packagewd
  library(VIM)
  a <- aggr(data$Activity)
  a_plot <- aggr(data, col = c("navyblue", "red"), numbers = TRUE, sortVars = TRUE,
                 prop = FALSE, labels = names(data$G), cex.axis = 0.7, gap = 1, ylab = c("Number of missings",
                                                                                         "Combinations"))
  missings <- as.list(summary(a_plot))


  library(lubridate)
  library(dplyr)
  library(ggplot2)

  # Wrangle the dates and times
  Actig_Time <- parse_date_time(data$Time, "%I:%M:%S %p")
  Actig_Date <- dmy(data$Date)


  # Plot split by Date - will create a plot for each individual date
  no_dates <- ggplot(data = data, aes(x = Actig_Time, y = data$Off.Wrist.Status)) + geom_point() +
    scale_x_datetime(labels = function(x) format(x, format = "%H:%M")) +
    facet_wrap(~Actig_Date)

  # Plot for each day of the week over the sampling period - combines
  # duplicated days
  Weekdays <- weekdays(as.Date(Actig_Date))

  seven_days <- ggplot(data = data, aes(x = Actig_Time, y = data$Off.Wrist.Status)) + geom_point() +
    scale_x_datetime(labels = function(x) format(x, format = "%H:%M")) +
    facet_wrap(~Weekdays)

  # Test for outlying activity values

  na_rm <- na.omit(data$Activity)
  outlier_test <- ifelse(max(na_rm > 1800), print("_POSSIBLE_OUTLIERS"), print("_Unlikely_Outliers"))

  # Test for too many NA's

  NROW(data$Activity)
  percentage_na <- (sum(is.na(data$Activity))/(NROW(data$Activity))*100)
  percentage_na_test <- ifelse(percentage_na > 10, print("_HIGH_PERCENTAGE_MISSING_"), print("_lowmiss_"))

  # Test for enough days

  no_weekdays_df <- data.frame(unique(Weekdays))
  no_weekdays <- as.numeric(NROW(no_weekdays_df))
  weekdays_test <- ifelse(no_weekdays < 7, print("_NOT_ENOUGH_DAYS"), print(""))

  # Load the required packages
  library(mice)
  library(lattice)
  # Create a vector of variables to be imputed, with a predictor variable
  # (white light)
  missing_data <- data.frame(data$Activity, data$White.Light)

  tempdata2 <- mice(missing_data, m = 10, maxit = 5, method = "rf")

  #               --REMEMBER TO CHANGE THE FOLLOWING CODE TO 'tempdata' OR 'tempdata2'--

  # Visualise
  strip_stats <- summary(tempdata2)

  completedata <- mice::complete(tempdata2, 1)
  completedata <- as.data.frame(completedata)
  completedata2 <- data.frame(data, completedata)
  write.csv(completedata2, file = (paste0(f)))

  # Save lists to pass in naming conventions

  low_percentage <- round(a$percent[a$percent < 40], digits = 2)
  naming <- paste0(f, "_Imputation_Report", weekdays_test, outlier_test, percentage_na_test, low_percentage, ".pdf")

  # Create data frame with variables for plots
  plot_data <- data.frame(data$Off.Wrist.Status, Actig_Time, Actig_Date, Weekdays)

  # Save plots as a report

  pdf(file = paste0(naming))

  aggr(data, col = c("navyblue", "red"), numbers = TRUE, sortVars = TRUE,
       prop = FALSE, labels = names(data$G), cex.axis = 0.7, gap = 1, ylab = c("Number of missings",
                                                                               "Combinations"))

  print(ggplot(data = plot_data, aes(x = Actig_Time, y = data$Off.Wrist.Status)) + geom_point() +
          scale_x_datetime(labels = function(x) format(x, format = "%H:%M")) +
          facet_wrap(~Actig_Date))

  print(ggplot(data = plot_data, aes(x = Actig_Time, y = data$Off.Wrist.Status)) + geom_point() +
          scale_x_datetime(labels = function(x) format(x, format = "%H:%M")) +
          facet_wrap(~Weekdays))

  print(stripplot(tempdata2, pch = 20, cex = 1.2, main = "Observed versus Imputed with m = 10"))

  plot(data$Activity,
       col = "darkred",
       cex = 0.70,
       main = "Raw dataset",
       xlab = "Cumulative frequency of 30-second epochs",
       ylab = "Activity counts")
  abline(h = 1800, lty = "twodash", lwd = 2)


  plot(completedata$data.Activity,
       col = "darkblue",
       cex = 0.70,
       main = "Imputed dataset",
       xlab = "Cumulative frequency of 30-second epochs",
       ylab = "Activity counts")
  abline(h=1800, lty = "twodash", lwd = 2)

  dev.off()

  print(paste0(f, "        has been processed"))

})
