#' Extract number of days and missingness in actigraphy recordings
#'
#' After setting your working directory, assign your .csv's to 'files' and run the loop as shown in example
#' @param files list of files in your wd
#' @keywords process, actigraphy, missing, length
#' @export
#' @param files list of .csv actigraphy files in your WD
#' @examples
#' unique_dates_and_missingness <- do.call(rbind, lapply(files, unique_dates_and_missingness))
#'
#'
#'
#' unique_dates_and_missingness()

files <- Sys.glob("*.csv")
unique_dates_and_missingness <- function(files) {
  data <- read.csv(files)
  dates <- data$Date
  unique_days <- length(unique(data$Date))
  percentage_na <- (sum(is.na(data$Activity))/(NROW(data$Activity))*100)
  percentage_na <- round(percentage_na, digits = 2)
  missing_level <- ifelse(percentage_na > 10,
                          print("File contains over 10% missing activity data"), print("OK"))
  combined <- cbind.data.frame(files, unique_days, percentage_na, missing_level)
  return(combined)
}
