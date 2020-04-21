#' Calculate nonparametric measures for HBA actigraphy recordings
#' Always refer to https://rpubs.com/Jonathon_Pye for full explanation and package dependencies
#' After setting your working directory, assign your .csv's to 'files' and run the loop.
#' npar_results will be saved in your working directory and the header names will be printed in the console
#' @keywords process, actigraphy, nonparametric
#' @export
#' @examples
#'
#'
#'
#'
#' process_nonparametric()


process_nonparametric <- function(files) {
  if (!require("pacman")) install.packages("pacman")
  pacman::p_load(nparact, tidyverse, lubridate)
  
  for (filename in files) {
    # Read data:
    data <- read.csv(filename, header = T, sep = ",")

    # Create data.frame for nparACT:
    data$npar_time <- parse_date_time(data$Time, '%I:%M:%S %p')
    npar <- data.frame(data$npar_time, data$data.Activity)

    # nparact function:
    npar_output <- nparACT_base("npar", 2/60, cutoff = 1, plot = F, fulldays = F)

    # Write new data to a new excel file:
    write.table(npar_output, "npar_Results.csv",
                append = TRUE, sep = ",", row.names = paste(filename), col.names = FALSE)

  }
}