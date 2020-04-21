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


process_nonparametric <- function(files = files) {
  if (!require("pacman")) install.packages("pacman")
  pacman::p_load(nparACT, tidyverse, lubridate)
  
  library(nparACT)
  library(tidyverse)
  library(lubridate)
  
  for (filename in files) {
    # Read data:
    data <- read.csv(filename, header = T, sep = ",")

    # Create data.frame for nparACT:
    npar_time <- lubridate::parse_date_time(data$Time, '%I:%M:%S %p')
    activity <- data$data.Activity
    npar <- data.frame(npar_time, activity)

    # nparact function:
    npar_output <- nparACT::nparACT_base("npar", 2/60, cutoff = 1, plot = F, fulldays = F)

    # Write new data to a new excel file:
    write.table(npar_output, "npar_Results.csv",
                append = TRUE, sep = ",", row.names = paste(filename), col.names = FALSE)

  }
}
