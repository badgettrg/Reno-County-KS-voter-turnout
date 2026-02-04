
# This file is available athttps://github.com/badgettrg/Hutchinson-KS-voter-turnout
# Author: bob.badgett@gmail.com
# Permissions:
#* Code GNU GPLv3 https://choosealicense.com/licenses/gpl-3.0/
#* Images CC BY-NC-SA 4.0 https://creativecommons.org/licenses/by-nc-sa/4.0/
# Optimized for coding with R Studio document outline view
# Last edited 2025-05-08

# Put the datafile into a subdiredctory, "DS0001"

# Startup -----
## Troubleshooting -----
options(error = NULL)   # Default

## Set working directory -----
if (Sys.getenv("RSTUDIO") != "1"){
  args <- commandArgs(trailingOnly = FALSE)
  script_path <- sub("--file=", "", args[grep("--file=", args)])  
  script_path <- dirname(script_path)
  setwd(script_path)
}else{
  setwd(dirname(rstudioapi::getSourceEditorContext()$path))
  #ScriptsDir <- paste(getwd(),'/Scripts',sep='')
}
getwd()

# Packages ------
library(prettyR)
library(crayon)


load(paste0("DS0001/","38506-0001-Data.rda"))

names(da38506.0001)

summary(da38506.0001$VOTER_TURNOUT_PCT)

summary(da38506.0001$REG_VOTER_TURNOUT_PCT)

years <- sort(unique(da38506.0001$YEAR))

df_list <- lapply(years, function(yr) {
  year_data <- da38506.0001[da38506.0001$YEAR == yr, ]
  
  # Presidential indicator (2000, 2004, 2008, etc.)
  is_presidential <- if ((yr - 2000) %% 4 == 0) 1 else 0
  
  # National Summary & Median
  s <- summary(year_data$VOTER_TURNOUT_PCT)
  national_summary <- paste(names(s), s, sep = ": ", collapse = " | ")
  nat_median <- median(year_data$VOTER_TURNOUT_PCT, na.rm = TRUE)
  
  # Hutchinson Specific (FIPS 20155)
  hutch_row_val <- year_data$VOTER_TURNOUT_PCT[year_data$STCOFIPS10 == 20155]
  
  if (length(hutch_row_val) > 0 && !is.na(hutch_row_val)) {
    hutch_val <- hutch_row_val
    h_percentile <- mean(year_data$VOTER_TURNOUT_PCT <= hutch_val, na.rm = TRUE) * 100
  } else {
    hutch_val <- NA
    h_percentile <- NA
  }
  
  data.frame(
    YEAR = yr,
    Presidential_election = is_presidential,
    national_VOTER_TURNOUT_PCT_summary = national_summary,
    national_median_VOTER_TURNOUT_PCT = nat_median,
    Hutch_VOTER_TURNOUT_PCT = hutch_val,
    Hutch_VOTER_TURNOUT_percentile = h_percentile,
    stringsAsFactors = FALSE
  )
})

df_National_and_Hutch <- do.call(rbind, df_list)

# Convert the percentile string column back to numeric if it isn't already
# (Ensure Hutch_VOTER_TURNOUT_percentile is numeric for calculation)

# Calculate medians grouped by Presidential_election
# 1. Aggregate the three key values by the Presidential_election indicator
summary_by_type <- aggregate(
  cbind(
    national_median_VOTER_TURNOUT_PCT, 
    Hutch_VOTER_TURNOUT_PCT, 
    Hutch_VOTER_TURNOUT_percentile
  ) ~ Presidential_election, 
  data = df_National_and_Hutch, 
  FUN = median, 
  na.rm = TRUE
)

# 2. Rename columns for clarity (placing Hutch Turnout in the second data column)
colnames(summary_by_type) <- c(
  "Is_Presidential", 
  "National_Median_Turnout", 
  "Hutch_Voter_Turnout_Pct", 
  "Hutch_Median_Percentile"
)

# 3. Display the results
print(summary_by_type)

# Ensure we are using the summary_by_type created in the previous step
cat("\n--- Voter Turnout Comparison ---\n\n")

for(i in 1:nrow(summary_by_type)) {
  # Determine label based on Is_Presidential column
  type_label <- ifelse(summary_by_type$Is_Presidential[i] == 1, 
                       "Presidential Years", 
                       "Non-Presidential Years")
  
  cat(type_label, ":\n")
  
  # Display the National Median
  cat("  National Median Turnout: ", 
      round(summary_by_type$National_Median_Turnout[i], 4), "\n")
  
  # Display the Hutchinson specific Turnout
  cat("  Hutchinson Turnout: ", 
      round(summary_by_type$Hutch_Voter_Turnout_Pct[i], 4), "\n")
  
  # Display the Hutchinson Median Percentile
  cat("  Hutchinson Median Percentile: ", 
      round(summary_by_type$Hutch_Median_Percentile[i], 1), "%\n\n")
}


