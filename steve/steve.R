# Clear workspace
rm(list=ls())
# install packages
options(repos = c(CRAN = "https://cran.r-project.org"))
# Install the ggplot2 package if it is not already installed
if (!requireNamespace("ggplot2", quietly = TRUE)) {
  install.packages("ggplot2")
}

# Load the ggplot2 library
library(ggplot2)

# Read in thickness/volume table
file_path <- "./"
setwd(file_path)
data <- read.table('../Master_CSV_aging_neural.csv', header=TRUE, sep=',')
# Find the subcortical structure (indicated by subnames) columns
# Also make them the bilateral volume by combining left and right volumes -- Hippocampus is bilateral as it already exists as a bilateral volume in dataset
columns <- colnames(data)
subnames <- c('caudate', 'thalamus', 'putamen', 'pallidum', 'bilateral_hippocampus', 'amygdala', 'accumbens.area', 'VentralDC')

# Initialize a data frame for the subcortical volumes with the same number of rows as the original data
VOI_volumes_raw <- data.frame(matrix(ncol = length(subnames) + 1, nrow = nrow(data)))
colnames(VOI_volumes_raw) <- c(subnames, "ICV")

# Loop through each subname and combine left and right volumes if they exist
for (name in subnames) {
  # Find columns that match the current subname
  matched_columns <- grep(name, colnames(data), ignore.case = TRUE, value = TRUE)
  
  if (length(matched_columns) > 0) {
    # Sum the values of the matched columns
    combined_values <- rowSums(data[, matched_columns, drop = FALSE], na.rm = TRUE)
    # Add the combined values to the VOI_volumes data frame
    VOI_volumes_raw[[name]] <- combined_values
  }
}

# Add the intracranial volume (ICV) as the last column
VOI_volumes_raw[["ICV"]] <- data[["EstimatedTotalIntraCranialVol"]]

# Output the data frame to a CSV file for debugging purposes
write.csv(VOI_volumes_raw, file = "VOI_volumes_raw.csv", row.names = FALSE)

# Create a new data frame where each column is divided by the ICV
VOI_volumes_linear <- VOI_volumes_raw

# Divide each column by the ICV, except the last column
for (name in subnames) {
  VOI_volumes_linear[[name]] <- VOI_volumes_raw[[name]] / VOI_volumes_raw[["ICV"]]
}
colnames(VOI_volumes_linear) <- c(subnames, "ICV")
VOI_volumes_linear[["ICV"]] <- VOI_volumes_raw[["ICV"]] / VOI_volumes_raw[["ICV"]]

# Output the new data frame to a CSV file for debugging purposes
write.csv(VOI_volumes_linear, file = "VOI_volumes_linear.csv", row.names = FALSE)



# Define the function to create and display the plot
create_plot <- function(data, structure_to_plot, regression_vector) {
  plot_data <- data.frame(
    VOI = data[[structure_to_plot]],
    ICV = data[["ICV"]]
  )
  
  # Extract regression formula and name
  regression_formula <- regression_info$formula
  regression_name <- regression_info$name
  # Create the plot
  p <- ggplot(plot_data, aes(x = ICV, y = VOI)) +
    geom_point(color = "blue") +  # Plot the data points
    geom_smooth(method = "lm", formula = regression_formula, color = "red") +  # Add the line of best fit
    labs(title = paste("Regression of", structure_to_plot, regression_name, "Volume by ICV"),
         x = "ICV",
         y = paste(structure_to_plot, regression_name, "Volume")) +
    theme_minimal() +
    geom_errorbar(aes(ymin = VOI - sd(VOI), ymax = VOI + sd(VOI)), width = 0.2)
  
  # Display the plot
  print(p)
  
  # Save the plot to a file (optional)
  ggsave(filename = paste0(structure_to_plot, "_", regression_name, "_regression_plot.png"), plot = p)
}
regression_formulas <- list(
  list(name = "linear", formula = VOI ~ ICV),
  list(name = "log", formula = VOI ~ log(ICV)),
  list(name = "power", formula = VOI ~ ICV^2)
)
# Loop through each subcortical structure and each regression formula
for (structure in subnames) {
  for (regression_info in regression_formulas) {
    cat("Creating", regression_info$name, "regression plot for:", structure, "\n")
    create_plot(VOI_volumes_raw, structure, regression_info)
  }
}