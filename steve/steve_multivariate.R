# Clear workspace
rm(list=ls())
# install packages
install.packages("ggplot2")

# Read in thickness/volume table
file_path <- "./"
setwd(file_path)
data <- read.table('../Master_CSV_aging_neural.csv', header=TRUE, sep=',')
# Find the subcortical structure (indicated by subnames) columns
# Also make them the bilateral volume by combining left and right volumes -- Hippocampus is bilateral as it already exists as a bilateral volume in dataset
columns <- colnames(data)
subnames <- c('caudate', 'thalamus', 'putamen', 'pallidum', 'bilateral_hippocampus', 'amygdala', 'accumbens.area', 'VentralDC')
multivariates <- c('Age.yrs', 'Gender', 'BMI', 'height', 'weight', 'Education.yrs', 'walk.pace', '2.min', 'walk.pace')

# Initialize a data frame for the subcortical volumes with the same number of rows as the original data
VOI_volumes_raw <- data.frame(matrix(ncol = length(subnames) + 1, nrow = nrow(data)))
VOI_volumes_raw_multivariate <- data.frame(matrix(ncol = length(subnames+multivariates) + 1, nrow = nrow(data)))
colnames(VOI_volumes_raw) <- c(subnames, "ICV")
colnames(VOI_volumes_raw_multivariate) <- c(subnames, "ICV")

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
  VOI_volumes_linear[[name]] <- VOI_volumes[[name]] / VOI_volumes[["ICV"]]
}

# Set the last column to 1
VOI_volumes_linear[["ICV"]] <- 1

# Output the new data frame to a CSV file for debugging purposes
write.csv(VOI_volumes_linear, file = "VOI_volumes_linear.csv", row.names = FALSE)



# Define the subcortical structure to plot (e.g., 'caudate')
local_structure <- "caudate"

# Create a data frame for plotting
plot_data <- data.frame(
  VOI = VOI_volumes_linear[[local_structure]],
  ICV = VOI_volumes_linear[["ICV"]]
)

# Fit a linear model
linear_model <- lm(VOI ~ ICV, data = plot_data)

# Create the plot
p <- ggplot(plot_data, aes(x = ICV, y = VOI)) +
  geom_point(color = "blue") +  # Plot the data points
  geom_smooth(method = "lm", color = "red") +  # Add the line of best fit
  labs(title = paste("Linear Regression of", local_structure, "Volume by ICV"),
       x = "ICV",
       y = paste(local_structure, "Volume")) +
  theme_minimal()

# Display the plot
print(p)

# Add standard deviation bands
p <- p + geom_errorbar(aes(ymin = VOI - sd(VOI), ymax = VOI + sd(VOI)), width = 0.2)

# Display the updated plot with standard deviation bands
print(p)