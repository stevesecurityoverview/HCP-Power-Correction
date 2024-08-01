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

# set WD
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

# Function to find the best B value and return additional metrics
find_beta <- function(data) {
  best_B <- NULL
  best_model <- NULL
  smallest_residual_sum <- Inf
  best_residuals <- NULL
  best_sd <- NULL
  best_se <- NA  # Initialize as NA to handle missing cases

  # Iterate over the range from 0 to 2 with steps of 0.01
  for (B in seq(0, 2, by = 0.01)) {
    try({
      # Fit the nonlinear model using nls
      nls_model <- nls(VOI ~ a * ICV^B, data = data, start = list(a = 1))
      residual_sum <- sum(resid(nls_model)^2)
      if (residual_sum < smallest_residual_sum) {
        smallest_residual_sum <- residual_sum
        best_B <- B
      }
    }, silent = TRUE)
  }

  # Print the best B value for debugging
  cat("Best B:", best_B, "\n")

  return(list(best_B = best_B, residual_sum = residual_sum))
}

# Function to create and display the plot with the best B
plot_regression <- function(data, local_voi) {
  local_data <- data.frame(
    VOI = data[[local_voi]],
    ICV = data[["ICV"]]
  )

  # Find the best B for PPC with the std. deviation for comparison
  best_fit <- find_beta(local_data)
  best_B <- best_fit$best_B

  # Fit the linear model
  linear_model <- lm(VOI ~ ICV, data = local_data)
  # Fit the power model by transforming ICV with the best B value
  local_data$ICV_powercorrected <- local_data$ICV^best_B
  power_model <- lm(VOI ~ ICV_powercorrected, data = local_data)
 # Calculate RSS and determine the better model
  linear_rss <- sum(residuals(linear_model)^2)
  power_rss <- sum(residuals(power_model)^2)
  better_model <- c("Linear Model", "Power Model")[which.min(c(linear_rss, power_rss))]

  # Create the plot
  p <- ggplot(local_data, aes(x = ICV, y = VOI)) +
      geom_point(color = "blue") +
      labs(
        title = paste("Regression of", local_voi, "Volume by ICV - Better Model:", better_model),
        subtitle = paste("Linear Model RSS:", round(linear_rss, 3), 
                         "| Power Model RSS:", round(power_rss, 3)),
        x = paste(local_voi, "VOI"),
        y = "Intracranial Volume (ICV)",
        linetype = "Model"
      ) +
      theme_minimal()

    p <- p + geom_smooth(method = "lm", formula = y ~ x, color = "red", se = TRUE )

    # Display the plot
    print(p)
    
    # Save the plot to a file (optional)
    ggsave(filename = paste0(local_voi, "_PPC_regression_plot.png"), plot = p)
}

# Loop through each subcortical structure and find the best x
for ( subcortical_voi in subnames) {
  cat("Finding PPC beta for:",  subcortical_voi, "\n")
  plot_regression( VOI_volumes_raw,  subcortical_voi )
}