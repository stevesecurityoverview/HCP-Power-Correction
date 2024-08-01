# Clear workspace
rm(list=ls())

# Install and load required packages
if (!requireNamespace("ggplot2", quietly = TRUE)) install.packages("ggplot2")
if (!requireNamespace("minpack.lm", quietly = TRUE)) install.packages("minpack.lm")
library(ggplot2)
library(minpack.lm)

# Read data
data <- read.csv('Master_CSV_aging_neural.csv', header=TRUE)

# Define subcortical structures
subnames <- c('caudate', 'thalamus', 'putamen', 'pallidum', 'bilateral_hippocampus', 'amygdala', 'accumbens.area', 'VentralDC')

# Initialize data frame for subcortical volumes
VOI_volumes_raw <- data.frame(matrix(ncol = length(subnames) + 1, nrow = nrow(data)))
colnames(VOI_volumes_raw) <- c(subnames, "ICV")

# Combine left and right volumes
for (name in subnames) {
  matched_columns <- grep(name, colnames(data), ignore.case = TRUE, value = TRUE)
  if (length(matched_columns) > 0) {
    VOI_volumes_raw[[name]] <- rowSums(data[, matched_columns, drop = FALSE], na.rm = TRUE)
  }
}

# Add ICV
VOI_volumes_raw[["ICV"]] <- data[["EstimatedTotalIntraCranialVol"]]

# Function to estimate beta using nonlinear least squares
estimate_beta <- function(voi, icv) {
  model <- nlsLM(voi ~ alpha * icv^beta, 
                 start = list(alpha = 1, beta = 1),
                 data = data.frame(voi = voi, icv = icv))
  return(coef(model)["beta"])
}

# Function to apply PPC
apply_ppc <- function(voi, icv, beta) {
  return(voi / (icv^beta))
}

# Function to plot regression and PPC results
plot_regression_and_ppc <- function(data, voi_name, beta) {
  voi <- data[[voi_name]]
  icv <- data[["ICV"]]
  voi_ppc <- apply_ppc(voi, icv, beta)
  
  # Create plot data
  plot_data <- data.frame(
    ICV = icv,
    VOI_Raw = voi,
    VOI_PPC = voi_ppc
  )
  
  # Raw data plot
  p1 <- ggplot(plot_data, aes(x = ICV, y = VOI_Raw)) +
    geom_point(color = "blue", alpha = 0.5) +
    geom_smooth(method = "lm", formula = y ~ x, color = "red", se = TRUE) +
    labs(title = paste(voi_name, "- Raw Data"),
         x = "ICV", y = "Raw VOI") +
    theme_minimal()
  
  # PPC data plot
  p2 <- ggplot(plot_data, aes(x = ICV, y = VOI_PPC)) +
    geom_point(color = "green", alpha = 0.5) +
    geom_smooth(method = "lm", formula = y ~ x, color = "red", se = TRUE) +
    labs(title = paste(voi_name, "- PPC Data"),
         x = "ICV", y = "PPC VOI") +
    theme_minimal()
  
  # Combine plots
  combined_plot <- gridExtra::grid.arrange(p1, p2, ncol = 2)
  
  # Save the combined plot
  ggsave(filename = paste0(voi_name, "_PPC_comparison_plot.png"), plot = combined_plot, width = 12, height = 6)
  
  # Display the plot
  print(combined_plot)
}

# Initialize results dataframe
results <- data.frame(VOI = character(), Beta = numeric(), stringsAsFactors = FALSE)

# Loop through each subcortical structure
for (voi_name in subnames) {
  cat("Processing:", voi_name, "\n")
  
  # Estimate beta
  beta <- estimate_beta(VOI_volumes_raw[[voi_name]], VOI_volumes_raw[["ICV"]])
  
  # Store results
  results <- rbind(results, data.frame(VOI = voi_name, Beta = beta))
  
  # Plot regression and PPC results
  plot_regression_and_ppc(VOI_volumes_raw, voi_name, beta)
}

# Display results
print(results)

# Save results to CSV
write.csv(results, "PPC_results.csv", row.names = FALSE)