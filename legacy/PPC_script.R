# All is done using the beta values and equation from the following article
# https://www.ncbi.nlm.nih.gov/pmc/articles/PMC4222222
# This script applies the power proportion correction method to the subcortical 
# volumes found in the specified CSV

# Clear workspace
rm(list=ls())

# Read in thickness/volume table
file_path <- "/Users/samrosenberg/Desktop/Research/HCP-A_Project/HCP-A_Data/"
setwd(file_path)
data <- read.table('/Users/samrosenberg/Desktop/Research/HCP-A_Project/HCP-A_Data/Master_CSV_aging_neural.csv', header=TRUE, sep=',')

# Find the subcortical structure(indicated by subnames) columns
columns <- colnames(data)
#Excluded VentralDC since there was no beta value found rom it
subnames <- c('caudate', 'thalamus', 'putamen', 'pallidum', 'hippocampus', 'amygdala', 'accumbens-area')
sub_ind <- grep(paste(subnames, collapse ='|'), columns, ignore.case = TRUE)
subnames_extract <- columns[sub_ind]

# Apply PPC correction
VOI_volumes <- data.frame()
#beta <- list(0.8, 0.89, 0.72, 0.8, 0.62, 0.77, 0.95)

#matching_index <- sapply(subnames, function(x) grepl(paste(x, collapse='|'), subnames_extract[1], ignore.case = TRUE))


for (i in 1:length(sub_ind)) {
  data <- read.table('/Users/samrosenberg/Desktop/Research/HCP-A_Project/HCP-A_Data/Master_CSV_aging_neural.csv', header=TRUE, sep=',')
  
  # Find the subcortical structure(indicated by subnames) columns
  columns <- colnames(data)
  #Excluded VentralDC since there was no beta value found rom it
  subnames <- c('caudate', 'thalamus', 'putamen', 'pallidum', 'hippocampus', 'amygdala', 'accumbens-area')
  sub_ind <- grep(paste(subnames, collapse ='|'), columns, ignore.case = TRUE)
  subnames_extract <- columns[sub_ind]
}

# Make df that has all VOI and one for tICV
voi_data <- data[subnames_extract]
ICV_data <- data[["EstimatedTotalIntraCranialVol"]]

######___________________________________________________________________######
#######################Linear regression on raw volumes ####################### 

for (i in 1:length(subnames_extract)) {
  # Define the volume of interest+name
  voi <- voi_data[[i]]
  reg <- subnames_extract[[i]]
  
  # Make the linear model
  lin_model <- lm(voi ~ ICV_data)
  
  # Uncomment if you want to view all statistics
  summary(lin_model)
  
  # Make a function for line of best ffit
  func <- function(x) {
    lin_model$coefficients[[2]]*x+lin_model$coefficients[[1]]
  }
  
  
  # Plot the data with the line
  plot(ICV_data, voi,main = paste('Volume of ', reg, ' vs. tICV (uncorrected linear regression)'), xlab = 'Total Intracranial Volume(tICV)(mm^3', ylab = paste(reg, ' Volume'))
  curve(func, from = min(ICV_data), to = max(ICV_data), , col='red',  add=TRUE)
  legend("topleft", legend=c('Data', paste('y=\u03B2*x+\u03B5 \n\u03B2 = ', format(lin_model$coefficients[[2]], scientific=TRUE), '\n\u03B5 = ', 
         format(lin_model$coefficients[[1]], scientific=TRUE), '\n\n')), col = c("black", "red"), 
         pch = c(1, NA), lty = c(NA, 1), lwd = 2, cex=0.8, 
         ,x.intersp = 0.2, y.intersp = 0.1)
  ######___________________________________________________________________######
  ####################Linear regression on raw volume ratio ##################### 
  
  for (i in 1:length(subnames_extract)) {
    # Define the volume of interest+name
    voi <- voi_data[[i]]/ICV_data
    reg <- subnames_extract[[i]]
    
    # Make the linear model
    lin_model2 <- lm(voi ~ ICV_data)
    
    # Uncomment if you want to view all statistics
    summary(lin_model2)
    
    # Make a function for line of best ffit
    func <- function(x) {
      lin_model2$coefficients[[2]]*x+lin_model2$coefficients[[1]]
    }
    
    
    # Plot the data with the line
    plot(ICV_data, voi,main = paste('Volume of ', reg, '/tICV vs. tICV (uncorrected linear regression)', sep=''), xlab = 'Total Intracranial Volume(tICV)(mm^3', ylab = paste(reg, ' Volume'))
    curve(func, from = min(ICV_data), to = max(ICV_data), , col='red',  add=TRUE)
    legend("topleft", legend=c('Data', paste('y=\u03B2*x+\u03B5 \n\u03B2 = ', format(lin_model2$coefficients[[2]], scientific=TRUE), '\n\u03B5 = ', 
                                             format(lin_model2$coefficients[[1]], scientific=TRUE), '\n\n')), col = c("black", "red"), 
           pch = c(1, NA), lty = c(NA, 1), lwd = 2, cex=0.8, 
           ,x.intersp = 0.2, y.intersp = 0.1)
  }


######___________________________________________________________________######
############################## Power Regression ############################### 
# This part is done in order to find beta 
# Linear regressionn using y=log(voi), x=log(tICV)
beta <- data.frame(matrix(ncol = length(subnames_extract), nrow = 1))
colnames(beta) <- subnames_extract
alpha <- data.frame(matrix(ncol = length(subnames_extract), nrow = 1))
colnames(alpha) <- subnames_extract

ICV_log <- log(ICV_data)

for (i in 1:length(subnames_extract)) {
  # Define the volume of interest+name
  voi <- voi_data[[i]]
  voi_log <- log(voi_data[[i]])
  reg <- subnames_extract[[i]]
  
  # Make the linear model
  log_model <- lm(voi_log ~ ICV_log)
  
  # Uncomment if you want to view all statistics
  summary(log_model)
  
  # Output gives: ln(y)=coeff[1]+coeff[2]*ln(x)
  # Which is the same as: y=e^(coefff[1])*x^(coeff[2])

  func <- function(x) {
    y = exp(log_model$coefficients[[1]])*x^log_model$coefficients[[2]]
  }
  
  
  # Save the Beta Values to an Dictrionary
  beta[1, reg] <- log_model$coefficients[[2]]
  summary(log_model)
  
  # Plot the data with the line
  plot(ICV_data, voi, main = paste('Volume of ', reg, ' vs. tICV (log transformed linear regression)'), xlab = 'Total Intracranial Volume(tICV)(mm^3)', ylab = paste(reg, ' Volume'))
  curve(func, from = min(ICV_data), to = max(ICV_data), , col='red',  add=TRUE)
  legend("topleft", legend=c('Data', paste('y=\u03B1 * x^(\u03B2) \n\u03B2 = ', format(log_model$coefficients[[2]], scientific=TRUE), '\n\u03B1 = ', 
                                           format(exp(log_model$coefficients[[1]]), scientific=TRUE), '\n\n')), col = c("black", "red"), 
         pch = c(1, NA), lty = c(NA, 1), lwd = 2, cex=0.8, 
         ,x.intersp = 0.2, y.intersp = 0.1)
}


######___________________________________________________________________######
#######################Linear regression on PPC volumes ####################### 
ppc_data <- data.frame(matrix(ncol = length(subnames_extract), nrow = 722))
colnames(ppc_data) <- subnames_extract

for (i in 1:length(subnames_extract)) {
  # Define the volume of interest+name
  icv_ppc <- ICV_data^beta[[i]]
  ppc_voi <- voi_data[[i]]/icv_ppc
  reg <- subnames_extract[[i]]
  
  ppc_data[[reg]] <- ppc_voi
  
  # Make the linear model
  ppc_model <- lm(ppc_voi ~ ICV_data)
  
  # Uncomment if you want to view all statistics
  summary(ppc_model)
  
  # Make a function for line of best ffit
  func3 <- function(x) {
    ppc_model$coefficients[[2]]*x+ppc_model$coefficients[[1]]
  }
  
  
  # Plot the data with the line
  plot(ICV_data, ppc_voi,main = paste('Volume of ', reg, ' vs. tICV (linear regression with PPC voi)'), xlab = 'Total Intracranial Volume(tICV)(mm^3', ylab = paste('PPC ', reg, ' Volume'))
  curve(func3, from = min(ICV_data), to = max(ICV_data), , col='red',  add=TRUE)
  legend("topleft", legend=c('Data', paste('y=\u03B2*x+\u03B5 \n\u03B2 = ', format(ppc_model$coefficients[[2]], scientific=TRUE), '\n\u03B5 = ', 
                                           format(ppc_model$coefficients[[1]], scientific=TRUE), '\n\n')), col = c("black", "red"), 
         pch = c(1, NA), lty = c(NA, 1), lwd = 2, cex=0.8, 
         ,x.intersp = 0.2, y.intersp = 0.1)
}

data[subnames_extract] <- ppc_data

fpath <- paste(getwd(), '/ppc_updated_Master_CSV_aging_neural.csv', sep='')

write.csv(data, file=fpath,row.names=FALSE, col.names=TRUE)

