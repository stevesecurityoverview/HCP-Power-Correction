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
subnames <- c('caudate', 'thalamus', 'putamen', 'pallidum', 'hippocampus', 'amygdala', 'accumbens', 'ventralDC')
sub_ind <- grep(paste(subnames, collapse ='|'), columns, ignore.case = TRUE)
subnames_extract <- columns[sub_ind]
bilateral_ind <- grep('bilateral', subnames_extract, ignore.case = TRUE)
subnames_extract <- subnames_extract[-bilateral_ind]

# Make datframe with VOI data and a vector of ICV data
VOI_volumes <- data.frame()
voi_data <- data[subnames_extract]
ICV_data <- data[["EstimatedTotalIntraCranialVol"]]

######___________________________________________________________________######
#######################Correlation on raw volumes ####################### 
corr_vals_raw <-list(length(subnames_extract))
for (i in 1:length(subnames_extract)) {
  # Define the volume of interest+name
  voi <- voi_data[[i]]
  reg <- subnames_extract[[i]]
  corr_results<- cor.test(ICV_data, voi)
  corr_vals_raw[i] <-corr_results$estimate
}
# Find max correlation 
high_raw <- which.max(unlist(corr_vals_raw))
reg<-subnames_extract[high_raw]
lin_model <- lm(voi_data[[reg]] ~ ICV_data)
func <- function(x) {
  lin_model$coefficients[[2]]*x+lin_model$coefficients[[1]]
}
par(mfrow = c(1, 2), main='PPC Results for Highest Correlated Raw Volume')
# Plot the ratio data with the best fit line
plot(ICV_data, voi_data[[reg]],main = paste('Volume of ', reg, ' vs. tICV (r=', max(unlist(corr_vals_raw)), ')',sep=''), xlab = 'Total Intracranial Volume(tICV)(mm^3', ylab = paste(reg, ' Volume', sep=''))
curve(func, from = min(ICV_data), to = max(ICV_data), , col='red',  add=TRUE)
legend("topleft", legend=c('Data', paste('y=\u03B2*x+\u03B5 \n\u03B2 = ', format(lin_model$coefficients[[2]], scientific=TRUE), '\n\u03B5 = ', 
                                         format(lin_model$coefficients[[1]], scientific=TRUE), '\n\n')), col = c("black", "red"), 
       pch = c(1, NA), lty = c(NA, 1), lwd = 2, cex=0.8, 
       ,x.intersp = 0.2, y.intersp = 0.1)

###########################Power Proportion Correction#########################

# Get beta with log transformed power regression
voi_log <- log(voi_data[[reg]])
ICV_log <- log(ICV_data)
log_model <- lm(voi_log ~ ICV_log)
summary(log_model)
beta <- log_model$coefficients[[2]]
# Output gives: ln(y)=coeff[1]+coeff[2]*ln(x)
# Which is the same as: y=e^(coefff[1])*x^(coeff[2])

# Use the found beta to get ppc volume
voi_ppc <- voi_data[[reg]]/ICV_data^beta

ppc_corr_results<- cor.test(ICV_data, voi_ppc)
ppc_corr <-ppc_corr_results$estimate

# Plot the PPC volume linear regression  
ppc_model <- lm(voi_ppc ~ ICV_data)

func <- function(x) {
  ppc_model$coefficients[[2]]*x+ppc_model$coefficients[[1]]
}


# Plot the data with the line
plot(ICV_data, voi_ppc, main = paste('Volume of PPC', reg, ' vs. tICV (r=', ppc_corr, ')', sep=''), xlab = 'tICV (mm^3)', ylab = paste('PPC ', reg, sep=''))
curve(func, from = min(ICV_data), to = max(ICV_data), , col='red',  add=TRUE)
legend("topleft", legend=c('Data', paste('y=\u03B2*x+\u03B5 \n\u03B2 = ', format(ppc_model$coefficients[[2]], scientific=TRUE), '\n\u03B5 = ', 
                                         format(ppc_model$coefficients[[1]], scientific=TRUE), '\n\n')), col = c("black", "red"), 
       pch = c(1, NA), lty = c(NA, 1), lwd = 2, cex=0.8, 
       ,x.intersp = 0.2, y.intersp = 0.1)

######___________________________________________________________________######
#######################Correlation on ratio volumes ####################### 

corr_vals_rat <-list(length(subnames_extract))
for (i in 1:length(subnames_extract)) {
  # Define the volume of interest+name
  voi <- voi_data[[i]]/ICV_data
  reg <- subnames_extract[[i]]
  corr_results<- cor.test(ICV_data, voi)
  corr_vals_rat[i] <-corr_results$estimate
}
# Find max correlation 
high_rat <- which.min(unlist(corr_vals_rat))
rat <- voi_data[[high_rat]]/ICV_data
reg<-subnames_extract[high_rat]
lin_model <- lm(rat ~ ICV_data)
func <- function(x) {
  lin_model$coefficients[[2]]*x+lin_model$coefficients[[1]]
}

par(mfrow = c(1, 2), main='PPC Results for Highest Correlated Volume Ratio(VOI/ICV)')


# Plot the ratio data with the best fit line
plot(ICV_data, rat,main = paste('Volume of ', reg, '/tICV vs. tICV (r=', min(unlist(corr_vals_rat)), ')',sep=''), xlab = 'Total Intracranial Volume(tICV)(mm^3', ylab = paste(reg, 'volume/(tICV volume)'))
curve(func, from = min(ICV_data), to = max(ICV_data), , col='red',  add=TRUE)
legend("topleft", legend=c('Data', paste('y=\u03B2*x+\u03B5 \n\u03B2 = ', format(lin_model$coefficients[[2]], scientific=TRUE), '\n\u03B5 = ', 
                                         format(lin_model$coefficients[[1]], scientific=TRUE), '\n\n')), col = c("black", "red"), 
       pch = c(1, NA), lty = c(NA, 1), lwd = 2, cex=0.8, 
       ,x.intersp = 0.2, y.intersp = 0.1)

###########################Power Proportion Correction#########################

# Get beta with log transformed power regression
voi_log <- log(voi_data[[reg]])
ICV_log <- log(ICV_data)
log_model <- lm(voi_log ~ ICV_log)
summary(log_model)
beta <- log_model$coefficients[[2]]
# Output gives: ln(y)=coeff[1]+coeff[2]*ln(x)
# Which is the same as: y=e^(coefff[1])*x^(coeff[2])

# Use the found beta to get ppc volume
voi_ppc <- voi_data[[reg]]/ICV_data^beta

ppc_corr_results<- cor.test(ICV_data, voi_ppc)
ppc_corr <-ppc_corr_results$estimate

# Plot the PPC volume linear regression  
ppc_model <- lm(voi_ppc ~ ICV_data)

func <- function(x) {
  ppc_model$coefficients[[2]]*x+ppc_model$coefficients[[1]]
}

# Plot the data with the line
plot(ICV_data, voi_ppc, main = paste('Volume of PPC', reg, ' vs. tICV (r=', ppc_corr, ')', sep=''), xlab = 'tICV (mm^3)', ylab = paste('PPC ', reg))
curve(func, from = min(ICV_data), to = max(ICV_data), , col='red',  add=TRUE)
legend("topleft", legend=c('Data', paste('y=\u03B2*x+\u03B5 \n\u03B2 = ', format(ppc_model$coefficients[[2]], scientific=TRUE), '\n\u03B5 = ', 
                                         format(ppc_model$coefficients[[1]], scientific=TRUE), '\n\n')), col = c("black", "red"), 
       pch = c(1, NA), lty = c(NA, 1), lwd = 2, cex=0.8, 
       ,x.intersp = 0.2, y.intersp = 0.1)


