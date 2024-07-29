#model <- nls(Y ~ a * exp(b * X))
setwd("C:/Users/david/Desktop/R_code/HCP_PPC")
master_df <- read.csv("Master_CSV_aging_neural.csv")
head(data_frame)

df <- master_df[, c("EstimatedTotalIntraCranialVol","Right.Hippocampus")]
# head(df)

ICV <- df[,"EstimatedTotalIntraCranialVol"]
VOI_H <- df[,"Right.Hippocampus"]

plot(ICV,VOI_H)

# model <- nls(VOI_H ~ a * exp(b * ICV), start = list(a = 1, b = 1))
# model <- nls(log(VOI_H) ~ a * exp(b * log(ICV)), start = list(a = 1, b = 0.5))
model <- nls(log(VOI_H) ~ a * exp(b * log(ICV)), start = list(a = 1, b = 0.5), control = nls.control(maxiter = 1000))