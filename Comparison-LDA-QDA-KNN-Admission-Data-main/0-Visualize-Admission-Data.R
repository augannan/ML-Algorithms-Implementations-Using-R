# Clear console
cat("\014")
dev.off()

# Remove all variables
rm(list=ls(all=TRUE)) 

# Set working directory
setwd('FolderPath')
# Load Data
admission = read.csv("admission.csv")

#### Data Visualization ####
str(admission)
plot(admission$GPA,admission$Group)
plot(admission$GMAT,admission$Group)
plot(admission$GMAT,admission$GPA)
cor(admission)
