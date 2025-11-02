#APHS Workshop Drs. Din Chen, Jeffrey Wilson, and Niloofar Ramezani

setwd("C:/your/folder/path")

library(haven)

#If readinghte csv file (Recommneded)
add_health_main <-read.csv(file="add_health_main.csv", header = TRUE, stringsAsFactors = FALSE)

#If reading in the full SAS data (we should delete this before presenting at APHA)
add_health_valid <- read_sas("add_health_valid.sas7bdat", NULL)
View(add_health_valid)
View(add_health_valid)
add_health_main<-add_health_valid[, -c(9:15)]

#Data prepration: GMM code requires the addition of the intercept column (1's) and certain names for the t and id variables, hence the next lines
add_health_main$ID<-as.integer(add_health_main$ID)
add_health_main <- cbind(1, add_health_main)

colnames(add_health_main)[1] <- "(Intercept)"
colnames(add_health_main)[2] <- "t"
colnames(add_health_main)[3] <- "id"

#This step needs to be done if we have unbalanced data since we are focusign on balanced data now
unique_IDs <- unique(add_health_main[,"id"])
add_health_main[,"id"] <- match(add_health_main[,"id"], unique_IDs)
unique_IDs <- unique(add_health_main[,"id"])
add_health_main[,"id"] <- match(add_health_main[,"id"], unique_IDs)

#Fixing format of variables - no need to do it here because binary variables are all coded as 0 or 1
#add_health_main$BMI<-as.factor(add_health_main$BMI)
#add_health_main$alcohol<-as.factor(add_health_main$alcohol)
#add_health_main$race_<-as.factor(add_health_main$race_)

################################
#############GLM################
################################

glm_model <- glm(BMI ~ race_ + tvhrs + alcohol+ activity+depression, data = add_health_main, family = binomial)
summary(glm_model)
glm_p_value<-coef(summary(glm_model))[,4]

#######Code to extract GLM coefficient table as an Excel sheet
# Load required package
install.packages("writexl")  # Run only once
library(writexl)

# Extract coefficients from GLM summary
coef_matrix <- summary(glm_model)$coefficients

# Convert to data frame and add variable names as a column
coef_df <- as.data.frame(coef_matrix)
coef_df$Variable <- rownames(coef_df)

# Reorder columns to put Variable first
coef_df <- coef_df[, c("Variable", "Estimate", "Std. Error", "z value", "Pr(>|z|)")]

# Optional: round for manuscript formatting
coef_df <- round(coef_df, 4)

# Add significance codes
coef_df$Significance <- cut(coef_df$`Pr(>|z|)`,
                            breaks = c(-Inf, 0.001, 0.01, 0.05, 0.1, Inf),
                            labels = c("***", "**", "*", ".", ""),
                            right = FALSE)

# Export to Excel
write_xlsx(coef_df, "glm_coefficients.xlsx")


################################
#############GLMM################
################################

# Load the lme4 package
install.packages("lme4")  # Run only once
library(lme4)

# Fit a GLMM with a random intercept for each individual
glmm_model <- glmer(BMI ~ race_ + tvhrs + alcohol + activity + depression + (1 | id),
                    data = add_health_main,
                    family = binomial)
summary(glmm_model)

#Extracting the GLMM results as Excel table
# Extract fixed effects
coef_df <- as.data.frame(summary(glmm_model)$coefficients)
coef_df$Variable <- rownames(coef_df)
coef_df <- coef_df[, c("Variable", "Estimate", "Std. Error", "z value", "Pr(>|z|)")]

# Add significance codes
coef_df$Significance <- cut(coef_df$`Pr(>|z|)`,
                            breaks = c(-Inf, 0.001, 0.01, 0.05, 0.1, Inf),
                            labels = c("***", "**", "*", ".", ""),
                            right = FALSE)

# Export to Excel
write_xlsx(coef_df, "Dglmm_coefficients.xlsx")

#############GEE################
################################

# Load the geepack package
install.packages("geepack")  # Run only once
library(geepack)

# Fit a GEE model with clustering by id
#But in geeglm() from the geepack package, the response variable must be numeric (0/1) when using family = binomial. Unlike glm(), which can handle factor responses internally, geeglm() expects a numeric binary outcome.

#If you converted variables to factor, you need to convert it back to numeric
#add_health_main$BMI<-as.numeric(add_health_main$BMI)
#add_health_main$alcohol<-as.numeric(add_health_main$alcohol)
#add_health_main$race_<-as.numeric(add_health_main$race_)

gee_model <- geeglm(BMI ~ race_ + tvhrs + alcohol + activity + depression,
                    data = add_health_main,
                    id = id,
                    family = binomial,
                    corstr = "exchangeable")
summary(gee_model)


# Extract GEE summary coefficients
gee_summary <- summary(gee_model)$coefficients
gee_df <- as.data.frame(gee_summary)

# Add variable names as a column
gee_df$Variable <- rownames(gee_df)

# Reorder columns for clarity
gee_df <- gee_df[, c("Variable", "Estimate", "Std.err", "Wald", "Pr(>|W|)")]

# Add significance codes
gee_df$Significance <- cut(gee_df$`Pr(>|W|)`,
                           breaks = c(-Inf, 0.001, 0.01, 0.05, 0.1, Inf),
                           labels = c("***", "**", "*", ".", ""),
                           right = FALSE)

# Export to Excel
write_xlsx(gee_df, "gee_coefficients.xlsx")


################################
#############GMM################
################################

#GMM using Lai and Small method
#sourcing my own GMM code 
source("gmm_logistic.R")

glm00 <- glm(BMI ~ race_ + tvhrs + alcohol+ activity+depression, data = add_health_main, family = binomial)
#glm pvalue: coef(summary(glm00))[,4]
glm00_p_value<-coef(summary(glm00))[,4]

#For this code which we source, dataset needs to be in matrix format and binary variables need to be converted to numeric
add_health_main$BMI<-as.numeric(add_health_main$BMI)
add_health_main$alcohol<-as.numeric(add_health_main$alcohol)
add_health_main$race_<-as.numeric(add_health_main$race_)
add_health_main2<-as.matrix(add_health_main)

gmm_model <- gmm_logistic(add_health_main2, subjects = "id", time = "t", response = "BMI", type0 = c("(Intercept)", "race_"), type1 = c("tvhrs", "alcohol"), type2 = c("activity"), type3 = c("depression"), init = coef(glm00))

gmm_model$coef
sqrt(diag(gmm_model$vcov))
P_values<- 2 * pnorm(-abs(gmm_model$coef/sqrt(diag(gmm_model$vcov)))) 
labels1<-c("GMM coeff", "GMM SE", "GMM p-value")

results<-cbind(gmm_model$coefficients, sqrt(diag(vcov(summary(gmm_model)))),gmm_model$coef, sqrt(diag(gmm_model$vcov)),P_values)
final_table<-rbind(labels1, results)
write.csv(final_table, file = "GMM_logistic_noLags_BMIdata.csv")
