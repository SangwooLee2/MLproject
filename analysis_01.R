house = read.csv("./data/train.csv", header = TRUE)
house_smpl = read.csv("./data/traindata_smpl.csv", header = TRUE) # simplieified
house_smpl_std = read.csv("./data/traindata_smpl_std.csv", header = TRUE) # simplieified
house_smpl_sh = read.csv("./data/traindata_smpl_sh.csv", header = TRUE)
house_smpl_sh_std = read.csv("./data/traindata_smpl_sh_std.csv", header = TRUE)

nrow(house)
ncol(house)
colnames(house)
  
# house1 = house[,c(2:20, 81 )]
# house %>% select(., -is.null)

library(mice) #Load the multivariate imputation by chained equations library.
mice::md.pattern(house) #
mice::md.pattern(house_smpl) #
mice::md.pattern(house_smpl_std) #
mice::md.pattern(house_smpl_sh) #
mice::md.pattern(house_smpl_sh_std) #

# lsw) any patterns in missingness? 
library(VIM)
VIM::aggr(house, bars=TRUE, numbers = TRUE, prop = TRUE, combined = TRUE)
house_new = house[, colSums(is.na(house)) != 0 ]
VIM::aggr(house_new, bars=TRUE, numbers = TRUE, prop = TRUE, combined = TRUE)

#Basic numerical EDA for states dataset.
summary(house)
sapply(house, sd)
cor(house)

summary(house_smpl)
sapply(house_smpl, sd)
cor(house_smpl)

summary(house_smpl_sh)
sapply(house_smpl_sh, sd)
cor(house_smpl_sh)

summary(house_smpl_sh_std)
sapply(house_smpl_sh_std, sd)
cor(house_smpl_sh_std)


#Basic graphical EDA for the states dataset.
plot(house)
plot(house_sh_stand)

# ----------------------------

#Creating a saturated model 
# (a model with all variables included).
# model_smpl.saturated = lm(SalePrice ~ ., data = house_smpl)
# summary(model_smpl.saturated) #Many predictor variables are not significant, yet the
#overall regression is significant.


#Creating a saturated model 
# (a model with all variables included).
model_smpl_sh_std.saturated = lm(SalePrice ~ ., data = house_smpl_sh_std)
summary(model_smpl_sh_std.saturated) #Many predictor variables are not significant, yet the
#overall regression is significant? check by seeing f-test results 
plot(model_smpl_sh_std.saturated) 
#Assessing the assumptions of the model.

# model_smpl_std.saturated = lm(SalePrice ~ ., data = house_smpl_std)
# summary(model_smpl_std.saturated) #Many predictor variables are not significant, yet the
# overall regression is significant.

# -------------------------------

library(car) #Companion to applied regression.
# influencePlot(model.saturated)
influencePlot(model_smpl_sh_std.saturated)

# vif(model.saturated) 
#Assessing the variance inflation factors for the variables
#in our model.

vif(model_smpl_sh_std.saturated)

# -----------------

#Added variable plots for assessing the contribution of each 
# additional variable.
avPlots(model_smpl_sh_std.saturated) #Distinct patterns are indications 
# of good contributions
#to the model; absent patterns usually are pointers to
#variables that could be dropped from the model.

# ------------------------------
#We note that Illiteracy has a large VIF, an insignificant p-value in the overall
#regression, and no strong distinct pattern in the added-variable plot. What
#happens when we remove it from the model?
model2 = lm(SalePrice ~ . - YearBuilt, data = house_smpl_sh_std)# lsw) - Illiteracy: Make a model not using Illiteracy 
summary(model2) #R^2 adjusted went up, model still significant, etc.
plot(model2) #No overt additional violations.
influencePlot(model2) #No overt additional violations; Hawaii actually lowers
#its hat value (leverage).
vif(model2) #VIFs all decrease.

#We can compare these two models using a partial F-test using the anova function.
#Here, the first model we supply is the reduced model, and the second is the full
#model.
anova(model2, model_smpl_sh_std.saturated) #The p-value is quite large, indicating that we
#retain the null hypothesis. Recall that the null
#hypothesis is that the slope coefficients of the
#variables in the subset of interest are all 0.
#We retain this hypothesis and conclude that the
#Illiteracy variable is not informative in our
#model; we move forward with the reduced model.
# =========> lsw) makes no difference 

model3 = lm(SalePrice ~ . - Exterior1st, data = house_smpl_sh_std)# lsw) - Illiteracy: Make a model not using Illiteracy 
summary(model3) #R^2 adjusted went up, model still significant, etc.
plot(model3) #No overt additional violations.
influencePlot(model3) #No overt additional violations; Hawaii actually lowers
vif(model3) #VIFs all decrease.
anova(model3, model_smpl_sh_std.saturated) #Th
# lsw) =========> lsw) makes no diffefence 

model4 = lm(SalePrice ~ . - Exterior2nd, data = house_smpl_sh_std)# lsw) - Illiteracy: Make a model not using Illiteracy 
summary(model4) #R^2 adjusted went up, model still significant, etc.
plot(model4) #No overt additional violations.
influencePlot(model4) #No overt additional violations; Hawaii actually lowers
vif(model4) #VIFs all decrease.
anova(model4, model_smpl_sh_std.saturated) #Th
# lsw) =========> makes no difference

model5 = lm(SalePrice ~ . - TotalBsmtSF, data = house_smpl_sh_std)# lsw) - Illiteracy: Make a model not using Illiteracy 
summary(model5) #R^2 adjusted went up, model still significant, etc.
plot(model5) #No overt additional violations.
influencePlot(model5) #No overt additional violations; Hawaii actually lowers
vif(model5) #VIFs all decrease.
anova(model5, model_smpl_sh_std.saturated) #Th
# lsw) =========> makes no difference

model6 = lm(SalePrice ~ . - GrLivArea, data = house_smpl_sh_std)# lsw) - Illiteracy: Make a model not using Illiteracy 
summary(model6) #R^2 adjusted went up, model still significant, etc.
plot(model6) #No overt additional violations.
influencePlot(model6) #No overt additional violations; Hawaii actually lowers
vif(model6) #VIFs all decrease.
anova(model6, model_smpl_sh_std.saturated) #Th
# lsw) =========> makes difference

model7 = lm(SalePrice ~ . - GarageCars, data = house_smpl_sh_std)# lsw) - Illiteracy: Make a model not using Illiteracy 
summary(model7) #R^2 adjusted went up, model still significant, etc.
plot(model7) #No overt additional violations.
influencePlot(model7) #No overt additional violations; Hawaii actually lowers
vif(model7) #VIFs all decrease.
anova(model7, model_smpl_sh_std.saturated) #Th
# lsw) =========> makes difference

model8 = lm(SalePrice ~ . - GarageArea, data = house_smpl_sh_std)# lsw) - Illiteracy: Make a model not using Illiteracy 
summary(model8) #R^2 adjusted went up, model still significant, etc.
plot(model8) #No overt additional violations.
influencePlot(model8) #No overt additional violations; Hawaii actually lowers
vif(model8) #VIFs all decrease.
anova(model8, model_smpl_sh_std.saturated) #Th
# lsw) =========> makes difference

# ------------------------------


#Let's use the partial F-test to test multiple predictors at once. 
# As compared
#to the saturated model, does the subset of Illiteracy, Area, and Income
# haveany effect on our prediction of Life.Exp?
model.reduced = lm(SalePrice ~ . - YearBuilt - Exterior1st - Exterior2nd - TotalBsmtSF - GrLivArea - GarageCars - GarageArea, 
                   data = house_smpl_sh_std)

anova(model.reduced, model_smpl_sh_std.saturated) 
# ==========> lsw) models are difference 

#Checking the model summary and assumptions of the reduced model.
summary(model.reduced)
plot(model.reduced)
influencePlot(model.reduced)
vif(model.reduced)

# ------------------------------

#We can also inspect the AIC and BIC values to 
# compare various models.
# AIC(model.full,    #Model with all variables.
#     model2,        #Model with all variables EXCEPT Illiteracy.
#     model.reduced) #Model with all variables EXCEPT Illiteracy, Area, and Income.

# BIC(model.full,
#     model2,
#     model.reduced) #Both the minimum AIC and BIC values appear alongside the
#reduced model that we tested above.

#We can use stepwise regression to help automate the 
# variable selection process.
#Here we define the minimal model, the full model, 
# and the scope of the models
#through which to search:
model.empty = lm( SalePrice ~ 1, data = house_smpl_sh_std ) 
#The model with an intercept ONLY.
scope = list(lower = formula(model.empty), upper = formula(model_smpl_sh_std.saturated))

library(MASS) #The Modern Applied Statistics library.

#Stepwise regression using AIC as the criteria (the penalty k = 2).
# lsw) 	 the multiple of the number of degrees of freedom used for the penalty. Only k = 2 gives the genuine AIC: k = log(n) is sometimes referred to as BIC or SBC 

forwardAIC = step(model.empty, scope, direction = "forward", k = 2)
backwardAIC = step(model_smpl_sh_std.saturated, scope, direction = "backward", k = 2)
bothAIC.empty = step(model.empty, scope, direction = "both", k = 2)
bothAIC.full = step(model_smpl_sh_std.saturated, scope, direction = "both", k = 2)

#Stepwise regression using BIC as the criteria (the penalty k = log(n)).
forwardBIC = step(model.empty, scope, direction = "forward", k = log(50))
backwardBIC = step(model_smpl_sh_std.saturated, scope, direction = "backward", k = log(50))
bothBIC.empty = step(model.empty, scope, direction = "both", k = log(50))
bothBIC.full = step(model_smpl_sh_std.saturated, scope, direction = "both", k = log(50))

#In this case, all procedures yield the model with only the Murder, HS.Grad,
#Frost, and Population variables intact.

#Checking the model summary and assumptions of the reduced model.
summary(forwardAIC)
plot(forwardAIC)
influencePlot(forwardAIC)
vif(forwardAIC)
avPlots(forwardAIC)
confint(forwardAIC)

summary(backwardAIC)
summary(bothAIC.empty)
summary(bothAIC.full)

summary(forwardBIC)
summary(backwardBIC)
summary(bothBIC.empty)
summary(bothBIC.full)