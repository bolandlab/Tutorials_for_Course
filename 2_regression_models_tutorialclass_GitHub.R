#Dr. Mary Regina Boland
#Regression Modeling
#Using Fake Patient-Level Data with 'Built-in' Correlation
#and Fake Random Patient-Level Data without 'Built-in' Correlation
#BMIN505 Spring 2021
#Script 2: Runs logistic regression on 2 fake patient datasets (created by script #1),
#          one with 'baked-in' correlation and one with spurious correlation

#it is important to set the seed to ensure reproducible results later
set.seed(1234) 

#Read in dummy data
#set working directory
fldr = "/Users/insertfilepathhere/"

#read in file
#file below has 'built-in' correlation
fake_pt_df = read.csv(paste(fldr, "fake_pts_for_classpractice.csv", sep=""))

#file below does not have 'built-in' correlation, but some correlations could occur randomly
BIRTHfake_pt_df = read.csv(paste(fldr, "random_pts_nocorrelation_for_classpractice.csv", sep=""))


#view data
head(fake_pt_df)
head(BIRTHfake_pt_df)

###TESTING REGRESSION MODEL--with correlation model
glm_fit = glm(fake_pt_df$outcome_ADE~fake_pt_df$diabetes)
summary(glm_fit)

glm_fit = glm(fake_pt_df$outcome_ADE~fake_pt_df$age_above50+
                fake_pt_df$diabetes+
                fake_pt_df$cyp2c9+
                fake_pt_df$hypertension+
                fake_pt_df$depression )
summary(glm_fit)

#good modeling practice would be to investigate the interaction between
#all significant variables
glm_fit = glm(fake_pt_df$outcome_ADE~fake_pt_df$age_above50+
                fake_pt_df$diabetes+fake_pt_df$cyp2c9+
                fake_pt_df$hypertension+fake_pt_df$depression +
                fake_pt_df$diabetes*fake_pt_df$depression+
                fake_pt_df$age_above50*fake_pt_df$depression+
                fake_pt_df$diabetes*fake_pt_df$age_above50)
summary(glm_fit)

glm_fit = glm(fake_pt_df$outcome_ADE~fake_pt_df$age_above50+
                fake_pt_df$diabetes+
                fake_pt_df$cyp2c9+
                fake_pt_df$hypertension+
                fake_pt_df$depression +
                fake_pt_df$diabetes*fake_pt_df$depression )
summary(glm_fit)  #depression and diabetes interaction is significant
exp(coefficients(glm_fit))

#Odds Ratio and 95% CI
exp(cbind("Odds ratio" = coef(glm_fit), confint.default(glm_fit, level = 0.95)))
output = exp(cbind("Odds ratio" = coef(glm_fit), confint.default(glm_fit, level = 0.95)))
head(output)
write.csv(output, paste(fldr, "random_pts_withcorrelation_regressionmodeloutput_classpractice.csv", sep=""))


###TESTING REGRESSION MODEL--without 'built-in' correlation model (2nd fake dataset)
glm_fit_birthmodel <- glm(as.numeric(as.character(CSECTION)) ~ MULTIPLE_BIRTH +
                            PRETERM + 
                            STILLBIRTH + 
                            NH_AFAM+
                            NH_WHITE+
                            NH_ASIAN+
                            HISPANIC+
                            WEIGHT_LBS+
                            ENC_AGE_DEC,
                           data=BIRTHfake_pt_df)
summary(glm_fit_birthmodel)
#If anything is significant above then it is random correlation - because variables were created without built in correlation

exp(coefficients(glm_fit_birthmodel))

#OR and 95% CI
exp(cbind("Odds ratio" = coef(glm_fit_birthmodel), confint.default(glm_fit_birthmodel, level = 0.95)))

output = exp(cbind("Odds ratio" = coef(glm_fit_birthmodel), confint.default(glm_fit_birthmodel, level = 0.95)))
head(output)
write.csv(output, paste(fldr, "random_pts_withoutcorrelation_regressionmodeloutput_classpractice.csv", sep=""))

