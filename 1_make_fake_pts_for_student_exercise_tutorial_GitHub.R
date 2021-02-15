#Dr. Mary Regina Boland
#Generating dummy/fake patient data
#BMIN505 Spring 2021
#Script 1: Creates 2 fake patient datasets, 
#          one with 'baked-in' correlation and one with spurious correlation

#it is important to set the seed to ensure reproducible results later
set.seed(1234) 

#set working directory
fldr = "/Users/insertfilepathhere/"


###FIRST FAKE DATASET WITH 'BAKED IN' CORRELATION
#Comorbidities (All Binary Variables):
#Age: Above 50 (Yes/No)
#Diabetes: Yes/No
#CYP2C9: Yes/No
#Hypertension: Yes/No
#Depression: Yes/No

#Outcome (Binary Variable):
#ADE (i.e., Adverse Drug Event / Reaction)

#We are 'baking-in' a correlation between depression and diabetes and the outcome of ADE

#making 100 fake patients
num_pts = 100

#patient id - first column
fake_pt_df = data.frame(pt_id = seq(from=1001, to=1100)) #fake patient id numbs
head(fake_pt_df)



#age_above50 - second column (1=above 50, 0=less than 50)
fake_pt_df$age_above50 = c(rep(1, 80), rep(0, 20))  #80 | 20 ratio
head(fake_pt_df)

#diabetes - third column (1=has diabetes, 0=has no diabetes)
num_pts_w_diab = 50
num_pts_wo_diab = num_pts-num_pts_w_diab


set.seed(1234)
random_diabetics = sample(fake_pt_df$pt_id, num_pts_w_diab) #totally random diabetes selection
fake_pt_df$diabetes=0
fake_pt_df$diabetes[which(fake_pt_df$pt_id %in% random_diabetics)]=1
head(fake_pt_df)

#Fourth column: Cyp2c9 presence (1 = present, 0=absent)
num_pts_w_cyp = 20
random_cyp_pts = sample(fake_pt_df$pt_id, num_pts_w_cyp) #totally random cyp selection
fake_pt_df$cyp2c9=0
fake_pt_df$cyp2c9[which(fake_pt_df$pt_id %in% random_cyp_pts)]=1

#Fifth column: hypertension (1=present, 0=absent)
#70% with hypertension, 30% without hypertension
#totally random
num_pts_w_hyper = 70
random_hyper_pts = sample(fake_pt_df$pt_id, num_pts_w_hyper) #totally random hypertension patients selection
fake_pt_df$hypertension=0
fake_pt_df$hypertension[which(fake_pt_df$pt_id %in% random_hyper_pts)]=1

View(fake_pt_df)

#Sixth column: depression (1=present, 0=absent)
#50 | 50 should have depression
#but 50% of people with diabetes should have depression and 50% of depressed patients should be non-diabetics
fake_pt_df$depression = 0
num_pts_w_dep = 50
pt_ids_w_diab_and_depr =sample(random_diabetics, floor(num_pts_w_dep*.5))

random_depr_pts = sample(fake_pt_df$pt_id, num_pts_w_dep) #totally random depression patients selection (may or may not have diabetes)

pt_ids_w_depr = c(random_depr_pts, pt_ids_w_diab_and_depr )
length(pt_ids_w_depr)
#75 pts with depression
fake_pt_df$depression[which(fake_pt_df$pt_id %in% pt_ids_w_depr)]=1 

#Seventh column: ADE (1=occurred / present, 0=absent)
#all patients with both depression and diabetes have the outcome
fake_pt_df$outcome_ADE =0 
length(pt_ids_w_diab_and_depr)
fake_pt_df$outcome_ADE[which(fake_pt_df$pt_id %in% pt_ids_w_diab_and_depr)]=1 
length(pt_ids_w_diab_and_depr) #25 patients 


#patients that don't have diabetes - 25% of these need to have depression
pt_ids_wo_diab_and_depr = fake_pt_df$pt_id[which(!(fake_pt_df$pt_id %in% pt_ids_w_diab_and_depr))] 


random_pts_without_diab_and_depr_but_have_ADE = sample(pt_ids_wo_diab_and_depr, ceiling(length(pt_ids_w_diab_and_depr)/10))
fake_pt_df$outcome_ADE[which(fake_pt_df$pt_id %in% random_pts_without_diab_and_depr_but_have_ADE)]=1 

length(which(fake_pt_df$outcome_ADE=="1"))
#28 patients with an ADE
#saving our fake dataset to an external file for later use in future tutorials
write.csv(fake_pt_df, paste(fldr, "fake_pts_for_classpractice.csv", sep=""))
###END MAKING FIRST FAKE DATASET WITH BAKED IN CORRELATION


###MAKING SECOND FAKE DATASET WITHOUT 'BAKED IN' CORRELATION
#creating fake patients ids of 7 characters in length
set.seed(1234)
ids_random = runif(63334, 100, 9999999) %/% 1

#creating fake year
random_year= runif(63334, 2010, 2017)
random_year = round(random_year)

#random preterm birth - around 10%
random_preterm <- rbinom(n=63334, size=1, prob=0.10)

#random still birth - around 1%
random_stillbirth <- rbinom(n=63334, size=1, prob=0.010)

#random C-section - around 30%
random_csect <- rbinom(n=63334, size=1, prob=0.30)
table(random_csect)
weight_lbs_random=rnorm(63334,mean=170,sd=20)
hist(weight_lbs_random)
boxplot(weight_lbs_random)

age_random=rnorm(63334,28,8)               
hist(age_random)

#random-aa distribution
random_AA <- rbinom(n=63334, size=1, prob=0.47)

#random-white distribution
random_White <- rbinom(n=63334, size=1, prob=0.34)

#random hispanic distribution
random_Hispanic <- rbinom(n=63334, size=1, prob=0.08)

random_Asian <- rbinom(n=63334, size=1, prob=0.06)
random_multiplebirth <- rbinom(n=63334, size=1, prob=0.03)

random_patients_nocorrelation        = data.frame(PATIENT_ID=ids_random,
                                                  YEAR=random_year,
                                                  NH_AFAM=random_AA,
                                                  NH_WHITE=random_White,
                                                  NH_ASIAN=random_Asian,
                                                  HISPANIC=random_Hispanic,
                                                  WEIGHT_LBS=weight_lbs_random,
                                                  ENC_AGE_DEC=age_random,
                                                  MULTIPLE_BIRTH=random_multiplebirth,
                                                  STILLBIRTH=random_stillbirth,
                                                  PRETERM=random_preterm,
                                                  CSECTION=random_csect)
#saving our second fake dataset to an external file for later use in future tutorials
write.csv(random_patients_nocorrelation, paste(fldr, "random_pts_nocorrelation_for_classpractice.csv", sep=""))

