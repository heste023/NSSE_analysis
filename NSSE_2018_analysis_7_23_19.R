library(tidyverse)
library(haven)
library(caret)

# problem: predict overall satisfaction with experience at UAB 

# data tidying ----------------------------------------------------------------

# read in nsse 2012, 2014, and 2016 files
df.nsse.2014 <- read_sav('NSSE14_data.sav')
df.nsse.2016 <- read_sav('NSSE16_data.sav')
df.nsse.2018 <- read_sav('NSSE18_data.sav')

# quick summaries
summary(df.nsse.2014)
summary(df.nsse.2016)
summary(df.nsse.2018)

# glance at data missingness
df.nsse.2014 %>%
  map_df(function(x) sum(is.na(x))) %>%
  gather(var.name, num_nulls) %>%
  arrange(-num_nulls) 

df.nsse.2016 %>%
  map_df(function(x) sum(is.na(x))) %>%
  gather(var.name, num_nulls) %>%
  arrange(-num_nulls) 

df.nsse.2018 %>%
  map_df(function(x) sum(is.na(x))) %>%
  gather(var.name, num_nulls) %>%
  arrange(-num_nulls)

# look at variables in common across all years
vnames.overlap <- intersect(intersect(names(df.nsse.2014), names(df.nsse.2016)), names(df.nsse.2018))

# quick variable renames and add census_desc variable for merging
# filter dataframes down to variables in missing.df and add CENSUS_DESC columns
df.nsse.2014 <- df.nsse.2014 %>%
  select(!!vnames.overlap, BANNER_ID = studentID)
df.nsse.2014$CENSUS_DESC <- '201430'

df.nsse.2016 <- df.nsse.2016 %>%
  select(!!vnames.overlap, PERSON_UID = studentID)
df.nsse.2016$CENSUS_DESC <- '201630'

df.nsse.2018 <- df.nsse.2018 %>%
  select(!!vnames.overlap, PERSON_UID = studentID)
df.nsse.2018$CENSUS_DESC <- '201830'

# read in spring 2014, 2016, and 2018 census files 
df.spring.census <- read_csv('Spring_census_records.csv')

# select relevant variables from census file
df.spring.census <- df.spring.census %>%
  select(PERSON_UID, BANNER_ID, CENSUS_DESC, AGE_AT_CENSUS, 
         REPORTING_LEVEL_DESC, PROGRAM_LEVEL_DESC,
         COLLEGE_DESC, MAJOR_DESC, GENDER_DESC, PRIMARY_ETHNICITY_DESC, 
         CITIZENSHIP_DESC, STUDENT_RESIDENCY_DESC, HOUSING_IND, TEST_SCORE,
         TEST_TYPE)

# merge 2014 data 
df.nsse.2014$CENSUS_DESC <- as.character(df.nsse.2014$CENSUS_DESC)
df.spring.census$CENSUS_DESC <- as.character(df.spring.census$CENSUS_DESC)
df.nsse.2014.2 <- df.nsse.2014 %>%
  inner_join(df.spring.census, by = c('BANNER_ID', 'CENSUS_DESC'))

# merge 2016 and 2018 data
df.nsse.16.18 <- df.nsse.2016 %>%
  rbind(df.nsse.2018)

df.nsse.16.18$PERSON_UID <- as.character(df.nsse.16.18$PERSON_UID)
df.nsse.16.18$CENSUS_DESC <- as.character(df.nsse.16.18$CENSUS_DESC)
df.spring.census$PERSON_UID <- as.character(df.spring.census$PERSON_UID)

df.nsse.16.18 <- df.nsse.16.18 %>%
  inner_join(df.spring.census, by = c('PERSON_UID', 'CENSUS_DESC'))

# make full nsse data frame 
df.combined <- df.nsse.2014.2 %>%
  rbind(df.nsse.16.18)

# a few visuals -----------------------------------------------------------

# key variables to explore: evalexp and sameinst

# plot evalexp  
ggplot(data = df.combined, aes(evalexp)) + 
  geom_bar() + 
  labs(title = 'Overall Evaluation of UAB Experience', 
       x = 'Overall Evaluation', 
       y = 'Count') +
  theme(plot.title = element_text(hjust = 0.5))

# plot sameinst
ggplot(data = df.combined, aes(sameinst)) + 
  geom_bar() + 
  labs(title = 'If Starting Over, Would Choose UAB', 
       x = 'Overall Evaluation', 
       y = 'Count') +
  theme(plot.title = element_text(hjust = 0.5))

# note: both outcome variables are heavily skewed

# let's start with clasfication models

# create classification variable 

# working dataset -------------------------------------------------------------
df1 <- df.combined

# initial issues --------------------------------------------------------------

# 1 - missing data - options: drop variables, impute missing data 

# 2 - variables/variable selection - options: identify near zero variance  
#                         predictors, remove correlated predictors, stepwise 
#                         selection, pca 

# 3 - unbalanced dataset - options: 

# filter to non-missing values of evalexp and fit a few options 
# start with evalexp

# no.missing evalexp data frame 
df.nomissing <- df1 %>%
  filter(!is.na(evalexp)) 

# data pre-processing ---------------------------------------------------------

# first, remove 'index' variables from model and repetitive demographic 
# variables
#
# note: circle back around to check model with index features and a model
#       that recreates index

df.nomissing <- df.nomissing %>%
  select(-HO, -RI, -LS, -QR, -CL, -DD, -SF, -ET, -QI, -SE) %>%
  select(-IRclass, -IRenrollment, -IRftfy, -popid, -unitid, 
         -satw, -satm, -satv, -MAJsecondcol, -MAJsecondcode,
         -actcomp, -MAJnum, -MAJfirst, -MAJsecond, -MAJfirstcode, 
         -MAJfirstcol, -onlinenum, -genderid_txt, -birthyear, 
         -re_amind, -re_asian, -re_black, -re_latino, 
         -re_pacific, -re_white, -re_other, -re_pnr, -re_all, 
         -sexorient14_txt, -IRsex, -IRrace, -group1, -group2, 
         -group3, -group4, -group5, -sample, -eligible, -modecomp,
         -genderid, -age, -agecat, -sameinst, 
         -disability_all, -living)  # disability_all and living dropped 
                                    # after exploratory fit 

# revisit missing values 
#library(naniar)
#vis_miss(df1) 

# visual was indecipherable...lets look at a table with missing value counts
missing.df <- df.nomissing %>%
  map_df(function(x) sum(is.na(x))) %>%
  gather(var.name, num_nulls) %>%
  arrange(num_nulls) 

# percent missing 
missing.df.2 <- df.nomissing %>%
  map_df( ~ mean(is.na(.))) %>%
  gather(var.name, pct_null) %>%
  arrange(-pct_null)  

# take a closer look at variables missing more than 20%

# 'dis' questions were summmed - can drop all but disability 
# note: may need to recode disability
df.nomissing <- df.nomissing %>%
  select(-dis_sense, -dis_mobility, -dis_learning, -dis_mental, -dis_other)

# reload missing.df.2 with dis variables dropped
missing.df.2 <- df.nomissing %>%
  map_df( ~ mean(is.na(.))) %>%
  gather(var.name, pct_null) %>%
  arrange(-pct_null)

# look at GPI, ADV, and HPI variables
# GPI, ADV, and HPI variables are from topical modules - can be dropped
df.nomissing <- df.nomissing %>%
  select(-starts_with('ADV')) %>%
  select(-starts_with('GPI')) %>%
  select(-starts_with('HIP'))

# reload missing.df.2 with variables dropped
missing.df.2 <- df.nomissing %>%
  map_df( ~ mean(is.na(.))) %>%
  gather(var.name, pct_null) %>%
  arrange(-pct_null)

# test_type and test_score are the only variables with over 
# 15% missingness - test_score is based on ACT standardized test scale
# so test_type can be dropped
df.nomissing <- df.nomissing %>%
  select(-TEST_TYPE)

# note: students without test_score are likely transfer students 
# should correspond with begincol

# revisit missing values 
library(naniar)
vis_m <- vis_miss(df.nomissing) 

# much better 

# imputation ------------------------------------------------------------------

# note: mostly ordinal data - let's treat it as interval data, nominal data  
#       should get one-hot encoding

# start with test_score imputation - try median

# convert to numeric 
df.nomissing$TEST_SCORE <- as.numeric(df.nomissing$TEST_SCORE)

# summarize before imputation
summary(df.nomissing$TEST_SCORE)

# select variables to impute 
df.medimpute <- df.nomissing %>%
  select(TEST_SCORE)

# median impute
medimpute_vars <- preProcess(df.medimpute, method = c("medianImpute"))

# Use predict to transform data
medimpute_df_pred <- predict(medimpute_vars, df.nomissing)

# summarize after imputation
summary(medimpute_df_pred$TEST_SCORE)

# look at transformed features
ggplot(medimpute_df_pred, aes(x = TEST_SCORE)) + 
  geom_histogram()

ggplot(df.nomissing, aes(x = TEST_SCORE)) + 
  geom_histogram()

# questionable - look for a better way to impute test_score

# any other predictors that are highly correlated with test_score?

library(Hmisc)

# convert variables to numeric and look 
df.nm.corr <- df.nomissing %>%
  select(-class, -fulltime, -begincol, -edaspire, -firstgen, -internat, -greek, 
         -athlete, -veteran, -disability, -sexorient14, -BANNER_ID, 
         -logdate, -duration, -CENSUS_DESC, -PERSON_UID, -AGE_AT_CENSUS, 
         -REPORTING_LEVEL_DESC, -PROGRAM_LEVEL_DESC, -COLLEGE_DESC, -MAJOR_DESC, 
         -GENDER_DESC, -PRIMARY_ETHNICITY_DESC, -CITIZENSHIP_DESC, -STUDENT_RESIDENCY_DESC, 
         -HOUSING_IND) %>% 
  mutate_if(is.character, as.numeric) %>%
  as.matrix() %>%
  rcorr(type = "spearman")

library(corrplot)
corrplot(df.nm.corr$r, type = "upper" )

# maybe grades, parent's ed, and research 

# come back to this: try to impute median on other variables, then impute 
# TEST_SCORE with knn

# median imputation 

df.medimpute <- medimpute_df_pred %>%
  select(-class, -fulltime, -begincol, -edaspire, -firstgen, -internat, -greek,  
         -athlete, -veteran, -disability, -sexorient14, -BANNER_ID, 
         -logdate, -duration, -CENSUS_DESC, -PERSON_UID, -AGE_AT_CENSUS, 
         -REPORTING_LEVEL_DESC, -PROGRAM_LEVEL_DESC, -COLLEGE_DESC, -MAJOR_DESC, 
         -GENDER_DESC, -PRIMARY_ETHNICITY_DESC, -CITIZENSHIP_DESC, -STUDENT_RESIDENCY_DESC, 
         -HOUSING_IND) 
df.medimpute <- lapply(df.medimpute, function(x) as.numeric(as.character(x)))
df.medimpute <- as.data.frame(df.medimpute)

# median impute
medimpute_vars <- preProcess(df.medimpute, method = c("medianImpute"))

# Use predict to transform data
df.medimpute <- predict(medimpute_vars, medimpute_df_pred)

# may need to come back and examine changes - for now, move forward 

# glance at missing values and make sure everything worked correctly
missing.df.2 <- df.medimpute %>%
  map_df( ~ mean(is.na(.))) %>%
  gather(var.name, pct_null) %>%
  arrange(-pct_null)

# modal imputation for the remaining missing values 

# note: tidyimpute errors out on haven_labelled objects

# restructure a few variables
cols <- c('veteran', 'internat', 'sexorient14', 'athlete', 
          'greek', 'disability', 'firstgen', 'edaspire', 
          'fulltime', 'begincol', 'class', 'PRIMARY_ETHNICITY_DESC')
df.medimpute[cols] <- lapply(df.medimpute[cols], factor)

# check the result
df.str.check <- sapply(df.medimpute, class)

# function to calculate mode
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

# impute mode for remaining variables with missing values 
df.medimpute <- df.medimpute %>% 
  mutate_if(is.factor, funs(replace(.,is.na(.), Mode(na.omit(.)))))

# final glance at missing values 
missing.df.2 <- df.medimpute %>%
  map_df( ~ mean(is.na(.))) %>%
  gather(var.name, pct_null) %>%
  arrange(-pct_null)

# one-hot encoding for nominal variables: sexorient14, PROGRAM_LEVEL_DESC, 
#                                         COLLEGE_DESC, MAJOR_DESC, 
#                                         PRIMARY_ETHNICITY_DESC

df.onehot <- df.medimpute %>%
  select(sexorient14, PROGRAM_LEVEL_DESC, 
         COLLEGE_DESC, MAJOR_DESC, PRIMARY_ETHNICITY_DESC) 
df.onehot <- dummyVars( ~ ., data = df.onehot, fullRank=T)
df.onehot <- data.frame(predict(df.onehot, df.medimpute))
df.medimpute <- df.medimpute %>%
  bind_cols(df.onehot)

# identify near zero variance predictors: remove_cols
remove_cols <- nearZeroVar(df.medimpute, names = TRUE, 
                           freqCut = 19, uniqueCut = 10) # could also try
                                                         # freqcut = 2, uC = 20
                                                         # if you want to be 
                                                         # more aggressive
                                                         # check setdiff
# identify near zero variance predictors: remove_cols
remove_cols_2 <- nearZeroVar(df.medimpute, names = TRUE, 
                           freqCut = 2, uniqueCut = 20)

# check difference 
setdiff(remove_cols_2, remove_cols)

# let's go with more aggressive cut, but revisit this if time allows

# get all column names and remove 
all_cols <- names(df.medimpute)

# remove 
df.medimpute <- df.medimpute[ , setdiff(all_cols, remove_cols_2)]

# fix edaspire error - about line 380
df.medimpute %>%
  select(edaspire) %>%
  glimpse() %>%
  summary()

df.medimpute$edaspire <- as.numeric(levels(df.medimpute$edaspire))[df.medimpute$edaspire]

df.medimpute %>%
  select(edaspire) %>%
  glimpse() %>%
  summary()

# check for multicollinearity
df.vifcheck <- df.medimpute  %>%
  select(-MAJOR_DESC, -BANNER_ID, -PERSON_UID, -CENSUS_DESC) 

# lm model 
m.vifcheck <- lm(evalexp ~ ., data = df.vifcheck)

library(olsrr)
output.vifcheck <- ols_vif_tol(m.vifcheck)
output.vifcheck <- output.vifcheck %>%
  filter(VIF > 4)

# select variables from df.medimpute
df.vifcheck <- df.medimpute %>%
  select(tmprephrs, tmprep, tmrelax, tmrelaxhrs,
         tmcocurr, tmcommute, tmcocurrhrs, tmcommutehrs, 
         tmservice, tmservicehrs, wrshort, wrshortnum,
         wrmed, wrmednum, tmreadinghrs, wrpages, 
         parented) %>%
  as.matrix() %>%
  rcorr(type = "spearman")  # note: firstgen is a factor
                           
# plot of correlations
corrplot(df.vifcheck$r, type = "upper" )

# look at correlations
df.medimpute %>% 
  select(tmprep, tmprephrs) %>%
  glimpse() %>%
  summary() # drop hrs variables 

df.medimpute %>% 
  select(wrshort, wrshortnum) %>%
  glimpse() %>%
  summary() # drop hrs variables 
            
# note: will want to revisit w/out dropping hrs variables

# drop correlated variables 
df.medimpute <- df.medimpute %>%
  select(-tmservicehrs, -tmprephrs, -tmrelaxhrs,
         -tmcocurrhrs, -tmcommutehrs, -tmservicehrs, -wrshortnum,
         -wrmednum, -tmreadinghrs, -wrpages) 

# check that output variable wasn't affected by transformations
df.nomissing %>%
  select(evalexp) %>%
  glimpse() %>%
  summary()

df.medimpute %>%
  select(evalexp) %>%
  glimpse() %>%
  summary()

# create classification output variable 
df.medimpute <- df.medimpute %>%
  mutate(ovr.eval = ifelse(evalexp == 1, 0, 
                           ifelse(evalexp == 2, 0, 1)))

# tidy dataset 
df2 <- df.medimpute %>%
  select(-BANNER_ID, -PERSON_UID)


# initial exploratory models: try logistic, glmnet, and random forest ---------

# note: a reminder - assumptions of binary regression - little or no 
#                    collinearity, linearity of features and log odds

# see if glm will fit 
m.glm <- glm(ovr.eval ~ ., data = df2, family = 'binomial')
summary(m.glm)

# did not converge - try dropping the problematic variables, 
# then try penalized regression

df_explore <- df2 %>%
  select(-evalexp, -MAJOR_DESC, -GENDER_DESC)

m.glm.2 <- glm(ovr.eval ~ ., data = df_explore, family = 'binomial')
summary(m.glm.2)

# fit

# quick confusion matrix to check fit 

# If pos_or_neg exceeds threshold of 0.5, 1 else 0
pos_or_neg <- predict(m.glm.2, df_explore)
pos_or_neg <- ifelse(pos_or_neg > 0.5, 1, 0)

# Convert to factors
p_class <- as.data.frame(pos_or_neg)
p_class$pos_or_neg <- as.factor(p_class$pos_or_neg)
df_explore$ovr.eval <- as.factor(df_explore$ovr.eval)

# confusion matrix on training data 
m.glm.2.cm <- confusionMatrix(p_class$pos_or_neg, df_explore$ovr.eval, positive = '1')

# fit glm with caret
# 5-fold cross validation 

# create 'yes' or 'no' output variable 
df_explore <- df_explore %>%
  mutate(ovr.eval = ifelse(ovr.eval == 1, 'Yes', 'No'))
df_explore$ovr.eval <- as.factor(df_explore$ovr.eval)

# reusable trainControl 
myControl <- trainControl(
  method = "cv", 
  number = 5,
  summaryFunction = twoClassSummary,
  classProbs = TRUE, 
  verboseIter = TRUE
)

# glm with caret 
m.glm <- train(
  ovr.eval ~ .,
  data = df_explore,
  method = "glm",
  trControl = myControl
)

# print model 
print(m.glm)

# print max ROC
res.glm1 <- max(m.glm[["results"]][["ROC"]])

# try penalized regression with default caret tuning

# glmnet attempt using caret defaults 
m.glmnet <- train(
  ovr.eval ~ .,
  data = df_explore,
  method = "glmnet",
  trControl = myControl
)

# print model 
print(m.glmnet)

# print max ROC
res.glmnet.1 <- max(m.glmnet[["results"]][["ROC"]])

# fit randomForest using caret defaults
m.rf <- train(
  ovr.eval ~ .,
  data = df_explore,
  method = "ranger",
  trControl = myControl, 
  importance = 'impurity'
)

# look at model results
print(m.rf) 
res.rf.1 <- max(m.rf[["results"]][["ROC"]])

# feature engineering ---------------------------------------------------------

df3 <- df_explore

# check distributions/need to center and scale
dist.check <- df3 %>%
  keep(is.numeric) %>% 
  gather() %>% 
  ggplot(aes(value)) +
  facet_wrap(~ key, scales = "free") +
  geom_histogram()

# quite a few with skewed distributions and several on different scales
# change attend_com to factor, drop attend_none
# change PRIMARY_ETHNICITY_DESC.White... to factor
# note: drop duration, but revisit if time allows

df3$attend_com <- as.factor(df3$attend_com)
df3$PRIMARY_ETHNICITY_DESC.White..Non.Hispanic <- as.factor(
  df3$PRIMARY_ETHNICITY_DESC.White..Non.Hispanic)
df3 <- df3 %>%
  select(-attend_none, -duration)

# center and scale remaining numeric variables
df.numeric <- df3 %>%
  keep(is.numeric) 

processed_num_vars <- preProcess(df.numeric, method = c('center', 'scale'))

# Use predict to assign standardized variables
df4 <- predict(processed_num_vars, df3)

# check distribution transformations
dist.check.2 <- df4 %>%
  keep(is.numeric) %>% 
  gather() %>% 
  ggplot(aes(value)) +
  facet_wrap(~ key, scales = "free") +
  geom_histogram()

# see if there are any improvements 

# glm with caret 
m.glm.2 <- train(
  ovr.eval ~ .,
  data = df4,
  method = "glm",
  trControl = myControl
)

# print model 
print(m.glm.2)

# print max ROC
res.glm.2 <- max(m.glm.2[["results"]][["ROC"]])

# penalized regression with no tuning

# glmnet attempt using caret defaults 
m.glmnet.2 <- train(
  ovr.eval ~ .,
  data = df4,
  method = "glmnet",
  trControl = myControl
)

# print model 
print(m.glmnet.2)

# print max ROC
res.glmnet.2 <- max(m.glmnet.2[["results"]][["ROC"]])

# fit randomForest using caret defaults
m.rf.2 <- train(
  ovr.eval ~ .,
  data = df_explore,
  method = "ranger",
  trControl = myControl, 
  importance = 'impurity'
)

# look at model results
print(m.rf.2) 
plot(m.rf.2)
res.rf.2 <- max(m.rf.2[["results"]][["ROC"]])

# model performance declined slightly

# if time allows, consider other transformations

# pre-tuning assessment -------------------------------------------------------

# look at most important features
glmImp <- varImp(m.glm, scale = FALSE)
glmImp
plot(glmImp, top = 20)

glmnetImp <- varImp(m.glmnet.2, scale = FALSE)
glmnetImp
plot(glmnetImp, top = 20)

rfImp <- varImp(m.rf, scale = FALSE)     
rfImp
plot(rfImp, top = 20)

# model tuning ----------------------------------------------------------------

df5 <- df_explore

# tune glm model with grid search 
myControl <- trainControl(
  method='cv',
  number=5,
  summaryFunction = twoClassSummary,
  classProbs = TRUE, 
  verboseIter = TRUE,
  savePredictions = TRUE,
  search = "grid"
)


m.glmnet.3 <- train(
  ovr.eval ~ .,
  data = df5,
  method = "glmnet",
  trControl = myControl, 
  tuneLength = 20)

# print model 
print(m.glmnet.3)

# print max ROC
res.glmnet.3 <- max(m.glmnet.3[["results"]][["ROC"]])

# rf
rf_random <- train(
  ovr.eval ~ .,
  data = df_explore,
  method = "ranger",
  trControl = myControl, 
  importance = 'impurity',
  tuneLength = 20)

# results
plot(rf_random) 
print(rf_random)
res.rf.2 <- max(rf_random[["results"]][["ROC"]])

# explore less interpretable models ------------------------------------------

# gbm on df_explore dataset
# reminder: loss function - minimizing total loss for each data point; 
#                           the difference between prob distribution
#                           and predicted probability distribution

# gbm 
m.gbm <- train(
  ovr.eval ~ .,
  data = df_explore,
  method = 'gbm',
  trControl = myControl)

# look at model results
print(m.gbm)
ggplot(m.gbm)
res.gbm.1 <- max(m.gbm[["results"]][["ROC"]])
 
# neural net model
m.nn <- train(
  ovr.eval ~ .,
  data = df4,
  method = 'nnet', 
  trControl = myControl,
  tuneLength = 20)

# look at model results
print(m.nn) 
res.m.nn <- max(m.nn[["results"]][["ROC"]])

# comparable performance - go with simpler models

# other methods -------------------------------------------------------------

# address class imbalance ---------------------------------------------------

# down sampling
# approach - sample across obs so that classes are balanced in the 
#            each set

# look at classes
table(df5$ovr.eval)

df5 %>%
  select(ovr.eval) %>%
  glimpse() %>%
  str() %>%
  summary()

set.seed(123)
down_train <- downSample(x = df5[, -ncol(df5)],
                         y = df5$ovr.eval)
down_train <- as.data.frame(down_train)
down_train <- down_train %>%
  select(ovr.eval = Class, everything())
table(down_train$ovr.eval) 

# glmnet 
m.glmnet.4 <- train(
  ovr.eval ~ .,
  data = down_train,
  method = "glmnet",
  trControl = myControl,
  tuneLength = 20
)

# look at model results
print(m.glmnet.4) 
ggplot(m.glmnet.4)  
glmnetImp2 <- varImp(m.glmnet.4, scale = FALSE)
plot(glmnetImp2, top = 20)
res.glmnet.4 <- max(m.glmnet.4[["results"]][["ROC"]])

# rf 
m.rf.3 <- train(
  ovr.eval ~ .,
  data = down_train,
  method = "ranger",
  trControl = myControl, 
  importance = 'impurity',
  tuneLength = 20
)

# look at model results
print(m.rf.3) 
ggplot(m.rf.3)  
rfImp2 <- varImp(m.rf.3, scale = FALSE)
rfImp2
plot(rfImp2, top = 20)
res.rf.3 <- max(m.rf.3[["results"]][["ROC"]])

# glmnet model improved

# final models on same resamples ---------------------------------------------

# highest avg AUC and lower standard deviation 

# refit each model on df5 minus duration and CENSUS variables on downsampled 
#    data 
df5 <- df5 %>%
  select(-duration) %>%
  select(-starts_with('CENSUS'))

# fit on downsampled data 
set.seed(1234)
down_train2 <- downSample(x = df5[, -ncol(df5)],
                         y = df5$ovr.eval)
down_train2 <- as.data.frame(down_train2)
down_train2 <- down_train2 %>%
  select(ovr.eval = Class, everything())
table(down_train2$ovr.eval) 

# rf
rf_final2 <- train(
  ovr.eval ~ .,
  data = down_train2,
  method = "ranger",
  trControl = myControl, 
  importance = 'impurity',
  tuneLength = 20
)

# glmnet 
glmnet_final2 <- train(
  ovr.eval ~ .,
  data = down_train2,
  method = "glmnet",
  trControl = myControl,
  tuneLength = 20
)

# model list 
res.rf_final <- max(rf_final2[["results"]][["ROC"]])
res.glmnet_final <- max(glmnet_final2[["results"]][["ROC"]])
model_list <- list(item1 = rf_final2, item2  = glmnet_final2)

# pass model list to resamples
resamples <- resamples(model_list)
summary(resamples)

# look at model results

# final models
rfImp4 <- varImp(rf_final2, scale = FALSE)
plot(rfImp4, top = 20)

glmnetImp4 <- varImp(glmnet_final2, scale = FALSE)
plot(glmnetImp4, top = 20)














































