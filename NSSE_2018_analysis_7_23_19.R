library(tidyverse)
library(haven)
library(caret)
library(lubridate)
library(tidyimpute)
library(broom)
library(Hmisc)
library(corrr)
library(corrplot)
library(Boruta)
library(h2o)

# goal: predict overall satisfaction with experience 

#############################################################################
# load and merge data -------------------------------------------------------
#############################################################################

# read in nsse 2012, 2014, and 2016 files
df.nsse.2014 <- read_sav('NSSE14_data.sav')
df.nsse.2016 <- read_sav('NSSE16_data.sav')
df.nsse.2018 <- read_sav('NSSE18_data.sav')

# get variables in common across all years
vnames_overlap <- intersect(intersect(names(df.nsse.2014), 
                                      names(df.nsse.2016)), 
                                      names(df.nsse.2018))

# create list of dataframes
df_list <- list(df.nsse.2014, df.nsse.2016, df.nsse.2018)

# get overlapping variables and convert log date across dfs
df_list <- map(df_list, function(df) select(df, one_of(vnames_overlap)))
df_list <- map(df_list, function(df) mutate_if(df, is.POSIXct, as_date))

# combine dataframes
df1 <- df_list %>%
  reduce(bind_rows)

# filter out index variables
df1 <- df1 %>%
  select(-HO, -RI, -LS, -QR, -CL, -DD, -SF, -ET, -QI, -SE)


###############################################################################
# pre-processing --------------------------------------------------------------
###############################################################################

# drop samples where the outcome variable is missing 
df1 <- df1 %>%
  filter(!is.na(evalexp)) 

# drop variables with greater than 30% missing

# create dataframe of percent missingness
df1.pctmiss <- df1 %>%
  map_df( ~ mean(is.na(.))) %>%
  gather(var.name, pct_null) %>%
  arrange(-pct_null) 

# store names of variables with > 30% missingness to a vector
vnames_to_drop <- df1.pctmiss %>%
  filter(pct_null > .3) 
vnames_to_drop <- vnames_to_drop$var.name

# select variables that are < 30% missing 
df1 <- df1 %>%
  select(-one_of(vnames_to_drop))

# remove near zero variance predictors ---------------------------------------

# copy output variable to seperate dataframe
df.output.var <- df1 %>%
  select(evalexp)

# identify near zero variance predictors: remove_cols
remove_cols <- nearZeroVar(df1, names = TRUE, 
                           freqCut = 19, uniqueCut = 10) 

# get all column names and remove 
all_cols <- names(df1)

# remove near zero variance predictors from df1
df1 <- df1[ , setdiff(all_cols, remove_cols)]

# check that output variable was not removed from df1
if(colnames(df.output.var[1]) %in% colnames(df1)) {
  cat("Output variable retained");
} else {
  cat("Output variable missing!")
}

# remove correlated variables -----------------------------------------------

# visualize correlations with output variable

# set aside non-numerical features
non_num_cols <- c('MAJnum', 'MAJfirst', 'MAJfirstcode', 'MAJfirstcol', 
                  'class', 'fulltime', 'coursenum', 'onlinenum', 
                  'onlinecrscol', 'begincol', 'attend_com', 'attend_col', 
                  'attend_none', 'edaspire', 'parented',
                  'firstgen', 'genderid', 'birthyear', 'agecat', 're_asian', 
                  're_black', 're_white', 're_all', 'greek', 'living', 
                  'disability', 'sexorient14', 'IRsex', 'IRrace', 'IRclass', 
                  'IRenrollment', 'IRftfy', 'studentID', 'group1', 'group2',
                  'group3', 'group4', 'group5', 'logdate', 'duration')

# look at correlations with output variable
df.corr <- df1 %>%
  select(-one_of(non_num_cols)) %>%
  mutate_if(is.character, as.numeric) %>%
  as.matrix() %>%
  rcorr(type = "pearson")

df.corr <- as.data.frame(df.corr$r)

df.corr <- df.corr %>% 
  select(evalexp) %>%
  bind_cols(feature = rownames(df.corr)) %>%
  select(feature, evalexp) %>%
  filter(feature != 'evalexp') %>%
  arrange(-evalexp)
  
# plot correlations
ggplot(df.corr, aes(x = reorder(feature, evalexp), y = evalexp)) +
  geom_bar(stat = 'identity') +
  labs(title = 'Feature Correlation with Overall Evaluation of Experience',
       y = 'Correlation',
       x = 'Feature') +
  coord_flip() + 
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# correlations among predictors
df.corr2 <- df1 %>% 
  select(-one_of(non_num_cols)) %>%
  mutate_if(is.character, as.numeric)

# remove one of each correlated predictor pair 
vars_to_remove <- findCorrelation(cor(df.corr2, use = 'complete.obs'), cutoff = .6, 
                                  exact = TRUE, verbose = TRUE)
vars_to_remove <- as.double(str_split(vars_to_remove, " "))
df.corr2 <- df.corr2 %>%
  select(-vars_to_remove) 
df.corr2.names <- names(df.corr2)

# make list of column names to keep 
vnames_to_keep <- append(non_num_cols, df.corr2.names)

# Subset list of df1 variables to those in vars_to_keep
df1 <- df1 %>%
  select(evalexp, one_of(vnames_to_keep))

# imputation ------------------------------------------------------------------

# median imputation for numeric columns

# select numeric columns 
df.medimpute <- df1 %>%
  select(-one_of(non_num_cols))
df.medimpute <- df.medimpute %>%
  mutate_if(is.character, as.numeric)

# median impute
medimpute_vars <- preProcess(df.medimpute, method = c("medianImpute"))

# Use predict to transform data
df1 <- predict(medimpute_vars, df1)

# modal imputation for non-numeric columns

# impute mode for the missing values in categorical columns
df1 <- df1 %>% 
  impute_most_freq()

# one-hot encoding for categorical variables ----------------------------------

# set one-hot variables to list 
one_hot_variables <- c('MAJfirstcol', 'class', 'genderid', 're_all', 
                       'living', 'sexorient14')

# select one-hot variables  
df.onehot <- df1 %>%
  select(one_of(one_hot_variables)) %>%
  modify_if(is.numeric, as_factor)

# convert to dummyVars
df.onehot <- dummyVars( ~ ., data = df.onehot, fullRank=T)

# change variable types to match in df1
df1 <- df1 %>% 
  modify_at(one_hot_variables, as_factor)

# predict df.onehot on df1
df.onehot <- data.frame(predict(df.onehot, df1))

# bind df.onehot to df1 and remove one-hot variables
df1 <- df1 %>%
  bind_cols(df.onehot) %>%
  select(-one_of(one_hot_variables))

# drop redundant features 
redundant_features <- c('MAJfirst', 'MAJfirstcode', 'onlinecrscol', 
                        'birthyear', 're_asian', 're_black', 
                        're_white', 'IRsex', 'IRrace',
                        'IRclass', 'IRenrollment', 'group1', 
                        'group2', 'group3', 'group4',
                        'group5')

df1 <- df1 %>%
  select(-one_of(redundant_features))

# center and scale variables
pre_process_vals <- df1 %>%
  select(-evalexp)
pre_process_vals <- preProcess(pre_process_vals, method = c('center', 'scale'))
df1 <- predict(pre_process_vals, df1) 

# feature selection -----------------------------------------------------------

# boruta feature selection 
set.seed(21)
bor_profile <- Boruta(evalexp ~ . -studentID -logdate -duration -sameinst -age, 
                      data = df1,
                      doTrace=2)

# get selected attributes
bor_predictors <- getSelectedAttributes(bor_profile)

# glance at feature importance 
bor.profile.df <- attStats(bor_profile)
bor.profile.df <- bor.profile.df %>% 
  rownames_to_column(var = 'feature') %>%
  as_tibble() %>%
  arrange(-medianImp)

# recursive feature elimination 

# prep data for rfe 
x <- df1 %>% select(-evalexp, -studentID, -logdate, -duration, -sameinst, -age)
y <- df1 %>% select(evalexp) 
y <- as.matrix(y)

# create subset grid
subsets <- c(5, 10, 15, 20, 25, 30, 35, 45, 50)

# create rfecontrol 
set.seed(43)
ctrl <- rfeControl(functions = lmFuncs,
                   method = "repeatedcv",
                   repeats = 5,
                   verbose = TRUE)

# run algorithm
rfe_profile <- rfe(x, y, rfeControl = ctrl, sizes = subsets)

# view results
rfe_profile

# list of variable names used in the final model 
rfe_predictors <- predictors(rfe_profile)

# get intersection of feature sets 
final_features <- intersect(bor_predictors, rfe_predictors)














































