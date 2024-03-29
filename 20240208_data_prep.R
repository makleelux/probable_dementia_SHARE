#############################################################
#                                                           #
#         Data Preparation                                  #
#                                                           #
#                                                           #
#                                           maklee 20220810 #
#############################################################

source("./scripts/20240121_packages.R")
source("./scripts/20240210_func.R")
load("./data/probdem_dat_complete.RData")

# set seed
SEED = 123
set.seed(SEED)

# compute and store country-level prevalence for later
country_prev = dat %>% 
  group_by(country) %>% 
  summarise(dementia_prev = first(dementia_prev))


#----------------------- Train Test Split -----------------------#

# select predictors to reduce data size
dat_predictors = dat %>% 
  # select predictors
  select(mergeid, dementia, dementia_dbl, 
         age, is_female, isced, country,
         cf008tot, cf016tot,
         contains("adl_"),
         reading, willing, askclarification, understood, proxy,
         cciw_w7, dementia_prev,
         maxgrip, eurod, orienti, numeracy, numeracy2, vfluency)

# train-test split per country
train_dat = data.frame()
test_dat = data.frame()
for(i in 1:nrow(country_prev)){
  
  # filter country
  dat_predictors_temp = dat_predictors %>% filter(country == country_prev$country[i])
  
  # split sample
  sample = sample.split(dat_predictors_temp$dementia_dbl, SplitRatio = .5)
  train_dat_temp = subset(dat_predictors_temp %>% select(-cciw_w7, -dementia_prev), sample == TRUE)
  test_dat_temp = subset(dat_predictors_temp, sample == FALSE)
  
  # merge
  train_dat = rbind(train_dat, train_dat_temp)
  test_dat = rbind(test_dat, test_dat_temp)
}

# shuffle
train_dat = train_dat[sample(1:nrow(train_dat)), ]
test_dat = test_dat[sample(1:nrow(test_dat)), ]


#----------------------- Class Imbalance  -----------------------#

#### (1) Class Imbalance: Compute Weights for Training Set ####
# Goal: Upweight participants with self-report dementia diagnosis according to OECD prevalence.
weights_cimbal = train_dat %>% 
  left_join(dat %>% group_by(country) %>% summarise(dementia_prev = first(dementia_prev))) %>% 
  group_by(country) %>% 
  summarise(
    N = n(), 
    dementia = dementia, 
    Ndem = sum(dementia == "dementia"),
    dementia_prev = dementia_prev,
    weights = ifelse(dementia == "dementia", 
                     (1/(dementia_prev)),  
                     (1/(1-(dementia_prev))))) %>% 
  group_by(country, dementia) %>%
  summarise(
    dementia = first(dementia),
    country = first(country),
    weights = first(weights),
    N = first(N),
    dementia_prev = first(dementia_prev))

# Attach computed weights
train_dat_w = train_dat %>% 
  left_join(weights_cimbal %>% select(dementia, country, weights))

#### (2) Class Imbalance: Down Sampling ####
train_dat_down = downSample(train_dat_w %>% select(-dementia, -weights), y = train_dat_w$dementia, yname = "dementia")

# shuffle
train_dat_down = train_dat_down[sample(1:nrow(train_dat_down)), ]

##### (3) Class Imbalance: SMOTE ####
train_dat_for_smote = train_dat_w %>% select(dementia, contains(c("country", "cf0", "age", "is_", "isced", "iadl_", "adl_")), askclarification, understood, contains(c("reading", "willing", "proxy"))) %>% select(-contains("LW"))

# meaning 595 new minority votes (+100%) and 500% majority votes
# grid search available on request
train_dat_smote = performanceEstimation::smote(dementia ~., 
                                               data = train_dat_for_smote, 
                                               perc.under = 5, perc.over = 1) 

train_dat_smote$dementia_dbl = ifelse(train_dat_smote$dementia == "dementia", 1, 0)


#----------------------- Classifications -----------------------#

#### LW classifications ####

# Compute LW (based on recall or recall & iadls) with cutoffs based on 
# distribution (performance below percentile 2.5) or 
# prevalence (performance below percentile reflecting prevalence). 
# With filtering and looping, country specific scores are derived.

# for country level LW adaptations
test_dat_LW = data.frame()
for(i in 1:nrow(country_prev)){
  temp = adapt_lw_per_country(country_string = country_prev$country[i], 
                              traindat = train_dat_w, 
                              testdat = test_dat)
  test_dat_LW = rbind(test_dat_LW, temp)
}


#----------------------- Housekeeping -----------------------#

save(train_dat_w, file = "./data/_probdem_training_dat.RData")
save(train_dat_down, file = "./data/_probdem_training_dat_down.RData")
save(train_dat_smote, file = "./data/_probdem_training_dat_smote.RData")
save(test_dat_LW, file = "./data/_probdem_test_dat.RData")

rm(list = ls())
