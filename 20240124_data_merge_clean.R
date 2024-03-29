#############################################################
#                                                           #
#         Data Merging and Cleaning                         #
#                                                           #
#                                                           #
#                                           maklee 20220810 #
#############################################################

source("./scripts/20240121_packages.R")

# weights
svyweights = read_spss("../data/sharew7_rel8-0-0_ALL_datasets_spss/sharew7_rel8-0-0_gv_weights.sav")

## immediate and delayed recall (+ further variables for later validation)
recall7 = read_spss("../data/sharew7_rel8-0-0_ALL_datasets_spss/sharew7_rel8-0-0_gv_health.sav") %>% 
  select(mergeid, cf016tot, cf008tot, maxgrip, numeracy, orienti, casp, loneliness, eurod, numeracy2, country) %>%
  mutate(country = as_factor(country))

## verbal fluency (for later validation)
cf7 = read_spss("../data/sharew7_rel8-0-0_ALL_datasets_spss/sharew7_rel8-0-0_cf.sav") %>% 
  mutate(vfluency = ifelse(cf010_ >= 0, cf010_, NA)) %>% 
  select(mergeid, vfluency) 

## education
isced7 = read_spss("../data/sharew7_rel8-0-0_ALL_datasets_spss/sharew7_rel8-0-0_gv_isced.sav") %>% 
  select(mergeid, isced1997_r) %>% 
  mutate(isced = isced1997_r %>% as.character %>% as.numeric,
         isced = factor(case_when(isced == 0 | isced == 1 | isced == 2 ~ "lower_secondary",
                   isced == 3 ~ "upper_secondary", 
                   isced == 4 | isced == 5 | isced == 6 ~ "tertiary"),
         levels = c("tertiary", "upper_secondary", "lower_secondary"))) %>% 
  select(-isced1997_r)

## age and gender (+ further variables for later validation)
cv7 = read_spss("../data/sharew7_rel8-0-0_ALL_datasets_spss/sharew7_rel8-0-0_cv_r.sav") %>% 
  select(mergeid, age2017, gender, int_year, int_month, deceased, cvresp) %>% 
  mutate(age = ifelse(is.na(age2017), NA, age2017),
         is_female = factor(ifelse(gender == 1, 0, 1)),
         int_month = int_month %>% as.character %>% as.double) %>% 
  select(-gender)

## individual IADLs and ADLs and dementia self-report diagnosis
iadl7 = read_spss("../data/sharew7_rel8-0-0_ALL_datasets_spss/sharew7_rel8-0-0_ph.sav") %>% 
  select(mergeid, contains("ph049"), ph006d16) %>% 
  mutate_at(vars(matches("ph")), as.character) %>% 
  mutate_at(vars(matches("ph")), as.numeric) %>% 
  rename(# IADLs
         iadl_meal = ph049d8, 
         iadl_groceries = ph049d9, 
         iadl_phone = ph049d10, 
         iadl_medi = ph049d11, 
         iadl_money = ph049d13,
         iadl_map = ph049d7,
         iadl_garden = ph049d12,
         iadl_leaving = ph049d14,
         iadl_laundry = ph049d15, 
         # ADLs 
         adl_dressing = ph049d1,
         adl_walking = ph049d2,
         adl_bathing = ph049d3,
         adl_eating = ph049d4,
         adl_outofbed = ph049d5,
         adl_toilet = ph049d6) %>% 
  # recode iadls
  mutate_if(is.double, function(x){x = ifelse(x %in% c(-1, -2), NA, x)}) %>% 
  # recode dementia
  mutate(dementia = factor(ifelse(ph006d16 == 1, "dementia", "no dementia")),
         dementia_dbl = ifelse(dementia == "dementia", 1, 0)) %>% 
  select(-ph006d16, -ph049dno)

## interviewer prevd variables
#IV007_AskClarification	Did [FLRespondentName] ask for clarification on any questions?	
#IV008_RespUnderstoodQst	Overall, did you feel that [FLRespondentName] understood the questions?	
#IV018_HelpShowcards	Did the respondent need any help reading the showcards during the interview?
#IV012_StepstoEntrance	How many steps had to be climbed (up or down) to get to the main entrance of the household's flat?
#IV020_RelProxy	A proxy respondent has answered some or all of the questions we had for [FLRespondentName]. How is the proxy respondent related to [FLRespondentName]?	
#IV002d1 nobody else PersPresent	Were any third persons, except proxy respondent, present during (parts of) the interview with [FLRespondentName]?
#IV004_WillingAnswer	How would you describe the willingness of [FLRespondentName] to answer?	
#IV005_WillingnessWorse	Why did the respondent's willingness to answer get worse during the interview?
iv7 = read_spss("../data/sharew7_rel8-0-0_ALL_datasets_spss/sharew7_rel8-0-0_iv.sav") %>% 
  mutate(proxy = case_when(
    is.na(iv020_) | iv020_ %in% c(-1, -2) ~ "not present", 
    iv020_ == 1 ~ "partner",
    iv020_ %in% c(2,3,4,5,6) ~ "relatives",
    iv020_ %in% c(7,8,9,10) ~ "helper/other") %>% factor(levels = c("not present", "partner", "relatives", "helper/other")),
    reading = case_when(
      iv018_ %in% c(1, 2) ~ "help",
      iv018_ == 3 ~ "no help") %>% 
      factor(levels = c("no help", "help")),
    willing = case_when(
      iv004_ %in% c(1, 2, 3) ~ "good", 
      iv004_ %in% c(4, 5, 6) ~ "bad at some time") %>% 
      factor(levels = c("good", "bad at some time")),
    askclarification = ifelse(iv007_ %in% c(-1, -2), NA, iv007_),
    understood = ifelse(iv008_ %in% c(-1, -2), NA, iv008_)) %>% 
  select(mergeid, proxy, reading, willing, askclarification, understood)

# Estimated prevalence of dementia among people aged 60 and over, 2018
# based on OECD (prevalence 60+)
# https://www.oecd-ilibrary.org/social-issues-migration-health/health-at-a-glance-europe-2018_health_glance_eur-2018-en
expected_prevalence_OECD = read.csv2("./20240124_health_at_a_glance.csv") %>% 
  mutate(country = str_replace(country, "Czech Rep.", "Czech Republic"),
         country = str_replace(country, "Slovak Rep.", "Slovakia")) %>% 
  filter(country %in% recall7$country) %>% 
  mutate(source = "OECD",
         country = factor(country),
         dementia_prev = prevalence.2018.60) %>% 
  select(country, dementia_prev, source) %>% 
  # no OECD data available for Israel
  # https://pubmed.ncbi.nlm.nih.gov/30318021/#:~:text=The%20dementia%20group%20consisted%20of,N%20%3D%203%2C254%2C%205.7%25)
  rbind(data.frame(country = "Israel", dementia_prev = 0.06, source = "Other"))


## merging and filtering
dat = 
  recall7 %>% 
  left_join(cf7) %>% 
  left_join(iadl7) %>% 
  left_join(cv7) %>%
  left_join(isced7) %>% 
  left_join(iv7) %>% 
  left_join(expected_prevalence_OECD) %>% 
  left_join(svyweights %>% select(mergeid, cciw_w7))
  
## application of eligibility criteria
cat("respondents at wave 7: ", dat %>% filter(mergeid %in% recall7$mergeid) %>% nrow(),
    "\n  at least 60 years old: ", dat %>% filter(mergeid %in% recall7$mergeid, age >= 60) %>% nrow(),
    "\n  no missings: ", dat %>% filter(mergeid %in% recall7$mergeid, age >= 60) %>% drop_na(isced, is_female, age, cf016tot, cf008tot, contains("adl"), dementia) %>% nrow(),
    "\n  no missings in interviewer variables: ", dat %>% filter(mergeid %in% recall7$mergeid, age >= 60) %>% drop_na(isced, is_female, age, cf016tot, cf008tot, contains("adl"), dementia, reading, willing, askclarification, understood) %>% nrow(),
    # remove Slovakia because n cases < 5
    "\n  excluded due to low base prev (Slovakia): ", dat %>% filter(mergeid %in% recall7$mergeid, age >= 60) %>% drop_na(isced, is_female, age, cf016tot, cf008tot, contains("adl"), dementia, reading, willing, askclarification, understood) %>% filter(country %in% c("Slovakia") == F) %>% nrow())

dat = dat %>% 
  filter(mergeid %in% recall7$mergeid, age >= 60, country %in% c("Slovakia") == F) %>% 
  drop_na(isced, is_female, age, cf016tot, cf008tot, contains("adl"), dementia, 
          reading, willing, askclarification, understood) %>% 
  droplevels()

"# check n dem per country
dat_ndem = dat %>% 
  group_by(country) %>% 
  summarise(dementia_prev = sum(dementia == 'dementia')/n())

dat_ndem %>% 
  ggplot(aes(1, dementia_prev, group = 1, colour = country)) +
  geom_boxplot() +
  geom_jitter(width = .2) +
  scale_y_continuous() +
  theme_bw() +
  xlab('') +
  theme(axis.ticks.x = element_blank(), axis.text.x = element_blank())
"

## housekeeping
save(dat, file =  "./data/probdem_dat_complete.RData")
rm(list = ls())
