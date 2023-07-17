# 05.1_generate_ipw.R

library(tidyverse)
library(readxl)
library(phenotools)
library(splitstackshape)
library(estimatr)
library(lavaan.survey)

# source a script with modified functions needed
source("./scripts/99_utils.R")

# make a MoBa dataset with parity and cohabitation status
load("./data/processed_data_new.RData")

moba <- curate_dataset(c("PARITET_5","AA1123"), out_format = "merged_df") 

# read in SSB data
parity<- read_excel("./data/parity_ssb.xlsx")

# align categories to SSB dataset
moba_proc <- moba %>% 
  mutate(parity= factor(case_when(PARITET_5_raw ==0  ~ "1st child",
                                  PARITET_5_raw ==1  ~ "2nd child",
                                  PARITET_5_raw ==2  ~ "3rd child",
                                  PARITET_5_raw %in% c(3,4)  ~ "4th child and over"),ordered=T),
         cohab= factor(case_when(AA1123_raw==1 ~ "Married",
                                 AA1123_raw==3 ~ "Cohabitation",
                                 AA1123_raw%in%c(2,4,5,6) ~ "Single",#"Other" category not used in SSB data, so we need to class all of these as "single"
                                 TRUE ~ "Unknown"))) %>% # Quite a lot of missingess on cohab status in MoBa- can set this to "Unknown" without affecting estimates
  select(preg_id,m_id,BARN_NR, parity, cohab, birth_yr)

# Prep SSB data - we will use years 2003-2008 to correspond with the birth years most
# of the MoBa recruitment was done in. The other birth_yrs in MoBa (1999:2002, 2009)
# can serve as a test sample for the model (though these individuals should also 
# ultimately get weights)

ssb <-parity %>% 
  mutate(across(matches("200"), as.numeric)) %>% 
  select("parity" = `...1`, "cohab"=`...2`, `2003`,`2004`,`2005`,`2006`,`2007`,`2008`) %>% 
  rowwise() %>% 
  mutate(all= sum(c(`2003`,`2004`,`2005`,`2006`,`2007`,`2008`), na.rm=T)) %>% 
  ungroup() %>% 
  fill(parity) %>% 
  filter(!str_detect(cohab,"Other")) %>% 
  mutate(participation=0,
         parity=factor(parity, ordered=T),
         cohab=factor(cohab, levels=levels(moba_proc$cohab))) %>% 
  select(parity,cohab, all, participation)

# create equivalent summary table for MoBa
moba_summ <- moba_proc %>% 
  drop_na(parity) %>% 
  filter(!birth_yr%in%c(1999,2000,2001,2003,2009)) %>% 
  group_by(parity,cohab) %>% 
  summarise(allmoba=n()) 

# subtract MoBA Ns from the SSB population numbers, append MoBa 
# participators, and expand for logistic regression
ssb_moba <- ssb %>% 
  left_join(moba_summ) %>% 
  rowwise() %>% 
  mutate(all= all-allmoba) %>% 
  select(-allmoba) %>% 
  bind_rows(moba_summ %>% 
              rename("all"=allmoba) %>% 
              mutate(participation=1)) %>% 
  drop_na(all) %>% 
  splitstackshape::expandRows(., "all") %>% #this function from the splitstackshape package effectively makes summary data into one obs per row
  as.data.frame() #ipw function does not like tibbles

# run GLM of participation on cohab, parity
ipp_base <- glm('participation ~ 1', data= ssb_moba, family = binomial(link = "logit") ) 
ipp_mod <- glm('participation ~ parity + cohab', data= ssb_moba, family = binomial(link = "logit") ) 

lmtest::lrtest(ipp_base,ipp_mod)

pscl::pR2(ipp_mod)

moba_test <- moba_proc %>% 
  drop_na(parity) %>% 
  filter(birth_yr%in%c(1999,2000,2001,2003,2009)) %>% 
  group_by(parity,cohab) %>% 
  summarise(all=n()) %>% 
  splitstackshape::expandRows(., "all") %>% 
  mutate(participation =1)

# plus add a random sample of non-participators of the same length (strictly speaking this is training data, but so few variables I can't see how it matters)
test <- ssb_moba %>% 
  filter(participation==0) %>% 
  slice_sample(n=nrow(moba_test)) %>% 
  bind_rows(moba_test) %>% 
  mutate(pred = round(predict(ipp_mod, newdata=., type="response"))) 

accuracy <- table(test$pred, test$participation)
sum(diag(accuracy))/sum(accuracy) #only just above chance rate, which I think is to be expected from so few predictors

# check prediction accuracy in training sample
test2 <- ssb_moba %>% 
  mutate(pred = round(predict(ipp_mod, newdata=., type="response"))) 

accuracy <- table(test2$pred, test2$participation)
sum(diag(accuracy))/sum(accuracy) #much higher pprediction in training sample (maybe we should just report overall prediction accuracy, since weights will be used for all)

# get prediction accuracy overall
test3 <-ssb_moba %>% 
  bind_rows(moba_test) %>% 
  mutate(pred = round(predict(ipp_mod, newdata=., type="response"))) 

accuracy <- table(test3$pred, test3$participation)
sum(diag(accuracy))/sum(accuracy)

#73% prediction accuracy overall

# run IPPW based on the same underlying GLM

# unstabilized
ippwu <- myipwpoint(exposure=participation,
                    family="binomial",
                    link="logit",
                    denominator = ~ 1 + parity + cohab,
                    data= ssb_moba)
table(ippwu$ipw.weights)
range(ippwu$ipw.weights)

# stabilized
ippws <- myipwpoint(exposure=participation,
           family="binomial",
           link="logit",
           numerator = ~ 1,
           denominator = ~ 1 + parity + cohab,
           data= ssb_moba)
table(ippws$ipw.weights)
range(ippws$ipw.weights)

ippws$den.mod %>%  summary()

# attach stabilized weights to input dataset, restrict to the unique 
# values needed for moba participants

moba_weights <- ssb_moba %>% 
  mutate(ippw = ippws$ipw.weights) %>% 
  filter(participation==1) %>% 
  arrange(parity,cohab) %>% 
  distinct()

moba_weights #looks sensible: downweighting of obs from primiparous children with cohabiting/married parents;
             #upweighting of obs from children of mothers with multiple prevoius children and single mothers


# attach weights to Moba IDs, and then to processed dataset
moba_ipw <- moba_proc %>% 
  left_join(moba_weights) %>% 
  select(-parity) %>% 
  right_join(alldata_new) %>% 
  drop_na(ippw)

save(moba_ipw,file="./data/moba_ipw.RData")


