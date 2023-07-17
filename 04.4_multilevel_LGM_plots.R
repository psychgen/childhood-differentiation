# 04.4_multilevel_LGM_plots.R

# this script takes a) the 'sibling adjusted' effects of early exposures 
# from the multilevel latent growth models (LGMs) obtained in the script 
# '04_run_multilevel_LGMs.R', b) the 'sibling unadjusted' effects from the 
# script '03.4_run_onelevel_LGMs.R', and c) the 'observational' effects 
# from the script '03_run_LGMs_with_predictors.R', and plots them together

library(tidyverse)
library(lavaan)
library(MplusAutomation)
library(patchwork)
library(ggrepel)
library(viridis)

# read in processed data from 00_data_preparation.R
load("./data/processed_data_new.RData")

# prepare for Mplus
dat <- alldata_new %>% 
  select(ind_id, m_id, sex, BARN_NR, parity,
         age1, age2, age3, 
         diff1, diff2, diff3, tot1, tot2, tot3,
         inat_8yr, hyp_8yr, cd_8yr, odd_8yr, dep_8yr, anx_8yr,
         any_dx_dep, any_dx_anx, any_dx_dbd, any_dx_adhd, 
         f_predis = pre_distress_f, 
         m_prestr = pre_stress_m, 
         m_pststr = post_stress_m, 
         m_pstdep = post_dep_m,
         m_dist = distress_m,
         m_predis = pre_distress_m,
         m_rxp = relation_m, 
         f_rxp = relation_f, 
         f_stress = stress_f, 
         m_alcpr= alc_prob_m,  
         m_alcrisk = alc_risk_tot_m, 
         f_alcrisk = alc_risk_tot_f, 
         f_smok = smoking_f, 
         m_smok = smoking_m, 
         mf_edu = education,  
         mf_inc = income) %>% 
  rename_with(~str_remove(.,"any_")) %>% 
  mutate(ind_id=factor(ind_id),
         m_id=factor(m_id),
         sex=as.factor(sex),
         parity=as.factor(parity),
         BARN_NR= as.factor(BARN_NR),
         across(where(is.numeric),scale),
         across(where(is.numeric),as.numeric),
         sex=as.numeric(sex),
         parity=as.numeric(sex)) %>% 
  as.data.frame()

# rename x-variable columns to make mplus coding easier - save a lookup
# table for re-identifying these variables later
mplusdat<- dat
colnames(mplusdat) <- c(names(mplusdat %>% select(ind_id:dx_adhd)),
                   paste0("x",seq(1,ncol(mplusdat)-length(names(mplusdat %>% select(ind_id:dx_adhd))),1)))

lkp_names <- tibble(oldnames = names(dat),
                    newnames = names(mplusdat))

filepath1 <- "./scripts/mplus/scripts/twolevel_lgm/sibs_only"

# read in output
mplusOutput <- readModels("./scripts/mplus/output/twolevel_lgm/sibs_only", recursive=FALSE,
                          what = c("input", "warn_err", "data_summary", "sampstat", "covariance_coverage", "summaries", "parameters", "class_counts", "indirect", "mod_indices", "residuals", "savedata", "bparameters", "tech3", "tech4", "tech7", "tech8", "tech9", "tech10", "tech12", "fac_score_stats", "lcCondMeans", "gh5", "output"))

# extract relevant params and format as per results from standard LGMs
diffests <- mplusOutput$allpred_lgm_diff_2l.out$parameters$ci.unstandardized %>% 
  filter(paramHeader == "New.Additional.Parameters",
         str_detect(param,"BI|BS")  )  %>%
  mutate(predictor = rep(paste0("x",seq(1,16)), each=6)) %>% 
  left_join(mplusOutput$allpred_lgm_diff_2l.out$parameters$unstandardized %>% 
              filter(paramHeader == "New.Additional.Parameters",
                     str_detect(param,"BI|BS")  ) %>% 
              select(param,est,se,pval) %>% 
              mutate(predictor = rep(paste0("x",seq(1,16)), each=6))) %>% 
  mutate(param2 = rep(c("b_i_adjust","b_s_adjust",
                        "b_i_confound","b_s_confound",
                        "b_i_btwind","b_s_btwind"),16)) %>% 
  select(predictor, param2, est, se, pval, lci= low2.5, uci= up2.5 ) %>% 
  filter(!param2%in%c("b_i_btwind","b_s_btwind") ) %>% # The between-indirect estimates are not of interest here, so drop them
  gather(key="val_type", value= "val", -param2, -predictor ) %>% 
 unite("param", c("param2","val_type"),sep="_") %>% 
  spread(param,val) %>% 
  left_join(lkp_names %>% 
              select(predictor= newnames,oldnames)) %>% 
  select(-predictor) %>% 
  select(predictor = oldnames, everything())# Use lookup table to retrieve predictor names
  
totests <- mplusOutput$allpred_lgm_tot_2l.out$parameters$ci.unstandardized %>% 
  filter(paramHeader == "New.Additional.Parameters",
         str_detect(param,"BI|BS")  )  %>%
  mutate(predictor = rep(paste0("x",seq(1,16)), each=6)) %>% 
  left_join(mplusOutput$allpred_lgm_tot_2l.out$parameters$unstandardized %>% 
              filter(paramHeader == "New.Additional.Parameters",
                     str_detect(param,"BI|BS")  ) %>% 
              select(param,est,se,pval) %>% 
              mutate(predictor = rep(paste0("x",seq(1,16)), each=6))) %>% 
  mutate(param2 = rep(c("b_i_adjust","b_s_adjust",
                        "b_i_confound","b_s_confound",
                        "b_i_btwind","b_s_btwind"),16)) %>% 
  select(predictor, param2, est, se, pval, lci= low2.5, uci= up2.5 ) %>% 
  filter(!param2%in%c("b_i_btwind","b_s_btwind") ) %>% # The between-indirect estimates are not of interest here, so drop them
  gather(key="val_type", value= "val", -param2, -predictor ) %>% 
  unite("param", c("param2","val_type"),sep="_") %>% 
  spread(param,val) %>% 
  left_join(lkp_names %>% 
              select(predictor= newnames,oldnames)) %>% 
  select(-predictor) %>% 
  select(predictor = oldnames, everything())# use lookup table to retrieve predictor names

# save out diff and tot multilevel results
save(diffests, file= "./output/diffests_sibs.RData")
load("./output/diffests_sibs.RData")

save(totests, file= "./output/totests_sibs.RData")
load("./output/totests_sibs.RData")

# process differentiation multilevel results

# adjust pvalues
diffests$FDR_corrected_pvalue_i_adjust <- p.adjust(diffests$b_i_adjust_pval, method="fdr")
diffests$FDR_corrected_pvalue_s_adjust <- p.adjust(diffests$b_s_adjust_pval, method="fdr")

# create parent variable
diffests <- diffests %>%
  mutate(predictor = as.factor(predictor),
         parent=case_when(str_detect(as.character(predictor),"mf_") ~"Both",
                          str_detect(as.character(predictor),"m_") ~"Mother",
                          str_detect(as.character(predictor),"f_") ~"Father"))

# rename predictors
levels(diffests$predictor)[levels(diffests$predictor)=="mf_edu"] <- "Education"
levels(diffests$predictor)[levels(diffests$predictor)=="mf_inc"] <- "Income"
levels(diffests$predictor)[levels(diffests$predictor)=="m_prestr"] <- "Prenatal life events"
levels(diffests$predictor)[levels(diffests$predictor)=="f_stress"] <- "Prenatal life events"
levels(diffests$predictor)[levels(diffests$predictor)=="m_smok"] <- "Prenatal smoking"
levels(diffests$predictor)[levels(diffests$predictor)=="f_smok"] <- "Prenatal smoking"
levels(diffests$predictor)[levels(diffests$predictor)=="m_predis"] <- "Prenatal distress"
levels(diffests$predictor)[levels(diffests$predictor)=="f_predis"] <- "Prenatal distress"
levels(diffests$predictor)[levels(diffests$predictor)=="m_alcpr"] <- "Alcohol problems"
levels(diffests$predictor)[levels(diffests$predictor)=="f_rxp"] <- "Relationship problems"
levels(diffests$predictor)[levels(diffests$predictor)=="f_alcrisk"] <- "At-risk drinking"
levels(diffests$predictor)[levels(diffests$predictor)=="m_alcrisk"] <- "At-risk drinking"
levels(diffests$predictor)[levels(diffests$predictor)=="m_rxp"] <- "Relationship problems"
levels(diffests$predictor)[levels(diffests$predictor)=="m_pststr"] <- "Adverse life events"
levels(diffests$predictor)[levels(diffests$predictor)=="m_dist"] <- "Concurrent distress"
levels(diffests$predictor)[levels(diffests$predictor)=="m_pstdep"] <- "Postnatal depression"

# create categories of predictors
education <-                      "Education"
income <-                         "Income"
prenatal_life_events <-         c("Prenatal life events", "Prenatal life events") 
adverse_life_events <-            "Adverse life events"
prenatal_smoking <-             c("Prenatal smoking", "Prenatal smoking")
prenatal_distress <-            c("Prenatal distress", "Prenatal distress")
at_risk_drinking <-             c("At-risk drinking", "At-risk drinking")
relationship_problems <-        c("Relationship problems", "Relationship problems")
postnatal_depression <-           "Postnatal depression"
concurrent_distress <-            "Concurrent distress"
alcohol_problems <-               "Alcohol problems"

# add a column with the category name 
diffests$category <- NULL
diffests$category[diffests$predictor %in% education] <- "Education"
diffests$category[diffests$predictor %in% income] <- "Income"
diffests$category[diffests$predictor %in% prenatal_life_events] <- "Prenatal life events"
diffests$category[diffests$predictor %in% adverse_life_events] <- "Adverse life events"
diffests$category[diffests$predictor %in% prenatal_smoking] <- "Prenatal smoking"
diffests$category[diffests$predictor %in% prenatal_distress] <- "Prenatal distress"
diffests$category[diffests$predictor %in% at_risk_drinking] <- "At-risk drinking"
diffests$category[diffests$predictor %in% relationship_problems] <- "Relationship problems"
diffests$category[diffests$predictor %in% postnatal_depression] <- "Postnatal depression"
diffests$category[diffests$predictor %in% concurrent_distress] <- "Concurrent distress"
diffests$category[diffests$predictor %in% alcohol_problems] <- "Alcohol problems"

# select diff variables
diffests2 <- diffests %>%
  select(category,b_i_adjust_est,b_s_adjust_est,b_i_adjust_lci,b_i_adjust_uci,
         b_s_adjust_lci,b_s_adjust_uci,fdr_i_pval=FDR_corrected_pvalue_i_adjust,
         fdr_s_pval=FDR_corrected_pvalue_s_adjust,parent)

# process total problem multilevel results

# adjust pvalues
totests$FDR_corrected_pvalue_i_adjust <- p.adjust(totests$b_i_adjust_pval, method="fdr")
totests$FDR_corrected_pvalue_s_adjust <- p.adjust(totests$b_s_adjust_pval, method="fdr")

# create parent variable
totests <- totests %>%
  mutate(predictor = as.factor(predictor),
         parent=case_when(str_detect(as.character(predictor),"mf_") ~"Both",
                          str_detect(as.character(predictor),"m_") ~"Mother",
                          str_detect(as.character(predictor),"f_") ~"Father"))

# rename predictors
levels(totests$predictor)[levels(totests$predictor)=="mf_edu"] <- "Education"
levels(totests$predictor)[levels(totests$predictor)=="mf_inc"] <- "Income"
levels(totests$predictor)[levels(totests$predictor)=="m_prestr"] <- "Prenatal life events"
levels(totests$predictor)[levels(totests$predictor)=="f_stress"] <- "Prenatal life events"
levels(totests$predictor)[levels(totests$predictor)=="m_smok"] <- "Prenatal smoking"
levels(totests$predictor)[levels(totests$predictor)=="f_smok"] <- "Prenatal smoking"
levels(totests$predictor)[levels(totests$predictor)=="m_predis"] <- "Prenatal distress"
levels(totests$predictor)[levels(totests$predictor)=="f_predis"] <- "Prenatal distress"
levels(totests$predictor)[levels(totests$predictor)=="m_alcpr"] <- "Alcohol problems"
levels(totests$predictor)[levels(totests$predictor)=="f_rxp"] <- "Relationship problems"
levels(totests$predictor)[levels(totests$predictor)=="f_alcrisk"] <- "At-risk drinking"
levels(totests$predictor)[levels(totests$predictor)=="m_alcrisk"] <- "At-risk drinking"
levels(totests$predictor)[levels(totests$predictor)=="m_rxp"] <- "Relationship problems"
levels(totests$predictor)[levels(totests$predictor)=="m_pststr"] <- "Adverse life events"
levels(totests$predictor)[levels(totests$predictor)=="m_dist"] <- "Concurrent distress"
levels(totests$predictor)[levels(totests$predictor)=="m_pstdep"] <- "Postnatal depression"

# create categories of predictors
education <-                      "Education"
income <-                         "Income"
prenatal_life_events <-         c("Prenatal life events", "Prenatal life events") 
adverse_life_events <-            "Adverse life events"
prenatal_smoking <-             c("Prenatal smoking", "Prenatal smoking")
prenatal_distress <-            c("Prenatal distress", "Prenatal distress")
at_risk_drinking <-             c("At-risk drinking", "At-risk drinking")
relationship_problems <-        c("Relationship problems", "Relationship problems")
postnatal_depression <-           "Postnatal depression"
concurrent_distress <-            "Concurrent distress"
alcohol_problems <-               "Alcohol problems"

# add a column with the category name 
totests$category <- NULL
totests$category[totests$predictor %in% education] <- "Education"
totests$category[totests$predictor %in% income] <- "Income"
totests$category[totests$predictor %in% prenatal_life_events] <- "Prenatal life events"
totests$category[totests$predictor %in% adverse_life_events] <- "Adverse life events"
totests$category[totests$predictor %in% prenatal_smoking] <- "Prenatal smoking"
totests$category[totests$predictor %in% prenatal_distress] <- "Prenatal distress"
totests$category[totests$predictor %in% at_risk_drinking] <- "At-risk drinking"
totests$category[totests$predictor %in% relationship_problems] <- "Relationship problems"
totests$category[totests$predictor %in% postnatal_depression] <- "Postnatal depression"
totests$category[totests$predictor %in% concurrent_distress] <- "Concurrent distress"
totests$category[totests$predictor %in% alcohol_problems] <- "Alcohol problems"

# select tot variables
totests2 <- totests %>%
  select(category,b_i_adjust_est,b_s_adjust_est,b_i_adjust_lci,b_i_adjust_uci,
         b_s_adjust_lci,b_s_adjust_uci,fdr_i_pval=FDR_corrected_pvalue_i_adjust,
         fdr_s_pval=FDR_corrected_pvalue_s_adjust,parent)

# create model variable (diff/tot)
diffests2 <- diffests2 %>% 
  mutate(model = "Differentiation")

totests2 <- totests2 %>% 
  mutate(model = "Total problems")

#merge diff and tot and rename vars
ests_ml <- diffests2 %>%
  full_join(totests2) %>%
  mutate(model = fct_relevel(model,"Total problems","Differentiation"),
         adj = "Sibling adjusted") %>%
  rename(beta_i = b_i_adjust_est,
         beta_s = b_s_adjust_est,
         beta_i_lci = b_i_adjust_lci,
         beta_i_uci = b_i_adjust_uci,
         beta_s_lci = b_s_adjust_lci,
         beta_s_uci = b_s_adjust_uci,
         fdr_pval_i = fdr_i_pval,
         fdr_pval_s = fdr_s_pval)

# save processed multilevel estimates
save(ests_ml, file= "./output/ests_multilevel_sibonly.RData")

# read in output from constrained 1-level model
mplusOutput_obs <- readModels("./scripts/mplus/output/lgm/sibs_only", recursive=FALSE,
                          what = c("input", "warn_err", "data_summary", "sampstat", "covariance_coverage", "summaries", "parameters", "class_counts", "indirect", "mod_indices", "residuals", "savedata", "bparameters", "tech3", "tech4", "tech7", "tech8", "tech9", "tech10", "tech12", "fac_score_stats", "lcCondMeans", "gh5", "output"))

# extract relevant params and format as per results from standard LGMs
diffests_obs <- mplusOutput_obs$allpred_lgm_diff_1l_constrained_hpc.out$parameters$ci.unstandardized %>% 
  filter(paramHeader == "New.Additional.Parameters",
         str_detect(param,"BI|BS")  )  %>%
  mutate(predictor = rep(paste0("x",seq(1,16)), each=6)) %>% 
  left_join(mplusOutput_obs$allpred_lgm_diff_1l_constrained_hpc.out$parameters$unstandardized %>% 
              filter(paramHeader == "New.Additional.Parameters",
                     str_detect(param,"BI|BS")  ) %>% 
              select(param,est,se,pval) %>% 
              mutate(predictor = rep(paste0("x",seq(1,16)), each=6))) %>% 
  mutate(param2 = rep(c("b_i_est","b_s_est",
                        "b_i_confound","b_s_confound",
                        "b_i_btwind","b_s_btwind"),16)) %>% 
  select(predictor, param2, est, se, pval, lci= low2.5, uci= up2.5 ) %>% 
  filter(!param2%in%c("b_i_btwind","b_s_btwind","b_i_confound","b_s_confound") ) %>% 
  gather(key="val_type", value= "val", -param2, -predictor ) %>% 
  unite("param", c("param2","val_type"),sep="_") %>% 
  spread(param,val) %>% 
  left_join(lkp_names %>% 
              select(predictor= newnames,oldnames)) %>% 
  select(-predictor) %>% 
  select(predictor = oldnames, everything())# Use lookup table to retrieve predictor names

totests_obs <- mplusOutput_obs$allpred_lgm_tot_1l_constrained_hpc.out$parameters$ci.unstandardized %>% 
  filter(paramHeader == "New.Additional.Parameters",
         str_detect(param,"BI|BS")  )  %>%
  mutate(predictor = rep(paste0("x",seq(1,16)), each=6)) %>% 
  left_join(mplusOutput_obs$allpred_lgm_tot_1l_constrained_hpc.out$parameters$unstandardized %>% 
              filter(paramHeader == "New.Additional.Parameters",
                     str_detect(param,"BI|BS")  ) %>% 
              select(param,est,se,pval) %>% 
              mutate(predictor = rep(paste0("x",seq(1,16)), each=6))) %>% 
  mutate(param2 = rep(c("b_i_est","b_s_est",
                        "b_i_confound","b_s_confound",
                        "b_i_btwind","b_s_btwind"),16)) %>% 
  select(predictor, param2, est, se, pval, lci= low2.5, uci= up2.5 ) %>% 
  filter(!param2%in%c("b_i_btwind","b_s_btwind","b_i_confound","b_s_confound") ) %>% 
  gather(key="val_type", value= "val", -param2, -predictor ) %>% 
  unite("param", c("param2","val_type"),sep="_") %>% 
  spread(param,val) %>% 
  left_join(lkp_names %>% 
              select(predictor= newnames,oldnames)) %>% 
  select(-predictor) %>% 
  select(predictor = oldnames, everything())# Use lookup table to retrieve predictor names

# save results
save(diffests_obs, file= "./output/diffests_obs_sibs.RData")
load("./output/diffests_obs_sibs.RData")

save(totests_obs, file= "./output/totests_obs_sibs.RData")
load("./output/totests_obs_sibs.RData")

# process differentiation 1-level results

# adjust pvalues
diffests_obs$FDR_corrected_pval_i <- p.adjust(diffests_obs$b_i_est_pval, method="fdr")
diffests_obs$FDR_corrected_pval_s <- p.adjust(diffests_obs$b_s_est_pval, method="fdr")

# create parent var
diffests_obs <- diffests_obs %>%
  mutate(predictor = as.factor(predictor),
         parent=case_when(str_detect(as.character(predictor),"mf_") ~"Both",
                          str_detect(as.character(predictor),"m_") ~"Mother",
                          str_detect(as.character(predictor),"f_") ~"Father"))

# rename predictors
levels(diffests_obs$predictor)[levels(diffests_obs$predictor)=="mf_edu"] <- "Education"
levels(diffests_obs$predictor)[levels(diffests_obs$predictor)=="mf_inc"] <- "Income"
levels(diffests_obs$predictor)[levels(diffests_obs$predictor)=="m_prestr"] <- "Prenatal life events"
levels(diffests_obs$predictor)[levels(diffests_obs$predictor)=="f_stress"] <- "Prenatal life events"
levels(diffests_obs$predictor)[levels(diffests_obs$predictor)=="m_smok"] <- "Prenatal smoking"
levels(diffests_obs$predictor)[levels(diffests_obs$predictor)=="f_smok"] <- "Prenatal smoking"
levels(diffests_obs$predictor)[levels(diffests_obs$predictor)=="m_predis"] <- "Prenatal distress"
levels(diffests_obs$predictor)[levels(diffests_obs$predictor)=="f_predis"] <- "Prenatal distress"
levels(diffests_obs$predictor)[levels(diffests_obs$predictor)=="m_alcpr"] <- "Alcohol problems"
levels(diffests_obs$predictor)[levels(diffests_obs$predictor)=="f_rxp"] <- "Relationship problems"
levels(diffests_obs$predictor)[levels(diffests_obs$predictor)=="f_alcrisk"] <- "At-risk drinking"
levels(diffests_obs$predictor)[levels(diffests_obs$predictor)=="m_alcrisk"] <- "At-risk drinking"
levels(diffests_obs$predictor)[levels(diffests_obs$predictor)=="m_rxp"] <- "Relationship problems"
levels(diffests_obs$predictor)[levels(diffests_obs$predictor)=="m_pststr"] <- "Adverse life events"
levels(diffests_obs$predictor)[levels(diffests_obs$predictor)=="m_dist"] <- "Concurrent distress"
levels(diffests_obs$predictor)[levels(diffests_obs$predictor)=="m_pstdep"] <- "Postnatal depression"

# create categories of predictors
education <-                      "Education"
income <-                         "Income"
prenatal_life_events <-         c("Prenatal life events", "Prenatal life events") 
adverse_life_events <-            "Adverse life events"
prenatal_smoking <-             c("Prenatal smoking", "Prenatal smoking")
prenatal_distress <-            c("Prenatal distress", "Prenatal distress")
at_risk_drinking <-             c("At-risk drinking", "At-risk drinking")
relationship_problems <-        c("Relationship problems", "Relationship problems")
postnatal_depression <-           "Postnatal depression"
concurrent_distress <-            "Concurrent distress"
alcohol_problems <-               "Alcohol problems"

# add a column with the category name 
diffests_obs$category <- NULL
diffests_obs$category[diffests_obs$predictor %in% education] <- "Education"
diffests_obs$category[diffests_obs$predictor %in% income] <- "Income"
diffests_obs$category[diffests_obs$predictor %in% prenatal_life_events] <- "Prenatal life events"
diffests_obs$category[diffests_obs$predictor %in% adverse_life_events] <- "Adverse life events"
diffests_obs$category[diffests_obs$predictor %in% prenatal_smoking] <- "Prenatal smoking"
diffests_obs$category[diffests_obs$predictor %in% prenatal_distress] <- "Prenatal distress"
diffests_obs$category[diffests_obs$predictor %in% at_risk_drinking] <- "At-risk drinking"
diffests_obs$category[diffests_obs$predictor %in% relationship_problems] <- "Relationship problems"
diffests_obs$category[diffests_obs$predictor %in% postnatal_depression] <- "Postnatal depression"
diffests_obs$category[diffests_obs$predictor %in% concurrent_distress] <- "Concurrent distress"
diffests_obs$category[diffests_obs$predictor %in% alcohol_problems] <- "Alcohol problems"

# select diff variables
diffests_obs2 <- diffests_obs %>%
  select(category,b_i_est=b_i_est_est,b_s_est=b_s_est_est,b_i_est_lci,b_i_est_uci,
         b_s_est_lci,b_s_est_uci,fdr_i_pval=FDR_corrected_pval_i,
         fdr_s_pval=FDR_corrected_pval_s,parent)

# save processed results
save(diffests_obs2, file="./output/diffests_obs2_sibonly.RData")

# process total problems 1-level results

# adjust pvalues
totests_obs$FDR_corrected_pval_i <- p.adjust(totests_obs$b_i_est_pval, method="fdr")
totests_obs$FDR_corrected_pval_s <- p.adjust(totests_obs$b_s_est_pval, method="fdr")

# create parent variable
totests_obs <- totests_obs %>%
  mutate(predictor = as.factor(predictor),
         parent=case_when(str_detect(as.character(predictor),"mf_") ~"Both",
                          str_detect(as.character(predictor),"m_") ~"Mother",
                          str_detect(as.character(predictor),"f_") ~"Father"))
# rename predictors
levels(totests_obs$predictor)[levels(totests_obs$predictor)=="mf_edu"] <- "Education"
levels(totests_obs$predictor)[levels(totests_obs$predictor)=="mf_inc"] <- "Income"
levels(totests_obs$predictor)[levels(totests_obs$predictor)=="m_prestr"] <- "Prenatal life events"
levels(totests_obs$predictor)[levels(totests_obs$predictor)=="f_stress"] <- "Prenatal life events"
levels(totests_obs$predictor)[levels(totests_obs$predictor)=="m_smok"] <- "Prenatal smoking"
levels(totests_obs$predictor)[levels(totests_obs$predictor)=="f_smok"] <- "Prenatal smoking"
levels(totests_obs$predictor)[levels(totests_obs$predictor)=="m_predis"] <- "Prenatal distress"
levels(totests_obs$predictor)[levels(totests_obs$predictor)=="f_predis"] <- "Prenatal distress"
levels(totests_obs$predictor)[levels(totests_obs$predictor)=="m_alcpr"] <- "Alcohol problems"
levels(totests_obs$predictor)[levels(totests_obs$predictor)=="f_rxp"] <- "Relationship problems"
levels(totests_obs$predictor)[levels(totests_obs$predictor)=="f_alcrisk"] <- "At-risk drinking"
levels(totests_obs$predictor)[levels(totests_obs$predictor)=="m_alcrisk"] <- "At-risk drinking"
levels(totests_obs$predictor)[levels(totests_obs$predictor)=="m_rxp"] <- "Relationship problems"
levels(totests_obs$predictor)[levels(totests_obs$predictor)=="m_pststr"] <- "Adverse life events"
levels(totests_obs$predictor)[levels(totests_obs$predictor)=="m_dist"] <- "Concurrent distress"
levels(totests_obs$predictor)[levels(totests_obs$predictor)=="m_pstdep"] <- "Postnatal depression"

# create categories of predictors
education <-                      "Education"
income <-                         "Income"
prenatal_life_events <-         c("Prenatal life events", "Prenatal life events") 
adverse_life_events <-            "Adverse life events"
prenatal_smoking <-             c("Prenatal smoking", "Prenatal smoking")
prenatal_distress <-            c("Prenatal distress", "Prenatal distress")
at_risk_drinking <-             c("At-risk drinking", "At-risk drinking")
relationship_problems <-        c("Relationship problems", "Relationship problems")
postnatal_depression <-           "Postnatal depression"
concurrent_distress <-            "Concurrent distress"
alcohol_problems <-               "Alcohol problems"

# add a column with the category name 
totests_obs$category <- NULL
totests_obs$category[totests_obs$predictor %in% education] <- "Education"
totests_obs$category[totests_obs$predictor %in% income] <- "Income"
totests_obs$category[totests_obs$predictor %in% prenatal_life_events] <- "Prenatal life events"
totests_obs$category[totests_obs$predictor %in% adverse_life_events] <- "Adverse life events"
totests_obs$category[totests_obs$predictor %in% prenatal_smoking] <- "Prenatal smoking"
totests_obs$category[totests_obs$predictor %in% prenatal_distress] <- "Prenatal distress"
totests_obs$category[totests_obs$predictor %in% at_risk_drinking] <- "At-risk drinking"
totests_obs$category[totests_obs$predictor %in% relationship_problems] <- "Relationship problems"
totests_obs$category[totests_obs$predictor %in% postnatal_depression] <- "Postnatal depression"
totests_obs$category[totests_obs$predictor %in% concurrent_distress] <- "Concurrent distress"
totests_obs$category[totests_obs$predictor %in% alcohol_problems] <- "Alcohol problems"

# select tot vars
totests_obs2 <- totests_obs %>%
  select(category,b_i_est=b_i_est_est,b_s_est=b_s_est_est,b_i_est_lci,
         b_i_est_uci,b_s_est_lci,b_s_est_uci,fdr_i_pval=FDR_corrected_pval_i,
         fdr_s_pval=FDR_corrected_pval_s,parent)

# save processed tot (1-level) results
save(totests_obs2, file="./output/totests_obs2_sibonly.RData")

# load observational results
load("./output/diffests_obs2_sibonly.RData")
load("./output/totests_obs2_sibonly.RData")
load(file="./output/obs_pred_diff.RData")
load(file="./output/obs_pred_tot.RData")

# create model variable (diff/tot)
diffests_obs2 <- diffests_obs2 %>% 
  mutate(model = "Differentiation")

totests_obs2 <- totests_obs2 %>% 
  mutate(model = "Total problems")

# rename diff vars before merging
diffests2 <- ests2 %>% 
  mutate(model = "Differentiation",
         adj = "Observational") %>%
  select(category,b_i_est=beta_i,b_s_est=beta_s,b_i_est_lci=beta_i_lci,
         b_i_est_uci=beta_i_uci,b_s_est_lci=beta_s_lci,b_s_est_uci=beta_s_uci,
         fdr_i_pval=fdr_pval_i,fdr_s_pval=fdr_pval_s,parent,model,adj)

# rename tot vars before merging
totests2 <- ests4 %>% 
  mutate(model = "Total problems",
         adj = "Observational") %>%
  select(category,b_i_est=beta_i,b_s_est=beta_s,b_i_est_lci=beta_i_lci,
         b_i_est_uci=beta_i_uci,b_s_est_lci=beta_s_lci,b_s_est_uci=beta_s_uci,
         fdr_i_pval=fdr_pval_i,fdr_s_pval=fdr_pval_s,parent,model,adj)

# merge diff and tot, and rename vars
ests_obs <- diffests_obs2 %>%
  full_join(totests_obs2) %>%
  mutate(adj = "Sibling unadjusted") %>%
  full_join(diffests2) %>%
  full_join(totests2) %>%
  mutate(model = fct_relevel(model,"Total problems","Differentiation")) %>%
  rename(beta_i = b_i_est,
         beta_s = b_s_est,
         beta_i_lci = b_i_est_lci,
         beta_i_uci = b_i_est_uci,
         beta_s_lci = b_s_est_lci,
         beta_s_uci = b_s_est_uci,
         fdr_pval_i = fdr_i_pval,
         fdr_pval_s = fdr_s_pval)

# load multilevel results to merge with 1-level
load("./output/ests_multilevel_sibonly.RData")

# put all results in a single df, reorder factors, restrict to FDRsig and diff
ests_all_diff <- ests_ml %>%
  bind_rows(ests_obs) %>%
  mutate(category=fct_relevel(
    category,"Income","Education",
    "Prenatal life events","Adverse life events",
    "Prenatal distress","Concurrent distress",
    "Alcohol problems","At-risk drinking",
    "Postnatal depression","Relationship problems",
    "Prenatal smoking") ) %>%
  mutate(parent=fct_relevel(parent,"Both","Mother","Father"),
         adj=fct_relevel(adj,"Sibling adjusted","Sibling unadjusted","Observational")) %>% 
  arrange(adj) %>% 
  group_by(category,parent,model) %>% 
  filter(any(fdr_pval_i < .05|fdr_pval_s < .05)) %>%
  ungroup() %>% 
  filter(model=="Differentiation") 

# create variable with colour associated with each category
my_colours <- c(RColorBrewer::brewer.pal(12, "Paired")[1:10],"#B15928")
my_colours<-`names<-`(my_colours,c("Income","Education",
                                   "Prenatal life events","Adverse life events",
                                   "Prenatal distress","Concurrent distress",
                                   "Alcohol problems","At-risk drinking",
                                   "Postnatal depression","Relationship problems",
                                   "Prenatal smoking"))
my_colours_fltrd <- my_colours[names(my_colours) %in% unique(ests_all_diff$category)]

# create dot plot intercept (diff)
p1<- ggplot(ests_all_diff, 
            mapping=aes(x=category, y=beta_i, shape=adj)) +
  geom_hline(aes(yintercept=0), colour = "grey50", linetype = 2) + theme_light() +
  geom_errorbar(aes(ymin=beta_i_lci, ymax=beta_i_uci, colour=category),alpha=0.3,position=position_dodge(0.5), width=0, size=1.1) +
  geom_point(size=3, aes(fill=category),position=position_dodge(0.5), show.legend=TRUE) +
  facet_grid(rows = vars(parent), scale = "free_y", space = "free", switch = "y", as.table = TRUE) +
  scale_shape_manual(values=c(24,21,22)) +
  scale_colour_manual(values=my_colours_fltrd) +
  scale_fill_manual(values=my_colours_fltrd) +
  scale_x_discrete(limits=rev) +
  scale_y_continuous(breaks=c(-0.05,0,0.05,0.1), limits=c(-0.09,0.137)) +
  guides(alpha="none",colour="none",shape=guide_legend(reverse=TRUE),fill=guide_legend(override.aes=list(shape=21))) +
  theme(axis.text.x = element_text(size=15, colour = "black"),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_text(size=17),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill="transparent"),
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.spacing = unit(0.2, "lines"),
        axis.line = element_line(colour="grey80"),
        plot.margin = margin(r = 0.2),
        legend.position = "right",
        legend.title = element_blank(),
        legend.text = element_text(size = 16),
        strip.text = element_text(size=15, colour = 'black'),
        strip.background = element_rect(colour="gray", fill="white")) +
  ylab(label=expression(beta[I])) + 
  coord_flip()

# create dot plot slope (diff)
p2<- ggplot(ests_all_diff, 
            mapping=aes(x=category, y=beta_s, shape=adj)) +
  geom_hline(aes(yintercept=0), colour = "grey50", linetype = 2) + theme_light() +
  geom_errorbar(aes(ymin=beta_s_lci, ymax=beta_s_uci, colour=category),alpha=0.3,position=position_dodge(0.5), width=0, size=1.1) +
  geom_point(size=3, aes(fill=category),position=position_dodge(0.5), show.legend=TRUE) +
  facet_grid(rows = vars(parent), scale = "free_y", space = "free", switch = "y", as.table = TRUE) +
  scale_shape_manual(values=c(24,21,22)) +
  scale_colour_manual(values=my_colours_fltrd) +
  scale_fill_manual(values=my_colours_fltrd) +
  scale_x_discrete(limits=rev) +
  scale_y_continuous(breaks=c(-0.05,0,0.05), limits=c(-0.09,0.09)) +
  guides(alpha="none",colour="none",shape=guide_legend(reverse = TRUE),fill=guide_legend(override.aes=list(shape=21))) +
  theme(axis.text.x = element_text(size=15, colour = "black"),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_text(size=17),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill="transparent"),
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.spacing = unit(0.2, "lines"),
        axis.line = element_line(colour="grey80"),
        plot.margin = margin(r = 0.2),
        legend.position = "right",
        legend.title = element_blank(),
        legend.text = element_text(size = 16),
        strip.text = element_blank(),
        strip.background = element_blank()) +
  ylab(label=expression(beta[S])) + 
  coord_flip()

# combine intercept + slope plots
patch <- p1 + p2 + 
  plot_layout(guides = 'collect',
              widths = c(1, 0.8)) & 
  theme(legend.position = 'right')

patch

# put all results in a single df, reorder factors, restrict to FDRsig and tot
ests_all_tot <- ests_ml %>%
  bind_rows(ests_obs) %>%
  mutate(category=fct_relevel(
    category,"Income","Education",
    "Prenatal life events","Adverse life events",
    "Prenatal distress","Concurrent distress",
    "Alcohol problems","At-risk drinking",
    "Postnatal depression","Relationship problems",
    "Prenatal smoking") ) %>%
  mutate(parent=fct_relevel(parent,"Both","Mother","Father"),
         adj=fct_relevel(adj,"Sibling adjusted","Sibling unadjusted","Observational")) %>% 
  arrange(adj) %>% 
  group_by(category,parent,model) %>% 
  filter(any(fdr_pval_i < .05|fdr_pval_s < .05)) %>%
  ungroup() %>% 
  filter(model=="Total problems") 

# create variable with colour associated with each category
my_colours <- c(RColorBrewer::brewer.pal(12, "Paired")[1:10],"#B15928")
my_colours<-`names<-`(my_colours,c("Income","Education",
                                   "Prenatal life events","Adverse life events",
                                   "Prenatal distress","Concurrent distress",
                                   "Alcohol problems","At-risk drinking",
                                   "Postnatal depression","Relationship problems",
                                   "Prenatal smoking"))
my_colours_fltrd <- my_colours[names(my_colours) %in% unique(ests_all_tot$category)]

# create dot plot intercept (tot)
p3<- ggplot(ests_all_tot, mapping=aes(x=category, y=beta_i, shape=adj)) +
  geom_hline(aes(yintercept=0), colour = "grey50", linetype = 2) + theme_light() +
  geom_errorbar(aes(ymin=beta_i_lci, ymax=beta_i_uci, colour=category),alpha=0.3,position=position_dodge(0.5), width=0, size=1.1) +
  geom_point(size=3, aes(fill=category),position=position_dodge(0.5), show.legend=TRUE) +
  facet_grid(rows = vars(parent), scale = "free_y", space = "free", switch = "y", as.table = TRUE) +
  scale_shape_manual(values=c(24,21,22)) +
  scale_colour_manual(values=my_colours_fltrd) +
  scale_fill_manual(values=my_colours_fltrd) +
  scale_x_discrete(limits=rev) +
  scale_y_continuous(breaks=c(-0.15,-0.1,-0.05,0,0.05,0.1,0.15), limits=c(-0.18,0.18)) +
  guides(alpha="none",colour="none",shape=guide_legend(reverse=TRUE),fill=guide_legend(override.aes=list(shape=21))) +
  theme(axis.text.x = element_text(size=15, colour = "black"),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_text(size=17),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill="transparent"),
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.spacing = unit(0.2, "lines"),
        axis.line = element_line(colour="grey80"),
        plot.margin = margin(r = 0.2),
        legend.position = "right",
        legend.title = element_blank(),
        legend.text = element_text(size = 16),
        strip.text = element_text(size=15, colour = 'black'),
        strip.background = element_rect(colour="gray", fill="white")) +
  ylab(label=expression(beta[I])) + 
  coord_flip()

# create dot plot slope (tot)
p4<- ggplot(ests_all_tot, mapping=aes(x=category, y=beta_s, shape=adj)) +
  geom_hline(aes(yintercept=0), colour = "grey50", linetype = 2) + theme_light() +
  geom_errorbar(aes(ymin=beta_s_lci, ymax=beta_s_uci, colour=category),alpha=0.3,position=position_dodge(0.5), width=0, size=1.1) +
  geom_point(size=3, aes(fill=category),position=position_dodge(0.5), show.legend=TRUE) +
  facet_grid(rows = vars(parent), scale = "free_y", space = "free", switch = "y", as.table = TRUE) +
  scale_shape_manual(values=c(24,21,22)) +
  scale_colour_manual(values=my_colours_fltrd) +
  scale_fill_manual(values=my_colours_fltrd) +
  scale_x_discrete(limits=rev) +
  scale_y_continuous(breaks=c(-0.05,0,0.05), limits=c(-0.09,0.09)) +
  guides(alpha="none",colour="none",shape=guide_legend(reverse = TRUE),fill=guide_legend(override.aes=list(shape=21))) +
  theme(axis.text.x = element_text(size=15, colour = "black"),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_text(size=17),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill="transparent"),
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.spacing = unit(0.2, "lines"),
        axis.line = element_line(colour="grey80"),
        plot.margin = margin(r = 0.2),
        legend.position = "right",
        legend.title = element_blank(),
        legend.text = element_text(size = 16),
        strip.text = element_blank(),
        strip.background = element_blank()) +
  ylab(label=expression(beta[S])) + 
  coord_flip()

# combine intercept + slope plots
patch2 <- p3 + p4 + 
  plot_layout(guides = 'collect',
              widths = c(1.2, 0.6)) & 
  theme(legend.position = 'right')

patch2
