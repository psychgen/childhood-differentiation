# 05.3_fullsample_LGM_ippw.R

library(tidyverse)
library(lavaan)
library(MplusAutomation)

# read in processed ipw data from 05.1
load(file="N:/durable/projects/childhood_differentiation/data/moba_ipw.RData")

# read in sib weighted data from 04.2
load("./data/sib_weighted_data.RData")

# IPP weights need to be invariant in a cluster (will naturally vary by parity,
# but we'll take the weight corresponding to the lowest parity)

ippw <- moba_ipw %>% 
  group_by(m_id) %>% 
  summarise(ind_id=ind_id,
            ippw=min(ippw)) %>% 
  ungroup()

# add participation weights to sib dataset
dat= wdat %>% 
  left_join(ippw %>% 
              select(ind_id,ippw)) %>% 
  drop_na(ippw)

# rename x-variable columns to make mplus coding easier - save a lookup
# table for re-identifying these variables later
mplusdat<- dat
colnames(mplusdat) <- c(names(mplusdat %>% select(ind_id:dx_adhd)),
                        paste0("x",seq(1,ncol(mplusdat)-length(names(mplusdat %>% select(ind_id:dx_adhd,ipsw,ippw))),1)),
                        "ipsw","ippw")

lkp_names <- tibble(oldnames = names(dat),
                    newnames = names(mplusdat))

#save(mplusdat, lkp_names, file="./data/mplusdat_IPW.RData")

# use MplusAutomation to make the data in an Mplus friendly format
# prepareMplusData(mplusdat, "//ess01/p471/data/durable/projects/childhood_differentiation/scripts/mplus/data/data_for_mlevel_withIPW.dat")

filepath1 <- "./scripts/mplus/output/lgm/ippw"

# read in output
mplusOutput <- readModels(filepath1, recursive=FALSE,
                          what = c("input", "warn_err", "data_summary", "sampstat", "covariance_coverage", "summaries", "parameters", "class_counts", "indirect", "mod_indices", "residuals", "savedata", "bparameters", "tech3", "tech4", "tech7", "tech8", "tech9", "tech10", "tech12", "fac_score_stats", "lcCondMeans", "gh5", "output"))

# extract relevant params and format as per results from standard LGMs
diffests <- mplusOutput$allpred_lgm_diff_1l_fullsample_ippw_hpc.out$parameters$ci.unstandardized %>% 
  filter(paramHeader == "New.Additional.Parameters",
         str_detect(param,"BI|BS")  )  %>%
  mutate(predictor = rep(paste0("x",seq(1,16)), each=6)) %>% 
  left_join(mplusOutput$allpred_lgm_diff_1l_fullsample_ippw_hpc.out$parameters$unstandardized %>% 
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

totests <- mplusOutput$allpred_lgm_tot_1l_fullsample_ippw_hpc.out$parameters$ci.unstandardized %>% 
  filter(paramHeader == "New.Additional.Parameters",
         str_detect(param,"BI|BS")  )  %>%
  mutate(predictor = rep(paste0("x",seq(1,16)), each=6)) %>% 
  left_join(mplusOutput$allpred_lgm_tot_1l_fullsample_ippw_hpc.out$parameters$unstandardized %>% 
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

# save out
save(diffests, file= "./output/diffests_fullsample_IPPW.RData")
load("./output/diffests_fullsample_IPPW.RData")

save(totests, file= "./output/totests_fullsample_IPPW.RData")
load("./output/totests_fullsample_IPPW.RData")

# process diff observational results

# adjust pvalues
diffests$FDR_corrected_pval_i <- p.adjust(diffests$b_i_adjust_pval, method="fdr")
diffests$FDR_corrected_pval_s <- p.adjust(diffests$b_s_adjust_pval, method="fdr")

# rename predictors
levels(diffests$predictor)[levels(diffests$predictor)=="mf_edu"] <- "Education"
levels(diffests$predictor)[levels(diffests$predictor)=="mf_inc"] <- "Income"
levels(diffests$predictor)[levels(diffests$predictor)=="m_prestr"] <- "Prenatal adverse life events"
levels(diffests$predictor)[levels(diffests$predictor)=="f_stress"] <- "Prenatal adverse life events"
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

diffests2 <- diffests %>%
  select(predictor,b_i_adjust_est,b_s_adjust_est,b_i_adjust_lci,b_i_adjust_uci,
         b_s_adjust_lci,b_s_adjust_uci,fdr_i_pval=FDR_corrected_pval_i,
         fdr_s_pval=FDR_corrected_pval_s)

# process tot observational results

# adjust pvalues
totests$FDR_corrected_pvalue_i_adjust <- p.adjust(totests$b_i_adjust_pval, method="fdr")
totests$FDR_corrected_pvalue_s_adjust <- p.adjust(totests$b_s_adjust_pval, method="fdr")

totests <- totests %>%
  mutate(predictor = as.factor(predictor))

# rename predictors
levels(totests$predictor)[levels(totests$predictor)=="mf_edu"] <- "Education"
levels(totests$predictor)[levels(totests$predictor)=="mf_inc"] <- "Income"
levels(totests$predictor)[levels(totests$predictor)=="m_prestr"] <- "Prenatal adverse life events"
levels(totests$predictor)[levels(totests$predictor)=="f_stress"] <- "Prenatal adverse life events"
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

totests2 <- totests %>%
  select(predictor,b_i_adjust_est,b_s_adjust_est,b_i_adjust_lci,b_i_adjust_uci,
         b_s_adjust_lci,b_s_adjust_uci,fdr_i_pval=FDR_corrected_pvalue_i_adjust,
         fdr_s_pval=FDR_corrected_pvalue_s_adjust)

# save out
save(diffests2, file="./data/observational_IPPW_diff.RData")
save(totests2, file="./data/observational_IPPW_tot.RData")

