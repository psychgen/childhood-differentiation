# 05.4_sibonly_LGM_ippw.R

library(tidyverse)
library(lavaan)
library(MplusAutomation)

# load prepped data from 05.3
load(file="./data/mplusdat_IPW.RData")

# restrict to sibling pairs only
sibids = mplusdat %>% 
  filter(duplicated(m_id))

mplusdat = mplusdat %>% 
  filter(m_id %in% sibids$m_id)

prepareMplusData(mplusdat, "//ess01/p471/data/durable/projects/childhood_differentiation/scripts/mplus/data/data_for_mlevel_SIBSONLY_withIPW.dat")

filepath1 <- "./scripts/mplus/output/lgm/ippw/sib_only"

# read in output
mplusOutput <- readModels(filepath1, recursive=FALSE,
                          what = c("input", "warn_err", "data_summary", "sampstat", "covariance_coverage", "summaries", "parameters", "class_counts", "indirect", "mod_indices", "residuals", "savedata", "bparameters", "tech3", "tech4", "tech7", "tech8", "tech9", "tech10", "tech12", "fac_score_stats", "lcCondMeans", "gh5", "output"))

# extract relevant params and format as per results from standard LGMs
diffests <- mplusOutput$allpred_lgm_diff_1l_sibonly_ippw_hpc.out$parameters$ci.unstandardized %>% 
  filter(paramHeader == "New.Additional.Parameters",
         str_detect(param,"BI|BS")  )  %>%
  mutate(predictor = rep(paste0("x",seq(1,16)), each=6)) %>% 
  left_join(mplusOutput$allpred_lgm_diff_1l_sibonly_ippw_hpc.out$parameters$unstandardized %>% 
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

totests <- mplusOutput$allpred_lgm_tot_1l_sibonly_ippw_hpc.out$parameters$ci.unstandardized %>% 
  filter(paramHeader == "New.Additional.Parameters",
         str_detect(param,"BI|BS")  )  %>%
  mutate(predictor = rep(paste0("x",seq(1,16)), each=6)) %>% 
  left_join(mplusOutput$allpred_lgm_tot_1l_sibonly_ippw_hpc.out$parameters$unstandardized %>% 
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

# save / load
save(diffests, file= "./output/diffests_sibonly_IPPW.RData")
load("./output/diffests_sibonly_IPPW.RData")

save(totests, file= "./output/totests_sibonly_IPPW.RData")
load("./output/totests_sibonly_IPPW.RData")

# process differentiation 1-level IPPW results

# adjust pvalues
diffests <- diffests %>%
  mutate(FDR_pval_i = p.adjust(b_i_adjust_pval, method="fdr"),
         FDR_pval_s = p.adjust(b_s_adjust_pval, method="fdr"),
         predictor = as.factor(predictor))

# rename predictors
levels(diffests$predictor)[levels(diffests$predictor)=="mf_edu"] <- "Parental education"
levels(diffests$predictor)[levels(diffests$predictor)=="mf_inc"] <- "Parental income"
levels(diffests$predictor)[levels(diffests$predictor)=="m_prestr"] <- "Maternal prenatal life events"
levels(diffests$predictor)[levels(diffests$predictor)=="f_stress"] <- "Paternal prenatal life events"
levels(diffests$predictor)[levels(diffests$predictor)=="m_smok"] <- "Maternal prenatal smoking"
levels(diffests$predictor)[levels(diffests$predictor)=="f_smok"] <- "Paternal prenatal smoking"
levels(diffests$predictor)[levels(diffests$predictor)=="m_predis"] <- "Maternal prenatal distress"
levels(diffests$predictor)[levels(diffests$predictor)=="f_predis"] <- "Paternal prenatal distress"
levels(diffests$predictor)[levels(diffests$predictor)=="m_alcpr"] <- "Maternal alcohol problems"
levels(diffests$predictor)[levels(diffests$predictor)=="f_rxp"] <- "Paternal relationship problems"
levels(diffests$predictor)[levels(diffests$predictor)=="f_alcrisk"] <- "Paternal at-risk drinking"
levels(diffests$predictor)[levels(diffests$predictor)=="m_alcrisk"] <- "Maternal at-risk drinking"
levels(diffests$predictor)[levels(diffests$predictor)=="m_rxp"] <- "Maternal relationship problems"
levels(diffests$predictor)[levels(diffests$predictor)=="m_pststr"] <- "Maternal adverse life events"
levels(diffests$predictor)[levels(diffests$predictor)=="m_dist"] <- "Maternal concurrent distress"
levels(diffests$predictor)[levels(diffests$predictor)=="m_pstdep"] <- "Maternal postnatal depression"

# rename to unadj for consistency
# since these are 1-level results
diffests2_ippw <- diffests %>%
  select(Predictor=predictor,Unadj_est_i=b_i_adjust_est,Unadj_lci_i=b_i_adjust_lci,
         Unadj_uci_i=b_i_adjust_uci,FDR_pval_i,Unadj_est_s=b_s_adjust_est,
         Unadj_lci_s=b_s_adjust_lci,Unadj_uci_s=b_s_adjust_uci,FDR_pval_s) 

# save out
save(diffests2_ippw, file= "./output/diffests2_sibonly_1l_IPPW.RData")

# process total problem 1-level IPPW results

# adjust pvalues
totests <- totests %>%
  mutate(FDR_pval_i = p.adjust(b_i_adjust_pval, method="fdr"),
         FDR_pval_s = p.adjust(b_s_adjust_pval, method="fdr"),
         predictor = as.factor(predictor))

# rename predictors
levels(totests$predictor)[levels(totests$predictor)=="mf_edu"] <- "Parental education"
levels(totests$predictor)[levels(totests$predictor)=="mf_inc"] <- "Parental income"
levels(totests$predictor)[levels(totests$predictor)=="m_prestr"] <- "Maternal prenatal life events"
levels(totests$predictor)[levels(totests$predictor)=="f_stress"] <- "Paternal prenatal life events"
levels(totests$predictor)[levels(totests$predictor)=="m_smok"] <- "Maternal prenatal smoking"
levels(totests$predictor)[levels(totests$predictor)=="f_smok"] <- "Paternal prenatal smoking"
levels(totests$predictor)[levels(totests$predictor)=="m_predis"] <- "Maternal prenatal distress"
levels(totests$predictor)[levels(totests$predictor)=="f_predis"] <- "Paternal prenatal distress"
levels(totests$predictor)[levels(totests$predictor)=="m_alcpr"] <- "Maternal alcohol problems"
levels(totests$predictor)[levels(totests$predictor)=="f_rxp"] <- "Paternal relationship problems"
levels(totests$predictor)[levels(totests$predictor)=="f_alcrisk"] <- "Paternal at-risk drinking"
levels(totests$predictor)[levels(totests$predictor)=="m_alcrisk"] <- "Maternal at-risk drinking"
levels(totests$predictor)[levels(totests$predictor)=="m_rxp"] <- "Maternal relationship problems"
levels(totests$predictor)[levels(totests$predictor)=="m_pststr"] <- "Maternal adverse life events"
levels(totests$predictor)[levels(totests$predictor)=="m_dist"] <- "Maternal concurrent distress"
levels(totests$predictor)[levels(totests$predictor)=="m_pstdep"] <- "Maternal postnatal depression"

# rename to unadj for consistency
# since these are 1-level results
totests2_ippw <- totests %>%
  select(Predictor=predictor,Unadj_est_i=b_i_adjust_est,Unadj_lci_i=b_i_adjust_lci,
         Unadj_uci_i=b_i_adjust_uci,FDR_pval_i,Unadj_est_s=b_s_adjust_est,
         Unadj_lci_s=b_s_adjust_lci,Unadj_uci_s=b_s_adjust_uci,FDR_pval_s)

# save out
save(totests2_ippw, file= "./output/totests2_sibonly_1l_IPPW.RData")

