# 03.5_run_onelevel_LGMs_sensitivity.R

# this script runs the 1-level sensitivity LGMs for the externalising
# and internalising CBCL subscales via Mplus using MplusAutomation

library(tidyverse)
library(lavaan)
library(MplusAutomation)
library(patchwork)
library(ggrepel)
library(viridis)

# read in processed data from 00_data_preparation.R
load("./data/processed_data_new.RData")

# prepare for Mplus - sensitivity so INT/EXT not DIFF/TOT
dat <- alldata_new %>% 
  select(ind_id, m_id, sex, BARN_NR, parity,
         age1, age2, age3, int1, int2, int3, ext1, ext2, ext3, 
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

# read in sib weighted data from 04.2
load("./data/sib_weighted_data.RData")

# add sib weights to sensitivity dataset
dat= dat %>% 
  left_join(wdat %>% 
              select(ind_id,ipsw))

# rename x-variable columns to make mplus coding easier - save a lookup
# table for re-identifying these variables later
mplusdat<- dat
colnames(mplusdat) <- c(names(mplusdat %>% select(ind_id:dx_adhd)),
                        paste0("x",seq(1,ncol(mplusdat)-length(names(mplusdat %>% select(ind_id:dx_adhd,ipsw))),1)),
                        "ipsw")

lkp_names <- tibble(oldnames = names(dat),
                    newnames = names(mplusdat))

# restrict to sibling pairs only
sibids = mplusdat %>% 
  filter(duplicated(m_id))

mplusdat = mplusdat %>% 
  filter(m_id %in% sibids$m_id)

# save the data and the lookup table for the multilevel version of this analysis
save(mplusdat, lkp_names, file="./data/SIBSONLY_sensitivity.RData")

# use MplusAutomation to make the data in an Mplus friendly format
prepareMplusData(mplusdat, "//ess01/p471/data/durable/projects/childhood_differentiation/scripts/mplus/data/data_for_mlevel_SIBSONLY_sensitivity.dat")

# read in output
mplusOutput <- readModels("./scripts/mplus/output/lgm/sibs_only/sensitivity", recursive=FALSE,
                          what = c("input", "warn_err", "data_summary", "sampstat", "covariance_coverage", "summaries", "parameters", "class_counts", "indirect", "mod_indices", "residuals", "savedata", "bparameters", "tech3", "tech4", "tech7", "tech8", "tech9", "tech10", "tech12", "fac_score_stats", "lcCondMeans", "gh5", "output"))

# extract relevant params and format as per results from standard LGMs
ests <- mplusOutput$parameters$ci.unstandardized %>% 
  filter(paramHeader == "New.Additional.Parameters",
         str_detect(param,"BI|BS")  )  %>%
  mutate(predictor = rep(paste0("x",seq(1,16)), each=12),
         outcome = ifelse(str_detect(param,"BI1|BS1"),"Externalising","Internalising")) %>%
  left_join(mplusOutput$parameters$unstandardized %>% 
              filter(paramHeader == "New.Additional.Parameters",
                     str_detect(param,"BI|BS")  ) %>% 
              select(param,est,se,pval) %>% 
              mutate(predictor = rep(paste0("x",seq(1,16)), each=12))) %>% 
  mutate(param2 = rep(c("b_i_adjust","b_s_adjust",
                        "b_i_confound","b_s_confound",
                        "b_i_btwind","b_s_btwind",
                        "b_i_adjust","b_s_adjust",
                        "b_i_confound","b_s_confound",
                        "b_i_btwind","b_s_btwind"),16)) %>% 
  select(predictor, param2, est, se, pval, lci=low2.5, uci=up2.5, outcome) %>% 
  filter(!param2%in%c("b_i_btwind","b_s_btwind") ) %>% # The between-indirect estimates are not of interest here, so drop them
  gather(key="val_type", value= "val", -param2, -predictor, -outcome) %>% 
  unite("param", c("param2","val_type"),sep="_") %>% 
  spread(param,val) %>% 
  left_join(lkp_names %>% 
              select(predictor= newnames,oldnames)) %>% 
  select(-predictor) %>% 
  select(predictor = oldnames, everything())# Use lookup table to retrieve predictor names

# save estimates
save(ests, file= "./output/int_ext_ests_1l.RData")
load("./output/int_ext_ests_1l.RData")

# adjust pvalues
extests <- ests %>%
  filter(outcome=="Externalising") %>%
  mutate(FDR_pval_i_unadj = p.adjust(b_i_adjust_pval, method="fdr"),
         FDR_pval_s_unadj = p.adjust(b_s_adjust_pval, method="fdr"),
         predictor = as.factor(predictor))

intests <- ests %>%
  filter(outcome=="Internalising") %>%
  mutate(FDR_pval_i_unadj = p.adjust(b_i_adjust_pval, method="fdr"),
         FDR_pval_s_unadj = p.adjust(b_s_adjust_pval, method="fdr"),
         predictor = as.factor(predictor))

# rename predictors
levels(intests$predictor)[levels(intests$predictor)=="mf_edu"] <- "Parental education"
levels(intests$predictor)[levels(intests$predictor)=="mf_inc"] <- "Parental income"
levels(intests$predictor)[levels(intests$predictor)=="m_prestr"] <- "Maternal prenatal life events"
levels(intests$predictor)[levels(intests$predictor)=="f_stress"] <- "Paternal prenatal life events"
levels(intests$predictor)[levels(intests$predictor)=="m_smok"] <- "Maternal prenatal smoking"
levels(intests$predictor)[levels(intests$predictor)=="f_smok"] <- "Paternal prenatal smoking"
levels(intests$predictor)[levels(intests$predictor)=="m_predis"] <- "Maternal prenatal distress"
levels(intests$predictor)[levels(intests$predictor)=="f_predis"] <- "Paternal prenatal distress"
levels(intests$predictor)[levels(intests$predictor)=="m_alcpr"] <- "Maternal alcohol problems"
levels(intests$predictor)[levels(intests$predictor)=="f_rxp"] <- "Paternal relationship problems"
levels(intests$predictor)[levels(intests$predictor)=="f_alcrisk"] <- "Paternal at-risk drinking"
levels(intests$predictor)[levels(intests$predictor)=="m_alcrisk"] <- "Maternal at-risk drinking"
levels(intests$predictor)[levels(intests$predictor)=="m_rxp"] <- "Maternal relationship problems"
levels(intests$predictor)[levels(intests$predictor)=="m_pststr"] <- "Maternal adverse life events"
levels(intests$predictor)[levels(intests$predictor)=="m_dist"] <- "Maternal concurrent distress"
levels(intests$predictor)[levels(intests$predictor)=="m_pstdep"] <- "Maternal postnatal depression"

# select variables
intests2 <- intests %>%
  select(Predictor=predictor,Unadj_est_i=b_i_adjust_est,Unadj_lci_i=b_i_adjust_lci,
         Unadj_uci_i=b_i_adjust_uci,FDR_pval_i_unadj,Unadj_est_s=b_s_adjust_est,
         Unadj_lci_s=b_s_adjust_lci,Unadj_uci_s=b_s_adjust_uci,FDR_pval_s_unadj)

# rename predictors
levels(extests$predictor)[levels(extests$predictor)=="mf_edu"] <- "Parental education"
levels(extests$predictor)[levels(extests$predictor)=="mf_inc"] <- "Parental income"
levels(extests$predictor)[levels(extests$predictor)=="m_prestr"] <- "Maternal prenatal life events"
levels(extests$predictor)[levels(extests$predictor)=="f_stress"] <- "Paternal prenatal life events"
levels(extests$predictor)[levels(extests$predictor)=="m_smok"] <- "Maternal prenatal smoking"
levels(extests$predictor)[levels(extests$predictor)=="f_smok"] <- "Paternal prenatal smoking"
levels(extests$predictor)[levels(extests$predictor)=="m_predis"] <- "Maternal prenatal distress"
levels(extests$predictor)[levels(extests$predictor)=="f_predis"] <- "Paternal prenatal distress"
levels(extests$predictor)[levels(extests$predictor)=="m_alcpr"] <- "Maternal alcohol problems"
levels(extests$predictor)[levels(extests$predictor)=="f_rxp"] <- "Paternal relationship problems"
levels(extests$predictor)[levels(extests$predictor)=="f_alcrisk"] <- "Paternal at-risk drinking"
levels(extests$predictor)[levels(extests$predictor)=="m_alcrisk"] <- "Maternal at-risk drinking"
levels(extests$predictor)[levels(extests$predictor)=="m_rxp"] <- "Maternal relationship problems"
levels(extests$predictor)[levels(extests$predictor)=="m_pststr"] <- "Maternal adverse life events"
levels(extests$predictor)[levels(extests$predictor)=="m_dist"] <- "Maternal concurrent distress"
levels(extests$predictor)[levels(extests$predictor)=="m_pstdep"] <- "Maternal postnatal depression"

# select variables
extests2 <- extests %>%
  select(Predictor=predictor,Unadj_est_i=b_i_adjust_est,Unadj_lci_i=b_i_adjust_lci,
         Unadj_uci_i=b_i_adjust_uci,FDR_pval_i_unadj,Unadj_est_s=b_s_adjust_est,
         Unadj_lci_s=b_s_adjust_lci,Unadj_uci_s=b_s_adjust_uci,FDR_pval_s_unadj)

# save out
save(intests2, file= "./output/intests2_1l.RData")
save(extests2, file= "./output/extests2_1l.RData")
