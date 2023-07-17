# 05.5_sibonly_multilevel_LGM_ippw.R

# This script runs the multilevel LGMs via Mplus using
# MplusAutomation - the aim is to provide within-family
# estimates with adjustment for familial confounding 
# factors, and additionally including the IPPW weights
# It then pulls unweighted and weighted results from
# previous scripts to produce Tables S13-S20 with results 
# from the IPPW sensitivity analyses of early exposures

library(tidyverse)
library(lavaan)
library(MplusAutomation)

# load prepped data from 05.3
load( file="./data/mplusdat_IPW.RData")

# restrict to sibling pairs only
sibids = mplusdat %>% 
  filter(duplicated(m_id))

mplusdat = mplusdat %>% 
  filter(m_id %in% sibids$m_id)

filepath1 <- "./scripts/mplus/scripts/twolevel_lgm/sibs_only/ippw"

# run multilevel models
runModels(filepath1, recursive =F,replaceOutfile="modifiedDate", Mplus_command = "C:/Program Files/Mplus/Mplus" )

# read in output
mplusOutput <- readModels(filepath1, recursive=FALSE,
                          what = c("input", "warn_err", "data_summary", "sampstat", "covariance_coverage", "summaries", "parameters", "class_counts", "indirect", "mod_indices", "residuals", "savedata", "bparameters", "tech3", "tech4", "tech7", "tech8", "tech9", "tech10", "tech12", "fac_score_stats", "lcCondMeans", "gh5", "output"))

# extract relevant params and format as per results from standard LGMs
diffests <- mplusOutput$allpred_lgm_diff_2l_weighted.out$parameters$ci.unstandardized %>% 
  filter(paramHeader == "New.Additional.Parameters",
         str_detect(param,"BI|BS")  )  %>%
  mutate(predictor = rep(paste0("x",seq(1,16)), each=6)) %>% 
  left_join(mplusOutput$allpred_lgm_diff_2l_weighted.out$parameters$unstandardized %>% 
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

totests <- mplusOutput$allpred_lgm_tot_2l_weighted.out$parameters$ci.unstandardized %>% 
  filter(paramHeader == "New.Additional.Parameters",
         str_detect(param,"BI|BS")  )  %>%
  mutate(predictor = rep(paste0("x",seq(1,16)), each=6)) %>% 
  left_join(mplusOutput$allpred_lgm_tot_2l_weighted.out$parameters$unstandardized %>% 
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

# save results
save(diffests, file= "./output/diffests_multilevel_IPPW.RData")
load("./output/diffests_multilevel_IPPW.RData")

save(totests, file= "./output/totests_multilevel_IPPW.RData")
load("./output/totests_multilevel_IPPW.RData")

# process differentiation results from multilevel adjusted model

# adjust pvalues
diffests <- diffests %>%
  mutate(FDR_pvalue_i = p.adjust(diffests$b_i_adjust_pval, method="fdr"),
         FDR_pvalue_s = p.adjust(diffests$b_s_adjust_pval, method="fdr"),
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

# select diff variables
diffests2 <- diffests %>%
  select(Predictor=predictor,Adj_est_i=b_i_adjust_est,Adj_lci_i=b_i_adjust_lci,
         Adj_uci_i=b_i_adjust_uci,FDR_pval_i=FDR_pvalue_i,Adj_est_s=b_s_adjust_est,
         Adj_lci_s=b_s_adjust_lci,Adj_uci_s=b_s_adjust_uci,FDR_pval_s=FDR_pvalue_s)

# save weighted, diff adj results
save(diffests2, file="./output/diffests2_multilevel_IPPW.RData")

# process total results from multilevel adjusted model

# adjust pvalues
totests <- totests %>%
  mutate(FDR_pvalue_i = p.adjust(totests$b_i_adjust_pval, method="fdr"),
         FDR_pvalue_s = p.adjust(totests$b_s_adjust_pval, method="fdr"),
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

# select tot variables
totests2 <- totests %>%
  select(Predictor=predictor,Adj_est_i=b_i_adjust_est,Adj_lci_i=b_i_adjust_lci,
         Adj_uci_i=b_i_adjust_uci,FDR_pval_i=FDR_pvalue_i,Adj_est_s=b_s_adjust_est,
         Adj_lci_s=b_s_adjust_lci,Adj_uci_s=b_s_adjust_uci,FDR_pval_s=FDR_pvalue_s)

# save weighted, tot adj results
save(totests2, file="./output/totests2_multilevel_IPPW.RData")

# get unweighted diff/tot results from the script:
# 04.4_multilevel_LGMs_cleveland_plot.R

load("./output/diffests_obs_sibs.RData")
load("./output/totests_obs_sibs.RData")

# adjust pvalues
diffests <- diffests_obs %>%
  mutate(FDR_pvalue_i = p.adjust(diffests_obs$b_i_est_pval, method="fdr"),
         FDR_pvalue_s = p.adjust(diffests_obs$b_s_est_pval, method="fdr"),
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

# select diff variables
diffests_unadj_unw <- diffests %>%
  select(Predictor=predictor,Unadj_est_i=b_i_est_est,Unadj_lci_i=b_i_est_lci,
         Unadj_uci_i=b_i_est_uci,FDR_pval_i=FDR_pvalue_i,Unadj_est_s=b_s_est_est,
         Unadj_lci_s=b_s_est_lci,Unadj_uci_s=b_s_est_uci,FDR_pval_s=FDR_pvalue_s)

# process total unweighted, unadjusted results

# adjust pvalues
totests <- totests_obs %>%
  mutate(FDR_pvalue_i = p.adjust(totests_obs$b_i_est_pval, method="fdr"),
         FDR_pvalue_s = p.adjust(totests_obs$b_s_est_pval, method="fdr"),
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

# select tot variables
totests_unadj_unw <- totests %>%
  select(Predictor=predictor,Unadj_est_i=b_i_est_est,Unadj_lci_i=b_i_est_lci,
         Unadj_uci_i=b_i_est_uci,FDR_pval_i=FDR_pvalue_i,Unadj_est_s=b_s_est_est,
         Unadj_lci_s=b_s_est_lci,Unadj_uci_s=b_s_est_uci,FDR_pval_s=FDR_pvalue_s)

# save out
save(diffests_unadj_unw, file="./output/diffests_1l_unweighted.RData")
save(totests_unadj_unw, file="./output/totests_1l_unweighted.RData")

# format unweighted, multilevel adjusted results
load("./output/diffests_sibs.RData")
load("./output/totests_sibs.RData")

# adjust pvalues
diffests <- diffests %>%
  mutate(FDR_pvalue_i = p.adjust(diffests$b_i_adjust_pval, method="fdr"),
         FDR_pvalue_s = p.adjust(diffests$b_s_adjust_pval, method="fdr"),
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

# select diff variables
diffests_adj_unw <- diffests %>%
  select(Predictor=predictor,Adj_est_i=b_i_adjust_est,Adj_lci_i=b_i_adjust_lci,
         Adj_uci_i=b_i_adjust_uci,FDR_pval_i=FDR_pvalue_i,Adj_est_s=b_s_adjust_est,
         Adj_lci_s=b_s_adjust_lci,Adj_uci_s=b_s_adjust_uci,FDR_pval_s=FDR_pvalue_s)

# adjust pvalues
totests <- totests %>%
  mutate(FDR_pvalue_i = p.adjust(totests$b_i_adjust_pval, method="fdr"),
         FDR_pvalue_s = p.adjust(totests$b_s_adjust_pval, method="fdr"),
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

# select tot variables
totests_adj_unw <- totests %>%
  select(Predictor=predictor,Adj_est_i=b_i_adjust_est,Adj_lci_i=b_i_adjust_lci,
         Adj_uci_i=b_i_adjust_uci,FDR_pval_i=FDR_pvalue_i,Adj_est_s=b_s_adjust_est,
         Adj_lci_s=b_s_adjust_lci,Adj_uci_s=b_s_adjust_uci,FDR_pval_s=FDR_pvalue_s)

# save out
save(diffests_adj_unw, file="./output/diffests_multilevel_unweighted.RData")
save(totests_adj_unw, file="./output/totests_multilevel_unweighted.RData")

###############################################
################ LOAD RESULTS #################
###############################################

# load unweighted, unadjusted results diff/tot
load(file="./output/diffests_1l_unweighted.RData")
load(file="./output/totests_1l_unweighted.RData")

# load weighted, unadjusted results diff/tot
load("./output/diffests2_sibonly_1l_IPPW.RData")
load("./output/totests2_sibonly_1l_IPPW.RData")

# load unweighted, adjusted results diff/tot
load(file="./output/diffests_multilevel_unweighted.RData")
load(file="./output/totests_multilevel_unweighted.RData")

# load weighted, adjusted results diff/tot
load("./output/diffests2_multilevel_IPPW.RData")
load("./output/totests2_multilevel_IPPW.RData")

###############################################
################ CREATE TABLES ################
###############################################

# Table S13: create table unweighted diff unadjusted
tbl_unw_diff_unadj <- diffests_unadj_unw %>%
  mutate(across(where(is.numeric), round, digits=2))

(ft_tbl_unw_diff_unadj <- tbl_unw_diff_unadj %>% 
    flextable() %>% 
    theme_booktabs(bold_header = TRUE) %>% 
    align(align = "center", part="all") %>% 
    bg(i = seq(2,nrow(.$body$dataset),2), bg="#F3F3F3") %>% 
    set_header_labels(values=list(Predictor="Predictor",Unadj_est_i="EST",
                                  Unadj_lci_i="LCI",Unadj_uci_i="UCI",
                                  FDR_pval_i="",Unadj_est_s="EST", 
                                  Unadj_lci_s="LCI",Unadj_uci_s="UCI",FDR_pval_s="")) %>%
    add_header_row(values=list("","Intercept","Slope"), colwidths = c(1,4,4)) %>%
    add_header_row(values="Table S13: Unweighted results from sensitivity analysis (differentiation, unadjusted)",colwidths = 9) %>%
    compose(i = c(3,3), j = c(5,9), part = "header", value = as_paragraph("P",as_sub("FDR"))) %>%
    flextable::font(fontname = "Times New Roman", part= "all") %>% 
    vline(j = c(1,5)) %>%
    set_table_properties(layout = "autofit"))

save_as_docx(ft_tbl_unw_diff_unadj, path = "./tables/Table_S13_sensitivity_IPPunw_diff_unadjusted.docx")

# Table S14: create table weighted diff unadjusted
tbl_IPPW_diff_unadj <- diffests2_ippw %>%
  mutate(across(where(is.numeric), round, digits=2))

(ft_tbl_IPPW_diff_unadj <- tbl_IPPW_diff_unadj %>% 
    flextable() %>% 
    theme_booktabs(bold_header = TRUE) %>% 
    align(align = "center", part="all") %>% 
    bg(i = seq(2,nrow(.$body$dataset),2), bg="#F3F3F3") %>% 
    set_header_labels(values=list(Predictor="Predictor",Unadj_est_i="EST",
                                  Unadj_lci_i="LCI",Unadj_uci_i="UCI",
                                  FDR_pval_i="",Unadj_est_s="EST", 
                                  Unadj_lci_s="LCI",Unadj_uci_s="UCI",FDR_pval_s="")) %>%
    add_header_row(values=list("","Intercept","Slope"), colwidths = c(1,4,4)) %>%
    add_header_row(values="Table S14: Weighted results from sensitivity analysis (differentiation, unadjusted)",colwidths = 9) %>%
    compose(i = c(3,3), j = c(5,9), part = "header", value = as_paragraph("P",as_sub("FDR"))) %>%
    flextable::font(fontname = "Times New Roman", part= "all") %>% 
    vline(j = c(1,5)) %>%
    set_table_properties(layout = "autofit"))

save_as_docx(ft_tbl_IPPW_diff_unadj, path = "./tables/Table_S14_sensitivity_IPPW_diff_unadjusted.docx")

# Table S15: create table unweighted tot unadjusted
tbl_unw_tot_unadj <- totests_unadj_unw %>%
  mutate(across(where(is.numeric), round, digits=2))

(ft_tbl_unw_tot_unadj <- tbl_unw_tot_unadj %>% 
    flextable() %>% 
    theme_booktabs(bold_header = TRUE) %>% 
    align(align = "center", part="all") %>% 
    bg(i = seq(2,nrow(.$body$dataset),2), bg="#F3F3F3") %>% 
    set_header_labels(values=list(Predictor="Predictor",Unadj_est_i="EST",
                                  Unadj_lci_i="LCI",Unadj_uci_i="UCI",
                                  FDR_pval_i="",Unadj_est_s="EST", 
                                  Unadj_lci_s="LCI",Unadj_uci_s="UCI",FDR_pval_s="")) %>%
    add_header_row(values=list("","Intercept","Slope"), colwidths = c(1,4,4)) %>%
    add_header_row(values="Table S15: Unweighted results from sensitivity analysis (total, unadjusted)",colwidths = 9) %>%
    compose(i = c(3,3), j = c(5,9), part = "header", value = as_paragraph("P",as_sub("FDR"))) %>%
    flextable::font(fontname = "Times New Roman", part= "all") %>% 
    vline(j = c(1,5)) %>%
    set_table_properties(layout = "autofit"))

save_as_docx(ft_tbl_unw_tot_unadj, path = "./tables/Table_S15_sensitivity_IPPunw_tot_unadjusted.docx")

# Table S16: create table weighted tot unadjusted
tbl_IPPW_tot_unadj <- totests2_ippw %>%
  mutate(across(where(is.numeric), round, digits=2))

(ft_tbl_IPPW_tot_unadj <- tbl_IPPW_tot_unadj %>% 
    flextable() %>% 
    theme_booktabs(bold_header = TRUE) %>% 
    align(align = "center", part="all") %>% 
    bg(i = seq(2,nrow(.$body$dataset),2), bg="#F3F3F3") %>% 
    set_header_labels(values=list(Predictor="Predictor",Unadj_est_i="EST",
                                  Unadj_lci_i="LCI",Unadj_uci_i="UCI",
                                  FDR_pval_i="",Unadj_est_s="EST", 
                                  Unadj_lci_s="LCI",Unadj_uci_s="UCI",FDR_pval_s="")) %>%
    add_header_row(values=list("","Intercept","Slope"), colwidths = c(1,4,4)) %>%
    add_header_row(values="Table S16: Weighted results from sensitivity analysis (total, unadjusted)",colwidths = 9) %>%
    compose(i = c(3,3), j = c(5,9), part = "header", value = as_paragraph("P",as_sub("FDR"))) %>%
    flextable::font(fontname = "Times New Roman", part= "all") %>% 
    vline(j = c(1,5)) %>%
    set_table_properties(layout = "autofit"))

save_as_docx(ft_tbl_IPPW_tot_unadj, path = "./tables/Table_S16_sensitivity_IPPW_tot_unadjusted.docx")

# Table S17: create table unweighted diff adjusted
tbl_unw_diff_adj <- diffests_adj_unw %>%
  mutate(across(where(is.numeric), round, digits=2))

(ft_tbl_unw_diff_adj <- tbl_unw_diff_adj %>% 
    flextable() %>% 
    theme_booktabs(bold_header = TRUE) %>% 
    align(align = "center", part="all") %>% 
    bg(i = seq(2,nrow(.$body$dataset),2), bg="#F3F3F3") %>% 
    set_header_labels(values=list(Predictor="Predictor",Adj_est_i="EST",
                                  Adj_lci_i="LCI",Adj_uci_i="UCI",
                                  FDR_pval_i="",Adj_est_s="EST", 
                                  Adj_lci_s="LCI",Adj_uci_s="UCI",FDR_pval_s="")) %>%
    add_header_row(values=list("","Intercept","Slope"), colwidths = c(1,4,4)) %>%
    add_header_row(values="Table S17: Unweighted results from sensitivity analysis (differentiation, adjusted)",colwidths = 9) %>%
    compose(i = c(3,3), j = c(5,9), part = "header", value = as_paragraph("P",as_sub("FDR"))) %>%
    flextable::font(fontname = "Times New Roman", part= "all") %>% 
    vline(j = c(1,5)) %>%
    set_table_properties(layout = "autofit"))

save_as_docx(ft_tbl_unw_diff_adj, path = "./tables/Table_S17_sensitivity_IPPunw_diff_adjusted.docx")

# Table S18: create table weighted diff adjusted
tbl_IPPW_diff_adj <- diffests2 %>%
  mutate(across(where(is.numeric), round, digits=2))

(ft_tbl_IPPW_diff_adj <- tbl_IPPW_diff_adj %>% 
    flextable() %>% 
    theme_booktabs(bold_header = TRUE) %>% 
    align(align = "center", part="all") %>% 
    bg(i = seq(2,nrow(.$body$dataset),2), bg="#F3F3F3") %>% 
    set_header_labels(values=list(Predictor="Predictor",Adj_est_i="EST",
                                  Adj_lci_i="LCI",Adj_uci_i="UCI",
                                  FDR_pval_i="",Adj_est_s="EST", 
                                  Adj_lci_s="LCI",Adj_uci_s="UCI",FDR_pval_s="")) %>%
    add_header_row(values=list("","Intercept","Slope"), colwidths = c(1,4,4)) %>%
    add_header_row(values="Table S18: Weighted results from sensitivity analysis (differentiation, adjusted)",colwidths = 9) %>%
    compose(i = c(3,3), j = c(5,9), part = "header", value = as_paragraph("P",as_sub("FDR"))) %>%
    flextable::font(fontname = "Times New Roman", part= "all") %>% 
    vline(j = c(1,5)) %>%
    set_table_properties(layout = "autofit"))

save_as_docx(ft_tbl_IPPW_diff_adj, path = "./tables/Table_S18_sensitivity_IPPW_diff_adjusted.docx")

# Table S19: create table unweighted tot adjusted
tbl_unw_tot_adj <- totests_adj_unw %>%
  mutate(across(where(is.numeric), round, digits=2))

(ft_tbl_unw_tot_adj <- tbl_unw_tot_adj %>% 
    flextable() %>% 
    theme_booktabs(bold_header = TRUE) %>% 
    align(align = "center", part="all") %>% 
    bg(i = seq(2,nrow(.$body$dataset),2), bg="#F3F3F3") %>% 
    set_header_labels(values=list(Predictor="Predictor",Adj_est_i="EST",
                                  Adj_lci_i="LCI",Adj_uci_i="UCI",
                                  FDR_pval_i="",Adj_est_s="EST", 
                                  Adj_lci_s="LCI",Adj_uci_s="UCI",FDR_pval_s="")) %>%
    add_header_row(values=list("","Intercept","Slope"), colwidths = c(1,4,4)) %>%
    add_header_row(values="Table S19: Unweighted results from sensitivity analysis (total, adjusted)",colwidths = 9) %>%
    compose(i = c(3,3), j = c(5,9), part = "header", value = as_paragraph("P",as_sub("FDR"))) %>%
    flextable::font(fontname = "Times New Roman", part= "all") %>% 
    vline(j = c(1,5)) %>%
    set_table_properties(layout = "autofit"))

save_as_docx(ft_tbl_unw_tot_adj, path = "./tables/Table_S19_sensitivity_IPPunw_tot_adjusted.docx")

# Table S20: create table weighted tot adjusted
tbl_IPPW_tot_adj <- totests2 %>%
  mutate(across(where(is.numeric), round, digits=2))

(ft_tbl_IPPW_tot_adj <- tbl_IPPW_tot_adj %>% 
    flextable() %>% 
    theme_booktabs(bold_header = TRUE) %>% 
    align(align = "center", part="all") %>% 
    bg(i = seq(2,nrow(.$body$dataset),2), bg="#F3F3F3") %>% 
    set_header_labels(values=list(Predictor="Predictor",Adj_est_i="EST",
                                  Adj_lci_i="LCI",Adj_uci_i="UCI",
                                  FDR_pval_i="",Adj_est_s="EST", 
                                  Adj_lci_s="LCI",Adj_uci_s="UCI",FDR_pval_s="")) %>%
    add_header_row(values=list("","Intercept","Slope"), colwidths = c(1,4,4)) %>%
    add_header_row(values="Table S20: Weighted results from sensitivity analysis (total, adjusted)",colwidths = 9) %>%
    compose(i = c(3,3), j = c(5,9), part = "header", value = as_paragraph("P",as_sub("FDR"))) %>%
    flextable::font(fontname = "Times New Roman", part= "all") %>% 
    vline(j = c(1,5)) %>%
    set_table_properties(layout = "autofit"))

save_as_docx(ft_tbl_IPPW_tot_adj, path = "./tables/Table_S20_sensitivity_IPPW_tot_adjusted.docx")


