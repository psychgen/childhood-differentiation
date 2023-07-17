# 04.1_run_multilevel_LGMs_sensitivity.R

# this script runs the multilevel LGMs via Mplus using
# MplusAutomation - the aim is to provide within-family
# estimates of the effects for INT and EXT as sensitivity

library(tidyverse)
library(flextable)
library(ftExtra)
library(equatags)
library(officer)
library(reporter)
library(MplusAutomation)

# read in fully processed, sib only data ready for mplus from 03.5 
load(file="./data/SIBSONLY_sensitivity.RData")

# run multilevel models
filepath1 <- "./scripts/mplus/scripts/twolevel_lgm/sibs_only/sensitivity"

runModels(filepath1, recursive =F,replaceOutfile="modifiedDate", Mplus_command = "C:/Program Files/Mplus/Mplus" )

# move the results files to the output file and clean up the scripts folder
file.copy(from=paste0(filepath1,"/",list.files(filepath1)[str_detect(list.files(filepath1),".inp",negate=T)]),
          to="./scripts/mplus/output/twolevel_lgm/sibs_only/sensitivity",
          overwrite = TRUE, recursive = F,
          copy.mode = TRUE)

junk <- dir(path=filepath1, pattern=".out|.dat") 
file.remove(paste0(filepath1,"/",junk))

# read in output
mplusOutput <- readModels("./scripts/mplus/output/twolevel_lgm/sibs_only/sensitivity", recursive=FALSE,
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

# save output
save(ests, file= "./output/int_ext_ests_2l.RData")
load("./output/int_ext_ests_2l.RData")

# adjust pvalues
extests <- ests %>%
  filter(outcome=="Externalising") %>%
  mutate(FDR_pval_i_adj = p.adjust(b_i_adjust_pval, method="fdr"),
         FDR_pval_s_adj = p.adjust(b_s_adjust_pval, method="fdr"),
         predictor = as.factor(predictor))

intests <- ests %>%
  filter(outcome=="Internalising") %>%
  mutate(FDR_pval_i_adj = p.adjust(b_i_adjust_pval, method="fdr"),
         FDR_pval_s_adj = p.adjust(b_s_adjust_pval, method="fdr"),
         predictor = as.factor(predictor))

# rename predictors int
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

intests2_2l <- intests %>%
  select(Predictor=predictor,Adj_est_i=b_i_adjust_est,Adj_lci_i=b_i_adjust_lci,
         Adj_uci_i=b_i_adjust_uci,FDR_pval_i_adj,Adj_est_s=b_s_adjust_est,
         Adj_lci_s=b_s_adjust_lci,Adj_uci_s=b_s_adjust_uci,FDR_pval_s_adj)

# rename predictors ext
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

extests2_2l <- extests %>%
  select(Predictor=predictor,Adj_est_i=b_i_adjust_est,Adj_lci_i=b_i_adjust_lci,
         Adj_uci_i=b_i_adjust_uci,FDR_pval_i_adj,Adj_est_s=b_s_adjust_est,
         Adj_lci_s=b_s_adjust_lci,Adj_uci_s=b_s_adjust_uci,FDR_pval_s_adj)

# load unadjusted results
load("./output/intests2_1l.RData")
load("./output/extests2_1l.RData")

# create table externalising unadjusted

tbl_ext_unadj <- extests2 %>%
  mutate(across(where(is.numeric), round, digits=2))

(ft_tbl_ext_unadj <- tbl_ext_unadj %>% 
   flextable() %>% 
    theme_booktabs(bold_header = TRUE) %>% 
    align(align = "center", part="all") %>% 
    bg(i = seq(2,nrow(.$body$dataset),2), bg="#F3F3F3") %>% 
    set_header_labels(values=list(Predictor="Predictor",Unadj_est_i="EST",
                                  Unadj_lci_i="LCI",Unadj_uci_i="UCI",
                                  FDR_pval_i_unadj="",Unadj_est_s="EST", 
                                  Unadj_lci_s="LCI",Unadj_uci_s="UCI",FDR_pval_s_unadj="")) %>%
    add_header_row(values=list("","Intercept","Slope"), colwidths = c(1,4,4)) %>%
    add_header_row(values="Table S9: Sensitivity analysis with behavioural problems as the outcome (unadjusted)",colwidths = 9) %>%
    compose(i = c(3,3), j = c(5,9), part = "header", value = as_paragraph("P",as_sub("FDR"))) %>%
    flextable::font(fontname = "Times New Roman", part= "all") %>% 
    vline(j = c(1,5)) %>%
    set_table_properties(layout = "autofit"))

save_as_docx(ft_tbl_ext_unadj, path = "./tables/Table_S9_sensitivity_ext_unadjusted.docx")

# create table externalising adjusted

tbl_ext_adj <- extests2_2l %>%
  mutate(across(where(is.numeric), round, digits=2))

(ft_tbl_ext_adj <- tbl_ext_adj %>% 
    flextable() %>% 
    theme_booktabs(bold_header = TRUE) %>% 
    align(align = "center", part="all") %>% 
    bg(i = seq(2,nrow(.$body$dataset),2), bg="#F3F3F3") %>% 
    set_header_labels(values=list(Predictor="Predictor",Adj_est_i="EST",
                                  Adj_lci_i="LCI",Adj_uci_i="UCI",
                                  FDR_pval_i_adj="",Adj_est_s="EST", 
                                  Adj_lci_s="LCI",Adj_uci_s="UCI",FDR_pval_s_adj="")) %>%
    add_header_row(values=list("","Intercept","Slope"), colwidths = c(1,4,4)) %>%
    add_header_row(values="Table S10: Sensitivity analysis with behavioural problems as the outcome (adjusted)",colwidths = 9) %>%
    compose(i = c(3,3), j = c(5,9), part = "header", value = as_paragraph("P",as_sub("FDR"))) %>%
    flextable::font(fontname = "Times New Roman", part= "all") %>% 
    vline(j = c(1,5)) %>%
    set_table_properties(layout = "autofit"))

save_as_docx(ft_tbl_ext_adj, path = "./tables/Table_S10_sensitivity_ext_adjusted.docx")

# create table internalising unadjusted

tbl_int_unadj <- intests2 %>%
  mutate(across(where(is.numeric), round, digits=2))

(ft_tbl_int_unadj <- tbl_int_unadj %>% 
    flextable() %>% 
    theme_booktabs(bold_header = TRUE) %>% 
    align(align = "center", part="all") %>% 
    bg(i = seq(2,nrow(.$body$dataset),2), bg="#F3F3F3") %>% 
    set_header_labels(values=list(Predictor="Predictor",Unadj_est_i="EST",
                                  Unadj_lci_i="LCI",Unadj_uci_i="UCI",
                                  FDR_pval_i_unadj="",Unadj_est_s="EST", 
                                  Unadj_lci_s="LCI",Unadj_uci_s="UCI",FDR_pval_s_unadj="")) %>%
    add_header_row(values=list("","Intercept","Slope"), colwidths = c(1,4,4)) %>%
    add_header_row(values="Table S11: Sensitivity analysis with emotional problems as the outcome (unadjusted)",colwidths = 9) %>%
    compose(i = c(3,3), j = c(5,9), part = "header", value = as_paragraph("P",as_sub("FDR"))) %>%
    flextable::font(fontname = "Times New Roman", part= "all") %>% 
    vline(j = c(1,5)) %>%
    set_table_properties(layout = "autofit"))

save_as_docx(ft_tbl_int_unadj, path = "./tables/Table_S11_sensitivity_int_unadjusted.docx")

# create table internalising adjusted

tbl_int_adj <- intests2_2l %>%
  mutate(across(where(is.numeric), round, digits=2))

(ft_tbl_int_adj <- tbl_int_adj %>% 
    flextable() %>% 
    theme_booktabs(bold_header = TRUE) %>% 
    align(align = "center", part="all") %>% 
    bg(i = seq(2,nrow(.$body$dataset),2), bg="#F3F3F3") %>% 
    set_header_labels(values=list(Predictor="Predictor",Adj_est_i="EST",
                                  Adj_lci_i="LCI",Adj_uci_i="UCI",
                                  FDR_pval_i_adj="",Adj_est_s="EST", 
                                  Adj_lci_s="LCI",Adj_uci_s="UCI",FDR_pval_s_adj="")) %>%
    add_header_row(values=list("","Intercept","Slope"), colwidths = c(1,4,4)) %>%
    add_header_row(values="Table S12: Sensitivity analysis with emotional problems as the outcome (adjusted)",colwidths = 9) %>%
    compose(i = c(3,3), j = c(5,9), part = "header", value = as_paragraph("P",as_sub("FDR"))) %>%
    flextable::font(fontname = "Times New Roman", part= "all") %>% 
    vline(j = c(1,5)) %>%
    set_table_properties(layout = "autofit"))

save_as_docx(ft_tbl_int_adj, path = "./tables/Table_S12_sensitivity_int_adjusted.docx")


