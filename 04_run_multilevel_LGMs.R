# 04_run_multilevel_LGMs.R

# this script runs the multilevel LGMs via Mplus using
# MplusAutomation - the aim is to provide within-family
# estimates of the effects calculated in 03.4, with adjustment
# for familial confounding factors

library(tidyverse)
library(lavaan)
library(MplusAutomation)
library(patchwork)
library(ggrepel)
library(viridis)

# load fully prepped, sib weighted, data for Mplus created in 03.4
load(file="./data/SIBSONLY.RData")

# set filepath for running Mplus scripts
filepath1 <- "./scripts/mplus/scripts/twolevel_lgm/sibs_only"

# run multilevel models (takes a long time! ~ 12 hours for all models )
runModels(filepath1, recursive =F,replaceOutfile="modifiedDate", Mplus_command = "C:/Program Files/Mplus/Mplus" )

# move the results files to the output file and clean up the scripts folder
file.copy(from=paste0(filepath1,"/",list.files(filepath1)[str_detect(list.files(filepath1),".inp",negate=T)]),
          to="./scripts/mplus/output/twolevel_lgm/sibs_only",
          overwrite = TRUE, recursive = F,
          copy.mode = TRUE)

junk <- dir(path=filepath1, pattern=".out|.dat")
file.remove(paste0(filepath1,"/",junk))

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
  select(predictor = oldnames, everything())# Use lookup table to retrieve predictor names

# save 
save(diffests, file= "./output/diffests_new_IPSW.RData")
load("./output/diffests_new_IPSW.RData")

save(totests, file= "./output/totests_new_IPSW.RData")
load("./output/totests_new_IPSW.RData")

# adjust pvalues
diffests$FDR_corrected_pvalue_i_adjust <- p.adjust(diffests$b_i_adjust_pval, method="fdr")
diffests$FDR_corrected_pvalue_s_adjust <- p.adjust(diffests$b_s_adjust_pval, method="fdr")

diffests <- diffests %>%
  mutate(predictor = as.factor(predictor))

# create variable with parent
diffests <- diffests %>%
  mutate(parent=case_when(str_detect(as.character(predictor),"mf_") ~"Both",
                          str_detect(as.character(predictor),"m_") ~"Mother",
                          str_detect(as.character(predictor),"f_") ~"Father"))

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

# create categories of predictors
education <-                      "Education"
income <-                         "Income"
prenatal_adverse_life_events <- c("Prenatal adverse life events", "Prenatal adverse life events") 
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
diffests$category[diffests$predictor %in% prenatal_adverse_life_events] <- "Prenatal adverse life events"
diffests$category[diffests$predictor %in% adverse_life_events] <- "Adverse life events"
diffests$category[diffests$predictor %in% prenatal_smoking] <- "Prenatal smoking"
diffests$category[diffests$predictor %in% prenatal_distress] <- "Prenatal distress"
diffests$category[diffests$predictor %in% at_risk_drinking] <- "At-risk drinking"
diffests$category[diffests$predictor %in% relationship_problems] <- "Relationship problems"
diffests$category[diffests$predictor %in% postnatal_depression] <- "Postnatal depression"
diffests$category[diffests$predictor %in% concurrent_distress] <- "Concurrent distress"
diffests$category[diffests$predictor %in% alcohol_problems] <- "Alcohol problems"

# fix order of categories
diffests <- diffests %>%
  mutate(category=fct_relevel(category,"Income","Education",
                              "Prenatal adverse life events","Adverse life events",
                              "Prenatal distress","Concurrent distress",
                              "Alcohol problems","At-risk drinking",
                              "Postnatal depression","Relationship problems",
                              "Prenatal smoking"))

# fix order of rater/parent
diffests <- diffests %>%
  mutate(parent=fct_relevel(parent,"Mother","Father","Both"))

# select and create vars for diff plot
diffests2 <- diffests %>%
  select(predictor,b_i_adjust_est,b_s_adjust_est,b_i_adjust_lci,b_i_adjust_uci,
         b_s_adjust_lci,b_s_adjust_uci,fdr_i_pval=FDR_corrected_pvalue_i_adjust,
         fdr_s_pval=FDR_corrected_pvalue_s_adjust,parent,category) %>%
  mutate(fdr_pred = ifelse(fdr_i_pval<.05|fdr_s_pval<.05, paste(predictor), "")) %>%
  mutate(fdr_alpha = ifelse(fdr_i_pval<.05|fdr_s_pval<.05, "FDR-corrected p < .05", "FDR-corrected p > .05")) %>%
  mutate(fdr_alpha = as.factor(fdr_alpha)) %>%
  mutate(fdr_alpha=fct_relevel(fdr_alpha,"FDR-corrected p > .05","FDR-corrected p < .05"))

my_colours <- c(RColorBrewer::brewer.pal(12, "Paired")[1:10],"#B15928")

# create diff spider plot of i/s
p<-ggplot(diffests2, mapping=aes(x=b_s_adjust_est,y=b_i_adjust_est,shape=parent,colour=category)) +
  geom_vline(aes(xintercept=0), colour = "black", linetype = 1) +
  geom_hline(aes(yintercept=0), colour = "black", linetype = 1) +
  geom_errorbar(aes(xmin = b_s_adjust_lci, xmax = b_s_adjust_uci, alpha=fdr_alpha), 
                size = 1, width = 0, show.legend = FALSE) +
  geom_errorbar(aes(ymin = b_i_adjust_lci, ymax = b_i_adjust_uci, alpha=fdr_alpha), 
                size = 1, width = 0, show.legend = FALSE) +
  geom_point(aes(alpha=fdr_alpha),fill="white",size=4) +
  geom_label_repel(aes(label=fdr_pred), show.legend = FALSE) +
  scale_colour_manual(values=my_colours) +
  scale_alpha_ordinal(range=c(0.25,1)) +
  scale_shape_manual(values=c(24,21,22))+
  guides(alpha="none") +
  coord_cartesian(xlim = c(-0.09,0.09), ylim = c(-0.09,0.09)) +
  xlab(label=expression(beta[S])) + ylab(label=expression(beta[I])) +
  theme_minimal(base_size=15) + 
  theme(legend.title = element_blank(),
        panel.grid = element_blank(),
        axis.line = element_blank(),
        axis.title.y = element_text(angle = 360, vjust = 0.5),
        axis.ticks = element_blank())
p

# save out
tiff("figures/all_preds_diff_adj_new_IPSW.tiff", res = 800, compression = "lzw", unit = "in",
     height = 7, width = 10)
p
p
dev.off()


# adjust pvalues
totests$FDR_corrected_pvalue_i_adjust <- p.adjust(totests$b_i_adjust_pval, method="fdr")
totests$FDR_corrected_pvalue_s_adjust <- p.adjust(totests$b_s_adjust_pval, method="fdr")

# rename
totests <- totests %>%
  mutate(predictor = as.factor(predictor))

# create variable with parent
totests <- totests %>%
  mutate(parent=case_when(str_detect(as.character(predictor),"mf_") ~"Both",
                          str_detect(as.character(predictor),"m_") ~"Mother",
                          str_detect(as.character(predictor),"f_") ~"Father"))

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

# create categories
education <-                      "Education"
income <-                         "Income"
prenatal_adverse_life_events <- c("Prenatal adverse life events", "Prenatal adverse life events") 
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
totests$category[totests$predictor %in% prenatal_adverse_life_events] <- "Prenatal adverse life events"
totests$category[totests$predictor %in% adverse_life_events] <- "Adverse life events"
totests$category[totests$predictor %in% prenatal_smoking] <- "Prenatal smoking"
totests$category[totests$predictor %in% prenatal_distress] <- "Prenatal distress"
totests$category[totests$predictor %in% at_risk_drinking] <- "At-risk drinking"
totests$category[totests$predictor %in% relationship_problems] <- "Relationship problems"
totests$category[totests$predictor %in% postnatal_depression] <- "Postnatal depression"
totests$category[totests$predictor %in% concurrent_distress] <- "Concurrent distress"
totests$category[totests$predictor %in% alcohol_problems] <- "Alcohol problems"

# fix order of categories
totests <- totests %>%
  mutate(category=fct_relevel(category,"Income","Education",
                              "Prenatal adverse life events","Adverse life events",
                              "Prenatal distress","Concurrent distress",
                              "Alcohol problems","At-risk drinking",
                              "Postnatal depression","Relationship problems",
                              "Prenatal smoking"))

# fix order of rater/parent
totests <- totests %>%
  mutate(parent=fct_relevel(parent,"Mother","Father","Both"))

# select and create vars for plotting tot figure
totests2 <- totests %>%
  select(predictor,b_i_adjust_est,b_s_adjust_est,b_i_adjust_lci,b_i_adjust_uci,
         b_s_adjust_lci,b_s_adjust_uci,fdr_i_pval=FDR_corrected_pvalue_i_adjust,
         fdr_s_pval=FDR_corrected_pvalue_s_adjust,parent,category) %>%
  mutate(fdr_pred = ifelse(fdr_i_pval<.05|fdr_s_pval<.05, paste(predictor), "")) %>%
  mutate(fdr_alpha = ifelse(fdr_i_pval<.05|fdr_s_pval<.05, "FDR-corrected p < .05", "FDR-corrected p > .05")) %>%
  mutate(fdr_alpha = as.factor(fdr_alpha)) %>%
  mutate(fdr_alpha=fct_relevel(fdr_alpha,"FDR-corrected p > .05","FDR-corrected p < .05"))

my_colours <- c(RColorBrewer::brewer.pal(12, "Paired")[1:10],"#B15928")

# create tot spider plot of i/s
p1<-ggplot(totests2, mapping=aes(x=b_s_adjust_est,y=b_i_adjust_est,shape=parent,colour=category)) +
  geom_vline(aes(xintercept=0), colour = "black", linetype = 1) +
  geom_hline(aes(yintercept=0), colour = "black", linetype = 1) +
  geom_errorbar(aes(xmin = b_s_adjust_lci, xmax = b_s_adjust_uci, alpha=fdr_alpha), 
                size = 1, width = 0, show.legend = FALSE) +
  geom_errorbar(aes(ymin = b_i_adjust_lci, ymax = b_i_adjust_uci, alpha=fdr_alpha), 
                size = 1, width = 0, show.legend = FALSE) +
  geom_point(aes(alpha=fdr_alpha),fill="white",size=4) +
  geom_label_repel(aes(label=fdr_pred), show.legend = FALSE) +
  scale_colour_manual(values=my_colours) +
  scale_alpha_ordinal(range=c(0.25,1)) +
  scale_shape_manual(values=c(24,21,22))+
  guides(alpha="none") +
  coord_cartesian(xlim = c(-0.17,0.17), ylim = c(-0.17,0.17)) +
  xlab(label=expression(beta[S])) + ylab(label=expression(beta[I])) +
  theme_minimal(base_size=15) + 
  theme(legend.title = element_blank(),
        panel.grid = element_blank(),
        axis.line = element_blank(),
        axis.title.y = element_text(angle = 360, vjust = 0.5),
        axis.ticks = element_blank())
 p1

# save out fig
 tiff("figures/all_preds_tot_adj_new_IPSW.tiff", res = 800, compression = "lzw", unit = "in",
     height = 7, width = 10)

 p1
 p1
 dev.off()
 
