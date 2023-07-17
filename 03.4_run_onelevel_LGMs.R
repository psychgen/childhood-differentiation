# 05.5_ipw_onelevel_LGMs.R

# this script runs the onelevel LGMs via Mplus using
# MplusAutomation - the aim is to provide overall
# estimates - with the addition of weights

library(tidyverse)
library(lavaan)
library(MplusAutomation)
library(patchwork)
library(ggrepel)
library(viridis)

# load data with weights for sib status from 04.2
load("./data/sib_weighted_data.RData")

# ename x-variable columns to make mplus coding easier - save a lookup
# table for re-identifying these variables later
mplusdat<- wdat 
colnames(mplusdat) <- c(names(mplusdat %>% select(ind_id:dx_adhd)),
                        paste0("x",seq(1,ncol(mplusdat)-length(names(mplusdat %>% select(ind_id:dx_adhd, ipsw))),1)),
                        "ipsw")


lkp_names <- tibble(oldnames = names(wdat),
                    newnames = names(mplusdat))

#Ascertain which predictors have meaningful within-level variance

#Rough'n'ready
check_var <-mplusdat %>% group_by(m_id) %>% summarise(n=n(),
                                                      across(starts_with('x'), n_distinct)) 

#Using lavaan multilevel functionality
check_var_lavaan <- function(pred){
  model=paste0('level:1 
 ',pred,'~1
 level:2
 ',pred,'~1')
  
  run <- sem(model, 
             estimator = "MLR",
             data = mplusdat,
             cluster = "m_id",
             fixed.x = F)
  
  tmp <- parameterEstimates(run) %>% 
    as.data.frame() %>% 
    filter(op=="~~") %>% 
    mutate(predictor=pred)
  
  return(tmp)
}

all <- data.frame()
for(i in paste0("x",seq(1,16))){
  
  tmp <- check_var_lavaan(i)
  all <- rbind(all,tmp)
}

wbvar <- all %>% 
  select(predictor,level,est,se) %>% 
  mutate(est=round(est,2),
         level=ifelse(level==1,"within","between")) %>% 
  left_join(lkp_names %>% 
              select(predictor=newnames,Predictor=oldnames))

# use MplusAutomation to make the data in an Mplus friendly format
prepareMplusData(mplusdat, "//ess01/p471/data/durable/projects/childhood_differentiation/scripts/mplus/data/data_for_mlevel.dat")

# restrict to sibling pairs only
sibids = mplusdat %>% 
  filter(duplicated(m_id))

mplusdat = mplusdat %>% 
  filter(m_id %in% sibids$m_id)

# save the data and the lookup table for the multilevel version of this analysis
save(mplusdat, lkp_names, file="./data/SIBSONLY.RData")

prepareMplusData(mplusdat, "//ess01/p471/data/durable/projects/childhood_differentiation/scripts/mplus/data/data_for_mlevel_SIBSONLY.dat")

filepath1 <- "./scripts/mplus/output/lgm/sibs_only"

# run models

runModels(filepath1, recursive =F,replaceOutfile="modifiedDate", Mplus_command = "C:/Program Files/Mplus/Mplus" )

# read in output
mplusOutput <- readModels(filepath1, recursive=FALSE,
                          what = c("input", "warn_err", "data_summary", "sampstat", "covariance_coverage", "summaries", "parameters", "class_counts", "indirect", "mod_indices", "residuals", "savedata", "bparameters", "tech3", "tech4", "tech7", "tech8", "tech9", "tech10", "tech12", "fac_score_stats", "lcCondMeans", "gh5", "output"))

# extract relevant params and format as per results from standard LGMs
diffests <- mplusOutput$allpred_lgm_diff_1l_constrained_hpc.out$parameters$ci.unstandardized %>% 
  filter(paramHeader == "New.Additional.Parameters",
         str_detect(param,"BI|BS")  )  %>%
  mutate(predictor = rep(paste0("x",seq(1,16)), each=6)) %>% 
  left_join(mplusOutput$allpred_lgm_diff_1l_constrained_hpc.out$parameters$unstandardized %>% 
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
save(diffests, file= "./output/diffests_constrained_IPSW.RData")
load("./output/diffests_constrained_IPSW.RData")

totests <- mplusOutput$allpred_lgm_tot_1l_constrained_hpc.out$parameters$ci.unstandardized %>% 
  filter(paramHeader == "New.Additional.Parameters",
         str_detect(param,"BI|BS")  )  %>%
  mutate(predictor = rep(paste0("x",seq(1,16)), each=6)) %>% 
  left_join(mplusOutput$allpred_lgm_tot_1l_constrained_hpc.out$parameters$unstandardized %>% 
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
save(totests, file= "./output/totests_constrained_IPSW.RData")
load("./output/totests_constrained_IPSW.RData")

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
                              "Prenatal smoking"),
         parent=fct_relevel(parent,"Mother","Father","Both"))

# select and create vars for plotting
diffests2 <- diffests %>%
  select(predictor,b_i_adjust_est,b_s_adjust_est,b_i_adjust_lci,b_i_adjust_uci,
         b_s_adjust_lci,b_s_adjust_uci,fdr_i_pval=FDR_corrected_pvalue_i_adjust,
         fdr_s_pval=FDR_corrected_pvalue_s_adjust,parent,category) %>%
  mutate(fdr_pred = ifelse(fdr_i_pval<.05|fdr_s_pval<.05, paste(predictor), "")) %>%
  mutate(fdr_alpha = ifelse(fdr_i_pval<.05|fdr_s_pval<.05, "FDR-corrected p < .05", "FDR-corrected p > .05")) %>%
  mutate(fdr_alpha = as.factor(fdr_alpha)) %>%
  mutate(fdr_alpha=fct_relevel(fdr_alpha,"FDR-corrected p > .05","FDR-corrected p < .05"))

# create colour scale
my_colours <- c(RColorBrewer::brewer.pal(12, "Paired")[1:10],"#B15928")

# create i/s spider plot
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
tiff("figures/all_preds_diff_1l_IPSW.tiff", res = 800, compression = "lzw", unit = "in",
     height = 7, width = 10)

p
p
dev.off()

# process total 1-level results

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
                              "Prenatal smoking"),
         parent=fct_relevel(parent,"Mother","Father","Both"))

# select and create vars for plotting
totests2 <- totests %>%
  select(predictor,b_i_adjust_est,b_s_adjust_est,b_i_adjust_lci,b_i_adjust_uci,
         b_s_adjust_lci,b_s_adjust_uci,fdr_i_pval=FDR_corrected_pvalue_i_adjust,
         fdr_s_pval=FDR_corrected_pvalue_s_adjust,parent,category) %>%
  mutate(fdr_pred = ifelse(fdr_i_pval<.05|fdr_s_pval<.05, paste(predictor), "")) %>%
  mutate(fdr_alpha = ifelse(fdr_i_pval<.05|fdr_s_pval<.05, "FDR-corrected p < .05", "FDR-corrected p > .05")) %>%
  mutate(fdr_alpha = as.factor(fdr_alpha)) %>%
  mutate(fdr_alpha=fct_relevel(fdr_alpha,"FDR-corrected p > .05","FDR-corrected p < .05"))

# create colour scale
my_colours <- c(RColorBrewer::brewer.pal(12, "Paired")[1:10],"#B15928")

# create i/s spider plot
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
tiff("figures/all_preds_tot_1l_IPSW.tiff", res = 800, compression = "lzw", unit = "in",
     height = 7, width = 10)

p1
p1
dev.off()

# save processed output for later visualisations
save(diffests2, file= "./output/diffests2_constrained_IPSW.RData")
load("./output/diffests2_constrained_IPSW.RData")

save(totests2, file= "./output/totests2_constrained_IPSW.RData")
load("./output/totests2_constrained_IPSW.RData")
