# 03.3_run_combined_LGMs_with_predictors.R

# this script runs the latent growth models (LGMs) by sourcing:
# 03.2_specify_combined_LGMs_with_predictors.R

library(tidyverse)
library(lavaan)
library(patchwork)
library(ggrepel)
library(viridis)

# read in processed data from 00_data_preparation.R
load("./data/processed_data_new.RData")

# specify LGMs
source("./scripts/03.2_specify_combined_LGMs_with_predictors.R")

# run LGM with predictors - both intercept and slope for diff and tot
fit_model1_both <- sem(model1, 
                       missing = "ML", 
                       estimator = "MLR", 
                       data = alldata_new, 
                       cluster = "m_id",
                       fixed.x = F)

# provide output for the model
summary(fit_model1_both, fit.measures = TRUE, std = TRUE)

save(fit_model1_both, file= "./output/fit_model1_both_new.RData")
load("./output/fit_model1_both_new.RData")

# run LGM with predictors - intercepts only for diff and tot
fit_model2_both <- sem(model2, 
                       missing = "ML", 
                       estimator = "MLR", 
                       data = alldata_new, 
                       cluster = "m_id",
                       fixed.x = F)

# provide output for the model
summary(fit_model2_both, fit.measures = TRUE, std = TRUE)

save(fit_model2_both, file= "./output/fit_model2_both_new.RData")

# run LGM with predictors - slopes only for diff and tot
fit_model3_both <- sem(model3, 
                       missing = "ML", 
                       estimator = "MLR", 
                       data = alldata_new, 
                       cluster = "m_id",
                       fixed.x = F)

# provide output for the model
summary(fit_model3_both, fit.measures = TRUE, std = TRUE)

save(fit_model3_both, file= "./output/fit_model3_both_new.RData")

# compare the different models
intercepts_only <- anova(fit_model1_both, fit_model2_both)
slopes_only <- anova(fit_model1_both, fit_model3_both)

intercepts_only 
slopes_only 

# take estimates from diff model
tmp_i <- standardizedSolution(fit_model1_both, ci = TRUE, level = 0.95) %>%
  filter(lhs=="i1", op=="~") %>%
  mutate(FDR_corrected_pvalue = p.adjust(pvalue, method="fdr"))
  
tmp_s <- standardizedSolution(fit_model1_both, ci = TRUE, level = 0.95) %>%
  filter(lhs=="s1", op=="~") %>%
  mutate(FDR_corrected_pvalue = p.adjust(pvalue, method="fdr"))
  
tmp1 <- tmp_i %>%
  full_join(tmp_s) %>%
  mutate(model = "diff_with_predictors")

# remove sex and parity from predictors
ests1 <- tmp1[-c(1,2,19,20), ]

# create parent var
ests1 <- ests1 %>%
  mutate(rhs = as.factor(rhs),
         parent=case_when(str_detect(as.character(rhs),"_m") ~"Mother",
                          str_detect(as.character(rhs),"_f") ~"Father",
                          str_detect(as.character(rhs),"education") ~"Both",
                          str_detect(as.character(rhs),"income") ~"Both"))

# rename predictors
levels(ests1$rhs)[levels(ests1$rhs)=="education"] <- "Education"
levels(ests1$rhs)[levels(ests1$rhs)=="income"] <- "Income"
levels(ests1$rhs)[levels(ests1$rhs)=="pre_stress_m"] <- "Prenatal life events"
levels(ests1$rhs)[levels(ests1$rhs)=="stress_f"] <- "Prenatal life events"
levels(ests1$rhs)[levels(ests1$rhs)=="smoking_m"] <- "Prenatal smoking"
levels(ests1$rhs)[levels(ests1$rhs)=="smoking_f"] <- "Prenatal smoking"
levels(ests1$rhs)[levels(ests1$rhs)=="pre_distress_m"] <- "Prenatal distress"
levels(ests1$rhs)[levels(ests1$rhs)=="pre_distress_f"] <- "Prenatal distress"
levels(ests1$rhs)[levels(ests1$rhs)=="alc_prob_m"] <- "Alcohol problems"
levels(ests1$rhs)[levels(ests1$rhs)=="relation_f"] <- "Relationship problems"
levels(ests1$rhs)[levels(ests1$rhs)=="alc_risk_tot_f"] <- "At-risk drinking"
levels(ests1$rhs)[levels(ests1$rhs)=="alc_risk_tot_m"] <- "At-risk drinking"
levels(ests1$rhs)[levels(ests1$rhs)=="relation_m"] <- "Relationship problems"
levels(ests1$rhs)[levels(ests1$rhs)=="post_stress_m"] <- "Adverse life events"
levels(ests1$rhs)[levels(ests1$rhs)=="distress_m"] <- "Concurrent distress"
levels(ests1$rhs)[levels(ests1$rhs)=="post_dep_m"] <- "Postnatal depression"

# create categories of predictors
education <-                      "Education"
income <-                         "Income"
prenatal_life_events <- c("Prenatal life events", "Prenatal life events") 
adverse_life_events <-            "Adverse life events"
prenatal_smoking <-             c("Prenatal smoking", "Prenatal smoking")
prenatal_distress <-            c("Prenatal distress", "Prenatal distress")
at_risk_drinking <-             c("At-risk drinking", "At-risk drinking")
relationship_problems <-        c("Relationship problems", "Relationship problems")
postnatal_depression <-           "Postnatal depression"
concurrent_distress <-            "Concurrent distress"
alcohol_problems <-               "Alcohol problems"

# add a column with the category name 
ests1$category <- NULL
ests1$category[ests1$rhs %in% education] <- "Education"
ests1$category[ests1$rhs %in% income] <- "Income"
ests1$category[ests1$rhs %in% prenatal_life_events] <- "Prenatal life events"
ests1$category[ests1$rhs %in% adverse_life_events] <- "Adverse life events"
ests1$category[ests1$rhs %in% prenatal_smoking] <- "Prenatal smoking"
ests1$category[ests1$rhs %in% prenatal_distress] <- "Prenatal distress"
ests1$category[ests1$rhs %in% at_risk_drinking] <- "At-risk drinking"
ests1$category[ests1$rhs %in% relationship_problems] <- "Relationship problems"
ests1$category[ests1$rhs %in% postnatal_depression] <- "Postnatal depression"
ests1$category[ests1$rhs %in% concurrent_distress] <- "Concurrent distress"
ests1$category[ests1$rhs %in% alcohol_problems] <- "Alcohol problems"

# fix order of categories
ests1 <- ests1 %>%
  mutate(category=fct_relevel(category,"Income","Education",
                              "Prenatal life events","Adverse life events",
                              "Prenatal distress","Concurrent distress",
                              "Alcohol problems","At-risk drinking",
                              "Postnatal depression","Relationship problems",
                              "Prenatal smoking"),
         parent=fct_relevel(parent,"Mother","Father","Both"),
         lhs=fct_relevel(lhs,"s1","i1"))

# rename growth factors
levels(ests1$lhs)[levels(ests1$lhs)=="s1"] <- "Slope"
levels(ests1$lhs)[levels(ests1$lhs)=="i1"] <- "Intercept"

# format output for plotting
ests2 <- ests1 %>%
  select(growth_factor=lhs, predictor=rhs, beta=est.std, lci=ci.lower, 
         uci=ci.upper,fdr_pval=FDR_corrected_pvalue, parent,category) %>%
  mutate(beta_i = case_when(growth_factor=="Intercept"~ beta)) %>%
  mutate(beta_s = case_when(growth_factor=="Slope"~ beta)) %>%
  mutate(beta_i_lci = case_when(growth_factor=="Intercept"~ lci)) %>%
  mutate(beta_i_uci = case_when(growth_factor=="Intercept"~ uci)) %>%
  mutate(beta_s_lci = case_when(growth_factor=="Slope"~ lci)) %>%
  mutate(beta_s_uci = case_when(growth_factor=="Slope"~ uci)) %>%
  mutate(predictor = case_when(growth_factor=="Intercept"~ predictor)) %>%
  mutate(fdr_pval_i = case_when(growth_factor=="Intercept"~ fdr_pval)) %>%
  mutate(fdr_pval_s = case_when(growth_factor=="Slope"~ fdr_pval)) %>%
  mutate(parent = case_when(growth_factor=="Intercept"~ parent)) %>%
  mutate(category = case_when(growth_factor=="Intercept"~ category)) %>%
  select(predictor,beta_i,beta_s,beta_i_lci,beta_i_uci,
         beta_s_lci,beta_s_uci,fdr_pval_i,fdr_pval_s,parent,category) %>%
  summarise_all(list(~ discard(., is.na))) %>%
  mutate(fdr_pred = ifelse(fdr_pval_i<.05|fdr_pval_s<.05, paste(predictor), "")) %>%
  mutate(fdr_alpha = ifelse(fdr_pval_i<.05|fdr_pval_s<.05, "FDR-corrected p < .05", "FDR-corrected p > .05")) %>%
  mutate(fdr_alpha = as.factor(fdr_alpha)) %>%
  mutate(fdr_alpha=fct_relevel(fdr_alpha,"FDR-corrected p > .05","FDR-corrected p < .05"))

# save out
save(ests2, file="./output/obs_pred_diff.RData")

# create colour scale
my_colours <- c(RColorBrewer::brewer.pal(12, "Paired")[1:10],"#B15928")

# create intercept/slope spider plot
p2<-ggplot(ests2, mapping=aes(x=beta_s,y=beta_i,shape=parent,colour=category)) +
  geom_vline(aes(xintercept=0), colour = "black", linetype = 1) +
  geom_hline(aes(yintercept=0), colour = "black", linetype = 1) +
  geom_errorbar(aes(xmin = beta_s_lci, xmax = beta_s_uci, alpha=fdr_alpha), 
                size = 1, width = 0, show.legend = FALSE) +
  geom_errorbar(aes(ymin = beta_i_lci, ymax = beta_i_uci, alpha=fdr_alpha), 
                size = 1, width = 0, show.legend = FALSE) +
  geom_point(aes(alpha=fdr_alpha),fill="white",size=4) +
geom_label_repel(aes(label=fdr_pred), show.legend = FALSE) +
  scale_colour_manual(values=my_colours) +
  scale_alpha_ordinal(range=c(0.25,1)) +
  scale_shape_manual(values=c(24,21,22))+
  guides(alpha=FALSE) +
  coord_cartesian(xlim = c(-0.09,0.09), ylim = c(-0.09,0.09)) +
  xlab(label=expression(beta[S])) + ylab(label=expression(beta[I])) +
  theme_minimal(base_size=15) + 
  theme(legend.title = element_blank(),
        panel.grid = element_blank(),
        axis.line = element_blank(),
        axis.title.y = element_text(angle = 360, vjust = 0.5),
        axis.ticks = element_blank())
p2

# save out
tiff("figures/all_preds_diff_unadj_obs.tiff", res = 800, compression = "lzw", unit = "in",
     height = 7, width = 10)

p2
p2
dev.off()

# take estimates from tot model
tmp_i3 <- standardizedSolution(fit_model1_both, ci = TRUE, level = 0.95) %>%
  filter(lhs=="i2", op=="~") %>%
  mutate(FDR_corrected_pvalue = p.adjust(pvalue, method="fdr"))

tmp_s3 <- standardizedSolution(fit_model1_both, ci = TRUE, level = 0.95) %>%
  filter(lhs=="s2", op=="~") %>%
  mutate(FDR_corrected_pvalue = p.adjust(pvalue, method="fdr"))

tmp3 <- tmp_i3 %>%
  full_join(tmp_s3) %>%
  mutate(model = "tot_with_predictors")

# remove sex and parity from predictors
ests3 <- tmp3[-c(1,2,19,20), ]

# create parent var
ests3 <- ests3 %>%
  mutate(rhs = as.factor(rhs),
         parent=case_when(str_detect(as.character(rhs),"_m") ~"Mother",
                          str_detect(as.character(rhs),"_f") ~"Father",
                          str_detect(as.character(rhs),"education") ~"Both",
                          str_detect(as.character(rhs),"income") ~"Both"))

# rename predictors
levels(ests3$rhs)[levels(ests3$rhs)=="education"] <- "Education"
levels(ests3$rhs)[levels(ests3$rhs)=="income"] <- "Income"
levels(ests3$rhs)[levels(ests3$rhs)=="pre_stress_m"] <- "Prenatal life events"
levels(ests3$rhs)[levels(ests3$rhs)=="stress_f"] <- "Prenatal life events"
levels(ests3$rhs)[levels(ests3$rhs)=="smoking_m"] <- "Prenatal smoking"
levels(ests3$rhs)[levels(ests3$rhs)=="smoking_f"] <- "Prenatal smoking"
levels(ests3$rhs)[levels(ests3$rhs)=="pre_distress_m"] <- "Prenatal distress"
levels(ests3$rhs)[levels(ests3$rhs)=="pre_distress_f"] <- "Prenatal distress"
levels(ests3$rhs)[levels(ests3$rhs)=="alc_prob_m"] <- "Alcohol problems"
levels(ests3$rhs)[levels(ests3$rhs)=="relation_f"] <- "Relationship problems"
levels(ests3$rhs)[levels(ests3$rhs)=="alc_risk_tot_f"] <- "At-risk drinking"
levels(ests3$rhs)[levels(ests3$rhs)=="alc_risk_tot_m"] <- "At-risk drinking"
levels(ests3$rhs)[levels(ests3$rhs)=="relation_m"] <- "Relationship problems"
levels(ests3$rhs)[levels(ests3$rhs)=="post_stress_m"] <- "Adverse life events"
levels(ests3$rhs)[levels(ests3$rhs)=="distress_m"] <- "Concurrent distress"
levels(ests3$rhs)[levels(ests3$rhs)=="post_dep_m"] <- "Postnatal depression"

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
ests3$category <- NULL
ests3$category[ests3$rhs %in% education] <- "Education"
ests3$category[ests3$rhs %in% income] <- "Income"
ests3$category[ests3$rhs %in% prenatal_life_events] <- "Prenatal life events"
ests3$category[ests3$rhs %in% adverse_life_events] <- "Adverse life events"
ests3$category[ests3$rhs %in% prenatal_smoking] <- "Prenatal smoking"
ests3$category[ests3$rhs %in% prenatal_distress] <- "Prenatal distress"
ests3$category[ests3$rhs %in% at_risk_drinking] <- "At-risk drinking"
ests3$category[ests3$rhs %in% relationship_problems] <- "Relationship problems"
ests3$category[ests3$rhs %in% postnatal_depression] <- "Postnatal depression"
ests3$category[ests3$rhs %in% concurrent_distress] <- "Concurrent distress"
ests3$category[ests3$rhs %in% alcohol_problems] <- "Alcohol problems"

# fix order of categories
ests3 <- ests3 %>%
  mutate(category=fct_relevel(category,"Income","Education",
                              "Prenatal life events","Adverse life events",
                              "Prenatal distress","Concurrent distress",
                              "Alcohol problems","At-risk drinking",
                              "Postnatal depression","Relationship problems",
                              "Prenatal smoking"),
         parent=fct_relevel(parent,"Mother","Father","Both"),
         lhs=fct_relevel(lhs,"s2","i2"))

# rename growth factors
levels(ests3$lhs)[levels(ests3$lhs)=="s2"] <- "Slope"
levels(ests3$lhs)[levels(ests3$lhs)=="i2"] <- "Intercept"

# format output for plotting
ests4 <- ests3 %>%
  select(growth_factor=lhs, predictor=rhs, beta=est.std, lci=ci.lower, 
         uci=ci.upper,fdr_pval=FDR_corrected_pvalue, parent,category) %>%
  mutate(beta_i = case_when(growth_factor=="Intercept"~ beta)) %>%
  mutate(beta_s = case_when(growth_factor=="Slope"~ beta)) %>%
  mutate(beta_i_lci = case_when(growth_factor=="Intercept"~ lci)) %>%
  mutate(beta_i_uci = case_when(growth_factor=="Intercept"~ uci)) %>%
  mutate(beta_s_lci = case_when(growth_factor=="Slope"~ lci)) %>%
  mutate(beta_s_uci = case_when(growth_factor=="Slope"~ uci)) %>%
  mutate(predictor = case_when(growth_factor=="Intercept"~ predictor)) %>%
  mutate(fdr_pval_i = case_when(growth_factor=="Intercept"~ fdr_pval)) %>%
  mutate(fdr_pval_s = case_when(growth_factor=="Slope"~ fdr_pval)) %>%
  mutate(parent = case_when(growth_factor=="Intercept"~ parent)) %>%
  mutate(category = case_when(growth_factor=="Intercept"~ category)) %>%
  select(predictor,beta_i,beta_s,beta_i_lci,beta_i_uci,
         beta_s_lci,beta_s_uci,fdr_pval_i,fdr_pval_s,parent,category) %>%
  summarise_all(list(~ discard(., is.na))) %>%
  mutate(fdr_pred = ifelse(fdr_pval_i<.05|fdr_pval_s<.05, paste(predictor), "")) %>%
  mutate(fdr_alpha = ifelse(fdr_pval_i<.05|fdr_pval_s<.05, "FDR-corrected p < .05", "FDR-corrected p > .05")) %>%
  mutate(fdr_alpha = as.factor(fdr_alpha)) %>%
  mutate(fdr_alpha=fct_relevel(fdr_alpha,"FDR-corrected p > .05","FDR-corrected p < .05"))

# save out
save(ests4, file="./output/obs_pred_tot.RData")

# create colour scale
my_colours <- c(RColorBrewer::brewer.pal(12, "Paired")[1:10],"#B15928")

# create intercept/slope spider plot
p4<-ggplot(ests4, mapping=aes(x=beta_s,y=beta_i,shape=parent,colour=category)) +
  geom_vline(aes(xintercept=0), colour = "black", linetype = 1) +
  geom_hline(aes(yintercept=0), colour = "black", linetype = 1) +
  geom_errorbar(aes(xmin = beta_s_lci, xmax = beta_s_uci, alpha=fdr_alpha), 
                size = 1, width = 0, show.legend = FALSE) +
  geom_errorbar(aes(ymin = beta_i_lci, ymax = beta_i_uci, alpha=fdr_alpha), 
                size = 1, width = 0, show.legend = FALSE) +
  geom_point(aes(alpha=fdr_alpha),fill="white",size=4) +
  geom_label_repel(aes(label=fdr_pred), show.legend = FALSE) +
  scale_colour_manual(values=my_colours) +
  scale_alpha_ordinal(range=c(0.25,1)) +
  scale_shape_manual(values=c(24,21,22))+
  guides(alpha=FALSE) +
  coord_cartesian(xlim = c(-0.17,0.17), ylim = c(-0.17,0.17)) +
  xlab(label=expression(beta[S])) + ylab(label=expression(beta[I])) +
  theme_minimal(base_size=15) + 
  theme(legend.title = element_blank(),
        panel.grid = element_blank(),
        axis.line = element_blank(),
        axis.title.y = element_text(angle = 360, vjust = 0.5),
        axis.ticks = element_blank())
p4

# save out
tiff("figures/all_preds_tot_unadj_obs.tiff", res = 800, compression = "lzw", unit = "in",
     height = 7, width = 10)

p4
p4
dev.off()
