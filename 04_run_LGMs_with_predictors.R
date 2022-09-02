# 04_run_LGMs_with_predictors.R

# this script runs the latent growth models (LGMs) including 'environmental' 
# predictors by sourcing: 04.1_specify_LGMs_with_predictors.R

  library(tidyverse)
  library(lavaan)
  library(patchwork)
  library(ggrepel)
  library(viridis)

# read in processed data from 00_data_preparation.R
  load("./data/processed_data.RData")

# specify LGMs
  source("./scripts/04.1_specify_LGMs_with_predictors.R")

# diff: run  LGMs and compare ####

# diff: run LGM with predictors on intercept and slope, with continuous drinking
  fit_modelD <- sem(modelD, 
                    missing = "ML", 
                    estimator = "MLR", 
                    data = alldata, 
                    cluster = "m_id",
                    fixed.x = F)

# provide output for the model
  summary(fit_modelD, fit.measures = TRUE, std = TRUE)
  
# diff: run modified LGM with predictors on intercept and slope, at-risk drinking
  fit_modelDm <- sem(modelDm, 
                     missing = "ML", 
                     estimator = "MLR", 
                     data = alldata, 
                     cluster = "m_id",
                     fixed.x = F)
  
# provide output for the model
  summary(fit_modelDm, fit.measures = TRUE, std = TRUE)
  
  save(fit_modelDm, file= "./output/fit_modelDm.RData")
  load("./output/fit_modelDm.RData")

# diff: run  LGM with predictors on intercept only, with at-risk drinking
  fit_modelDi <- sem(modelDi, 
                     missing = "ML", 
                     estimator = "MLR", 
                     data = alldata, 
                     cluster = "m_id",
                     fixed.x = F)

# provide output for the model
  summary(fit_modelDi, fit.measures = TRUE, std = TRUE)
  
  save(fit_modelDi, file= "./output/fit_modelDi.RData")
  load("./output/fit_modelDi.RData")

# diff: run  LGM with predictors on slope only, with at-risk drinking
  fit_modelDs <- sem(modelDs, 
                     missing = "ML", 
                     estimator = "MLR", 
                     data = alldata, 
                     cluster = "m_id",
                     fixed.x = F)

# provide output for the model
  summary(fit_modelDs, fit.measures = TRUE, std = TRUE)

  save(fit_modelDs, file= "./output/fit_modelDs.RData")
  load("./output/fit_modelDs.RData")

# compare the models
  anova(fit_modelDm, fit_modelDi)
  anova(fit_modelDm, fit_modelDs)
  

# tot: run  LGMs and compare ####
  
# tot: run  LGM with predictors on intercept and slope, with continuous drinking
  fit_modelT <- sem(modelT, 
                    missing = "ML", 
                    estimator = "MLR", 
                    data = alldata, 
                    cluster = "m_id",
                    fixed.x = F)
  
# provide output for the model
  summary(fit_modelT, fit.measures = TRUE, std = TRUE)

# tot: run modified LGM with predictors on intercept and slope, at-risk drinking
  fit_modelTm <- sem(modelTm, 
                     missing = "ML", 
                     estimator = "MLR", 
                     data = alldata, 
                     cluster = "m_id",
                     fixed.x = F)
  
# provide output for the model
  summary(fit_modelTm, fit.measures = TRUE, std = TRUE)  
  
  save(fit_modelTm, file= "./output/fit_modelTm.RData")
  load("./output/fit_modelTm.RData")
  
# tot: run  LGM with predictors on intercept only, with at-risk drinking
  fit_modelTi <- sem(modelTi, 
                     missing = "ML", 
                     estimator = "MLR", 
                     data = alldata, 
                     cluster = "m_id",
                     fixed.x = F)
  
# provide output for the model
  summary(fit_modelTi, fit.measures = TRUE, std = TRUE)
  
  
  save(fit_modelTi, file= "./output/fit_modelTi.RData")
  load("./output/fit_modelTi.RData")
  
# tot: run  LGM with predictors on slope only, with at-risk drinking
  fit_modelTs <- sem(modelTs, 
                     missing = "ML", 
                     estimator = "MLR", 
                     data = alldata, 
                     cluster = "m_id",
                     fixed.x = F)
  
# provide output for the model
  summary(fit_modelTs, fit.measures = TRUE, std = TRUE)
  
  save(fit_modelTs, file= "./output/fit_modelTs.RData")
  load("./output/fit_modelTs.RData")
  
# compare the models
  anova(fit_modelTm, fit_modelTi)
  anova(fit_modelTm, fit_modelTs)

# keep diff and tot models with both intercept and slope:
# fit_modelD and fit_modelT

  
# take estimates from diff model
  tmp1 <- standardizedSolution(fit_modelDm, ci = TRUE, level = 0.95) %>%
    filter(lhs=="i"|lhs=="s",
           op == "~") %>%
    mutate(model = "diff_with_predictors")

# remove sex from predictors
  ests1 <- tmp1[-c(1,2), ]
  
# adjust pvalues
  ests1$FDR_corrected_pvalue <- p.adjust(ests1$pvalue, method="fdr")
  
# rename
  ests1 <- ests1 %>%
    mutate(rhs = as.factor(rhs))
  
# create variable with parent
  ests1 <- ests1 %>%
    mutate(parent=case_when(str_detect(as.character(rhs),"_m") ~"Mother",
                            str_detect(as.character(rhs),"_f") ~"Father",
                            str_detect(as.character(rhs),"education") ~"Both",
                            str_detect(as.character(rhs),"income") ~"Both"))
  
  levels(ests1$rhs)[levels(ests1$rhs)=="education"] <- "Education"
  levels(ests1$rhs)[levels(ests1$rhs)=="income"] <- "Income"
  levels(ests1$rhs)[levels(ests1$rhs)=="pre_stress_m"] <- "Prenatal adverse life events"
  levels(ests1$rhs)[levels(ests1$rhs)=="stress_f"] <- "Prenatal adverse life events"
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
  ests1$category <- NULL
  ests1$category[ests1$rhs %in% education] <- "Education"
  ests1$category[ests1$rhs %in% income] <- "Income"
  ests1$category[ests1$rhs %in% prenatal_adverse_life_events] <- "Prenatal adverse life events"
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
                                "Prenatal adverse life events","Adverse life events",
                                "Prenatal distress","Concurrent distress",
                                "Alcohol problems","At-risk drinking",
                                "Postnatal depression","Relationship problems",
                                "Prenatal smoking"))

# fix order of rater/parent
  ests1 <- ests1 %>%
    mutate(parent=fct_relevel(parent,"Mother","Father","Both"))
  
# fix order of slope and intercept
  ests1 <- ests1 %>%
    mutate(lhs=fct_relevel(lhs,"s","i"))
  
# rename
  levels(ests1$lhs)[levels(ests1$lhs)=="s"] <- "Slope"
  levels(ests1$lhs)[levels(ests1$lhs)=="i"] <- "Intercept"
  
  
  p1<-ggplot(ests1, aes(x=est.std, y=reorder(rhs, desc(rhs)), xmin = ci.lower, xmax = ci.upper, colour = lhs, shape=lhs)) +
    geom_errorbar(size = 1, alpha=0.6, width = 0.5, position=position_dodge(0.5)) +
    geom_point(size=2, position=position_dodge(0.5)) +
    geom_vline(aes(xintercept=0), colour = "grey50", linetype = 2) + theme_light(base_size=10) +
    geom_point(data=ests1[ests1$FDR_corrected_pvalue <.05, ], 
               mapping=aes(x=-0.119, y=reorder(rhs, desc(rhs))),
               position=position_dodge2(0.6), colour="grey30", shape=8, size=1) +
    scale_colour_manual(values= c("#0072B2", "#D55E00")) +
    theme(axis.text.x = element_text(),
          axis.text.y = element_text(),
          axis.ticks.x = element_blank(),
          axis.title.y = element_blank(),
          text=element_text(size = 10),
          panel.background = element_rect(fill = "transparent"),
          plot.background = element_rect(fill="transparent"),
          panel.spacing = unit(0.2, "lines"),
          legend.title = element_blank(),
          panel.grid = element_blank(),
          axis.line = element_line(colour="grey80"),
          legend.position="top") + 
    xlab("Differentiation (-emotional +behavioural)") + 
    coord_cartesian(xlim = c(-0.12,0.12)) +
    facet_grid(category ~ ., scales = "free", space = "free") +
    theme(strip.text = element_text(size=8, colour = 'black'),
          strip.background = element_rect(colour="gray", fill="white"))


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
    mutate(fdr_pval = case_when(growth_factor=="Intercept"~ fdr_pval)) %>%
    mutate(parent = case_when(growth_factor=="Intercept"~ parent)) %>%
    mutate(category = case_when(growth_factor=="Intercept"~ category)) %>%
    select(predictor,beta_i,beta_s,beta_i_lci,beta_i_uci,
           beta_s_lci,beta_s_uci,fdr_pval,parent,category) %>%
    summarise_all(list(~ discard(., is.na))) %>%
    mutate(fdr_pred = ifelse(fdr_pval<.05, paste(predictor), "")) %>%
    mutate(fdr_alpha = ifelse(fdr_pval<.05, "FDR-corrected p < .05", "FDR-corrected p > .05")) %>%
    mutate(fdr_alpha = as.factor(fdr_alpha)) %>%
    mutate(fdr_alpha=fct_relevel(fdr_alpha,"FDR-corrected p > .05","FDR-corrected p < .05"))
  
  my_colours <- c(RColorBrewer::brewer.pal(12, "Paired")[1:10],"#B15928")
  
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
  
  tiff("figures/all_preds_diff_unadj.tiff", res = 800, compression = "lzw", unit = "in",
       height = 7, width = 10)
  
  p2
  p2
  dev.off()

  
# take estimates from tot model
  tmp3 <- standardizedSolution(fit_modelTm, ci = TRUE, level = 0.95) %>%
    filter(lhs=="i"|lhs=="s",
           op == "~") %>%
    mutate(model = "tot_with_predictors")
  
# remove sex from predictors
  ests3 <- tmp3[-c(1,2), ]
  
# adjust pvalues
  ests3$FDR_corrected_pvalue <- p.adjust(ests3$pvalue, method="fdr")
  
# rename
  ests3 <- ests3 %>%
    mutate(rhs = as.factor(rhs))
  
# create variable with parent
  ests3 <- ests3 %>%
    mutate(parent=case_when(str_detect(as.character(rhs),"_m") ~"Mother",
                            str_detect(as.character(rhs),"_f") ~"Father",
                            str_detect(as.character(rhs),"education") ~"Both",
                            str_detect(as.character(rhs),"income") ~"Both"))
  
  levels(ests3$rhs)[levels(ests3$rhs)=="education"] <- "Education"
  levels(ests3$rhs)[levels(ests3$rhs)=="income"] <- "Income"
  levels(ests3$rhs)[levels(ests3$rhs)=="pre_stress_m"] <- "Prenatal adverse life events"
  levels(ests3$rhs)[levels(ests3$rhs)=="stress_f"] <- "Prenatal adverse life events"
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
  ests3$category <- NULL
  ests3$category[ests3$rhs %in% education] <- "Education"
  ests3$category[ests3$rhs %in% income] <- "Income"
  ests3$category[ests3$rhs %in% prenatal_adverse_life_events] <- "Prenatal adverse life events"
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
                                "Prenatal adverse life events","Adverse life events",
                                "Prenatal distress","Concurrent distress",
                                "Alcohol problems","At-risk drinking",
                                "Postnatal depression","Relationship problems",
                                "Prenatal smoking"))
  
# fix order of rater/parent
  ests3 <- ests3 %>%
    mutate(parent=fct_relevel(parent,"Mother","Father","Both"))
  
# fix order of slope and intercept
  ests3 <- ests3 %>%
    mutate(lhs=fct_relevel(lhs,"s","i"))
  
# rename
  levels(ests3$lhs)[levels(ests3$lhs)=="s"] <- "Slope"
  levels(ests3$lhs)[levels(ests3$lhs)=="i"] <- "Intercept"
  
  
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
    mutate(fdr_pval = case_when(growth_factor=="Intercept"~ fdr_pval)) %>%
    mutate(parent = case_when(growth_factor=="Intercept"~ parent)) %>%
    mutate(category = case_when(growth_factor=="Intercept"~ category)) %>%
    select(predictor,beta_i,beta_s,beta_i_lci,beta_i_uci,
           beta_s_lci,beta_s_uci,fdr_pval,parent,category) %>%
    summarise_all(list(~ discard(., is.na))) %>%
    mutate(fdr_pred = ifelse(fdr_pval<.05, paste(predictor), "")) %>%
    mutate(fdr_alpha = ifelse(fdr_pval<.05, "FDR-corrected p < .05", "FDR-corrected p > .05")) %>%
    mutate(fdr_alpha = as.factor(fdr_alpha)) %>%
    mutate(fdr_alpha=fct_relevel(fdr_alpha,"FDR-corrected p > .05","FDR-corrected p < .05"))
  
  my_colours <- c(RColorBrewer::brewer.pal(12, "Paired")[1:10],"#B15928")
  
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
  
  tiff("figures/all_preds_tot_unadj.tiff", res = 800, compression = "lzw", unit = "in",
       height = 7, width = 10)
  
  p4
  p4
  dev.off()

  