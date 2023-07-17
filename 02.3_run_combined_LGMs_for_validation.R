# 02.3_run_combined_LGMs_for_validation.R

# this script runs the combined validation LGMs by sourcing:
# 02.2_specify_combined_LGMs_for_validation.R

library(tidyverse)
library(lavaan)
library(patchwork)
library(data.table)
library(ggh4x)

# read in processed data from 00_data_preparation.R
load("./data/processed_data_new.RData")

# specify validation LGMs
source("./scripts/02.2_specify_combined_LGMs_for_validation.R")


# run validation LGM with both intercept and slope for diff and tot
fit_model1_both <- sem(model1, 
                       missing = "ML", 
                       estimator = "MLR", 
                       data = alldata_new, 
                       cluster = "m_id",
                       fixed.x = F)

# provide output for the model
summary(fit_model1_both, fit.measures = TRUE, std = TRUE)


# run validation LGM with only intercept for diff and tot
fit_model2_both <- sem(model2, 
                       missing = "ML", 
                       estimator = "MLR", 
                       data = alldata_new, 
                       cluster = "m_id",
                       fixed.x = F)

# provide output for the model
summary(fit_model2_both, fit.measures = TRUE, std = TRUE)


# run validation LGM with only slope for diff and tot
fit_model3_both <- sem(model3, 
                       missing = "ML", 
                       estimator = "MLR", 
                       data = alldata_new, 
                       cluster = "m_id",
                       fixed.x = F)

# provide output for the model
summary(fit_model3_both, fit.measures = TRUE, std = TRUE)


# run validation LGM with no effects for diff or tot
fit_model4_both <- sem(model4, 
                       missing = "ML", 
                       estimator = "MLR", 
                       data = alldata_new, 
                       cluster = "m_id",
                       fixed.x = F)

# provide output for the model
summary(fit_model4_both, fit.measures = TRUE, std = TRUE)


# run validation LGM with only diff
fit_model5_diff <- sem(model5, 
                       missing = "ML", 
                       estimator = "MLR", 
                       data = alldata_new, 
                       cluster = "m_id",
                       fixed.x = F)

# provide output for the model
summary(fit_model5_diff, fit.measures = TRUE, std = TRUE)


# run validation LGM with only tot
fit_model6_tot <- sem(model6, 
                       missing = "ML", 
                       estimator = "MLR", 
                       data = alldata_new, 
                       cluster = "m_id",
                       fixed.x = F)

# provide output for the model
summary(fit_model6_tot, fit.measures = TRUE, std = TRUE)


# compare the different models
intercepts_only <- anova(fit_model1_both, fit_model2_both)
slopes_only <- anova(fit_model1_both, fit_model3_both)
no_effects <- anova(fit_model1_both, fit_model4_both)
diff_only <- anova(fit_model1_both, fit_model5_diff) #did not converge
tot_only <- anova(fit_model1_both, fit_model6_tot)

intercepts_only 
slopes_only 
no_effects 
tot_only

# take estimates from diff model
tmp1 <- standardizedSolution(fit_model1_both, ci = TRUE, level = 0.95) %>%
  filter(rhs=="i1"|rhs=="s1",
         op =="~") %>%
  mutate(model = "diff")

# adjust pvalues
ests <- tmp1 %>%
  mutate(FDR_corrected_pvalue = p.adjust(pvalue, method="fdr"),
         FDR = ifelse(FDR_corrected_pvalue <.05, "FDR p <.05", "FDR p >.05"),
         FDR = as.factor(FDR),
         lhs = as.factor(lhs))

# fix order of slope and intercept
ests <- ests %>%
  mutate(rhs=fct_relevel(rhs,"s1","i1"))

# rename growth factors and symptom vars
levels(ests$rhs)[levels(ests$rhs)=="s1"] <- "Slope"
levels(ests$rhs)[levels(ests$rhs)=="i1"] <- "Intercept"
levels(ests$lhs)[levels(ests$lhs)=="anx_8yr"] <- "ANX"
levels(ests$lhs)[levels(ests$lhs)=="dep_8yr"] <- "DEP"
levels(ests$lhs)[levels(ests$lhs)=="hyp_8yr"] <- "HYP"
levels(ests$lhs)[levels(ests$lhs)=="inat_8yr"] <- "INAT"
levels(ests$lhs)[levels(ests$lhs)=="odd_8yr"] <- "ODD"
levels(ests$lhs)[levels(ests$lhs)=="cd_8yr"] <- "CD"

# take total problem estimates
tmp2 <- standardizedSolution(fit_model1_both, ci = TRUE, level = 0.95) %>%
  filter(rhs=="i2"|rhs=="s2",
         op == "~") %>%
  mutate(model = "tot")

# adjust pvalues
ests2 <- tmp2 %>%
  mutate(FDR_corrected_pvalue = p.adjust(pvalue, method="fdr"),
         FDR = ifelse(FDR_corrected_pvalue <.05, "FDR p <.05", "FDR p >.05"),
         FDR = as.factor(FDR),
         lhs = as.factor(lhs))

# fix order of slope and intercept
ests2 <- ests2 %>%
  mutate(rhs=fct_relevel(rhs,"s2","i2"))

# rename growth factors and symptom vars
levels(ests2$rhs)[levels(ests2$rhs)=="s2"] <- "Slope"
levels(ests2$rhs)[levels(ests2$rhs)=="i2"] <- "Intercept"
levels(ests2$lhs)[levels(ests2$lhs)=="anx_8yr"] <- "ANX"
levels(ests2$lhs)[levels(ests2$lhs)=="dep_8yr"] <- "DEP"
levels(ests2$lhs)[levels(ests2$lhs)=="hyp_8yr"] <- "HYP"
levels(ests2$lhs)[levels(ests2$lhs)=="inat_8yr"] <- "INAT"
levels(ests2$lhs)[levels(ests2$lhs)=="odd_8yr"] <- "ODD"
levels(ests2$lhs)[levels(ests2$lhs)=="cd_8yr"] <- "CD"

# merge
ests_both <- full_join(ests, ests2)

# rename diff and tot
levels(ests_both$model)[levels(ests_both$model)=="diff"] <- "Differentiation"
levels(ests_both$model)[levels(ests_both$model)=="tot"] <- "Total score"

# fix order of diff and tot
ests_both <- ests_both %>%
  mutate(model=fct_relevel(model,"Total score","Differentiation"))

# create dot plot
p1<- ests_both %>%
  mutate(lhs = fct_relevel(lhs,"ANX","DEP","HYP","INAT","ODD","CD")) %>%
  ggplot(ests_both, mapping=aes(x=est.std, y=reorder(rhs, desc(rhs)), xmin = ci.lower, xmax = ci.upper, colour = model)) +
  geom_vline(aes(xintercept=0), colour = "grey50", linetype = 2) + theme_light(base_size=18) +
  geom_errorbar(size = 1.2, alpha=0.7, width = 0, position=position_dodge(0.5)) +
  geom_point(size=2.7, fill="white", alpha=1, position=position_dodge(0.5)) +
  facet_grid(rows = vars(lhs), scales = "fixed", space = "fixed", as.table = TRUE) +
  scale_colour_manual(values=c("#0072B2","#D55E00")) +
  guides(colour=guide_legend(order=0, reverse=FALSE)) + 
  theme(axis.text = element_text(size=15, colour = "black"),
        axis.ticks.x = element_blank(),
        axis.title.y = element_blank(),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill="transparent"),
        panel.spacing = unit(0.2, "lines"),
        panel.grid = element_blank(),
        axis.line = element_line(colour="grey80"),
        legend.position = "top",
        legend.title = element_blank(),
        legend.text = element_text(size = 18),
        strip.text = element_text(size=15, colour = 'black'),
        strip.background = element_rect(colour="gray", fill="white")) + 
  xlab(label=expression(Standardised~"beta"~(beta))) + 
  scale_shape_manual(values=c(24,21)) +
  coord_cartesian(xlim = c(-0.5,0.5))

# format results for bar plot diff
barplot_diff <- ests %>%
  select(growth_factor=rhs,predictor=lhs,est=est.std,ci.lower,ci.upper) %>%
  mutate(est_slope = case_when(growth_factor == "Slope" ~ est)) %>%
  mutate(est_int = case_when(growth_factor == "Intercept" ~ est)) %>%
  mutate(est_slope_lci = case_when(growth_factor == "Slope" ~ ci.lower)) %>%
  mutate(est_slope_uci = case_when(growth_factor == "Slope" ~ ci.upper)) %>%
  mutate(est_int_lci = case_when(growth_factor == "Intercept" ~ ci.lower)) %>%
  mutate(est_int_uci = case_when(growth_factor == "Intercept" ~ ci.upper)) %>%
  mutate(predictor = case_when(growth_factor == "Intercept" ~ predictor)) %>%
  select(-growth_factor,-est,-ci.lower,-ci.upper) %>%
  summarise_all(list(~ discard(., is.na))) %>%
  mutate(model = "Differentiation") %>%
  mutate(R2 = est_slope^2 + est_int^2 + (est_slope*0.636*est_int))#slope~~int=.636

# format results for bar plot tot
barplot_tot <- ests2 %>%
  select(growth_factor=rhs,predictor=lhs,est=est.std,ci.lower,ci.upper) %>%
  mutate(est_slope = case_when(growth_factor == "Slope" ~ est)) %>%
  mutate(est_int = case_when(growth_factor == "Intercept" ~ est)) %>%
  mutate(est_slope_lci = case_when(growth_factor == "Slope" ~ ci.lower)) %>%
  mutate(est_slope_uci = case_when(growth_factor == "Slope" ~ ci.upper)) %>%
  mutate(est_int_lci = case_when(growth_factor == "Intercept" ~ ci.lower)) %>%
  mutate(est_int_uci = case_when(growth_factor == "Intercept" ~ ci.upper)) %>%
  mutate(predictor = case_when(growth_factor == "Intercept" ~ predictor)) %>%
  select(-growth_factor,-est,-ci.lower,-ci.upper) %>%
  summarise_all(list(~ discard(., is.na))) %>%
  mutate(model = "Total") %>%
  mutate(R2 = est_slope^2 + est_int^2 + (est_slope*0.546*est_int))#slope~~int=.546

# merge
bar_both <- barplot_diff %>%
  full_join(barplot_tot)

# create bar plot
p2<- bar_both %>%
  mutate(predictor = fct_relevel(predictor,"ANX","DEP","HYP","INAT","ODD","CD")) %>%
  ggplot(bar_both, mapping=aes(x= predictor, y = R2, colour = model, fill = model)) +
  ylab(expression(R^2)) + theme_light(base_size=18) +
  geom_col(width=4.8,position="stack") +
  facet_grid(rows = vars(predictor), scales = "fixed", space = "fixed", as.table = TRUE) +
  scale_colour_manual(values=c("#D55E00","#0072B2"),guide="none") +
  scale_fill_manual(values=c("#D55E00","#0072B2"),guide="none") +
  theme(axis.text = element_text(size=15, colour = "black"),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_line(colour="grey80"),
        panel.spacing = unit(0.2, "lines"),
        panel.grid = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank(),
        plot.background = element_blank(),
        strip.background = element_rect(colour="white", fill="white"),
        legend.title = element_blank(),
        legend.text = element_text(size = 18)) +
  scale_y_continuous(limits=c(0,0.4)) +
  coord_flip()

# patch together dot and bar plots
patch <- p1 + p2

p3 <- patch +
  plot_layout(guides = "collect",
              widths = c(2, 0.8)) &
  theme(legend.position = "top")

p3

# save out
tiff("figures/validation_symptoms.tiff", res = 800, compression = "lzw", unit = "in",
     height = 6, width = 9)

p3

dev.off()
