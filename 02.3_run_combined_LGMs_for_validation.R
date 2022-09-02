# 02.3_run_combined_LGMs_for_validation.R

# this script runs the combined validation LGMs by sourcing:
# 02.2_specify_combined_LGMs_for_validation.R

library(tidyverse)
library(lavaan)
library(patchwork)

# read in processed data from 00_data_preparation.R

load("./data/processed_data.RData")

# specify validation LGMs

source("./scripts/02.2_specify_combined_LGMs_for_validation.R")


# run validation LGM with both intercept and slope for diff and tot

fit_model1_both <- sem(model1, 
                       missing = "ML", 
                       estimator = "MLR", 
                       data = alldata, 
                       cluster = "m_id",
                       fixed.x = F)

# provide output for the model

summary(fit_model1_both, fit.measures = TRUE, std = TRUE)


# run validation LGM with only intercept for diff and tot

fit_model2_both <- sem(model2, 
                       missing = "ML", 
                       estimator = "MLR", 
                       data = alldata, 
                       cluster = "m_id",
                       fixed.x = F)

# provide output for the model

summary(fit_model2_both, fit.measures = TRUE, std = TRUE)


# run validation LGM with only slope for diff and tot

fit_model3_both <- sem(model3, 
                       missing = "ML", 
                       estimator = "MLR", 
                       data = alldata, 
                       cluster = "m_id",
                       fixed.x = F)

# provide output for the model

summary(fit_model3_both, fit.measures = TRUE, std = TRUE)


# run validation LGM with no effects for diff or tot

fit_model4_both <- sem(model4, 
                       missing = "ML", 
                       estimator = "MLR", 
                       data = alldata, 
                       cluster = "m_id",
                       fixed.x = F)

# provide output for the model

summary(fit_model4_both, fit.measures = TRUE, std = TRUE)


# run validation LGM with only diff

fit_model5_diff <- sem(model5, 
                       missing = "ML", 
                       estimator = "MLR", 
                       data = alldata, 
                       cluster = "m_id",
                       fixed.x = F)

# provide output for the model

summary(fit_model5_diff, fit.measures = TRUE, std = TRUE)


# run validation LGM with only tot

fit_model6_tot <- sem(model6, 
                       missing = "ML", 
                       estimator = "MLR", 
                       data = alldata, 
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
#diff_only 
tot_only


# take estimates from diff model
tmp1 <- standardizedSolution(fit_model1_both, ci = TRUE, level = 0.95) %>%
  filter(rhs=="i1"|rhs=="s1",
         op == "~") %>%
  mutate(model = "diff")

# adjust pvalues
ests <- tmp1 %>%
  mutate(FDR_corrected_pvalue = p.adjust(pvalue, method="fdr")) %>%
  mutate(FDR = ifelse(FDR_corrected_pvalue <.05, "FDR p <.05", "FDR p >.05")) %>%
  mutate(FDR = as.factor(FDR))

# rename
ests <- ests %>%
  mutate(lhs = as.factor(lhs))

# fix order of slope and intercept
ests <- ests %>%
  mutate(rhs=fct_relevel(rhs,"s1","i1"))

# rename
levels(ests$rhs)[levels(ests$rhs)=="s1"] <- "Slope"
levels(ests$rhs)[levels(ests$rhs)=="i1"] <- "Intercept"
levels(ests$lhs)[levels(ests$lhs)=="anx_8yr"] <- "anx"
levels(ests$lhs)[levels(ests$lhs)=="dep_8yr"] <- "dep"
levels(ests$lhs)[levels(ests$lhs)=="hyp_8yr"] <- "hyp"
levels(ests$lhs)[levels(ests$lhs)=="inat_8yr"] <- "inat"
levels(ests$lhs)[levels(ests$lhs)=="odd_8yr"] <- "odd"
levels(ests$lhs)[levels(ests$lhs)=="cd_8yr"] <- "cd"

tmp2 <- standardizedSolution(fit_model1_both, ci = TRUE, level = 0.95) %>%
  filter(rhs=="i2"|rhs=="s2",
         op == "~") %>%
  mutate(model = "tot")

# adjust pvalues
ests2 <- tmp2 %>%
  mutate(FDR_corrected_pvalue = p.adjust(pvalue, method="fdr")) %>%
  mutate(FDR = ifelse(FDR_corrected_pvalue <.05, "FDR p <.05", "FDR p >.05")) %>%
  mutate(FDR = as.factor(FDR))

# rename
ests2 <- ests2 %>%
  mutate(lhs = as.factor(lhs))

# fix order of slope and intercept
ests2 <- ests2 %>%
  mutate(rhs=fct_relevel(rhs,"s2","i2"))

# rename
levels(ests2$rhs)[levels(ests2$rhs)=="s2"] <- "Slope"
levels(ests2$rhs)[levels(ests2$rhs)=="i2"] <- "Intercept"
levels(ests2$lhs)[levels(ests2$lhs)=="anx_8yr"] <- "anx"
levels(ests2$lhs)[levels(ests2$lhs)=="dep_8yr"] <- "dep"
levels(ests2$lhs)[levels(ests2$lhs)=="hyp_8yr"] <- "hyp"
levels(ests2$lhs)[levels(ests2$lhs)=="inat_8yr"] <- "inat"
levels(ests2$lhs)[levels(ests2$lhs)=="odd_8yr"] <- "odd"
levels(ests2$lhs)[levels(ests2$lhs)=="cd_8yr"] <- "cd"

ests_both <- full_join(ests, ests2)

p1<- ests_both %>%
  mutate(lhs = fct_relevel(lhs,"anx","dep","hyp","inat","odd","cd")) %>%
  ggplot(ests_both, mapping=aes(x=est.std, y=reorder(model, desc(model)), xmin = ci.lower, xmax = ci.upper, colour = rhs, shape=FDR)) +
  geom_vline(aes(xintercept=0), colour = "grey50", linetype = 2) + theme_light(base_size=14) +
  geom_errorbar(size = 1, alpha=0.7, width = 0, position=position_dodge(0.5)) +
  geom_point(size=3, fill="white", alpha=1, position=position_dodge(0.5)) +
  facet_grid(rows = vars(lhs), scales = "fixed", space = "fixed", as.table = TRUE) +
  scale_colour_manual(values=c("#0072B2", "#D55E00")) +
  guides(colour=guide_legend(order=1, reverse=TRUE),shape=guide_legend(order=0)) + 
  theme(axis.text = element_text(size=14, colour = "black"),
        axis.ticks.x = element_blank(),
        axis.title.y = element_blank(),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill="transparent"),
        panel.spacing = unit(0.2, "lines"),
        panel.grid = element_blank(),
        axis.line = element_line(colour="grey80"),
        legend.position = "top",
        legend.title = element_blank(),
        legend.text = element_text(size = 13)) + 
  xlab(label=expression(beta)) + 
  scale_shape_manual(values=c(24,21)) +
  coord_cartesian(xlim = c(-0.5,0.5)) +
  theme(strip.text = element_text(size=14, colour = 'black'),
        strip.background = element_rect(colour="gray", fill="white"))
p1

#dev.off()