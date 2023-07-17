# 05.2_ippw_validation_analyses.R

# this script runs inverse probability of participation weighted and 
# unweighted validation models and plots the results to enable comparisons

library(ggplot2)
library(tidyverse)
library(patchwork)
library(lavaan)
library(lavaan.survey)
library(ggrepel)
library(viridis)

# load data
load(file="N:/durable/projects/childhood_differentiation/data/moba_ipw.RData")

# run validation LGM with diff and tot on 8-year symptoms
source("N:/durable/projects/childhood_differentiation/scripts/02.2_specify_combined_LGMs_for_validation.R")

fit_model1_both <- sem(model1, 
                       estimator = "MLR", 
                       data = moba_ipw, 
                       cluster = "m_id",
                       fixed.x = F)

# apply weights
svy<- survey::svydesign(ids=~m_id, weights = ~ippw, data=moba_ipw)

# obtain unweighted and weighted estimates from the same model
fit_model1_unweighted <- fit_model1_both
fit_model1_weighted <- lavaan.survey(fit_model1_both, svy, estimator="MLM")

# take estimates from weighted diff model
tmp1 <- standardizedSolution(fit_model1_weighted, ci = TRUE, level = 0.95) %>%
  filter(rhs=="i1"|rhs=="s1",
         op =="~") %>%
  mutate(model = "diff")

# create vars for plotting
ests1 <- tmp1 %>%
  mutate(FDR_corrected_pvalue = p.adjust(pvalue, method="fdr"),
         FDR = ifelse(FDR_corrected_pvalue <.05, "FDR p <.05", "FDR p >.05"),
         FDR = as.factor(FDR),
         lhs = as.factor(lhs),
         rhs=fct_relevel(rhs,"s1","i1"))

# rename growth factors and symptom vars
levels(ests1$rhs)[levels(ests1$rhs)=="s1"] <- "Slope"
levels(ests1$rhs)[levels(ests1$rhs)=="i1"] <- "Intercept"
levels(ests1$lhs)[levels(ests1$lhs)=="anx_8yr"] <- "ANX"
levels(ests1$lhs)[levels(ests1$lhs)=="dep_8yr"] <- "DEP"
levels(ests1$lhs)[levels(ests1$lhs)=="hyp_8yr"] <- "HYP"
levels(ests1$lhs)[levels(ests1$lhs)=="inat_8yr"] <- "INAT"
levels(ests1$lhs)[levels(ests1$lhs)=="odd_8yr"] <- "ODD"
levels(ests1$lhs)[levels(ests1$lhs)=="cd_8yr"] <- "CD"

# take estimates from weighted tot model
tmp2 <- standardizedSolution(fit_model1_weighted, ci = TRUE, level = 0.95) %>%
  filter(rhs=="i2"|rhs=="s2",
         op == "~") %>%
  mutate(model = "tot")

# create vars for plotting
ests2 <- tmp2 %>%
  mutate(FDR_corrected_pvalue = p.adjust(pvalue, method="fdr"),
         FDR = ifelse(FDR_corrected_pvalue <.05, "FDR p <.05", "FDR p >.05"),
         FDR = as.factor(FDR),
         lhs = as.factor(lhs),
         rhs=fct_relevel(rhs,"s2","i2"))

# rename growth factors and symptom vars
levels(ests2$rhs)[levels(ests2$rhs)=="s2"] <- "Slope"
levels(ests2$rhs)[levels(ests2$rhs)=="i2"] <- "Intercept"
levels(ests2$lhs)[levels(ests2$lhs)=="anx_8yr"] <- "ANX"
levels(ests2$lhs)[levels(ests2$lhs)=="dep_8yr"] <- "DEP"
levels(ests2$lhs)[levels(ests2$lhs)=="hyp_8yr"] <- "HYP"
levels(ests2$lhs)[levels(ests2$lhs)=="inat_8yr"] <- "INAT"
levels(ests2$lhs)[levels(ests2$lhs)=="odd_8yr"] <- "ODD"
levels(ests2$lhs)[levels(ests2$lhs)=="cd_8yr"] <- "CD"

# merge diff / tot
ests_both <- full_join(ests1, ests2)

# rename diff/tot
levels(ests_both$model)[levels(ests_both$model)=="diff"] <- "Differentiation"
levels(ests_both$model)[levels(ests_both$model)=="tot"] <- "Total score"

# fix order of diff/tot
ests_both <- ests_both %>%
  mutate(model=fct_relevel(model,"Total score","Differentiation"))

# create dot plot
p1<- ests_both %>%
  mutate(lhs = fct_relevel(lhs,"ANX","DEP","HYP","INAT","ODD","CD")) %>%
  ggplot(ests_both, mapping=aes(x=est.std, y=reorder(rhs, desc(rhs)), xmin = ci.lower, xmax = ci.upper, colour = model)) +
  geom_vline(aes(xintercept=0), colour = "grey50", linetype = 2) + theme_light(base_size=17) +
  geom_errorbar(linewidth = 1.2, alpha=0.7, width = 0, position=position_dodge(0.7)) +
  geom_point(size=2.7, fill="white", alpha=1, position=position_dodge(0.7)) +
  facet_grid(rows = vars(lhs), scales = "fixed", space = "fixed", as.table = TRUE) +
  scale_colour_manual(values=c("#0072B2","#D55E00")) +
  guides(colour=guide_legend(order=0, reverse=FALSE)) + 
  theme(axis.text = element_text(size=14, colour = "black"),
        axis.ticks.x = element_blank(),
        axis.title.y = element_blank(),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill="transparent"),
        panel.spacing = unit(0.2, "lines"),
        panel.grid = element_blank(),
        axis.line = element_line(colour="grey80"),
        legend.position = "blank",
        legend.title = element_blank(),
        strip.text = element_text(size=14, colour = 'black'),
        strip.background = element_rect(colour="gray", fill="white")) + 
  xlab(label=expression(Standardised~"beta"~(beta))) + 
  scale_shape_manual(values=c(24,21)) +
  coord_cartesian(xlim = c(-0.5,0.5))

# format diff and tot ests for barplot
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
  mutate(R2 = est_slope^2 + est_int^2 + (est_slope*0.624*est_int))

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
  mutate(R2 = est_slope^2 + est_int^2 + (est_slope*0.520*est_int))

# join together
bar_both <- barplot_diff %>%
  full_join(barplot_tot)

# create bar plot diff + tot
p2<- bar_both %>%
  mutate(predictor = fct_relevel(predictor,"ANX","DEP","HYP","INAT","ODD","CD")) %>%
  ggplot(bar_both, mapping=aes(x= predictor, y = R2, colour = model, fill = model)) +
  ylab(expression(R^2)) + theme_light(base_size=17) +
  geom_col(width=4.8,position="stack") +
  facet_grid(rows = vars(predictor), scales = "fixed", space = "fixed", as.table = TRUE) +
  scale_colour_manual(values=c("#D55E00","#0072B2"),guide="none") +
  scale_fill_manual(values=c("#D55E00","#0072B2"),guide="none") +
  theme(axis.text = element_text(size=14, colour = "black"),
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
        legend.position = "blank") +
  scale_y_continuous(limits=c(0,0.4)) +
  coord_flip()

# patch dot and bar plots together
patch <- p1 + p2

p3 <- patch +
  plot_layout(widths = c(2, 0.8))

# take estimates from unweighted diff model
tmp3 <- standardizedSolution(fit_model1_unweighted, ci = TRUE, level = 0.95) %>%
  filter(rhs=="i1"|rhs=="s1",
         op =="~") %>%
  mutate(model = "diff")

# create vars for plotting
ests3 <- tmp3 %>%
  mutate(FDR_corrected_pvalue = p.adjust(pvalue, method="fdr"),
         FDR = ifelse(FDR_corrected_pvalue <.05, "FDR p <.05", "FDR p >.05"),
         FDR = as.factor(FDR),
         lhs = as.factor(lhs),
         rhs=fct_relevel(rhs,"s1","i1"))

# rename growth factors and symptom vars
levels(ests3$rhs)[levels(ests3$rhs)=="s1"] <- "Slope"
levels(ests3$rhs)[levels(ests3$rhs)=="i1"] <- "Intercept"
levels(ests3$lhs)[levels(ests3$lhs)=="anx_8yr"] <- "ANX"
levels(ests3$lhs)[levels(ests3$lhs)=="dep_8yr"] <- "DEP"
levels(ests3$lhs)[levels(ests3$lhs)=="hyp_8yr"] <- "HYP"
levels(ests3$lhs)[levels(ests3$lhs)=="inat_8yr"] <- "INAT"
levels(ests3$lhs)[levels(ests3$lhs)=="odd_8yr"] <- "ODD"
levels(ests3$lhs)[levels(ests3$lhs)=="cd_8yr"] <- "CD"

# take estimates from unweighted tot model
tmp4 <- standardizedSolution(fit_model1_unweighted, ci = TRUE, level = 0.95) %>%
  filter(rhs=="i2"|rhs=="s2",
         op == "~") %>%
  mutate(model = "tot")

# create vars for plotting
ests4 <- tmp4 %>%
  mutate(FDR_corrected_pvalue = p.adjust(pvalue, method="fdr"),
         FDR = ifelse(FDR_corrected_pvalue <.05, "FDR p <.05", "FDR p >.05"),
         FDR = as.factor(FDR),
         lhs = as.factor(lhs),
         rhs=fct_relevel(rhs,"s2","i2"))

# rename growth factors and symptom vars
levels(ests4$rhs)[levels(ests4$rhs)=="s2"] <- "Slope"
levels(ests4$rhs)[levels(ests4$rhs)=="i2"] <- "Intercept"
levels(ests4$lhs)[levels(ests4$lhs)=="anx_8yr"] <- "ANX"
levels(ests4$lhs)[levels(ests4$lhs)=="dep_8yr"] <- "DEP"
levels(ests4$lhs)[levels(ests4$lhs)=="hyp_8yr"] <- "HYP"
levels(ests4$lhs)[levels(ests4$lhs)=="inat_8yr"] <- "INAT"
levels(ests4$lhs)[levels(ests4$lhs)=="odd_8yr"] <- "ODD"
levels(ests4$lhs)[levels(ests4$lhs)=="cd_8yr"] <- "CD"

# merge diff/tot
ests_both2 <- full_join(ests3, ests4)

# rename diff/tot
levels(ests_both2$model)[levels(ests_both2$model)=="diff"] <- "Differentiation"
levels(ests_both2$model)[levels(ests_both2$model)=="tot"] <- "Total score"

# fix order of diff and tot
ests_both2 <- ests_both2 %>%
  mutate(model=fct_relevel(model,"Total score","Differentiation"))

# create dot plot
p4<- ests_both2 %>%
  mutate(lhs = fct_relevel(lhs,"ANX","DEP","HYP","INAT","ODD","CD")) %>%
  ggplot(ests_both2, mapping=aes(x=est.std, y=reorder(rhs, desc(rhs)), xmin = ci.lower, xmax = ci.upper, colour = model)) +
  geom_vline(aes(xintercept=0), colour = "grey50", linetype = 2) + theme_light(base_size=17) +
  geom_errorbar(linewidth = 1.2, alpha=0.7, width = 0, position=position_dodge(0.7)) +
  geom_point(size=2.7, fill="white", alpha=1, position=position_dodge(0.7)) +
  facet_grid(rows = vars(lhs), scales = "fixed", space = "fixed", as.table = TRUE) +
  scale_colour_manual(values=c("#0072B2","#D55E00")) +
  guides(colour=guide_legend(order=0, reverse=FALSE)) + 
  theme(axis.text = element_text(size=14, colour = "black"),
        axis.ticks.x = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill="transparent"),
        panel.spacing = unit(0.2, "lines"),
        panel.grid = element_blank(),
        axis.line = element_line(colour="grey80"),
        legend.position = "top",
        legend.title = element_blank(),
        legend.text = element_text(size = 17),
        strip.text = element_text(size=14, colour = 'black'),
        strip.background = element_rect(colour="gray", fill="white")) + 
  scale_shape_manual(values=c(24,21)) +
  coord_cartesian(xlim = c(-0.5,0.5))

# format results for barplot for diff and tot
barplot_diff2 <- ests3 %>%
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
  mutate(R2 = est_slope^2 + est_int^2 + (est_slope*0.629*est_int))

barplot_tot2 <- ests4 %>%
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
  mutate(R2 = est_slope^2 + est_int^2 + (est_slope*0.531*est_int))

# join together
bar_both2 <- barplot_diff2 %>%
  full_join(barplot_tot2)

# create bar plot
p5<- bar_both2 %>%
  mutate(predictor = fct_relevel(predictor,"ANX","DEP","HYP","INAT","ODD","CD")) %>%
  ggplot(bar_both2, mapping=aes(x= predictor, y = R2, colour = model, fill = model)) +
  theme_light(base_size=17) +
  geom_col(width=4.8,position="stack") +
  facet_grid(rows = vars(predictor), scales = "fixed", space = "fixed", as.table = TRUE) +
  scale_colour_manual(values=c("#D55E00","#0072B2"),guide="none") +
  scale_fill_manual(values=c("#D55E00","#0072B2"),guide="none") +
  theme(axis.text = element_text(size=14, colour = "black"),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_line(colour="grey80"),
        panel.spacing = unit(0.2, "lines"),
        panel.grid = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank(),
        plot.background = element_blank(),
        strip.background = element_rect(colour="white", fill="white"),
        legend.title = element_blank(),
        legend.text = element_text(size = 17)) +
  scale_y_continuous(limits=c(0,0.4)) +
  coord_flip()

# patch together dot and bar plots
patch <- p4 + p5

p6 <- patch +
  plot_layout(guides = "collect",
              widths = c(2, 0.8)) &
  theme(legend.position = "top")

patch2 <- p6 / p3 + plot_layout(tag_level = 'new') &
  plot_annotation(tag_levels = list(c('A', '', 'B', '')))

patch2

# save out
tiff("N:/durable/projects/childhood_differentiation/figures/validation_symptoms_ipw.tiff", 
     res = 800, compression = "lzw", unit = "in",
     height = 9, width = 10)

patch2

dev.off()

#validation with diagnoses as outcomes
source("N:/durable/projects/childhood_differentiation/scripts/02.4_specify_LGMs_for_validation_diags.R")

# run validation LGM with both intercept and slope for diff and tot
# and save factor score to estimate associations with diagnoses
fit_model_both <- sem(model, 
                      missing = "ML", 
                      estimator = "MLR", 
                      data = moba_ipw, 
                      cluster = "m_id",
                      fixed.x = F)

# save factor scores 
alldata_ipw <- moba_ipw %>%
  cbind(lavPredict(fit_model_both))

###########################################################
### run logistic regressions to obtain weighted log ORs ###
###########################################################

dx_anx <- miceadds::glm.cluster(
  formula=any_dx_anx ~ i1 + s1 + i2 + s2 + sex + parity,
  data=alldata_ipw,
  weights=alldata_ipw$ippw,
  cluster="m_id",
  family=quasibinomial)

dx_dep <- miceadds::glm.cluster(
  formula=any_dx_dep ~ i1 + s1 + i2 + s2 + sex + parity,
  data=alldata_ipw,
  weights=alldata_ipw$ippw,
  cluster="m_id",
  family=quasibinomial)

dx_adhd <- miceadds::glm.cluster(
  formula=any_dx_adhd ~ i1 + s1 + i2 + s2 + sex + parity,
  data=alldata_ipw,
  weights=alldata_ipw$ippw,
  cluster="m_id",
  family=quasibinomial)

dx_dbd <- miceadds::glm.cluster(
  formula=any_dx_dbd ~ i1 + s1 + i2 + s2 + sex + parity,
  data=alldata_ipw,
  weights=alldata_ipw$ippw,
  cluster="m_id",
  family=quasibinomial)

models <- list(dx_anx$glm_res,dx_dep$glm_res,dx_adhd$glm_res,dx_dbd$glm_res)

all_res <- map(models, function(res){
  
  # extract the results
  nhst_res <- res %>% 
    summary() %>% 
    .$coefficients %>%
    as.data.frame()%>% 
    rownames_to_column("predictor")
  
})

# compute ORs and other vars for plotting
est <- map(all_res, function(x){
  x[2:5, ] %>%
    mutate(SE = `Std. Error`,
           ci.lower = Estimate - 1.96*SE,
           ci.upper = Estimate + 1.96*SE,
           Estimate = exp(Estimate),
           ci.lower = exp(ci.lower),
           ci.upper = exp(ci.upper),
           outcome_type = "Diagnosis\nafter age 8",
           model = ifelse(predictor == "s1"|predictor == "i1",
                          "Differentiation", "Total score"),
           predictor = ifelse(predictor == "s1"|predictor == "s2",
                              "Slope", "Intercept")) %>%
    select(Estimate, ci.lower, ci.upper, P=`Pr(>|t|)`, 
           outcome_type, model, predictor)
})

# format df for plotting
ests <- reduce(est, bind_rows) %>%
  mutate(outcome = c(rep("ANX",4),rep("DEP",4),rep("ADHD",4),rep("DBD",4)))

# fix order of factors
ests <- ests %>%
  mutate(predictor=fct_relevel(predictor,"Slope","Intercept"),
         outcome = as.factor(outcome),
         model=fct_relevel(model,"Total score","Differentiation"))

# create dot plot (weighted)
p <- ests %>%
  mutate(outcome = fct_relevel(outcome,"ANX","DEP","ADHD","DBD")) %>%
  ggplot(ests, mapping=aes(x=Estimate, y=reorder(predictor, desc(predictor)), xmin = ci.lower, xmax = ci.upper, colour = model)) +
  geom_vline(aes(xintercept=1), colour = "grey50", linetype = 2) + theme_light(base_size=18) +
  scale_x_log10(breaks = c(0,1,2,4)) +
  geom_errorbar(size = 1, alpha=1, width = 0.4, position=position_dodge(0.5)) +
  geom_point(size=2.7, fill="white", alpha=1, position=position_dodge(0.5)) +
  facet_grid(rows = vars(outcome), scales = "fixed", space = "fixed", as.table = TRUE) +
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
        legend.position = "none",
        legend.title = element_blank(),
        strip.text = element_text(size=15, colour = 'black'),
        strip.background = element_rect(colour="gray", fill="white")) + 
  xlab(label="Odds ratio (log scale)")

p

# save out
tiff("N:/durable/projects/childhood_differentiation/figures/validation_diags_ipw.tiff", res = 800, compression = "lzw", unit = "in",
     height = 6, width = 7)

p

dev.off()

#############################################################
### run logistic regressions to obtain unweighted log ORs ###
#############################################################

dx_anx_unw <- miceadds::glm.cluster(
  formula=any_dx_anx ~ i1 + s1 + i2 + s2 + sex + parity,
  data=alldata_ipw,
  cluster="m_id",
  family=quasibinomial)

dx_dep_unw <- miceadds::glm.cluster(
  formula=any_dx_dep ~ i1 + s1 + i2 + s2 + sex + parity,
  data=alldata_ipw,
  cluster="m_id",
  family=quasibinomial)

dx_adhd_unw <- miceadds::glm.cluster(
  formula=any_dx_adhd ~ i1 + s1 + i2 + s2 + sex + parity,
  data=alldata_ipw,
  cluster="m_id",
  family=quasibinomial)

dx_dbd_unw <- miceadds::glm.cluster(
  formula=any_dx_dbd ~ i1 + s1 + i2 + s2 + sex + parity,
  data=alldata_ipw,
  cluster="m_id",
  family=quasibinomial)

models <- list(dx_anx_unw$glm_res,dx_dep_unw$glm_res,dx_adhd_unw$glm_res,dx_dbd_unw$glm_res)

all_res2 <- map(models, function(res){
  
  # extract the results
  nhst_res <- res %>% 
    summary() %>% 
    .$coefficients %>%
    as.data.frame()%>% 
    rownames_to_column("predictor")
  
})

# compute ORs and other vars for plotting
est2 <- map(all_res2, function(x){
  x[2:5, ] %>%
    mutate(SE = `Std. Error`,
           ci.lower = Estimate - 1.96*SE,
           ci.upper = Estimate + 1.96*SE,
           Estimate = exp(Estimate),
           ci.lower = exp(ci.lower),
           ci.upper = exp(ci.upper),
           outcome_type = "Diagnosis\nafter age 8",
           model = ifelse(predictor == "s1"|predictor == "i1",
                          "Differentiation", "Total score"),
           predictor = ifelse(predictor == "s1"|predictor == "s2",
                              "Slope", "Intercept")) %>%
    select(Estimate, ci.lower, ci.upper, P=`Pr(>|t|)`, 
           outcome_type, model, predictor)
})

# format df for plotting
ests_unw <- reduce(est2, bind_rows) %>%
  mutate(outcome = c(rep("ANX",4),rep("DEP",4),rep("ADHD",4),rep("DBD",4)))

# fix order of factors
ests_unw <- ests_unw %>%
  mutate(predictor=fct_relevel(predictor,"Slope","Intercept"),
         outcome = as.factor(outcome),
         model=fct_relevel(model,"Total score","Differentiation"))

# create dot plot (unweighted)
pu <- ests_unw %>%
  mutate(outcome = fct_relevel(outcome,"ANX","DEP","ADHD","DBD")) %>%
  ggplot(ests_unw, mapping=aes(x=Estimate, y=reorder(predictor, desc(predictor)), xmin = ci.lower, xmax = ci.upper, colour = model)) +
  geom_vline(aes(xintercept=1), colour = "grey50", linetype = 2) + theme_light(base_size=18) +
  scale_x_log10(breaks = c(0,1,2,4)) +
  geom_errorbar(size = 1, alpha=1, width = 0.4, position=position_dodge(0.5)) +
  geom_point(size=2.7, fill="white", alpha=1, position=position_dodge(0.5)) +
  facet_grid(rows = vars(outcome), scales = "fixed", space = "fixed", as.table = TRUE) +
  scale_colour_manual(values=c("#0072B2","#D55E00")) +
  guides(colour=guide_legend(order=0, reverse=FALSE)) + 
  theme(axis.text = element_text(size=15, colour = "black"),
        axis.ticks.x = element_blank(),
        axis.title.x = element_blank(),
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
        strip.background = element_rect(colour="gray", fill="white"))

# patch together weighted and unweighted dx plots
patchwork <- pu / p + 
  plot_annotation(tag_levels = 'A')

patchwork

# save out
tiff("N:/durable/projects/childhood_differentiation/figures/validation_diags_ipw.tiff", 
     res = 800, compression = "lzw", unit = "in",
     height = 9, width = 7)

patchwork

dev.off()

