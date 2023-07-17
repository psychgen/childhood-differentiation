# 02.5_run_LGMs_for_validation_diags.R

# this script runs the validation latent growth models (LGMs) by sourcing:
# 02.4_specify_LGMs_for_validation_diags.R

library(lavaan)
library(tidyverse)
library(patchwork)

# read in processed data from 00_data_preparation.R
load("N:/durable/projects/childhood_differentiation/data/processed_data_new.RData")

# specify validation LGMs
source("N:/durable/projects/childhood_differentiation/scripts/02.4_specify_LGMs_for_validation_diags.R")

# run validation LGM with both intercept and slope for diff and tot
# and save factor score to estimate associations with diagnoses
fit_model_both <- sem(model, 
                      missing = "ML", 
                      estimator = "MLR", 
                      data = alldata_new, 
                      cluster = "m_id",
                      fixed.x = F)

# check results
summary(fit_model_both, fit.measures = TRUE, std = TRUE)

# save factor scores 
alldata_new <- alldata_new %>%
  cbind(lavPredict(fit_model_both))

# run logistic regressions to obtain log ORs
dx_anx <- miceadds::glm.cluster(
  formula=any_dx_anx ~ i1 + s1 + i2 + s2 + sex + parity,
  data = alldata_new,
  cluster="m_id",
  family=binomial(link="logit"))

dx_dep <- miceadds::glm.cluster(
  formula=any_dx_dep ~ i1 + s1 + i2 + s2 + sex + parity,
  data = alldata_new,
  cluster="m_id",
  family=binomial(link="logit"))

dx_adhd <- miceadds::glm.cluster(
  formula=any_dx_adhd ~ i1 + s1 + i2 + s2 + sex + parity,
  data = alldata_new,
  cluster="m_id",
  family=binomial(link="logit"))

dx_dbd <- miceadds::glm.cluster(
  formula=any_dx_dbd ~ i1 + s1 + i2 + s2 + sex + parity,
  data = alldata_new,
  cluster="m_id",
  family=binomial(link="logit"))

models <- list(dx_anx$glm_res,dx_dep$glm_res,dx_adhd$glm_res,dx_dbd$glm_res)

all_res <- map(models, function(res){
  
  # extract the results
  nhst_res <- res %>% 
    summary() %>% 
    .$coefficients %>%
    as.data.frame()%>% 
    rownames_to_column("predictor")
  
})

# compute OR estimates for each dx outcome
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
    select(Estimate, ci.lower, ci.upper, P=`Pr(>|z|)`, 
           outcome_type, model, predictor)
})

# format df for plotting
ests <- reduce(est, bind_rows) %>%
  mutate(outcome = c(rep("ANX",4),rep("DEP",4),rep("ADHD",4),rep("DBD",4)))

# fix factor levels
ests <- ests %>%
  mutate(predictor=fct_relevel(predictor,"Slope","Intercept"),
         outcome = as.factor(outcome),
         model=fct_relevel(model,"Total score","Differentiation"))

# create dot plot
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
        legend.position = "top",
        legend.title = element_blank(),
        legend.text = element_text(size = 18),
        strip.text = element_text(size=15, colour = 'black'),
        strip.background = element_rect(colour="gray", fill="white")) + 
  xlab(label="Odds ratio (log scale)")

p

# save out
tiff("N:/durable/projects/childhood_differentiation/figures/validation_diags.tiff", res = 800, compression = "lzw", unit = "in",
     height = 6, width = 7)

p

dev.off()
