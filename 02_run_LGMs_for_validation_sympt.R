# 02_run_LGMs_for_validation_sympt.R

# this script runs the validation latent growth models (LGMs) by sourcing:
# 02.1_specify_LGMs_for_validation_sympt.R

# load packages
library(lavaan)
library(tidyverse)
library(DiagrammeR)
library(patchwork)
library(broman)

# read in processed data from 00_data_preparation.R
load("./data/processed_data_new.RData")

# specify validation LGMs
source("./scripts/02.1_specify_LGMs_for_validation_sympt.R")

# model0 - run p factor model
fit_model0 <- sem(model0, 
                  missing = "ML", 
                  estimator = "MLR", 
                  data = alldata_new, 
                  cluster = "m_id",
                  fixed.x = F)

# provide output for the model
summary(fit_model0, fit.measures = TRUE, std = TRUE) # poor fit

# run the models for differentiation/total variables
vars <- c("diff","tot")

# loop through the CBCL diff and tot measures, running all models and
# saving fit comparisons in a data frame

fitcomps_final<- data.frame()
all_model_fits_nest<- list()

for(var in vars){
  message(paste0("Running models for ",var))
  alldata_new$cbcl1 <- alldata_new[,paste0(var,1)][[1]]
  alldata_new$cbcl2 <- alldata_new[,paste0(var,2)][[1]]
  alldata_new$cbcl3 <- alldata_new[,paste0(var,3)][[1]]
  
  # run all the models, saving fit objects as list elements
  all_model_fits <- list()
  for(model in names(models)) {
    
    message(paste0("Running model: ",model))
    all_model_fits[[model]] <- sem(models[[model]], estimator = "MLR", missing = "ML", 
                                   data = alldata_new, cluster = "m_id", fixed.x = F)
  }
  
  fitcomps <- rbind(as.data.frame(anova(all_model_fits[["model1"]], all_model_fits[["model2"]])),
                    as.data.frame(anova(all_model_fits[["model1"]], all_model_fits[["model3"]]))[2,],
                    as.data.frame(anova(all_model_fits[["model1"]], all_model_fits[["model4"]]))[2,]) %>% 
    mutate(cbcl_var = var )
  
  
  fitcomps_final <- rbind(fitcomps_final, fitcomps)
  
  all_model_fits_nest[[var]] <-  all_model_fits
  
  
}

fitcomps_final

# save out
save(fitcomps_final, file= "./output/sTable_validation_fits_sympt.RData")
save(all_model_fits_nest, file= "./output/all_model_fits_validation_sympt.RData")

load("./output/all_model_fits_validation_sympt.RData")

# extracting results for reporting/display - betas/CI/pvalue for best-fitting models
est <- standardizedSolution(all_model_fits_nest[["diff"]]$model1, ci = TRUE, level = 0.95) %>% 
  filter(op=="~",
         !rhs%in%c("sex","parity","age1","age2","age3")) %>% 
  select(predictor=rhs,
         outcome=lhs,
         beta=est.std,
         se,
         p=pvalue,
         lci=ci.lower,
         uci=ci.upper) %>% 
  mutate(FDR_corrected_pvalue = p.adjust(p, method="fdr"), 
         FDR=ifelse(FDR_corrected_pvalue <.05, "FDR p <.05", "FDR p >.05"),
         FDR=as.factor(FDR)) %>%
  mutate(model="Differentiation") %>% 
  bind_rows(standardizedSolution(all_model_fits_nest[["tot"]]$model1, ci = TRUE, level = 0.95) %>% 
              filter(op=="~",
                     !rhs%in%c("sex","parity","age1","age2","age3")) %>% 
              select(predictor=rhs,
                     outcome=lhs,
                     beta=est.std,
                     se,
                     p=pvalue,
                     lci=ci.lower,
                     uci=ci.upper) %>% 
              mutate(FDR_corrected_pvalue = p.adjust(p, method="fdr"), 
                     FDR=ifelse(FDR_corrected_pvalue <.05, "FDR p <.05", "FDR p >.05"),
                     FDR=as.factor(FDR),
                     model="Total problems"))

ests <- est %>%
  mutate(predictor=fct_relevel(predictor,"s","i"),
         outcome = as.factor(outcome),
         model=fct_relevel(model,"Total problems","Differentiation"))

# rename growth factors and symptom vars
levels(ests$predictor)[levels(ests$predictor)=="s"] <- "Slope"
levels(ests$predictor)[levels(ests$predictor)=="i"] <- "Intercept"
levels(ests$outcome)[levels(ests$outcome)=="anx_8yr"] <- "ANX"
levels(ests$outcome)[levels(ests$outcome)=="dep_8yr"] <- "DEP"
levels(ests$outcome)[levels(ests$outcome)=="hyp_8yr"] <- "HYP"
levels(ests$outcome)[levels(ests$outcome)=="inat_8yr"] <- "INAT"
levels(ests$outcome)[levels(ests$outcome)=="odd_8yr"] <- "ODD"
levels(ests$outcome)[levels(ests$outcome)=="cd_8yr"] <- "CD"

# create dot plot
p <- ests %>%
  mutate(outcome = fct_relevel(outcome,"ANX","DEP","HYP","INAT","ODD","CD")) %>%
  ggplot(ests, mapping=aes(x=beta, y=reorder(predictor, desc(predictor)), xmin = lci, xmax = uci, colour = model)) +
  geom_vline(aes(xintercept=0), colour = "grey50", linetype = 2) + theme_light(base_size=18) +
  geom_errorbar(size = 1.2, alpha=0.7, width = 0, position=position_dodge(0.5)) +
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
  xlab(label=expression(Standardised~"beta"~(beta))) + 
  scale_shape_manual(values=c(24,21)) +
  coord_cartesian(xlim = c(-0.5,0.5))

barplot_diff <- ests %>%
  filter(model == "Differentiation") %>%
  select(growth_factor=predictor,predictor=outcome,est=beta,lci,uci) %>%
  mutate(est_slope = case_when(growth_factor == "Slope" ~ est),
         est_int = case_when(growth_factor == "Intercept" ~ est),
         est_slope_lci = case_when(growth_factor == "Slope" ~ lci),
         est_slope_uci = case_when(growth_factor == "Slope" ~ uci),
         est_int_lci = case_when(growth_factor == "Intercept" ~ lci),
         est_int_uci = case_when(growth_factor == "Intercept" ~ uci),
         predictor = case_when(growth_factor == "Intercept" ~ predictor)) %>%
  select(-growth_factor,-est,-lci,-uci) %>%
  summarise_all(list(~ discard(., is.na))) %>%
  mutate(model = "Differentiation") %>%
  mutate(R2 = est_slope^2 + est_int^2 + (est_slope*0.635*est_int))#slope~~int

barplot_tot <- ests %>%
  filter(model == "Total problems") %>%
  select(growth_factor=predictor,predictor=outcome,est=beta,lci,uci) %>%
  mutate(est_slope = case_when(growth_factor == "Slope" ~ est)) %>%
  mutate(est_int = case_when(growth_factor == "Intercept" ~ est)) %>%
  mutate(est_slope_lci = case_when(growth_factor == "Slope" ~ lci)) %>%
  mutate(est_slope_uci = case_when(growth_factor == "Slope" ~ uci)) %>%
  mutate(est_int_lci = case_when(growth_factor == "Intercept" ~ lci)) %>%
  mutate(est_int_uci = case_when(growth_factor == "Intercept" ~ uci)) %>%
  mutate(predictor = case_when(growth_factor == "Intercept" ~ predictor)) %>%
  select(-growth_factor,-est,-lci,-uci) %>%
  summarise_all(list(~ discard(., is.na))) %>%
  mutate(model = "Total problems") %>%
  mutate(R2 = est_slope^2 + est_int^2 + (est_slope*0.544*est_int))#slope~~int

# merge diff / tot
bar_both <- barplot_diff %>%
  full_join(barplot_tot)

p1<- bar_both %>%
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
patch <- p + p1

p2 <- patch +
  plot_layout(guides = "collect",
              widths = c(2, 0.8)) &
  theme(legend.position = "top")

p2

# save out
tiff("figures/validation_symptoms.tiff", res = 800, compression = "lzw", unit = "in",
     height = 6, width = 9)

p2

dev.off()

