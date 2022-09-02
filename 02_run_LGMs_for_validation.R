# 02_run_LGMs_for_validation.R

# this script runs the validation latent growth models (LGMs) by sourcing:
# 02.1_specify_LGMs_for_validation.R

# load packages ####

library(lavaan)
library(tidyverse)
library(DiagrammeR)
library(patchwork)
library(broman)

# read in processed data from 00_data_preparation.R ####

load("./data/processed_data.RData")

# specify validation LGMs ####

source("./scripts/02.1_specify_LGMs_for_validation.R")

# model0 - run p factor model ####

fit_model0 <- sem(model0, 
                  missing = "ML", 
                  estimator = "MLR", 
                  data = alldata, 
                  cluster = "m_id",
                  fixed.x = F)

# provide output for the model

summary(fit_model0, fit.measures = TRUE, std = TRUE) # poor fit

# run the models for differentiation/total variables ####

vars <- c("diff","tot")

# loop through the CBCL diff and tot measures, running all models and
# saving fit comparisons in a data frame

fitcomps_final<- data.frame()
all_model_fits_nest<- list()

for(var in vars){
  message(paste0("Running models for ",var))
  alldata$cbcl1 <- alldata[,paste0(var,1)]
  alldata$cbcl2 <- alldata[,paste0(var,2)]
  alldata$cbcl3 <- alldata[,paste0(var,3)]
  
  # run all the models, saving fit objects as list elements
  
  all_model_fits <- list()
  for(model in names(models)) {
    
    message(paste0("Running model: ",model))
    all_model_fits[[model]] <- sem(models[[model]], estimator = "MLR", missing = "ML", 
                                   data = alldata, cluster = "m_id", fixed.x = F)
  }
  
  fitcomps <- rbind(as.data.frame(anova(all_model_fits[["model1"]], all_model_fits[["model2"]])),
                    as.data.frame(anova(all_model_fits[["model1"]], all_model_fits[["model3"]]))[2,],
                    as.data.frame(anova(all_model_fits[["model1"]], all_model_fits[["model4"]]))[2,]) %>% 
    mutate(cbcl_var = var )
  
  
  fitcomps_final <- rbind(fitcomps_final, fitcomps)
  
  all_model_fits_nest[[var]] <-  all_model_fits
  
  
}

fitcomps_final

save(fitcomps_final, file= "./output/sTable_validation_fits.RData")

save(all_model_fits_nest, file= "./output/all_model_fits_validation.RData")

load("./output/all_model_fits_validation.RData")


# Extracting results for reporting/display - betas/CI/pvalue for best-fitting models

res<- standardizedSolution(all_model_fits_nest[["diff"]]$model1, ci = TRUE, level = 0.95) %>% 
  filter(op=="~",
         !rhs%in%c("sex","age1","age2","age3")) %>% 
  select(predictor=rhs,
         outcome=lhs,
         beta=est.std,
         se,
         p=pvalue,
         lci=ci.lower,
         uci=ci.upper) %>% 
  mutate(cbcl="diff") %>% 
  bind_rows(standardizedSolution(all_model_fits_nest[["tot"]]$model1, ci = TRUE, level = 0.95) %>% 
              filter(op=="~",
                     !rhs%in%c("sex","age1","age2","age3")) %>% 
              select(predictor=rhs,
                     outcome=lhs,
                     beta=est.std,
                     se,
                     p=pvalue,
                     lci=ci.lower,
                     uci=ci.upper) %>% 
              mutate(cbcl="tot"))


resdat <- alldata %>% 
  select(ind_id, preg_id,sex,BARN_NR, matches("_8yr")) %>% 
  cbind(lavPredict(all_model_fits_nest[["diff"]]$model1)) 

plotdat <-quantileSE(resdat$i, p = seq(0, 1, by = 0.1), na.rm=TRUE) %>% 
  t() %>%
  as.data.frame() %>% 
  `colnames<-`(c("intercept", "se_i")) %>% 
  rownames_to_column(var="quantile") %>% 
  cbind(t(quantileSE(resdat$s, p = seq(0, 1, by = 0.1), na.rm=TRUE)) %>% 
          as.data.frame() %>% 
          `colnames<-`(c("slope", "se_s"))) %>% 
  mutate(time1= -3.5,
         time2= -2,
         time3= 0) %>% 
  mutate(quantile=case_when(quantile==0~"0%",quantile==0.1~"10%",quantile==0.2~"20%",
                            quantile==0.3~"30%",quantile==0.4~"40%",quantile==0.5~"50%",
                            quantile==0.6~"60%",quantile==0.7~"70%",quantile==0.8~"80%",
                            quantile==0.9~"90%",quantile==1~"100%")) %>%
  gather(time,timeval,-se_i,-se_s, -intercept,-slope, -quantile)%>% 
  mutate(y= intercept+timeval*slope) %>% 
  mutate(se= se_i+timeval*se_s) %>%
  filter(quantile%in%c("10%","50%","90%")) %>% 
  mutate(quantile=factor(quantile,
                         levels=c("10%","50%","90%") )) %>% 
  mutate(ylci=y-1.96*se) %>%
  mutate(yuci=y+1.96*se) %>%
  mutate(realtime = timeval+5)

a<-plotdat %>%
  mutate(quantile=fct_relevel(quantile,"10%","50%","90%")) %>%
  ggplot(plotdat,mapping=aes(x=realtime,y=y,fill=quantile)) +
  geom_ribbon(aes(ymin=ylci,ymax=yuci),alpha=0.4,show.legend=FALSE) + 
  geom_line(aes(colour=quantile),size=1) + 
  scale_x_continuous(breaks=c(1.5,3,5)) +
  geom_hline(yintercept=0,linetype='dotted',col='grey',size=1) +
  geom_point(aes(colour=quantile,shape=quantile),size=2.5) +
  coord_cartesian(ylim=c(-0.5,1)) +
  scale_colour_manual(values= c("#E69F00", "#999999", "#56B4E9")) +
  scale_fill_manual(values= c("#E69F00", "#999999", "#56B4E9")) +
  ylab('Differentiation (-emotional +behavioural)')+xlab('Age (years)') +
  theme_minimal() + theme(legend.position="top",
                          legend.title=element_blank(),
                          legend.text = element_text(size=14),
                          panel.grid.major=element_blank(),
                          panel.grid.minor=element_blank(),
                          axis.line=element_line(colour="black"),
                          axis.text=element_text(size=14, colour="black"), 
                          axis.title=element_text(size=15))
a

plotdat2 <- plotdat %>% 
  rename("i"=intercept,"s"=slope) %>% 
  select(-se, -se_i, -se_s, -ylci,-yuci) %>%
  gather(predictor,value, -quantile, -time:-realtime) %>% 
  left_join(res) %>% 
  filter(cbcl=="diff") %>% 
  mutate(pred= beta*value,
         lcipred=lci*value,
         ucipred=uci*value) %>% 
  select(-beta:-cbcl, -value) %>% 
  gather(pred_type,pred_val, -quantile:-outcome) %>% 
  unite("pred_type_is", c("predictor","pred_type")) %>% 
  spread(pred_type_is, pred_val) %>% 
  gather(type, value, -quantile:-outcome ) %>% 
  separate(type, into=c("parameter","limit"), sep="_") %>% 
  spread(parameter, value) %>% 
  mutate(final_pred = i+s) %>% 
  select(-i,-s) %>% 
  spread(limit,final_pred) %>% 
  select(-time,-timeval,-realtime) %>% 
  distinct() %>%
  mutate_at(vars(matches("pred")), list(~ case_when(outcome=="dep_8yr" ~ .*2.45/(3*13),
                                                    outcome=="anx_8yr" ~ .*1.2/(3*5),
                                                    outcome=="cd_8yr" ~ .*1.51/(4*8),
                                                    outcome=="odd_8yr" ~ .*3.16/(4*8),
                                                    outcome=="hyp_8yr" ~ .*3.9/(4*9),
                                                    outcome=="inat_8yr" ~ .*4.13/(4*9))))


b<- plotdat2 %>%
  mutate(outcome = fct_relevel(outcome,
  "anx_8yr","dep_8yr","hyp_8yr","inat_8yr","odd_8yr","cd_8yr")) %>%
  ggplot(plotdat2, mapping=aes(x = pred, y = outcome, colour = quantile)) +
  geom_vline(xintercept = 0, linetype = 'dotted', col = 'grey',size=1) +
  geom_point(aes(colour=quantile,shape=quantile),size=2,position=position_dodge(0.4)) + 
  geom_errorbar(aes(xmin = lcipred, xmax = ucipred),size=1, position = position_dodge(0.4), width = 0.4) +
  coord_cartesian(xlim = c(-0.04,0.04)) + 
  scale_y_discrete(labels=c("anx_8yr"="anx","dep_8yr"="dep","hyp_8yr"="hyp",
                            "inat_8yr"="inat","odd_8yr"="odd","cd_8yr"="cd")) +
  scale_colour_manual(values= c("#E69F00", "#999999", "#56B4E9")) +
  scale_fill_manual(values= c("#E69F00", "#999999", "#56B4E9")) +
  ylab('') + xlab('Predicted score on domain at age 8') +
  theme_minimal() + theme(legend.position = "top",
                          legend.title = element_blank(),
                          legend.text = element_text(size=14),
                          panel.grid.major = element_blank(),
                          panel.grid.minor = element_blank(),
                          axis.line = element_line(colour="black"),
                          axis.text = element_text(size = 14, colour="black"), 
                          axis.title = element_text(size = 15))
b

# combine plots
  ab <- a + b
  ab + plot_annotation(
    tag_levels = 'A')

