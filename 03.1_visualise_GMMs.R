# 03.1_visualise_GMMs.R

# this script visualises the growth mixture models (GMMs) based on results from:
# 03_run_GMMs.R

# load packages ####

library(tidyverse)
library(patchwork)

# read in data extracted from models in 03_run_GMMs.R ####

load("./output/gmm/gmm_diff.RData")

levels(is_perclass_4c$Class)[levels(is_perclass_4c$Class)=="1"] <- "Conv (emo)\n3180 (4%)"
levels(is_perclass_4c$Class)[levels(is_perclass_4c$Class)=="2"] <- "Diff (beh)\n5029 (6.4%)"
levels(is_perclass_4c$Class)[levels(is_perclass_4c$Class)=="3"] <- "Reference\n69070 (87.5%)"
levels(is_perclass_4c$Class)[levels(is_perclass_4c$Class)=="4"] <- "Diff (emo)\n1703 (2.2%)"

# plot GMM trajectories ####

is <- ggplot(data=is_perclass_4c,aes(x=Time, y=predicted_score, colour=Class)) +
  geom_hline(yintercept=0,linetype="dotted",colour="grey",size=1) +
  geom_point(size=3,alpha=1) +
  geom_line(size=1.5,alpha=1) +
  geom_ribbon(aes(x=Time, y=predicted_score,ymin=Lci,ymax=Uci, fill=Class), 
              alpha=0.4, show.legend=FALSE,inherit.aes = FALSE) +
  coord_cartesian(ylim=c(-3,3)) + scale_x_continuous(breaks=c(1.5,3,5)) +
  ylab('Differentiation (-emotional +behavioural)')+ xlab('Age (years)') +
  scale_colour_manual(values= c("#0072B2", "#D55E00", "#000000", "#009E73")) +
  scale_fill_manual(values= c("#0072B2", "#D55E00", "#000000", "#009E73")) +
  theme_minimal() + theme(legend.position="top",
                          legend.direction = "horizontal",
                          legend.title = element_blank(),
                          legend.text = element_text(size=11.5),
                          panel.grid.major=element_blank(),
                          panel.grid.minor=element_blank(),
                          axis.line=element_line(colour="black"),
                          axis.text=element_text(size=16, colour="black"), 
                          axis.title=element_text(size=15))


# plot logORs for diagnoses on class ####

# remove comparisons that will not be included

dx_res <- dx_res[-c(4,5,6,10,11,12,16,17,18,22,23,24), ]

dx_res <- dx_res %>%
  mutate(diagnosis=case_when(str_detect(as.character(comparison), "DEP") ~"dep",
                             str_detect(as.character(comparison), "ANX") ~"anx",
                             str_detect(as.character(comparison), "DBD") ~"dbd",
                             str_detect(as.character(comparison), "ADH") ~"adhd")) %>%
  mutate(class_comp=case_when(str_detect(as.character(comparison), "43") ~"Diff (emo)\nvs reference",
                              str_detect(as.character(comparison), "23") ~"Diff (beh)\nvs reference",
                              str_detect(as.character(comparison), "13") ~"Conv (emo)\nvs reference")) %>%
  select(-comparison)
  
dx_res <- dx_res %>%
  mutate(class_comp = as.factor(class_comp)) %>%
  mutate(diagnosis = as.factor(diagnosis)) %>%
  mutate(OR_est = as.numeric(OR_est)) %>%
  mutate(OR_lci = as.numeric(OR_lci)) %>%
  mutate(OR_uci = as.numeric(OR_uci))


dx<- dx_res %>%
  mutate(diagnosis = fct_relevel(diagnosis,"dbd","adhd","dep","anx")) %>%
  ggplot(dx_res, mapping=aes(x=OR_est, y=diagnosis, colour=class_comp)) +
  geom_vline(xintercept = 1, linetype = 'dotted', col = 'grey',size=1) +
  geom_point(aes(colour=class_comp),size=3, position=position_dodge(0.4)) + 
  geom_errorbar(aes(colour=class_comp,xmin=OR_lci,xmax=OR_uci),size=1,position=position_dodge(0.4),width=0.3) +
  scale_x_continuous(trans = "log10",breaks = c(0,1,2,5,10,15,20,30)) +
  scale_colour_manual(values= c("#0072B2", "#D55E00", "#009E73")) +
  scale_fill_manual(values= c("#0072B2", "#D55E00", "#009E73")) +
  xlab('Odds ratios for diagnosis on class (log scale)') +
  theme_minimal() + theme(legend.position = "top",
                          legend.direction = "horizontal",
                          legend.title = element_blank(),
                          legend.text = element_text(size=11.5),
                          panel.grid.major = element_blank(),
                          panel.grid.minor = element_blank(),
                          axis.line = element_line(colour="black"),
                          axis.text = element_text(size = 16,colour="black"), 
                          axis.title = element_text(size = 15),
                          axis.title.y = element_blank())

# plot together

gmm <- is + dx
gmm <- gmm + plot_annotation(
  tag_levels = "A")
gmm

tiff("figures/gmm_diff_diag.tiff", res = 800, compression = "lzw", unit = "in",
     height = 5.5, width = 12)

gmm
gmm
dev.off()

# plot means for symptoms per class ####

sx_res <- sx_res %>%
  mutate(class = as.factor(class)) %>%
  mutate(outcome = as.factor(outcome)) %>%
  mutate(est = as.numeric(est)) %>%
  mutate(est_lci = as.numeric(lci)) %>%
  mutate(est_uci = as.numeric(uci)) %>%
  mutate_at(vars(matches("est")), list(~ case_when(outcome=="INAT_8YR" ~ .*4.05/(4*9),
                                                   outcome=="HYP_8YR" ~ .*3.84/(4*9),
                                                   outcome=="DEP_8YR" ~ .*2.45/(3*13),
                                                   outcome=="ANX_8YR" ~ .*1.2/(3*5),
                                                   outcome=="CD_8YR" ~ .*1.51/(4*8),
                                                   outcome=="ODD_8YR" ~ .*3.16/(4*8))))

levels(sx_res$class)[levels(sx_res$class)=="C#1"] <- "Converging (emo)"
levels(sx_res$class)[levels(sx_res$class)=="C#2"] <- "Differentiating (beh)"
levels(sx_res$class)[levels(sx_res$class)=="C#3"] <- "Reference"
levels(sx_res$class)[levels(sx_res$class)=="C#4"] <- "Differentiating (emo)"

sx<- sx_res %>%
  mutate(outcome = fct_relevel(outcome,"INAT_8YR","HYP_8YR","CD_8YR","ODD_8YR","DEP_8YR","ANX_8YR")) %>%
  ggplot(sx_res, mapping=aes(x=est, y=outcome)) +
  geom_vline(xintercept = 0, linetype = 'dotted', col = 'grey',size=1) +
  geom_point(aes(colour=class),size=2.5, position=position_dodge(0.4)) + 
  geom_errorbar(aes(colour=class,xmin=est_lci,xmax=est_uci),size=1,position=position_dodge(0.4),width=0.3) +
  coord_cartesian(xlim = c(-0.05,0.25)) + 
  scale_y_discrete(labels=c("INAT_8YR" ="inat","HYP_8YR"="hyp","CD_8YR"="cd","ODD_8YR"="odd","DEP_8YR"="dep","ANX_8YR"="anx")) +
  scale_colour_manual(values= c("#0072B2", "#D55E00", "#000000", "#009E73")) +
  scale_fill_manual(values= c("#0072B2", "#D55E00", "#000000", "#009E73")) +
  xlab('Mean of symptom domains at age 8 by class') +
  theme_minimal() + theme(legend.position = "top",
                          legend.title = element_blank(),
                          legend.text = element_text(size=10),
                          panel.grid.major = element_blank(),
                          panel.grid.minor = element_blank(),
                          axis.line = element_line(colour="black"),
                          axis.text = element_text(size = 14,colour="black"), 
                          axis.title = element_text(size = 15),
                          axis.title.y = element_blank())

# plot together

isx <- is + sx
isx <- isx + plot_annotation(
  title = 'Validation of differentiation measure',
  tag_levels = 'A')
isx

tiff("figures/gmm_diff_sympt.tiff", res = 800, compression = "lzw", unit = "in",
     height = 6, width = 14)

isx
isx
dev.off()
