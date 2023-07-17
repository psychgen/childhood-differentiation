# 04.2_explore_adjust_multilevel_LGMs.R

# Within-level estimates in MLMs rely substantially on families with more than one
# participating child; for estimates pertaining to fathers' variables, the fathers
# also need to have participated multiple times - this is likely a selected sample
# In this script, we explore this issue and seek adjustments

library(tidyverse)
library(ggplot2)
library(mice)
library(weights)
library(tictoc)

# source a script with modified functions needed
source("./scripts/99_utils.R")

############ 1 - EXPLORE BIASES ASSOCITED WITH SIB STATUS ###############

# read in processed data from 00_data_preparation.R
load("./data/processed_data_new.RData")

# prepare for Mplus
dat <- alldata_new %>% 
  select(ind_id, m_id, sex, BARN_NR, parity,
         age1, age2, age3, 
         diff1, diff2, diff3, tot1, tot2, tot3,
         inat_8yr, hyp_8yr, cd_8yr, odd_8yr, dep_8yr, anx_8yr,
         any_dx_dep, any_dx_anx, any_dx_dbd, any_dx_adhd, 
         f_predis = pre_distress_f, 
         m_prestr = pre_stress_m, 
         m_pststr = post_stress_m, 
         m_pstdep = post_dep_m,
         m_dist = distress_m,
         m_predis = pre_distress_m,
         m_rxp = relation_m, 
         f_rxp = relation_f, 
         f_stress = stress_f, 
         m_alcpr= alc_prob_m,  
         m_alcrisk = alc_risk_tot_m, 
         f_alcrisk = alc_risk_tot_f, 
         f_smok = smoking_f, 
         m_smok = smoking_m, 
         mf_edu = education,  
         mf_inc = income) %>% 
  rename_with(~str_remove(.,"any_")) %>% 
  mutate(ind_id=factor(ind_id),
         m_id=factor(m_id),
         sex=as.factor(sex),
         parity=as.factor(parity),
         BARN_NR= as.factor(BARN_NR),
         across(where(is.numeric),scale),
         across(where(is.numeric),as.numeric),
         sex=as.numeric(sex),
         parity=as.numeric(sex)) %>% 
  as.data.frame()

# separate out siblings and singletons
sib_ids= dat %>% 
  filter(duplicated(m_id)) %>% 
  .$m_id
sibs_dat = dat %>% 
  filter(m_id %in% sib_ids)
sing_dat = dat %>% 
  filter(!m_id %in% sibs_dat$m_id)

# make function to compare using t-tests
compare = function(sibs, sings, vars){
  all=data.frame()
  for(var in vars){
    temp=t.test(sibs[,var], sings[,var]) %>% broom::tidy() %>% 
      mutate(pred=var) %>% 
      select(pred, "mean_sibs"=estimate1,"mean_sings"=estimate2,"diff"=estimate, everything())
    all=rbind(all,temp)
  }
  return(all)
}

# run function
comparison = compare(sibs_dat,sing_dat,names(dat %>% 
                                               select(diff1:mf_inc)))
comparison %>%  print(n=Inf)

ggplot(comparison %>% 
         filter(pred%in%names(dat %>% select(f_predis:mf_inc)))
         , aes(x=diff, y=pred))+
  geom_vline(aes(xintercept=0),colour="grey60", linetype=2,size=1.1)+
  geom_errorbarh(aes(xmin=conf.low, xmax=conf.high), height=0, size=1.1)+
  geom_point(shape=21, size=4, stroke=1.1, fill="blue")+
  theme_minimal()+
  scale_x_continuous("Mean difference between siblings and singletons (SDs)")+
  scale_y_discrete("Predictor")
  
# Substantial differences across the board -
# can we treat being a sibling as a form of participation bias, and use IPW to 
# upweight those siblings who are more like singletons?
# Would rely on a) complete data; and b) being able to incorporate weights in MLMs

# Simplest approach would be to run both adjusted and unadjusted on sibs only, 
# accepting biases associated with being a sib avoiding the apples-with-oranges
# issue we currently have when comparing the adjusted and unadjusted ests - but having
# weights would also reduce the sib participation biases

###############################################################################  
############ 2 - MI and IPW to set up adjustment for sib biases ###############
###############################################################################

### MI - run on cluster
dat_for_mi <- dat %>% 
  select(sex,parity,starts_with("diff"),starts_with("tot"),starts_with("dx"), ends_with("8yr"), 
         f_predis, m_prestr, m_pststr, m_pstdep, m_dist, m_predis, m_rxp, f_rxp, f_stress, m_alcpr, m_alcrisk, f_alcrisk, f_smok, m_smok, mf_edu, mf_inc)

save(dat_for_mi,file="./data/dat_for_MI.RData")

# example code: check runtime, RAM
tic()
MI <- parlmice(dat_for_mi,n.core=parallel::detectCores(), n.imp.core=2, maxit = 2)#for linux, cl.type= "FORK")
toc()

load("./data/MI_60imp_15its.RData")

# obj name is MI

### IPW
all = complete(MI, "all")
all_weights = dat %>% 
  select(ind_id)

for (i in 1:length(all)){
  
    comp_dat <-  all[[i]] %>% 
    bind_cols(dat %>% 
                select(m_id, ind_id, BARN_NR)) %>%
    mutate(sibs=ifelse(m_id%in%sib_ids,1,0))
  
    #test impact of accounting for clustering:
    #a=glm(data=comp_dat, formula= 'sibs ~ 1 +diff1 + diff2 + diff3 + tot1 + tot2 + tot3 + dx_dep + dx_anx + dx_dbd + dx_adhd + inat_8yr + hyp_8yr + cd_8yr + odd_8yr + dep_8yr + anx_8yr + f_predis + m_prestr + m_pststr + m_pstdep + m_dist + m_predis + m_rxp + f_rxp + f_stress + m_alcpr + m_alcrisk + f_alcrisk + f_smok + m_smok + mf_edu + mf_inc',family=binomial(link = "logit") )
    #b=miceadds::glm.cluster(data=comp_dat, formula= 'sibs ~ 1 +diff1 + diff2 + diff3 + tot1 + tot2 + tot3 + dx_dep + dx_anx + dx_dbd + dx_adhd + inat_8yr + hyp_8yr + cd_8yr + odd_8yr + dep_8yr + anx_8yr + f_predis + m_prestr + m_pststr + m_pstdep + m_dist + m_predis + m_rxp + f_rxp + f_stress + m_alcpr + m_alcrisk + f_alcrisk + f_smok + m_smok + mf_edu + mf_inc',cluster = "m_id",family=binomial(link = "logit") )
    #minimal diff in precision, none in estimates
    
  # use to generate ipw
  # stabilized
  ippws <- myipwpoint(exposure=sibs,
                      family="binomial",
                      link="logit",
                      numerator = ~ 1,
                      denominator = ~ 1 +diff1 + diff2 + diff3 + tot1 + tot2 + tot3 + dx_dep + dx_anx + dx_dbd + dx_adhd + inat_8yr + hyp_8yr + cd_8yr + odd_8yr + dep_8yr + anx_8yr + f_predis + m_prestr + m_pststr + m_pstdep + m_dist + m_predis + m_rxp + f_rxp + f_stress + m_alcpr + m_alcrisk + f_alcrisk + f_smok + m_smok + mf_edu + mf_inc,
                      data= comp_dat)
  
  range(ippws$ipw.weights)
 
  # check accuracy of predictions
  test <- comp_dat %>% 
    mutate(pred = round(predict(ippws$den.mod, newdata=., type="response"))) 
  
  accuracy <- table(test$pred, test$sibs)
  print(sum(diag(accuracy))/sum(accuracy))
  
  all_weights =cbind(all_weights, ippws$ipw.weights)
}

# rename and save raw weights
colnames(all_weights)<-c("ind_id", paste0("ipw",seq(1,60)))

save(all_weights, file="./data/all_weights.RData")

load(file="./data/all_weights.RData")


##############################################################################
### Test out adding weights to each imputed dataset and exporting to Mplus ###
##############################################################################

full <- complete(MI, action = "long", include = TRUE)

long_weights= dat %>% 
  select(ind_id) %>% 
  mutate(ipsw=NA) %>% 
  bind_rows(all_weights %>% 
              pivot_longer(-ind_id, values_to = "ipsw") %>% 
              arrange(name,ind_id) %>% 
              select(-name))

full_weights = full %>% 
  bind_cols(long_weights) %>% 
  `colnames<-`(c(names(full %>% select(`.imp`:anx_8yr)),
                 paste0("x",seq(1,ncol(full)-length(names(full %>% select((`.imp`:anx_8yr)))),1)),
                 names(long_weights))) %>% 
  left_join(dat %>% select(ind_id,m_id))%>% 
  filter(m_id %in% sib_ids)


lkp_names <- tibble(oldnames = c(names(full),names(long_weights),"m_id"),
                    newnames = names(full_weights))

  
mplus_midat = as.mids(full_weights )
mice::mids2mplus(mplus_midat,path="./data/")

##########################################################
### smooth weights by averaging across all imputations ###
##########################################################

smoothed_weights <- all_weights %>% 
  rowwise() %>% 
  group_by(ind_id) %>% 
  summarise(ipsw= mean(c_across(ipw1:ipw60)))

save(smoothed_weights, file="./data/smoothed_weights.RData")

load(file="./data/smoothed_weights.RData")


# check performance with weighted t-tests
wdat <- dat %>% 
  left_join(smoothed_weights)

# show relationship between weights and predictors
demo = wdat %>% 
  mutate(sib= ifelse(m_id %in% sib_ids,"Sibling","Singleton")) %>% 
  group_by(sib) %>% 
  slice_sample(n=2000) %>% 
  ungroup() %>% 
  select(f_predis:sib) %>% 
  pivot_longer(f_predis:mf_inc, names_to ="Predictor")

ggplot(demo %>% 
         filter(sib=="Sibling"), aes(x=ipsw,y=value,size=ipsw, fill=sib)) +
  geom_point(shape=21)+
  geom_smooth(method="lm")+
  facet_wrap(vars(Predictor))+
  theme_minimal()

sibs_wdat = wdat %>% 
  filter(m_id %in% sib_ids)
sing_wdat = wdat %>% 
  filter(!m_id %in% sib_ids)

# make function to compare using weighted t-tests
wtcompare = function(sibs, sings, vars, wts){
  all=data.frame()
  for(var in vars){
    temp=weights::wtd.t.test(x=sibs[,var], y= sings[,var], weight=sibs[,wts]) 
    temp = c(temp$additional,temp$coefficients) %>% 
      as_tibble_row() %>% 
      rename_all(~paste0("wtd.",.))
    base= weights::wtd.t.test(x=sibs[,var], y= sings[,var]) 
    base = c(base$additional,base$coefficients) %>% 
      as_tibble_row() %>% 
      rename_all(~paste0("unw.",.))
    comp= cbind(base,temp) %>% 
      mutate(pred=var) %>% 
      select(pred,everything())
    all=rbind(all,comp)
  }
  return(as_tibble(all))
}

# run function
wtcomparison = wtcompare(sibs_wdat,sing_wdat,names(dat %>% 
                                               select(diff1:mf_inc)), "ipsw")
wtcomparison %>% filter(pred%in%names(dat %>% select(f_predis:mf_inc))) %>%  print(n=Inf)

wtcomplong = wtcomparison %>% 
  filter(pred%in%names(dat %>% select(f_predis:mf_inc))) %>% 
  select(pred, unw.Difference,`unw.Std. Err`,wtd.Difference,`wtd.Std. Err`) %>% 
  pivot_longer(cols=-pred) %>% 
  mutate(name = str_replace_all(name,"Std\\. ","Std")) %>% 
  separate(name, into=c("model","param"), sep="\\." ) %>% 
  pivot_wider(names_from = param, values_from = value) %>% 
  mutate(conf.low=Difference-1.96*StdErr,
         conf.high=Difference+1.96*StdErr,
         pred=as.factor(pred),
         model=as.factor(model))

# rename
levels(wtcomplong$pred)[levels(wtcomplong$pred)=="mf_edu"] <- "Education"
levels(wtcomplong$pred)[levels(wtcomplong$pred)=="mf_inc"] <- "Income"
levels(wtcomplong$pred)[levels(wtcomplong$pred)=="m_prestr"] <- "Prenatal life events"
levels(wtcomplong$pred)[levels(wtcomplong$pred)=="f_stress"] <- "Prenatal life events"
levels(wtcomplong$pred)[levels(wtcomplong$pred)=="m_smok"] <- "Prenatal smoking"
levels(wtcomplong$pred)[levels(wtcomplong$pred)=="f_smok"] <- "Prenatal smoking"
levels(wtcomplong$pred)[levels(wtcomplong$pred)=="m_predis"] <- "Prenatal distress"
levels(wtcomplong$pred)[levels(wtcomplong$pred)=="f_predis"] <- "Prenatal distress"
levels(wtcomplong$pred)[levels(wtcomplong$pred)=="m_alcpr"] <- "Alcohol problems"
levels(wtcomplong$pred)[levels(wtcomplong$pred)=="f_rxp"] <- "Relationship problems"
levels(wtcomplong$pred)[levels(wtcomplong$pred)=="f_alcrisk"] <- "At-risk drinking"
levels(wtcomplong$pred)[levels(wtcomplong$pred)=="m_alcrisk"] <- "At-risk drinking"
levels(wtcomplong$pred)[levels(wtcomplong$pred)=="m_rxp"] <- "Relationship problems"
levels(wtcomplong$pred)[levels(wtcomplong$pred)=="m_pststr"] <- "Adverse life events"
levels(wtcomplong$pred)[levels(wtcomplong$pred)=="m_dist"] <- "Concurrent distress"
levels(wtcomplong$pred)[levels(wtcomplong$pred)=="m_pstdep"] <- "Postnatal depression"

levels(wtcomplong$model)[levels(wtcomplong$model)=="unw"] <- "Unweighted"
levels(wtcomplong$model)[levels(wtcomplong$model)=="wtd"] <- "Weighted"

# create plot
ggplot(wtcomplong, aes(x=Difference, y=pred, fill=factor(model)))+
  geom_vline(aes(xintercept=0),colour="black", linetype=2,size=1)+
  geom_errorbarh(aes(xmin=conf.low, xmax=conf.high), height=0, size=1.1, position=position_dodge(0.4))+
  geom_point(shape=21, size=4, stroke=1.1, position=position_dodge(0.4))+
  theme_minimal(base_size=16)+
  scale_x_continuous("Mean difference between siblings and singletons (SDs)")+
  scale_fill_manual(values=c("blue","red"))+
  theme(legend.title = element_blank(),
        legend.text = element_text(size=18),
        legend.position = "top",
        axis.title.y = element_blank(),
        axis.text = element_text(size=16))


# save weighted dataset
save(wdat, file= "./data/sib_weighted_data.RData")

###############################################################################




