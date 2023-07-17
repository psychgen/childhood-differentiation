# 00_data_preparation.R

# this script curates and selects the analytic dataset
# based on questionnaire, covariate, and registry data
# and conducts manipulation and preparation of variables
# with basic tests of attrition and scale reliability

# install packages ####
 
 #install.packages("//tsd-evs/p471/data/durable/common/software/phenotools_0.2.7.zip", 
 #         repos=NULL,
 #         type = "binary")

 #install.packages('dplyr',
 #                  repos = "file://tsd-evs/shared/R/cran")

 #install.packages('stringr',
 #                  repos = "file://tsd-evs/shared/R/cran")

 #install.packages('haven',
 #                  repos = "file://tsd-evs/shared/R/cran")

 #install.packages('tidyr',
 #                  repos = "file://tsd-evs/shared/R/cran")

 #install.packages("lavaan", 
 #                  repos = "file://tsd-evs/shared/R/cran")

 #install.packages("Rtools", 
 #                  repos = "file://tsd-evs/shared/R/cran")

 #install.packages("GGally", 
 #                  repos = "file://tsd-evs/shared/R/cran")
  
 #install.packages("semPlot", 
 #                  repos = "file://tsd-evs/shared/R/cran")

 #install.packages("stationery", 
 #                  repos = "file://tsd-evs/shared/R/cran")

 #install.packages("//tsd-evs/p471/data/durable/projects/childhood_differentiation/packages/semTable_1.8.tar",
 #                  repos = NULL,
 #                  type = "source")
  
 #install.packages("//tsd-evs/p471/data/durable/projects/childhood_differentiation/packages/ggrepel_0.9.1.tar",
 #                  repos = NULL,
 #                  type = "source")
 
 #install.packages("//tsd-evs/p471/data/durable/projects/childhood_differentiation/packages/broman_0.76.tar",
 #                repos = NULL,
 #                type = "source")

 #install.packages("devtools",
 #                  repos = "file://tsd-evs/shared/R/cran")

 #install.packages("broom",
 #                  repos = "file://tsd-evs/shared/R/cran")

 #install.packages("//tsd-evs/p471/data/durable/projects/childhood_differentiation/packages/DiagrammeR_0.6.tar",
 #                  repos = NULL,
 #                  type = "source")

 #install.packages("//tsd-evs/p471/data/durable/projects/childhood_differentiation/packages/patchwork_1.1.1.tar",
 #                 repos = NULL,
 #                 type = "source")

 #install.packages("rmarkdown",
 #                  repos = "file://tsd-evs/shared/R/cran")

 #install.packages("caTools",
 #                  repos = "file://tsd-evs/shared/R/cran")
 
# load packages for data prep ####

 library(foreign)
 library(tidyverse)
 library(phenotools)
 library(car)
 library(Hmisc)
 library(psych)
  
# curate dataset ####
  npr_groups <- c(paste0("dep = ", paste0(c("F32", "F33", "F341"), collapse =",")),
                  paste0("anx = ", paste0(c("F40", "F41", "F42", "F43", "F44", "F930", "F931", "F932"), collapse =",")),
                  paste0("adhd = ", paste0("F90", collapse =",")),
                  paste0("dbd = ", paste0(c("F91", "F92"), collapse =",")))
  
  kuhr_groups <- c(paste0("dep = ", paste0("P76", collapse =",")),
                   paste0("adhd = ", paste0("P81", collapse =",")),
                   paste0("dbd = ", paste0("P23", collapse =",")),
                   paste0("anx = ", paste0(c("P74", "P79", "P82"), collapse =",")))

  mydata <- curate_dataset(variables_required=
                             list(
                               moba=c(
                                 "cbcl_ext_c_18m", "cbcl_int_c_18m",                      # CBCL 1.5yr
                                 "cbcl_ext_c_3yr", "cbcl_int_c_3yr",                      # CBCL 3yr
                                 "cbcl_ext_c_5yr", "cbcl_int_c_5yr",                      # CBCL 5yr
                                 "rsdbd_ina_c_8yr", "rsdbd_hyp_c_8yr",                    # RSDBD 8yr - ADHD
                                 "rsdbd_cd_c_8yr", "rsdbd_odd_c_8yr",                     # RSDBD 8yr - behavioural
                                 "smfq_dep_c_8yr", "scared_anx_c_8yr",                    # RSDBD 8yr - emotional
                                 "scl_full_f_far", "scl_full_m_q3",                       # paternal and maternal pre_distress (w30)
                                 "scl_short_m_q1", "scl_full_m_18m", "scl_full_m_3yr",    # maternal pre_distress (w17) & distress 1.5yr & 3yr
                                 "adlifev_short_m_q3", "adlifev_short_m_18m",             # maternal prenatal and postnatal stress
                                 "adlifev_short_f_far", "epds_short_m_6m",                # paternal stress and maternal postnatal depression
                                 "rs_full_m_18m", "rs_short_f_far",                       # maternal and paternal relationship problems
                                 "rapi_full_m_q1", "EE607", "EE608", "EE609",             # maternal alcohol problems & use 1.5yr
                                 "GG486", "GG487", "GG488", "FF244", "FF467", "FF468",    # maternal alcohol use 3yr & paternal use
                                 "AA1356", "CC1037", "CC1040",                            # maternal & paternal smoking (at present)
                                 "AA1315", "AA1316", "AA1124", "AA1126",                  # maternal & paternal income & education
                                 "ALDERRETUR_S5", "ALDERRETUR_S6", "PARITET_5",           # child age at questionnaire return 1.5yr & 3yr, parity
                                 "AGE_RETURN_MTHS_Q5AAR", "AGE_RETURN_MTHS_Q8AAR",        # child age at questionnaire return 5yr & 8yr
                                 "KJONN"), npr = npr_groups, kuhr = kuhr_groups),         # child sex and diagnostic groups
                           PDB = "2306",
                           moba_data_version = 12,
                           completion_threshold=0.5,
                           return_items=TRUE,
                           consistent_items=TRUE,
                           transformations=NULL,
                           exclusions=NULL,
                           recursive=TRUE,
                           dx_owner="child",
                           out_format="list")

  save(mydata, file="./scratch_data/curated_dataset.RData")
  load(file="./scratch_data/curated_dataset.RData")
  
  myscaledata <- mydata$moba$scales
  myitemdata <- mydata$moba$items
  mynprdata <- mydata$npr
  mykuhrdata <- mydata$kuhr
  
# calculate Cronbach's alphas based on item data ####
  
# create lists by scales
  inat <- select(myitemdata, 195, 196, 197, 198, 199, 200, 201, 202, 203)
  hyp <- select(myitemdata, 213, 214, 215, 216, 217, 218, 219, 220, 221) 
  cd <- select(myitemdata, 230, 231, 232, 233, 234, 235, 236, 237)
  odd <- select(myitemdata, 246, 247, 248, 249, 250, 251, 252, 253)
  anx <- select(myitemdata, 259, 260, 261, 262, 263)
  dep <- select(myitemdata, 327, 328, 329, 330, 331, 332, 333, 334, 
                            335, 336, 337, 338, 339)
  ext1 <- select(myitemdata, 74, 75, 76, 77, 78, 79, 80, 81)
  ext2 <- select(myitemdata, 106, 107, 108, 109, 110, 111, 112, 113)
  ext3 <- select(myitemdata, 90, 91, 92, 93, 94, 95, 96, 97)
  int1 <- select(myitemdata, 119, 120, 121, 122, 123)
  int2 <- select(myitemdata, 139, 140, 141, 142, 143)
  int3 <- select(myitemdata, 129, 130, 131, 132, 133)
  
# create function for ordinal data
  ordinal_alpha <- function(x){
    psych::alpha(psych::polychoric(x)$rho)
  }
  
# calculate ordinal Cronbach's alpha
  ordinal_alpha(inat)
  ordinal_alpha(hyp)
  ordinal_alpha(cd)
  ordinal_alpha(odd)
  ordinal_alpha(anx)
  ordinal_alpha(dep)
  ordinal_alpha(ext1)
  ordinal_alpha(ext2)
  ordinal_alpha(ext3)
  ordinal_alpha(int1)
  ordinal_alpha(int2)
  ordinal_alpha(int3)
  
# select data and covariates for LGM ####
  scaledata <- myscaledata %>% 
    mutate(ind_id = paste0(preg_id,"_",BARN_NR))%>%
    select(ind_id, preg_id, m_id, BARN_NR, 
           int1 = cbcl_int_c_18m, ext1 = cbcl_ext_c_18m,
           int2 = cbcl_int_c_3yr, ext2 = cbcl_ext_c_3yr,
           int3 = cbcl_int_c_5yr, ext3 = cbcl_ext_c_5yr,
           inat_8yr = rsdbd_ina_c_8yr, hyp_8yr = rsdbd_hyp_c_8yr, 
           cd_8yr = rsdbd_cd_c_8yr, odd_8yr = rsdbd_odd_c_8yr, 
           dep_8yr = smfq_dep_c_8yr, anx_8yr = scared_anx_c_8yr, 
           pre_distress_f = scl_full_f_far, distress_m_18m = scl_full_m_18m, 
           distress_m_3yr = scl_full_m_3yr, pre_distress_m_q3 = scl_full_m_q3,
           pre_stress_m = adlifev_short_m_q3, 
           post_stress_m = adlifev_short_m_18m, post_dep_m = epds_short_m_6m, 
           relation_m = rs_full_m_18m, relation_f = rs_short_f_far,
           pre_distress_m_q1 = scl_short_m_q1, stress_f = adlifev_short_f_far,
           alc_prob_m = rapi_full_m_q1) %>%
    as.data.frame()
  
  itemdata <- myitemdata %>%
    mutate(ind_id = paste0(preg_id,"_",BARN_NR)) %>%
    select(ind_id, preg_id, m_id, BARN_NR, sex = KJONN_raw, 
           age1 = ALDERRETUR_S5_raw, age2 = ALDERRETUR_S6_raw, 
           age3 = AGE_RETURN_MTHS_Q5AAR_raw, parity = PARITET_5_raw,
           EE607_raw, EE608_raw, EE609_raw,                                     # maternal alcohol problems & use 1.5yr
           GG486_raw, GG487_raw, GG488_raw, FF244_raw, FF467_raw, FF468_raw,    # maternal alcohol use 3yr & paternal use
           AA1356_raw, CC1037_raw,  CC1040_raw,                                 # maternal & paternal smoking (at present)
           AA1315_raw, AA1316_raw, AA1124_raw, AA1126_raw) %>%                  # maternal & paternal income & education)
    as.data.frame()
  
  alldata <- scaledata %>%
    left_join(itemdata)

# bring relevant diagnosis variables into the main dataset ####
  diagdata <- mynprdata %>% 
    select(preg_id,m_id,BARN_NR,matches("received_dx")) %>% 
    full_join(mykuhrdata %>% 
                select(preg_id,m_id,BARN_NR,matches("received_dx"))) %>% 
    mutate(any_dx_dep = ifelse(received_dx_2x_dep_npr=="yes"|
                               received_dx_2x_dep_kuhr=="yes"|
                               received_dx_dep_npr=="yes"&
                               received_dx_dep_kuhr=="yes", 1, 0),
           any_dx_anx = ifelse(received_dx_2x_anx_npr=="yes"|
                               received_dx_2x_anx_kuhr=="yes"|
                               received_dx_anx_npr=="yes"&
                               received_dx_anx_kuhr=="yes", 1, 0),
           any_dx_dbd = ifelse(received_dx_2x_dbd_npr=="yes"|
                               received_dx_2x_dbd_kuhr=="yes"|
                               received_dx_dbd_npr=="yes"&
                               received_dx_dbd_kuhr=="yes", 1, 0),
           any_dx_adhd = ifelse(received_dx_2x_adhd_npr=="yes"|
                                received_dx_2x_adhd_kuhr=="yes"|
                                received_dx_adhd_npr=="yes"&
                                received_dx_adhd_kuhr=="yes", 1, 0))
  
  alldata <- alldata %>% 
    left_join(diagdata) 

# check for selective attrition ####
  seldata <- alldata %>%
    select(preg_id, BARN_NR, ind_id, int1, int3, ext1, ext3) %>%
    mutate(label_int = case_when(!is.na(int3) ~ "hasdata_int",
                                 is.na(int3) ~"nodata_int")) %>%
    mutate(label_ext = case_when(!is.na(ext3) ~ "hasdata_ext",
                                 is.na(ext3) ~"nodata_ext"))
  
  sa <- list()
  sa[["int1_int3"]] <- t.test(seldata$int1 ~ seldata$label_int)
  sa[["ext1_ext3"]] <- t.test(seldata$ext1 ~ seldata$label_ext)
  sa
  
# filter sex and recode to dummy variable (0 = male, 1 = female) ####
  alldata <- alldata %>% 
    mutate(sex = case_when(sex==1~0,sex==2~1))
  
# manipulate and prepare variables ####
  
# re-scale child age at questionnaire return
  alldata <- alldata %>%
    mutate(age1 = round(age1/30)) %>%
    mutate(age2 = round(age2/30))
  
# change response ordering of education variable (combine '2' & '3' below)
  alldata <- alldata %>%
    mutate(education_m = case_when(AA1124_raw==1~1,
                                   AA1124_raw==2~2, 
                                   AA1124_raw==3~2,
                                   AA1124_raw==4~3,
                                   AA1124_raw==5~4,
                                   AA1124_raw==6~5,
                                   AA1124_raw==0~NA_real_)) %>%
    mutate(education_f = case_when(AA1126_raw==1~1,
                                   AA1126_raw==2~2, 
                                   AA1126_raw==3~2,
                                   AA1126_raw==4~3,
                                   AA1126_raw==5~4,
                                   AA1126_raw==6~5,
                                   AA1126_raw==0~NA_real_)) %>%
    mutate_at(vars(matches("education")), list(~scale(.))) %>%
    rowwise() %>%
    mutate(education = mean(c(education_m,education_f),na.rm=TRUE)) %>%
    ungroup() %>% 
    mutate_all(~ifelse(is.nan(.), NA, .))
  
# original response categories
# 1) 9-year secondary school
# 2) 1-2 year high school
# 3) Vocational high school
# 4) 3-year high school general studies, junior college
# 5) Regional technical college, 4-year university degree (Bachelor’s degree, nurse, teacher, engineer)
# 6) University, technical college, more than 4 years (Master’s degree, medical doctor, PhD)
  
# recode income variable (set '0' & '8' to NA)
  alldata <- alldata %>%
    mutate(income_m = case_when(AA1315_raw==1~1,
                                AA1315_raw==2~2, 
                                AA1315_raw==3~3,
                                AA1315_raw==4~4,
                                AA1315_raw==5~5,
                                AA1315_raw==6~6,
                                AA1315_raw==7~7,
                                AA1315_raw==0~NA_real_)) %>%
    mutate(income_f = case_when(AA1316_raw==1~1,
                                AA1316_raw==2~2, 
                                AA1316_raw==3~3,
                                AA1316_raw==4~4,
                                AA1316_raw==5~5,
                                AA1316_raw==6~6,
                                AA1316_raw==7~7, 
                                AA1316_raw==8~NA_real_,
                                AA1316_raw==0~NA_real_)) %>%
    mutate_at(vars(matches("income")), list(~scale(.))) %>%
    rowwise() %>%
    mutate(income = mean(c(income_m,income_f),na.rm=TRUE)) %>%
    ungroup() %>% 
    mutate_all(~ifelse(is.nan(.), NA, .))
  
# recode relationship satisfaction to relationship problems
  alldata <- alldata %>%
    mutate(relation_m = relation_m*-1+25) %>%
    mutate(relation_f = relation_f*-1+25)

# alt. alcohol risk drinking score to avoid confounding by socioeconomic status
  alldata <- alldata %>%
    mutate(alc_risk_we_m_18m = case_when(EE608_raw==0~NA_real_,
                                         EE608_raw==1~3,
                                         EE608_raw==2~2,
                                         EE608_raw==3~1,
                                         EE608_raw==4~0.5,
                                         EE608_raw==5~0,
                                         EE608_raw==6~NA_real_),
           alc_risk_wd_m_18m = case_when(EE609_raw==0~NA_real_,
                                         EE609_raw==1~4,
                                         EE609_raw==2~3,
                                         EE609_raw==3~2,
                                         EE609_raw==4~1,
                                         EE609_raw==5~0,
                                         EE609_raw==6~NA_real_)) %>%
    mutate_at(vars(matches("alc_risk_w")), list(~scale(.))) %>%
    rowwise() %>%
    mutate(alc_risk_tot_m_18m = mean(c(alc_risk_we_m_18m,alc_risk_wd_m_18m),na.rm=TRUE)) %>%
    ungroup() %>% 
    mutate_all(~ifelse(is.nan(.), NA, .))
  
  alldata <- alldata %>%
    mutate(alc_risk_we_m_3yr = case_when(GG487_raw==0~NA_real_,
                                         GG487_raw==1~3,
                                         GG487_raw==2~2,
                                         GG487_raw==3~1,
                                         GG487_raw==4~0.5,
                                         GG487_raw==5~0,
                                         GG487_raw==6~NA_real_),
           alc_risk_wd_m_3yr = case_when(GG488_raw==0~NA_real_,
                                         GG488_raw==1~4,
                                         GG488_raw==2~3,
                                         GG488_raw==3~2,
                                         GG488_raw==4~1,
                                         GG488_raw==5~0,
                                         GG488_raw==6~NA_real_)) %>%
    mutate_at(vars(matches("alc_risk_w")), list(~scale(.))) %>%
    rowwise() %>%
    mutate(alc_risk_tot_m_3yr = mean(c(alc_risk_we_m_3yr,alc_risk_wd_m_3yr),na.rm=TRUE)) %>%
    ungroup() %>% 
    mutate_all(~ifelse(is.nan(.), NA, .))
  
  alldata <- alldata %>% 
    mutate_at(vars(matches("alc_risk_tot")), list(~scale(.))) %>%
    rowwise() %>%
    mutate(alc_risk_tot_m = mean(c(alc_risk_tot_m_18m,alc_risk_tot_m_3yr),na.rm=TRUE)) %>%
    ungroup() %>% 
    mutate_all(~ifelse(is.nan(.), NA, .))
  
  alldata <- alldata %>%
    mutate(alc_risk_we_f = case_when(FF467_raw==0~NA_real_,
                                     FF467_raw==1~3,
                                     FF467_raw==2~2,
                                     FF467_raw==3~1,
                                     FF467_raw==4~0.5,
                                     FF467_raw==5~0,
                                     FF467_raw==6~NA_real_),
           alc_risk_wd_f = case_when(FF468_raw==0~NA_real_,
                                     FF468_raw==1~4,
                                     FF468_raw==2~3,
                                     FF468_raw==3~2,
                                     FF468_raw==4~1,
                                     FF468_raw==5~0,
                                     FF468_raw==6~NA_real_)) %>%
    mutate_at(vars(matches("alc_risk_w")), list(~scale(.))) %>%
    rowwise() %>%
    mutate(alc_risk_tot_f = mean(c(alc_risk_we_f,alc_risk_wd_f),na.rm=TRUE)) %>%
    ungroup() %>% 
    mutate_all(~ifelse(is.nan(.), NA, .))
  
  alldata <- alldata %>%
    mutate(smoking_m_w17 = case_when(AA1356_raw==1~1,
                                     AA1356_raw==2~2,
                                     AA1356_raw==3~3,
                                     AA1356_raw==4~NA_real_,
                                     AA1356_raw==5~NA_real_,
                                     AA1356_raw==6~NA_real_,
                                     AA1356_raw==7~NA_real_),
           smoking_m_w30 = case_when(CC1037_raw==1~1,
                                     CC1037_raw==2~2,
                                     CC1037_raw==3~3,
                                     CC1037_raw==4~NA_real_,
                                     CC1037_raw==5~NA_real_,
                                     CC1037_raw==6~NA_real_,
                                     CC1037_raw==7~NA_real_),
           smoking_f = as.numeric(case_when(CC1040_raw==1~1,
                                            CC1040_raw==2~2,
                                            CC1040_raw==3~3,
                                            CC1040_raw==4~NA_real_,
                                            CC1040_raw==5~NA_real_,
                                            CC1040_raw==6~NA_real_,
                                            CC1040_raw==7~NA_real_))) %>%
    mutate_at(vars(matches("smoking")), list(~scale(.))) %>%
    rowwise() %>%
    mutate(smoking_m = mean(c(smoking_m_w17,smoking_m_w30),na.rm=TRUE)) %>%
    ungroup() %>% 
    mutate_all(~ifelse(is.nan(.), NA, .))
  
# create mean scores
  alldata <- alldata %>%  
  mutate_at(vars(matches("distress")), list(~scale(.))) %>%
    rowwise() %>%
    mutate(distress_m = mean(c(distress_m_18m,distress_m_3yr),na.rm=TRUE)) %>% 
    mutate(pre_distress_m = mean(c(pre_distress_m_q1,pre_distress_m_q3),na.rm=TRUE)) %>%
    ungroup() %>% 
    mutate_all(~ifelse(is.nan(.), NA, .))
  
# scale predictors
  alldata <- alldata %>% 
    mutate_at(vars(matches("distress_|pre_|post_|stress_|relation_
                           |income|smoking_|alc_")), list(~scale(.)))
  
  alldata <- alldata %>% 
    mutate_at(vars(matches("education")), list(~scale(.)))
  
# scale CBCL scores
  alldata <- alldata %>% 
    mutate_at(vars(matches("int|ext")), list(~scale(.)))
  
# create and scale differentiation scores
  alldata <- alldata %>%
    mutate(diff1 = scale(ext1 - int1)) %>%
    mutate(diff2 = scale(ext2 - int2)) %>%
    mutate(diff3 = scale(ext3 - int3))
  
# create and scale total scores
  alldata <- alldata %>%
    mutate(tot1 = scale(ext1 + int1)) %>%
    mutate(tot2 = scale(ext2 + int2)) %>%
    mutate(tot3 = scale(ext3 + int3))
  
# scale 8 year outcome variables
  alldata <- alldata %>% 
    mutate_at(vars(matches("8yr")), list(~scale(.)))
  
# select sample ####
  
  # check how many have information at all waves
  # note that this would be the same if using tot
  check <- alldata %>%
    filter(!is.na(diff1)&!is.na(diff2)&!is.na(diff3))
  
  # restrict data to individuals with non-missing
  # on at least one observed CBCL variable
  alldata <- alldata %>% 
    filter(!is.na(diff1)|!is.na(diff2)|!is.na(diff3))
  
# write out processed dataset ####
  
  alldata_new <- alldata %>%
    select(ind_id,preg_id,m_id,BARN_NR,sex,age1,age2,age3,parity,
           inat_8yr,hyp_8yr,cd_8yr,odd_8yr,dep_8yr,anx_8yr,diff1,
           diff2,diff3,tot1,tot2,tot3,int1,int2,int3,ext1,ext2,ext3,
           any_dx_dep,any_dx_anx,any_dx_dbd,any_dx_adhd,distress_m,
           pre_distress_f,pre_distress_m,pre_stress_m,post_stress_m,
           stress_f,post_dep_m,relation_m,relation_f,education,income,
           smoking_m,smoking_f,alc_prob_m,alc_risk_tot_f,alc_risk_tot_m)
  
  save(alldata_new, file = './data/processed_data_new.RData')
  
# check number of discordant siblings ####
  alldata_count <- alldata_new
  
  alldata_count[, 32:47][is.na(alldata_count[, 32:47])] <- NaN
  alldata_count[, "m_id"] <- as.factor(alldata_count[, "m_id"])
  alldata_grouped <- alldata_count %>% group_by(m_id)
  
  summarized_vars_2 <- alldata_grouped %>%
    summarise_all(funs(
      sum(ifelse(n_distinct(.)==2, 1, ifelse(n_distinct(.)>2, 2, 0)))
    ))
  
  discordant_siblings_count <- colSums(summarized_vars_2[, 32:47] == 1)
  discordant_siblings_count
  
  
  