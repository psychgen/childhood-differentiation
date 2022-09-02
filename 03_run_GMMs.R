# 03_run_GMMs.R

# The purpose of this script is to run growth mixture models based on
# LGMs using CBCL data (differentiation & total scores) Additionally:
# - We cluster on maternal id to account for sibling relatedness
# - We include covariates for class assignment: sex (KJONN), parity, age of q return 
# - We predict distal outcomes (diagnoses from NPR/KUHR): Anxiety, Depression, 
#   ADHD, and Disruptive behaviour disorders (DBD)
# 
# The prediction of distal outcome is done in step 3 of the manual ML 3-step
# approach (described in 10.1080/10705511.2019.1590146 and in Mplus technical
# documentation)


# To install Mplus Automation:

# for(each in list.files("//tsd-evs/p471/data/durable/common/software/mA_deps_repo/4.0")){
#   install.packages(paste0("//tsd-evs/p471/data/durable/common/software/mA_deps_repo/4.0/",each),
#                    repos=NULL,
#                    type = "binary")
# }


# First we load relevant packages

library(MplusAutomation)
library(tidyverse)

# Next read in the data 

load("./data/processed_data.RData")

# Rename some variables as Mplus cuts off > 8 characters
# Other pre-processing steps to ensure Mplus output is interpretable

###

dat <- alldata %>% 
   select(ind_id, m_id, sex, parity, BARN_NR, 
          inat_8yr, hyp_8yr, cd_8yr, odd_8yr, dep_8yr, anx_8yr, 
          f_predis = pre_distress_f, 
          m_prestr = pre_stress_m, 
          m_pststr = post_stress_m, 
          m_pstdep = post_dep_m, 
          m_rxp = relation_m, 
          f_rxp = relation_f, 
          age1, age2, age3, 
          f_stress = stress_f, 
          m_alcpr= alc_prob_m, 
          any_dx_dep, any_dx_anx, any_dx_dbd, any_dx_adhd,  
          mf_edu = education,  
          mf_inc = income,  
          m_alcrisk = alc_risk_tot_m, 
          f_alcrisk = alc_risk_tot_f, 
          f_smok = smoking_f, 
          m_smok = smoking_m, 
          m_dist = distress_m,
          m_predis = pre_distress_m, 
          diff1, diff2, diff3, tot1, tot2, tot3) %>% 
   rename_with(~str_remove(.,"any_")) %>% 
   mutate(ind_id=factor(ind_id),
          m_id=factor(m_id),
          across(where(is.numeric),as.numeric)) %>% 
   as.data.frame()
###
# Use MplusAutomation to make the data in an Mplus friendly format

prepareMplusData(dat, "//tsd-evs/p471/data/durable/projects/childhood_differentiation/scripts/mplus/data/data_for_mplus.dat")

# Run LGM in Mplus and compare with lavaan

runModels("./scripts/mplus/scripts/lgm", recursive =F,replaceOutfile="modifiedDate", Mplus_command = "C:/Program Files/Mplus/Mplus" )
mySummaries <- readModels("./scripts/mplus/scripts/lgm", recursive=TRUE, what="summaries")

# Run GMMs for class enumeration in Mplus, using externally created .inp scripts (STEP1)

filepath1 <- "./scripts/mplus/scripts/gmm/step1"

runModels(filepath1, recursive =F,replaceOutfile="modifiedDate", Mplus_command = "C:/Program Files/Mplus/Mplus" )

# Move the results files to the output file and clean up the scripts folder
file.copy(from=paste0(filepath1,"/",list.files(filepath1)[str_detect(list.files(filepath1),".inp",negate=T)]),
          to="./scripts/mplus/output/gmm/step1",
          overwrite = TRUE, recursive = F,
          copy.mode = TRUE)

junk <- dir(path=filepath1, pattern=".out|.dat") 
file.remove(paste0(filepath1,"/",junk))

# Read in output

mplusOutput <- readModels("./scripts/mplus/output/gmm/step1", recursive=FALSE)
mySummaries <- readModels("./scripts/mplus/output/gmm/step1", recursive=FALSE, what="summaries")

# condense results and summaries into one single df each

summaries <-
   lapply(mySummaries, function(x) {
      x$summaries
   }) %>%  bind_rows() %>% as.data.frame()


row.names(summaries) <- NULL

summaries <- summaries %>% 
   select(Model = Title, Parameters, LL, AIC,BIC,aBIC,AICC, Entropy,T11_VLMR_2xLLDiff,T11_VLMR_PValue  ) %>% 
   mutate(Model = str_sub(Model,start=-2)) %>% 
   filter(Model %in% c("1c","2c","3c","4c","5c","6c"))


summaries

write.table(summaries, "./scripts/mplus/output/gmm/step1/gmm_model_fitting.txt", quote = F, row.names = F)

# Section for exploring the model(s) visually
source("./scripts/03.2_adapted_functions_GMMs.R")

diff <- MyplotGrowthMixtures(mplusOutput, bw = FALSE, rawdata = FALSE,
                             estimated = TRUE, poly = FALSE, alpha_range = c(0, 0.1),
                             growth_variables = c("i1","s1"), time_scale = c(1.5,3,5), jitter_lines = NULL,
                             coefficients = "unstandardized") %>% 
   mutate(Pheno = "diff") 

trajs <- diff %>% 
   mutate(Class=factor(Class, ordered = F)) %>% 
   mutate_at(vars(Value,Lci,Uci,Time), list(~as.numeric(.))) %>% 
   droplevels()

p2 <- ggplot(data=trajs, aes(x=Time, y=Value, group=interaction(Class,Pheno)))+
   geom_hline(yintercept = 0, linetype=5, colour="grey70", size=1)+
   geom_point(aes( colour=Class),size=3.5,alpha=1)+
   geom_line( aes( colour=Class, linetype=Pheno),size=1.2,alpha=1)+
   geom_ribbon(aes(ymin=Lci,ymax=Uci, fill= Class), alpha=0.4)+
   facet_grid(Title~Class)+
   theme(axis.title.y = element_text(margin = margin(t = -20, r = 30, b = 0, l = 0)),
         axis.title.x = element_blank(),
         text =element_text(size = 19.5),
         axis.line = element_line(colour="grey70", size=0.7),
         axis.ticks = element_line(colour="grey70", size=0.7),
         axis.text = element_text(size=20),
         axis.text.x.top = element_text(angle= 0,size=12, margin = margin(t = 0, r = 0, b = , l = 0), hjust=0.5),
         panel.background = element_rect(fill = "grey95", colour = "white"),
         panel.grid.major = element_line(colour="grey90"),
         panel.grid.minor = element_line(colour="grey90"),
         legend.position = "top",
         legend.direction = "horizontal",
         legend.title = element_text(size =18, face="bold"),
         strip.background = element_rect(fill="white",colour="white"),
         strip.placement = "outside",
         strip.text.x = element_text(angle=0,colour="slateblue4", face="bold", size=14.5))+
   coord_cartesian(xlim=c(1.2,5.3), ylim=c(-3,3))+
   scale_x_continuous(name="Age (yrs)", breaks=c(1,2,3,4,5), position = "top")+
   scale_y_continuous(name="Mean differentiation on CBCL\n(+ behavioural, - emotional)")+
   guides(fill= FALSE,
          shape= FALSE, colour= guide_legend(title="Class",override.aes = list(size=6))) 

p2

####
## 4 class model is chosen based on modified (<5% to <1% due to sample size)
## decision rules
###
# Get and process the new data from the chosen model

newdat <- mplusOutput$gmm_4c.out$savedata 
prepareMplusData(newdat, "//tsd-evs/p471/data/durable/projects/childhood_differentiation/scripts/mplus/data/data_for_mplus_step3.dat")

# Save the results
class_counts <- mplusOutput$gmm_4c.out$class_counts
params <- mplusOutput$gmm_4c.out$parameters$unstandardized

# Extract the logits to specify measurement error for profile assignment (STEP2)
logits <- mplusOutput$gmm_4c.out$class_counts$logitProbs.mostLikely

# Put the logits into the right place in the step3 inp file
filepath2 <- "./scripts/mplus/scripts/gmm/step3"
fileConn<-file(paste0(filepath2,"/gmm_4c_step3.inp"), open="r")
tmp <- readLines(fileConn, warn=F)
close(fileConn)

tmp_new <- c(tmp[1:min(grep("!logits from step2 start",tmp))],
             paste0("!
%c#1%
[cc#1@",logits[4,1]," cc#2@",logits[4,2]," cc#3@",logits[4,3],"];
[DX_DEP$1] (dep1);
[DX_ANX$1] (anx1);
[DX_DBD$1] (dbd1);
[DX_ADHD$1] (adh1);
[INAT_8YR] (isx1);
[HYP_8YR] (hsx1);
[CD_8YR] (csx1);
[ODD_8YR] (osx1);
[DEP_8YR] (dsx1);
[ANX_8YR] (asx1);
%c#2%
[cc#1@",logits[1,1]," cc#2@",logits[1,2]," cc#3@",logits[1,3],"];
[DX_DEP$1] (dep2);
[DX_ANX$1] (anx2);
[DX_DBD$1] (dbd2);
[DX_ADHD$1] (adh2);
[INAT_8YR] (isx2);
[HYP_8YR] (hsx2);
[CD_8YR] (csx2);
[ODD_8YR] (osx2);
[DEP_8YR] (dsx2);
[ANX_8YR] (asx2);
%c#3%
[cc#1@",logits[2,1]," cc#2@",logits[2,2]," cc#3@",logits[2,3],"];
[DX_DEP$1] (dep3);
[DX_ANX$1] (anx3);
[DX_DBD$1] (dbd3);
[DX_ADHD$1] (adh3);
[INAT_8YR] (isx3);
[HYP_8YR] (hsx3);
[CD_8YR] (csx3);
[ODD_8YR] (osx3);
[DEP_8YR] (dsx3);
[ANX_8YR] (asx3);
%c#4%
[cc#1@",logits[3,1]," cc#2@",logits[3,2]," cc#3@",logits[3,3],"];
[DX_DEP$1] (dep4);
[DX_ANX$1] (anx4);
[DX_DBD$1] (dbd4);
[DX_ADHD$1] (adh4);
[INAT_8YR] (isx4);
[HYP_8YR] (hsx4);
[CD_8YR] (csx4);
[ODD_8YR] (osx4);
[DEP_8YR] (dsx4);
[ANX_8YR] (asx4);
!"), tmp[min(grep("!logits from step2 end",tmp)):length(tmp)])

if(length(tmp_new)>0){
   fileconnWT <-file(paste0(filepath2,"/gmm_4c_step3.inp"), open="wt")
   writeLines(tmp_new,fileconnWT)
   close(fileconnWT)
}

# Run the GMM + auxiliary model(s) with fixed class assignment and calculated error (STEP3)

runModels(filepath2, logFile="allgmm.txt", recursive =F,replaceOutfile="always", Mplus_command = "C:/Program Files/Mplus/Mplus" )

# Move the results files to the output file and clean up the scripts folder
file.copy(from=paste0(filepath2,"/",list.files(filepath2)[str_detect(list.files(filepath2),".inp",negate=T)]),
          to="./scripts/mplus/output/gmm/step3",
          overwrite = TRUE, recursive = F,
          copy.mode = TRUE)

junk <- dir(path=filepath2, pattern=".out|.dat") 
file.remove(paste0(filepath1,"/",junk))

mplusOutput2 <- readModels("./scripts/mplus/output/gmm/step3", recursive=FALSE)#

# Extract relevant results from the lengthy Mplus output file

# Class counts (class shifting from step 1 should be minimal)

ccounts_3step_final <- mplusOutput2$class_counts$mostLikely

# Main outcomes: diagnoses (ORs with CIs)

dx_res <- mplusOutput2$output[(grep("New/Additional Parameters",mplusOutput2$output)[[2]]+1):
                                 (grep("CONFIDENCE INTERVALS FOR THE LOGISTIC REGRESSION ODDS RATIO RESULTS",mplusOutput2$output)-3)]%>% 
   as.data.frame() %>% 
   `colnames<-`("all") %>% 
   filter(str_detect(all, "LOR")) %>% 
   separate(col=all, into=
               c("comparison","lower_0.5","logOR_lci","lower_5","logOR_est","upper_5","logOR_uci","upper_.5"), 
            sep = "      ")   %>% 
   select(comparison, logOR_est, logOR_lci, logOR_uci) %>%
   mutate_all(str_trim) %>% 
   mutate(across(.cols = c("logOR_est","logOR_lci","logOR_uci"),                      
                 function(x){ exp(as.numeric(x)) },.names = "exp{.col}" ),
          across(.cols = c("logOR_est","logOR_lci","logOR_uci"), as.numeric) ) %>% 
   rename_at(vars(contains("explog")), 
                 function(x){str_remove_all(x,"explog")}) %>% 
   mutate(comparison = str_remove_all(comparison,"L"))


# Covariate (sex and parity) effects on class and main outcomes
## Note, for effects on class, class #3 serves as the reference. Effects on the main
## outcomes are invariant across class

cov_res <- mplusOutput2$output[(grep("CONFIDENCE INTERVALS FOR THE LOGISTIC REGRESSION ODDS RATIO RESULTS",mplusOutput2$output)+3):
                                  (grep("CONFIDENCE INTERVALS FOR THE LOGISTIC REGRESSION ODDS RATIO RESULTS",mplusOutput2$output)+18)]%>% 
   as.data.frame() %>% 
   `colnames<-`("all") %>% 
   filter(str_detect(all, "SEX|PARITY")) %>% 
   mutate(outcome = rep(c("DX_DEP","DX_ANX","DX_DBD","DX_ADHD"),each=2)) %>% 
   rbind(mplusOutput2$output[(grep("Parameterization using Reference Class 3",mplusOutput2$output)[[4]]+1):
                                (grep("MODEL COMMAND WITH FINAL ESTIMATES USED AS STARTING VALUES",mplusOutput2$output)-3)]%>% as.data.frame() %>% 
            `colnames<-`("all") %>% 
            filter(str_detect(all, "SEX|PARITY")) %>%  
            mutate(outcome = rep(c("C#1","C#2","C#4"), each=2))) %>%
   separate(col=all, into=
               c("predictor","null","lower_0.5","OR_lci","lower_5","OR_est","upper_5","OR_uci","upper_.5"), 
            sep = "      ")   %>% 
   select(predictor,outcome, OR_est, OR_lci, OR_uci) %>%
   mutate_all(str_trim) %>% 
   mutate(across(.cols = c("OR_est","OR_lci","OR_uci"),                      #adds an equivalent column for each on the log scale
                 function(x){ log(as.numeric(x)) },.names = "log{.col}" ))   #for plotting


# Secondary outcomes: 8-year symptom scores (class-specific means with SEs)
# Note, outcomes are on whatever scale they went into the model on- usually standardized 

getIntercepts <- function(x){ mplusOutput2$output[(grep("Intercepts",mplusOutput2$output)[[x]]+1): #edited from 3 to 1
                                                     (grep("Intercepts",mplusOutput2$output)[[x]]+6)]}

sx_res <-  lapply(c(9:12), getIntercepts) %>%
   unlist() %>% 
   as.data.frame() %>% 
   `colnames<-`("all") %>% 
   mutate(class = rep(c("C#1","C#2","C#3","C#4"), each=6)) %>% 
   separate(col=all, into=
               c("outcome","lower_0.5","lci","lower_5","est","upper_5","uci","upper_.5"), 
            sep = "      ")   %>% 
   select(class,outcome, est, lci, uci) %>%
   mutate_all(str_trim)

# Per class intercept and slope values for predicted scores

is_perclass_4c <- trajs %>% 
   filter(Title =="gmm_4c") %>% 
   rename("predicted_score" = Value) %>%
   mutate(Class = case_when(Class == 2 ~ 3,
                            Class == 3 ~ 4,
                            Class == 4 ~ 1,
                            Class == 1 ~ 2)) %>%
   mutate(Class = as.factor(Class))

# Cross-tab diagnosis and class
newdat <- newdat %>%
   mutate(CC = case_when(CC == 2 ~ 3,
                         CC == 3 ~ 4,
                         CC == 4 ~ 1,
                         CC == 1 ~ 2))

table(newdat$CC, newdat$DX_DEP)
table(newdat$CC, newdat$DX_ANX)
table(newdat$CC, newdat$DX_DBD)
table(newdat$CC, newdat$DX_ADHD)

# Individual level per class probabilities

classprobs <- newdat %>% 
   select(IND_ID, CPROB1,CPROB2,CPROB3,CPROB4,CC) %>% 
   gather(class_prob, value, -IND_ID,-CC) %>% 
   mutate(class_prob = str_remove_all(class_prob, "CPROB"))


#Save out all output for plotting/reporting

save(ccounts_3step_final, dx_res, cov_res, sx_res,is_perclass_4c, classprobs, file="./output/gmm/gmm_diff.RData")
