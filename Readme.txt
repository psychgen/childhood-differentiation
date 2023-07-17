
This folder contains the scripts needed to prepare data and run analyses 
for the paper: "Exploring the differentiation of behavioural and emotional problems across childhood: a prospective longitudinal cohort study".

Authors: Adrian Dahl Askelund (jaaskelu@uio.no), Helga Ask, Eivind Ystrom, Alexandra Havdahl, & Laurie John Hannigan (laurie.hannigan@bristol.ac.uk).

00_data_preparation.R curates and selects the analytic dataset
based on questionnaire, covariate, and registry data and 
does manipulation and preparation of variables.

01_run_basic_LGMs.R runs latent growth models by sourcing 01.1.

01.1_specify_basic_LGMs.R specifies basic latent growth models (LGMs)
with and without slope or age effects.

02_run_LGMs_for_validation_sympt.R runs validation LGMs with symptom outcomes by sourcing 02.1.

02.1_specify_LGMs_for_validation.R specifies a p factor model, 
and LGMs with and without effects of intercept and/or slope on 8-y 
symptom domains (symptoms of inattention, hyperactivity, conduct 
disorder, oppositional defiant disorder, depression, and anxiety).

02.2_specify_combined_LGMs_for_validation.R specifies combined LGMs
of both differentiation and total difficulties, with and without 
effects of intercept and/or slope on 8-year symptom domains. It also
runs models with either only effects of differentiation or of total.

02.3_run_combined_LGMs_for_validation.R runs the combined validation 
LGMs by sourcing 02.2.

02.4_specify_LGMs_for_validation_diags.R specifies LGMs with and without effects of intercept and/or slope on diagnoses after age 8 (anxiety disorders, depressive disorders, ADHD, and disruptive behaviour disorders).

02.5_run_LGMs_for_validation_diags.R runs validation LGMs with dx outcomes by sourcing 02.4. 

03_run_LGMs_with_predictors.R runs the LGMs including predictors by 
sourcing 03.1.

03.1_specify_LGMs_with_predictors.R specifies LGMs with all predictors, 
with and without effects on intercept and/or slope. 

03.2_specify_combined_LGMs_with_predictors.R specifies combined LGMs
of both differentiation and total difficulties, with and without effects
on intercept and/or slope. 

03.3_run_combined_LGMs_with_predictors.R runs the LGMs by sourcing 03.2.

03.4_run_onelevel_LGMs.R runs constrained multilevel LGMs (i.e., 1-level) with early life exposures predicting differentiation and total problems.

03.5_run_onelevel_LGMs_sensitivity.R runs constrained multilevel LGMs (i.e., 1-level) with early life exposures predicting externalising and internalising problems based on the CBCL subscales, as a sensitivity analysis. 

04_run_multilevel_LGMs.R runs the multilevel LGMs via Mplus using 
MplusAutomation - the aim is to provide within-family estimates of the 
effects calculated in 03.4, with adjustment for familial confounding. 

04.1_run_multilevel_LGMs_sensitivity.R runs the multilevel LGMs via Mplus - the aim is to provide within-family estimates of the effects early life exposures on the externalizing and internalizing CBCL subscales calculated in 03.5, with adjustment for familial confounding.

04.2_explore_adjust_multilevel_LGMs.R explores the issue of bias due to selection into participating in MoBa with more than one offspring, and seeks adjustments (using inverse probability weighting) to mitigate this issue, also sourcing the 99_utils.R script.

04.3_mi_sib_ipsw.R is used to run multiple imputation on the computing cluster to facilitate adjustment for bias from repeated participation.

04.4_multilevel_LGM_plots.R takes a) the 'sibling adjusted' effects of early exposures from the multilevel LGMs obtained in the script '04_run_multilevel_LGMs.R', b) the 'sibling unadjusted' effects from the script '03.4_run_onelevel_LGMs.R', and c) the 'observational' effects from the script '03_run_LGMs_with_predictors.R', and plots them together.

05.1_generate_ippw.R creates inverse probability of participation weights (IPPW) for use in sensitivity analyses testing the impact of adjusting for non-random participation, also sourcing the 99_utils.R script.

05.2_ippw_validation_analyses.R runs inverse probability of participation weighted and unweighted validation models and plots the results. 

05.3_fullsample_LGM_ippw.R runs IPPW LGM models on the full sample.

05.4_sibonly_LGM_ippw.R runs IPPW LGM models on the sibling sub-sample.

05.5_sibonly_multilevel_LGM_ippw.R runs IPPW multilevel LGMs on the sibling sub-sample. It then pulls unweighted and weighted results from previous scripts to produce Tables S13-S20 with results from the IPPW sensitivity analyses of early life exposures on differentiation and total problems. 

In addition, the 'mplus' folder contains scripts needed to run the constrained (1-level) LGMs and multilevel LGMs. 

