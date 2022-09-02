
This contains the scripts needed to prepare data and run analyses 
for the paper: "Beyond the "p" factor: exploring the role of symptom 
differentiation across childhood in the emergence of psychopathology".

Authors: Adrian Dahl Askelund (jaaskelu@uio.no), Helga Ask, Eivind Ystrom,
Alexandra Havdahl, & Laurie John Hannigan (laurie.hannigan@bristol.ac.uk).

See "Code walkthrough" for a visual representation of the script structure.

00_data_preparation.R curates and selects the analytic dataset
based on questionnaire, covariate, and registry data and 
conducts manipulation and preparation of variables.

01.1_specify_basic_LGMs.R specifies basic latent growth models (LGMs)
with and without slope or age effects.

01_run_basic_LGMs.R runs latent growth models by sourcing 01.1.

02.1_specify_LGMs_for_validation.R specifies a p factor model, 
and LGMs with and without effects of intercept and/or slope on 8-y 
symptom domains (symptoms of inattention, hyperactivity, conduct 
disorder, oppositional defiant disorder, depression, and anxiety).

02_run_LGMs_for_validation.R runs validation LGMs by sourcing 02.1.

02.2_specify_combined_LGMs_for_validation.R specifies combined LGMs
of both differentiation and total difficulties, with and without 
effects of intercept and/or slope on 8-year symptom domains. It also
runs models with either only effects of differentiation or of total.

02.3_run_combined_LGMs_for_validation.R runs the combined validation 
LGMs by sourcing 02.2.

03_run_GMMs.R runs growth mixture models (GMMs) based on LGMs. In addition:
- We cluster on maternal id to account for sibling relatedness
- We include covariates for class assignment: sex, parity, age of q return 
- We predict distal outcomes (diagnoses from NPR/KUHR): Anxiety, Depression, 
  ADHD, and Disruptive behaviour disorders (DBD).
 
The prediction of distal outcome is done in step 3 of the manual ML 3-step
approach (described in 10.1080/10705511.2019.1590146 and in Mplus documentation).

03.1_visualise_GMMs.R visualises the GMMs based on results from 03_run_GMMs.R

04.1_specify_LGMs_with_predictors.R specifies LGMs with all predictors, 
with and without effects on intercept and/or slope. 

04_run_LGMs_with_predictors.R runs the LGMs including predictors by 
sourcing 04.1.

04.2_specify_combined_LGMs_with_predictors.R specifies combined LGMs
of both differentiation and total difficulties, with and without effects
on intercept and/or slope. 

04.3_run_combined_LGMs_with_predictors.R runs the LGMs by sourcing 04.2.

05_run_multilevel_LGMs.R runs the multilevel LGMs via Mplus using 
MplusAutomation - the aim is to provide within-family estimates of the 
effects calculated in 04.3, with adjustment for familial confounding. 

In addition, the 'mplus' folder contains scripts needed to run GMMs and 
multilevel LGMs, sourced by 03_run_GMMs.R and 05_run_multilevel_LGMs.R. 

