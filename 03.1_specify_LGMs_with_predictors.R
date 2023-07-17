# 03.1_specify_LGMs_with_predictors.R

### steps followed for differentiation and total:

# 1. specify full LGM with predictors
# 2. specify LGM predicting intercept only
# 3. specify LGM predicting slope only
  
  
# diff: LGM with covariates and predictors ####
  
  modelDm <-  
           '
# growth parameters (latent variables)
  
           i =~ 1*diff1 + 1*diff2 + 1*diff3
           s =~ -3.5*diff1 + -2*diff2 + 0*diff3
  
# obs variable variances
  
           diff1 ~~ diff1
           diff2 ~~ diff2
           diff3 ~~ diff3
  
# growth parameter (co)variances
    
           i ~~ i
           s ~~ s
           i ~~ s
  
# obs variable intercepts (fixed to 0)
  
           diff1 ~ 0*1
           diff2 ~ 0*1
           diff3 ~ 0*1
  
# growth parameter intercepts (freely estimated)
  
           i ~ 1 
           s ~ 1

# time-invariant covariates (effects)

           i + s ~ sex
           i + s ~ parity

# time-invariant covariates (intercept & variance)

           sex ~~ sex
           sex ~ 1
           parity ~~ parity
           parity ~ 1

# predictors (effects)

           i + s ~ distress_m + pre_distress_m + pre_distress_f + pre_stress_m + 
                   post_stress_m + stress_f + post_dep_m + relation_m + relation_f +
                   education + income + smoking_m + smoking_f + 
                   alc_risk_tot_m + alc_risk_tot_f + alc_prob_m
                   
# predictors (intercept & variance)

           distress_m ~~ distress_m
           pre_distress_m ~~ pre_distress_m
           pre_distress_f ~~ pre_distress_f
           pre_stress_m ~~ pre_stress_m
           post_stress_m ~~ post_stress_m
           stress_f ~~ stress_f
           post_dep_m ~~ post_dep_m
           relation_m ~~ relation_m
           relation_f ~~ relation_f
           education ~~ education
           income ~~ income
           smoking_m ~~ smoking_m
           smoking_f ~~ smoking_f
           alc_risk_tot_m ~~ alc_risk_tot_m
           alc_risk_tot_f ~~ alc_risk_tot_f
           alc_prob_m ~~ alc_prob_m
           distress_m ~ 1
           pre_distress_m ~ 1
           pre_distress_f ~ 1
           pre_stress_m ~ 1
           post_stress_m ~ 1
           stress_f ~ 1
           post_dep_m ~ 1
           relation_m ~ 1
           relation_f ~ 1
           education ~ 1
           income ~ 1
           smoking_m ~ 1
           smoking_f ~ 1
           alc_risk_tot_m ~ 1
           alc_risk_tot_f ~ 1
           alc_prob_m ~ 1

# covariances

           sex ~~ education
           sex ~~ income
           sex ~~ smoking_m
           sex ~~ smoking_f
           sex ~~ alc_risk_tot_m
           sex ~~ alc_risk_tot_f
           sex ~~ distress_m
           sex ~~ pre_distress_m
           sex ~~ pre_distress_f
           sex ~~ pre_stress_m
           sex ~~ post_stress_m
           sex ~~ stress_f
           sex ~~ post_dep_m
           sex ~~ relation_m
           sex ~~ relation_f
           sex ~~ alc_prob_m
           parity ~~ education
           parity ~~ income
           parity ~~ smoking_m
           parity ~~ smoking_f
           parity ~~ alc_risk_tot_m
           parity ~~ alc_risk_tot_f
           parity ~~ distress_m
           parity ~~ pre_distress_m
           parity ~~ pre_distress_f
           parity ~~ pre_stress_m
           parity ~~ post_stress_m
           parity ~~ stress_f
           parity ~~ post_dep_m
           parity ~~ relation_m
           parity ~~ relation_f
           parity ~~ alc_prob_m
           distress_m ~~ pre_distress_f
           distress_m ~~ pre_distress_m
           distress_m ~~ pre_stress_m
           distress_m ~~ post_stress_m
           distress_m ~~ stress_f
           distress_m ~~ post_dep_m
           distress_m ~~ relation_m
           distress_m ~~ relation_f
           distress_m ~~ education
           distress_m ~~ income
           distress_m ~~ smoking_m
           distress_m ~~ smoking_f
           distress_m ~~ alc_risk_tot_m
           distress_m ~~ alc_risk_tot_f
           distress_m ~~ alc_prob_m
           pre_distress_m ~~ pre_distress_f
           pre_distress_m ~~ pre_stress_m
           pre_distress_m ~~ post_stress_m
           pre_distress_m ~~ stress_f
           pre_distress_m ~~ post_dep_m
           pre_distress_m ~~ relation_m
           pre_distress_m ~~ relation_f
           pre_distress_m ~~ education
           pre_distress_m ~~ income
           pre_distress_m ~~ smoking_m
           pre_distress_m ~~ smoking_f
           pre_distress_m ~~ alc_risk_tot_m
           pre_distress_m ~~ alc_risk_tot_f
           pre_distress_m ~~ alc_prob_m
           pre_distress_f ~~ pre_stress_m
           pre_distress_f ~~ post_stress_m
           pre_distress_f ~~ stress_f
           pre_distress_f ~~ post_dep_m
           pre_distress_f ~~ relation_m
           pre_distress_f ~~ relation_f
           pre_distress_f ~~ education
           pre_distress_f ~~ income
           pre_distress_f ~~ smoking_m
           pre_distress_f ~~ smoking_f
           pre_distress_f ~~ alc_risk_tot_m
           pre_distress_f ~~ alc_risk_tot_f
           pre_distress_f ~~ alc_prob_m
           pre_stress_m ~~ post_stress_m
           pre_stress_m ~~ stress_f
           pre_stress_m ~~ post_dep_m
           pre_stress_m ~~ relation_m
           pre_stress_m ~~ relation_f
           pre_stress_m ~~ education
           pre_stress_m ~~ income
           pre_stress_m ~~ smoking_m
           pre_stress_m ~~ smoking_f
           pre_stress_m ~~ alc_risk_tot_m
           pre_stress_m ~~ alc_risk_tot_f
           pre_stress_m ~~ alc_prob_m
           post_stress_m ~~ stress_f
           post_stress_m ~~ post_dep_m
           post_stress_m ~~ relation_m
           post_stress_m ~~ relation_f
           post_stress_m ~~ education
           post_stress_m ~~ income
           post_stress_m ~~ smoking_m
           post_stress_m ~~ smoking_f
           post_stress_m ~~ alc_risk_tot_m
           post_stress_m ~~ alc_risk_tot_f
           post_stress_m ~~ alc_prob_m
           stress_f ~~ post_dep_m
           stress_f ~~ relation_m
           stress_f ~~ relation_f
           stress_f ~~ education
           stress_f ~~ income
           stress_f ~~ smoking_m
           stress_f ~~ smoking_f
           stress_f ~~ alc_risk_tot_m
           stress_f ~~ alc_risk_tot_f
           stress_f ~~ alc_prob_m
           post_dep_m ~~ relation_m
           post_dep_m ~~ relation_f
           post_dep_m ~~ education
           post_dep_m ~~ income
           post_dep_m ~~ smoking_m
           post_dep_m ~~ smoking_f
           post_dep_m ~~ alc_risk_tot_m
           post_dep_m ~~ alc_risk_tot_f
           post_dep_m ~~ alc_prob_m
           relation_m ~~ relation_f
           relation_m ~~ education
           relation_m ~~ income
           relation_m ~~ smoking_m
           relation_m ~~ smoking_f
           relation_m ~~ alc_risk_tot_m
           relation_m ~~ alc_risk_tot_f
           relation_m ~~ alc_prob_m
           relation_f ~~ education
           relation_f ~~ income
           relation_f ~~ smoking_m
           relation_f ~~ smoking_f
           relation_f ~~ alc_risk_tot_m
           relation_f ~~ alc_risk_tot_f
           relation_f ~~ alc_prob_m
           education ~~ income
           education ~~ smoking_m
           education ~~ smoking_f
           education ~~ alc_risk_tot_m
           education ~~ alc_risk_tot_f
           education ~~ alc_prob_m
           income ~~ smoking_m
           income ~~ smoking_f
           income ~~ alc_risk_tot_m
           income ~~ alc_risk_tot_f
           income ~~ alc_prob_m
           smoking_m ~~ smoking_f
           smoking_m ~~ alc_risk_tot_m
           smoking_m ~~ alc_risk_tot_f
           smoking_m ~~ alc_prob_m
           smoking_f ~~ alc_risk_tot_m
           smoking_f ~~ alc_risk_tot_f
           smoking_f ~~ alc_prob_m
           alc_risk_tot_m ~~ alc_risk_tot_f
           alc_risk_tot_m ~~ alc_prob_m
           alc_risk_tot_f ~~ alc_prob_m
           '
  
  
# diff: LGM with covariates and predictors, intercept only ####
  
  modelDi <-  
           '
# growth parameters (latent variables)
  
           i =~ 1*diff1 + 1*diff2 + 1*diff3
           s =~ -3.5*diff1 + -2*diff2 + 0*diff3
  
# obs variable variances
  
           diff1 ~~ diff1
           diff2 ~~ diff2
           diff3 ~~ diff3
  
# growth parameter (co)variances
  
           i ~~ i
           s ~~ s
           i ~~ s
  
# obs variable intercepts (fixed to 0)
  
           diff1 ~ 0*1
           diff2 ~ 0*1
           diff3 ~ 0*1
  
# growth parameter intercepts (freely estimated)
  
           i ~ 1 
           s ~ 1
  
# time-invariant covariates (effects)

           i ~ sex
           s ~ 0*sex
           i ~ parity
           s ~ 0*parity

# time-invariant covariates (intercept & variance)

           sex ~~ sex
           sex ~ 1
           parity ~~ parity
           parity ~ 1
  
# predictors (effects)
  
           i ~ distress_m + pre_distress_m + pre_distress_f + pre_stress_m + 
               post_stress_m + stress_f + post_dep_m + relation_m + relation_f +
               education + income + smoking_m + smoking_f + 
               alc_risk_tot_m + alc_risk_tot_f + alc_prob_m
           s ~ 0*distress_m + 0*pre_distress_m + 0*pre_distress_f + 0*pre_stress_m + 
               0*post_stress_m + 0*stress_f + 0*post_dep_m + 0*relation_m + 0*relation_f +
               0*education + 0*income + 0*smoking_m + 0*smoking_f + 
               0*alc_risk_tot_m + 0*alc_risk_tot_f + 0*alc_prob_m
                   
# predictors (intercept & variance)

           distress_m ~~ distress_m
           pre_distress_m ~~ pre_distress_m
           pre_distress_f ~~ pre_distress_f
           pre_stress_m ~~ pre_stress_m
           post_stress_m ~~ post_stress_m
           stress_f ~~ stress_f
           post_dep_m ~~ post_dep_m
           relation_m ~~ relation_m
           relation_f ~~ relation_f
           education ~~ education
           income ~~ income
           smoking_m ~~ smoking_m
           smoking_f ~~ smoking_f
           alc_risk_tot_m ~~ alc_risk_tot_m
           alc_risk_tot_f ~~ alc_risk_tot_f
           alc_prob_m ~~ alc_prob_m
           distress_m ~ 1
           pre_distress_m ~ 1
           pre_distress_f ~ 1
           pre_stress_m ~ 1
           post_stress_m ~ 1
           stress_f ~ 1
           post_dep_m ~ 1
           relation_m ~ 1
           relation_f ~ 1
           education ~ 1
           income ~ 1
           smoking_m ~ 1
           smoking_f ~ 1
           alc_risk_tot_m ~ 1
           alc_risk_tot_f ~ 1
           alc_prob_m ~ 1

# covariances

           sex ~~ education
           sex ~~ income
           sex ~~ smoking_m
           sex ~~ smoking_f
           sex ~~ alc_risk_tot_m
           sex ~~ alc_risk_tot_f
           sex ~~ distress_m
           sex ~~ pre_distress_m
           sex ~~ pre_distress_f
           sex ~~ pre_stress_m
           sex ~~ post_stress_m
           sex ~~ stress_f
           sex ~~ post_dep_m
           sex ~~ relation_m
           sex ~~ relation_f
           sex ~~ alc_prob_m
           parity ~~ education
           parity ~~ income
           parity ~~ smoking_m
           parity ~~ smoking_f
           parity ~~ alc_risk_tot_m
           parity ~~ alc_risk_tot_f
           parity ~~ distress_m
           parity ~~ pre_distress_m
           parity ~~ pre_distress_f
           parity ~~ pre_stress_m
           parity ~~ post_stress_m
           parity ~~ stress_f
           parity ~~ post_dep_m
           parity ~~ relation_m
           parity ~~ relation_f
           parity ~~ alc_prob_m
           distress_m ~~ pre_distress_f
           distress_m ~~ pre_distress_m
           distress_m ~~ pre_stress_m
           distress_m ~~ post_stress_m
           distress_m ~~ stress_f
           distress_m ~~ post_dep_m
           distress_m ~~ relation_m
           distress_m ~~ relation_f
           distress_m ~~ education
           distress_m ~~ income
           distress_m ~~ smoking_m
           distress_m ~~ smoking_f
           distress_m ~~ alc_risk_tot_m
           distress_m ~~ alc_risk_tot_f
           distress_m ~~ alc_prob_m
           pre_distress_m ~~ pre_distress_f
           pre_distress_m ~~ pre_stress_m
           pre_distress_m ~~ post_stress_m
           pre_distress_m ~~ stress_f
           pre_distress_m ~~ post_dep_m
           pre_distress_m ~~ relation_m
           pre_distress_m ~~ relation_f
           pre_distress_m ~~ education
           pre_distress_m ~~ income
           pre_distress_m ~~ smoking_m
           pre_distress_m ~~ smoking_f
           pre_distress_m ~~ alc_risk_tot_m
           pre_distress_m ~~ alc_risk_tot_f
           pre_distress_m ~~ alc_prob_m
           pre_distress_f ~~ pre_stress_m
           pre_distress_f ~~ post_stress_m
           pre_distress_f ~~ stress_f
           pre_distress_f ~~ post_dep_m
           pre_distress_f ~~ relation_m
           pre_distress_f ~~ relation_f
           pre_distress_f ~~ education
           pre_distress_f ~~ income
           pre_distress_f ~~ smoking_m
           pre_distress_f ~~ smoking_f
           pre_distress_f ~~ alc_risk_tot_m
           pre_distress_f ~~ alc_risk_tot_f
           pre_distress_f ~~ alc_prob_m
           pre_stress_m ~~ post_stress_m
           pre_stress_m ~~ stress_f
           pre_stress_m ~~ post_dep_m
           pre_stress_m ~~ relation_m
           pre_stress_m ~~ relation_f
           pre_stress_m ~~ education
           pre_stress_m ~~ income
           pre_stress_m ~~ smoking_m
           pre_stress_m ~~ smoking_f
           pre_stress_m ~~ alc_risk_tot_m
           pre_stress_m ~~ alc_risk_tot_f
           pre_stress_m ~~ alc_prob_m
           post_stress_m ~~ stress_f
           post_stress_m ~~ post_dep_m
           post_stress_m ~~ relation_m
           post_stress_m ~~ relation_f
           post_stress_m ~~ education
           post_stress_m ~~ income
           post_stress_m ~~ smoking_m
           post_stress_m ~~ smoking_f
           post_stress_m ~~ alc_risk_tot_m
           post_stress_m ~~ alc_risk_tot_f
           post_stress_m ~~ alc_prob_m
           stress_f ~~ post_dep_m
           stress_f ~~ relation_m
           stress_f ~~ relation_f
           stress_f ~~ education
           stress_f ~~ income
           stress_f ~~ smoking_m
           stress_f ~~ smoking_f
           stress_f ~~ alc_risk_tot_m
           stress_f ~~ alc_risk_tot_f
           stress_f ~~ alc_prob_m
           post_dep_m ~~ relation_m
           post_dep_m ~~ relation_f
           post_dep_m ~~ education
           post_dep_m ~~ income
           post_dep_m ~~ smoking_m
           post_dep_m ~~ smoking_f
           post_dep_m ~~ alc_risk_tot_m
           post_dep_m ~~ alc_risk_tot_f
           post_dep_m ~~ alc_prob_m
           relation_m ~~ relation_f
           relation_m ~~ education
           relation_m ~~ income
           relation_m ~~ smoking_m
           relation_m ~~ smoking_f
           relation_m ~~ alc_risk_tot_m
           relation_m ~~ alc_risk_tot_f
           relation_m ~~ alc_prob_m
           relation_f ~~ education
           relation_f ~~ income
           relation_f ~~ smoking_m
           relation_f ~~ smoking_f
           relation_f ~~ alc_risk_tot_m
           relation_f ~~ alc_risk_tot_f
           relation_f ~~ alc_prob_m
           education ~~ income
           education ~~ smoking_m
           education ~~ smoking_f
           education ~~ alc_risk_tot_m
           education ~~ alc_risk_tot_f
           education ~~ alc_prob_m
           income ~~ smoking_m
           income ~~ smoking_f
           income ~~ alc_risk_tot_m
           income ~~ alc_risk_tot_f
           income ~~ alc_prob_m
           smoking_m ~~ smoking_f
           smoking_m ~~ alc_risk_tot_m
           smoking_m ~~ alc_risk_tot_f
           smoking_m ~~ alc_prob_m
           smoking_f ~~ alc_risk_tot_m
           smoking_f ~~ alc_risk_tot_f
           smoking_f ~~ alc_prob_m
           alc_risk_tot_m ~~ alc_risk_tot_f
           alc_risk_tot_m ~~ alc_prob_m
           alc_risk_tot_f ~~ alc_prob_m
          '
  
  
  
# diff: LGM with covariates and predictors, slope only ####
  
  modelDs <-  
           '
# growth parameters (latent variables)
  
           i =~ 1*diff1 + 1*diff2 + 1*diff3
           s =~ -3.5*diff1 + -2*diff2 + 0*diff3
  
# obs variable variances
  
           diff1 ~~ diff1
           diff2 ~~ diff2
           diff3 ~~ diff3
  
# growth parameter (co)variances
  
           i ~~ i
           s ~~ s
           i ~~ s
  
# obs variable intercepts (fixed to 0)
  
           diff1 ~ 0*1
           diff2 ~ 0*1
           diff3 ~ 0*1
  
# growth parameter intercepts (freely estimated)
  
           i ~ 1 
           s ~ 1
  
# time-invariant covariates (effects)

           i ~ 0*sex
           s ~ sex
           i ~ 0*parity
           s ~ parity

# time-invariant covariates (intercept & variance)

           sex ~~ sex
           sex ~ 1
           parity ~~ parity
           parity ~ 1
  
# predictors (effects)
  
           s ~ distress_m + pre_distress_m + pre_distress_f + pre_stress_m + 
               post_stress_m + stress_f + post_dep_m + relation_m + relation_f +
               education + income + smoking_m + smoking_f + 
               alc_risk_tot_m + alc_risk_tot_f + alc_prob_m
           i ~ 0*distress_m + 0*pre_distress_m + 0*pre_distress_f + 0*pre_stress_m + 
               0*post_stress_m + 0*stress_f + 0*post_dep_m + 0*relation_m + 0*relation_f +
               0*education + 0*income + 0*smoking_m + 0*smoking_f + 
               0*alc_risk_tot_m + 0*alc_risk_tot_f + 0*alc_prob_m
                   
# predictors (intercept & variance)

           distress_m ~~ distress_m
           pre_distress_m ~~ pre_distress_m
           pre_distress_f ~~ pre_distress_f
           pre_stress_m ~~ pre_stress_m
           post_stress_m ~~ post_stress_m
           stress_f ~~ stress_f
           post_dep_m ~~ post_dep_m
           relation_m ~~ relation_m
           relation_f ~~ relation_f
           education ~~ education
           income ~~ income
           smoking_m ~~ smoking_m
           smoking_f ~~ smoking_f
           alc_risk_tot_m ~~ alc_risk_tot_m
           alc_risk_tot_f ~~ alc_risk_tot_f
           alc_prob_m ~~ alc_prob_m
           distress_m ~ 1
           pre_distress_m ~ 1
           pre_distress_f ~ 1
           pre_stress_m ~ 1
           post_stress_m ~ 1
           stress_f ~ 1
           post_dep_m ~ 1
           relation_m ~ 1
           relation_f ~ 1
           education ~ 1
           income ~ 1
           smoking_m ~ 1
           smoking_f ~ 1
           alc_risk_tot_m ~ 1
           alc_risk_tot_f ~ 1
           alc_prob_m ~ 1

# covariances

           sex ~~ education
           sex ~~ income
           sex ~~ smoking_m
           sex ~~ smoking_f
           sex ~~ alc_risk_tot_m
           sex ~~ alc_risk_tot_f
           sex ~~ distress_m
           sex ~~ pre_distress_m
           sex ~~ pre_distress_f
           sex ~~ pre_stress_m
           sex ~~ post_stress_m
           sex ~~ stress_f
           sex ~~ post_dep_m
           sex ~~ relation_m
           sex ~~ relation_f
           sex ~~ alc_prob_m
           parity ~~ education
           parity ~~ income
           parity ~~ smoking_m
           parity ~~ smoking_f
           parity ~~ alc_risk_tot_m
           parity ~~ alc_risk_tot_f
           parity ~~ distress_m
           parity ~~ pre_distress_m
           parity ~~ pre_distress_f
           parity ~~ pre_stress_m
           parity ~~ post_stress_m
           parity ~~ stress_f
           parity ~~ post_dep_m
           parity ~~ relation_m
           parity ~~ relation_f
           parity ~~ alc_prob_m
           distress_m ~~ pre_distress_f
           distress_m ~~ pre_distress_m
           distress_m ~~ pre_stress_m
           distress_m ~~ post_stress_m
           distress_m ~~ stress_f
           distress_m ~~ post_dep_m
           distress_m ~~ relation_m
           distress_m ~~ relation_f
           distress_m ~~ education
           distress_m ~~ income
           distress_m ~~ smoking_m
           distress_m ~~ smoking_f
           distress_m ~~ alc_risk_tot_m
           distress_m ~~ alc_risk_tot_f
           distress_m ~~ alc_prob_m
           pre_distress_m ~~ pre_distress_f
           pre_distress_m ~~ pre_stress_m
           pre_distress_m ~~ post_stress_m
           pre_distress_m ~~ stress_f
           pre_distress_m ~~ post_dep_m
           pre_distress_m ~~ relation_m
           pre_distress_m ~~ relation_f
           pre_distress_m ~~ education
           pre_distress_m ~~ income
           pre_distress_m ~~ smoking_m
           pre_distress_m ~~ smoking_f
           pre_distress_m ~~ alc_risk_tot_m
           pre_distress_m ~~ alc_risk_tot_f
           pre_distress_m ~~ alc_prob_m
           pre_distress_f ~~ pre_stress_m
           pre_distress_f ~~ post_stress_m
           pre_distress_f ~~ stress_f
           pre_distress_f ~~ post_dep_m
           pre_distress_f ~~ relation_m
           pre_distress_f ~~ relation_f
           pre_distress_f ~~ education
           pre_distress_f ~~ income
           pre_distress_f ~~ smoking_m
           pre_distress_f ~~ smoking_f
           pre_distress_f ~~ alc_risk_tot_m
           pre_distress_f ~~ alc_risk_tot_f
           pre_distress_f ~~ alc_prob_m
           pre_stress_m ~~ post_stress_m
           pre_stress_m ~~ stress_f
           pre_stress_m ~~ post_dep_m
           pre_stress_m ~~ relation_m
           pre_stress_m ~~ relation_f
           pre_stress_m ~~ education
           pre_stress_m ~~ income
           pre_stress_m ~~ smoking_m
           pre_stress_m ~~ smoking_f
           pre_stress_m ~~ alc_risk_tot_m
           pre_stress_m ~~ alc_risk_tot_f
           pre_stress_m ~~ alc_prob_m
           post_stress_m ~~ stress_f
           post_stress_m ~~ post_dep_m
           post_stress_m ~~ relation_m
           post_stress_m ~~ relation_f
           post_stress_m ~~ education
           post_stress_m ~~ income
           post_stress_m ~~ smoking_m
           post_stress_m ~~ smoking_f
           post_stress_m ~~ alc_risk_tot_m
           post_stress_m ~~ alc_risk_tot_f
           post_stress_m ~~ alc_prob_m
           stress_f ~~ post_dep_m
           stress_f ~~ relation_m
           stress_f ~~ relation_f
           stress_f ~~ education
           stress_f ~~ income
           stress_f ~~ smoking_m
           stress_f ~~ smoking_f
           stress_f ~~ alc_risk_tot_m
           stress_f ~~ alc_risk_tot_f
           stress_f ~~ alc_prob_m
           post_dep_m ~~ relation_m
           post_dep_m ~~ relation_f
           post_dep_m ~~ education
           post_dep_m ~~ income
           post_dep_m ~~ smoking_m
           post_dep_m ~~ smoking_f
           post_dep_m ~~ alc_risk_tot_m
           post_dep_m ~~ alc_risk_tot_f
           post_dep_m ~~ alc_prob_m
           relation_m ~~ relation_f
           relation_m ~~ education
           relation_m ~~ income
           relation_m ~~ smoking_m
           relation_m ~~ smoking_f
           relation_m ~~ alc_risk_tot_m
           relation_m ~~ alc_risk_tot_f
           relation_m ~~ alc_prob_m
           relation_f ~~ education
           relation_f ~~ income
           relation_f ~~ smoking_m
           relation_f ~~ smoking_f
           relation_f ~~ alc_risk_tot_m
           relation_f ~~ alc_risk_tot_f
           relation_f ~~ alc_prob_m
           education ~~ income
           education ~~ smoking_m
           education ~~ smoking_f
           education ~~ alc_risk_tot_m
           education ~~ alc_risk_tot_f
           education ~~ alc_prob_m
           income ~~ smoking_m
           income ~~ smoking_f
           income ~~ alc_risk_tot_m
           income ~~ alc_risk_tot_f
           income ~~ alc_prob_m
           smoking_m ~~ smoking_f
           smoking_m ~~ alc_risk_tot_m
           smoking_m ~~ alc_risk_tot_f
           smoking_m ~~ alc_prob_m
           smoking_f ~~ alc_risk_tot_m
           smoking_f ~~ alc_risk_tot_f
           smoking_f ~~ alc_prob_m
           alc_risk_tot_m ~~ alc_risk_tot_f
           alc_risk_tot_m ~~ alc_prob_m
           alc_risk_tot_f ~~ alc_prob_m
           '
  
  
# tot: LGM with covariates and predictors ####
  
  modelTm <-  
           '
# growth parameters (latent variables)
  
           i =~ 1*tot1 + 1*tot2 + 1*tot3
           s =~ -3.5*tot1 + -2*tot2 + 0*tot3
  
# obs variable variances
  
           tot1 ~~ tot1
           tot2 ~~ tot2
           tot3 ~~ tot3
  
# growth parameter (co)variances
    
           i ~~ i
           s ~~ s
           i ~~ s
  
# obs variable intercepts (fixed to 0)
  
           tot1 ~ 0*1
           tot2 ~ 0*1
           tot3 ~ 0*1
  
# growth parameter intercepts (freely estimated)
  
           i ~ 1 
           s ~ 1

# time-invariant covariate (effects)

           i + s ~ sex
           i + s ~ parity
           
# time-varying covariates (effects for observed variables)

           tot1 ~ age1
           tot2 ~ age2
           tot3 ~ age3

# time-invariant covariate (intercept & variance)
  
           sex ~~ sex
           sex ~ 1
           parity ~ parity
           parity ~ 1
          
# time-varying covariates (intercept & variance)

           age1 ~~ age1
           age2 ~~ age2
           age3 ~~ age3
           age1 ~ 1
           age2 ~ 1
           age3 ~ 1

# covariances among covariates

           age1 ~~ age2
           age2 ~~ age3
           age1 ~~ age3
           age1 ~~ sex
           age2 ~~ sex
           age3 ~~ sex
           age1 ~~ parity
           age2 ~~ parity
           age3 ~~ parity

# predictors (effects)

           i + s ~ distress_m + pre_distress_m + pre_distress_f + pre_stress_m + 
                   post_stress_m + stress_f + post_dep_m + relation_m + relation_f +
                   education + income + smoking_m + smoking_f + 
                   alc_risk_tot_m + alc_risk_tot_f + alc_prob_m
                   
# predictors (intercept & variance)

           distress_m ~~ distress_m
           pre_distress_m ~~ pre_distress_m
           pre_distress_f ~~ pre_distress_f
           pre_stress_m ~~ pre_stress_m
           post_stress_m ~~ post_stress_m
           stress_f ~~ stress_f
           post_dep_m ~~ post_dep_m
           relation_m ~~ relation_m
           relation_f ~~ relation_f
           education ~~ education
           income ~~ income
           smoking_m ~~ smoking_m
           smoking_f ~~ smoking_f
           alc_risk_tot_m ~~ alc_risk_tot_m
           alc_risk_tot_f ~~ alc_risk_tot_f
           alc_prob_m ~~ alc_prob_m
           distress_m ~ 1
           pre_distress_m ~ 1
           pre_distress_f ~ 1
           pre_stress_m ~ 1
           post_stress_m ~ 1
           stress_f ~ 1
           post_dep_m ~ 1
           relation_m ~ 1
           relation_f ~ 1
           education ~ 1
           income ~ 1
           smoking_m ~ 1
           smoking_f ~ 1
           alc_risk_tot_m ~ 1
           alc_risk_tot_f ~ 1
           alc_prob_m ~ 1

# covariances

           sex ~~ education
           sex ~~ income
           sex ~~ smoking_m
           sex ~~ smoking_f
           sex ~~ alc_risk_tot_m
           sex ~~ alc_risk_tot_f
           sex ~~ distress_m
           sex ~~ pre_distress_m
           sex ~~ pre_distress_f
           sex ~~ pre_stress_m
           sex ~~ post_stress_m
           sex ~~ stress_f
           sex ~~ post_dep_m
           sex ~~ relation_m
           sex ~~ relation_f
           sex ~~ alc_prob_m
           parity ~~ education
           parity ~~ income
           parity ~~ smoking_m
           parity ~~ smoking_f
           parity ~~ alc_risk_tot_m
           parity ~~ alc_risk_tot_f
           parity ~~ distress_m
           parity ~~ pre_distress_m
           parity ~~ pre_distress_f
           parity ~~ pre_stress_m
           parity ~~ post_stress_m
           parity ~~ stress_f
           parity ~~ post_dep_m
           parity ~~ relation_m
           parity ~~ relation_f
           parity ~~ alc_prob_m
           distress_m ~~ pre_distress_f
           distress_m ~~ pre_distress_m
           distress_m ~~ pre_stress_m
           distress_m ~~ post_stress_m
           distress_m ~~ stress_f
           distress_m ~~ post_dep_m
           distress_m ~~ relation_m
           distress_m ~~ relation_f
           distress_m ~~ education
           distress_m ~~ income
           distress_m ~~ smoking_m
           distress_m ~~ smoking_f
           distress_m ~~ alc_risk_tot_m
           distress_m ~~ alc_risk_tot_f
           distress_m ~~ alc_prob_m
           pre_distress_m ~~ pre_distress_f
           pre_distress_m ~~ pre_stress_m
           pre_distress_m ~~ post_stress_m
           pre_distress_m ~~ stress_f
           pre_distress_m ~~ post_dep_m
           pre_distress_m ~~ relation_m
           pre_distress_m ~~ relation_f
           pre_distress_m ~~ education
           pre_distress_m ~~ income
           pre_distress_m ~~ smoking_m
           pre_distress_m ~~ smoking_f
           pre_distress_m ~~ alc_risk_tot_m
           pre_distress_m ~~ alc_risk_tot_f
           pre_distress_m ~~ alc_prob_m
           pre_distress_f ~~ pre_stress_m
           pre_distress_f ~~ post_stress_m
           pre_distress_f ~~ stress_f
           pre_distress_f ~~ post_dep_m
           pre_distress_f ~~ relation_m
           pre_distress_f ~~ relation_f
           pre_distress_f ~~ education
           pre_distress_f ~~ income
           pre_distress_f ~~ smoking_m
           pre_distress_f ~~ smoking_f
           pre_distress_f ~~ alc_risk_tot_m
           pre_distress_f ~~ alc_risk_tot_f
           pre_distress_f ~~ alc_prob_m
           pre_stress_m ~~ post_stress_m
           pre_stress_m ~~ stress_f
           pre_stress_m ~~ post_dep_m
           pre_stress_m ~~ relation_m
           pre_stress_m ~~ relation_f
           pre_stress_m ~~ education
           pre_stress_m ~~ income
           pre_stress_m ~~ smoking_m
           pre_stress_m ~~ smoking_f
           pre_stress_m ~~ alc_risk_tot_m
           pre_stress_m ~~ alc_risk_tot_f
           pre_stress_m ~~ alc_prob_m
           post_stress_m ~~ stress_f
           post_stress_m ~~ post_dep_m
           post_stress_m ~~ relation_m
           post_stress_m ~~ relation_f
           post_stress_m ~~ education
           post_stress_m ~~ income
           post_stress_m ~~ smoking_m
           post_stress_m ~~ smoking_f
           post_stress_m ~~ alc_risk_tot_m
           post_stress_m ~~ alc_risk_tot_f
           post_stress_m ~~ alc_prob_m
           stress_f ~~ post_dep_m
           stress_f ~~ relation_m
           stress_f ~~ relation_f
           stress_f ~~ education
           stress_f ~~ income
           stress_f ~~ smoking_m
           stress_f ~~ smoking_f
           stress_f ~~ alc_risk_tot_m
           stress_f ~~ alc_risk_tot_f
           stress_f ~~ alc_prob_m
           post_dep_m ~~ relation_m
           post_dep_m ~~ relation_f
           post_dep_m ~~ education
           post_dep_m ~~ income
           post_dep_m ~~ smoking_m
           post_dep_m ~~ smoking_f
           post_dep_m ~~ alc_risk_tot_m
           post_dep_m ~~ alc_risk_tot_f
           post_dep_m ~~ alc_prob_m
           relation_m ~~ relation_f
           relation_m ~~ education
           relation_m ~~ income
           relation_m ~~ smoking_m
           relation_m ~~ smoking_f
           relation_m ~~ alc_risk_tot_m
           relation_m ~~ alc_risk_tot_f
           relation_m ~~ alc_prob_m
           relation_f ~~ education
           relation_f ~~ income
           relation_f ~~ smoking_m
           relation_f ~~ smoking_f
           relation_f ~~ alc_risk_tot_m
           relation_f ~~ alc_risk_tot_f
           relation_f ~~ alc_prob_m
           education ~~ income
           education ~~ smoking_m
           education ~~ smoking_f
           education ~~ alc_risk_tot_m
           education ~~ alc_risk_tot_f
           education ~~ alc_prob_m
           income ~~ smoking_m
           income ~~ smoking_f
           income ~~ alc_risk_tot_m
           income ~~ alc_risk_tot_f
           income ~~ alc_prob_m
           smoking_m ~~ smoking_f
           smoking_m ~~ alc_risk_tot_m
           smoking_m ~~ alc_risk_tot_f
           smoking_m ~~ alc_prob_m
           smoking_f ~~ alc_risk_tot_m
           smoking_f ~~ alc_risk_tot_f
           smoking_f ~~ alc_prob_m
           alc_risk_tot_m ~~ alc_risk_tot_f
           alc_risk_tot_m ~~ alc_prob_m
           alc_risk_tot_f ~~ alc_prob_m
           '
  
  
  
# tot: LGM with covariates and predictors, intercept only ####
  
  modelTi <-  
           '
# growth parameters (latent variables)
  
           i =~ 1*tot1 + 1*tot2 + 1*tot3
           s =~ -3.5*tot1 + -2*tot2 + 0*tot3
  
# obs variable variances
  
           tot1 ~~ tot1
           tot2 ~~ tot2
           tot3 ~~ tot3
  
# growth parameter (co)variances
  
           i ~~ i
           s ~~ s
           i ~~ s
  
# obs variable intercepts (fixed to 0)
  
           tot1 ~ 0*1
           tot2 ~ 0*1
           tot3 ~ 0*1
  
# growth parameter intercepts (freely estimated)
  
           i ~ 1 
           s ~ 1
  
# time-invariant covariate (effects)
  
           i ~ sex
           s ~ 0*sex
           i ~ parity
           s ~ 0*parity

# time-varying covariates (effects for observed variables)

           tot1 ~ age1
           tot2 ~ age2
           tot3 ~ age3

# time-invariant covariate (intercept & variance)
  
           sex ~~ sex
           sex ~ 1
           parity ~~ parity
           parity ~ 1
          
# time-varying covariates (intercept & variance)

           age1 ~~ age1
           age2 ~~ age2
           age3 ~~ age3
           age1 ~ 1
           age2 ~ 1
           age3 ~ 1

# covariances among covariates

           age1 ~~ age2
           age2 ~~ age3
           age1 ~~ age3
           age1 ~~ sex
           age2 ~~ sex
           age3 ~~ sex
           age1 ~~ parity
           age2 ~~ parity
           age3 ~~ parity
  
# predictors (effects)
  
           i ~ distress_m + pre_distress_m + pre_distress_f + pre_stress_m + 
               post_stress_m + stress_f + post_dep_m + relation_m + relation_f +
               education + income + smoking_m + smoking_f + 
               alc_risk_tot_m + alc_risk_tot_f + alc_prob_m
           s ~ 0*distress_m + 0*pre_distress_m + 0*pre_distress_f + 0*pre_stress_m + 
               0*post_stress_m + 0*stress_f + 0*post_dep_m + 0*relation_m + 0*relation_f +
               0*education + 0*income + 0*smoking_m + 0*smoking_f + 
               0*alc_risk_tot_m + 0*alc_risk_tot_f + 0*alc_prob_m
                   
# predictors (intercept & variance)

           distress_m ~~ distress_m
           pre_distress_m ~~ pre_distress_m
           pre_distress_f ~~ pre_distress_f
           pre_stress_m ~~ pre_stress_m
           post_stress_m ~~ post_stress_m
           stress_f ~~ stress_f
           post_dep_m ~~ post_dep_m
           relation_m ~~ relation_m
           relation_f ~~ relation_f
           education ~~ education
           income ~~ income
           smoking_m ~~ smoking_m
           smoking_f ~~ smoking_f
           alc_risk_tot_m ~~ alc_risk_tot_m
           alc_risk_tot_f ~~ alc_risk_tot_f
           alc_prob_m ~~ alc_prob_m
           distress_m ~ 1
           pre_distress_m ~ 1
           pre_distress_f ~ 1
           pre_stress_m ~ 1
           post_stress_m ~ 1
           stress_f ~ 1
           post_dep_m ~ 1
           relation_m ~ 1
           relation_f ~ 1
           education ~ 1
           income ~ 1
           smoking_m ~ 1
           smoking_f ~ 1
           alc_risk_tot_m ~ 1
           alc_risk_tot_f ~ 1
           alc_prob_m ~ 1

# covariances

           sex ~~ education
           sex ~~ income
           sex ~~ smoking_m
           sex ~~ smoking_f
           sex ~~ alc_risk_tot_m
           sex ~~ alc_risk_tot_f
           sex ~~ distress_m
           sex ~~ pre_distress_m
           sex ~~ pre_distress_f
           sex ~~ pre_stress_m
           sex ~~ post_stress_m
           sex ~~ stress_f
           sex ~~ post_dep_m
           sex ~~ relation_m
           sex ~~ relation_f
           sex ~~ alc_prob_m
           parity ~~ education
           parity ~~ income
           parity ~~ smoking_m
           parity ~~ smoking_f
           parity ~~ alc_risk_tot_m
           parity ~~ alc_risk_tot_f
           parity ~~ distress_m
           parity ~~ pre_distress_m
           parity ~~ pre_distress_f
           parity ~~ pre_stress_m
           parity ~~ post_stress_m
           parity ~~ stress_f
           parity ~~ post_dep_m
           parity ~~ relation_m
           parity ~~ relation_f
           parity ~~ alc_prob_m
           distress_m ~~ pre_distress_f
           distress_m ~~ pre_distress_m
           distress_m ~~ pre_stress_m
           distress_m ~~ post_stress_m
           distress_m ~~ stress_f
           distress_m ~~ post_dep_m
           distress_m ~~ relation_m
           distress_m ~~ relation_f
           distress_m ~~ education
           distress_m ~~ income
           distress_m ~~ smoking_m
           distress_m ~~ smoking_f
           distress_m ~~ alc_risk_tot_m
           distress_m ~~ alc_risk_tot_f
           distress_m ~~ alc_prob_m
           pre_distress_m ~~ pre_distress_f
           pre_distress_m ~~ pre_stress_m
           pre_distress_m ~~ post_stress_m
           pre_distress_m ~~ stress_f
           pre_distress_m ~~ post_dep_m
           pre_distress_m ~~ relation_m
           pre_distress_m ~~ relation_f
           pre_distress_m ~~ education
           pre_distress_m ~~ income
           pre_distress_m ~~ smoking_m
           pre_distress_m ~~ smoking_f
           pre_distress_m ~~ alc_risk_tot_m
           pre_distress_m ~~ alc_risk_tot_f
           pre_distress_m ~~ alc_prob_m
           pre_distress_f ~~ pre_stress_m
           pre_distress_f ~~ post_stress_m
           pre_distress_f ~~ stress_f
           pre_distress_f ~~ post_dep_m
           pre_distress_f ~~ relation_m
           pre_distress_f ~~ relation_f
           pre_distress_f ~~ education
           pre_distress_f ~~ income
           pre_distress_f ~~ smoking_m
           pre_distress_f ~~ smoking_f
           pre_distress_f ~~ alc_risk_tot_m
           pre_distress_f ~~ alc_risk_tot_f
           pre_distress_f ~~ alc_prob_m
           pre_stress_m ~~ post_stress_m
           pre_stress_m ~~ stress_f
           pre_stress_m ~~ post_dep_m
           pre_stress_m ~~ relation_m
           pre_stress_m ~~ relation_f
           pre_stress_m ~~ education
           pre_stress_m ~~ income
           pre_stress_m ~~ smoking_m
           pre_stress_m ~~ smoking_f
           pre_stress_m ~~ alc_risk_tot_m
           pre_stress_m ~~ alc_risk_tot_f
           pre_stress_m ~~ alc_prob_m
           post_stress_m ~~ stress_f
           post_stress_m ~~ post_dep_m
           post_stress_m ~~ relation_m
           post_stress_m ~~ relation_f
           post_stress_m ~~ education
           post_stress_m ~~ income
           post_stress_m ~~ smoking_m
           post_stress_m ~~ smoking_f
           post_stress_m ~~ alc_risk_tot_m
           post_stress_m ~~ alc_risk_tot_f
           post_stress_m ~~ alc_prob_m
           stress_f ~~ post_dep_m
           stress_f ~~ relation_m
           stress_f ~~ relation_f
           stress_f ~~ education
           stress_f ~~ income
           stress_f ~~ smoking_m
           stress_f ~~ smoking_f
           stress_f ~~ alc_risk_tot_m
           stress_f ~~ alc_risk_tot_f
           stress_f ~~ alc_prob_m
           post_dep_m ~~ relation_m
           post_dep_m ~~ relation_f
           post_dep_m ~~ education
           post_dep_m ~~ income
           post_dep_m ~~ smoking_m
           post_dep_m ~~ smoking_f
           post_dep_m ~~ alc_risk_tot_m
           post_dep_m ~~ alc_risk_tot_f
           post_dep_m ~~ alc_prob_m
           relation_m ~~ relation_f
           relation_m ~~ education
           relation_m ~~ income
           relation_m ~~ smoking_m
           relation_m ~~ smoking_f
           relation_m ~~ alc_risk_tot_m
           relation_m ~~ alc_risk_tot_f
           relation_m ~~ alc_prob_m
           relation_f ~~ education
           relation_f ~~ income
           relation_f ~~ smoking_m
           relation_f ~~ smoking_f
           relation_f ~~ alc_risk_tot_m
           relation_f ~~ alc_risk_tot_f
           relation_f ~~ alc_prob_m
           education ~~ income
           education ~~ smoking_m
           education ~~ smoking_f
           education ~~ alc_risk_tot_m
           education ~~ alc_risk_tot_f
           education ~~ alc_prob_m
           income ~~ smoking_m
           income ~~ smoking_f
           income ~~ alc_risk_tot_m
           income ~~ alc_risk_tot_f
           income ~~ alc_prob_m
           smoking_m ~~ smoking_f
           smoking_m ~~ alc_risk_tot_m
           smoking_m ~~ alc_risk_tot_f
           smoking_m ~~ alc_prob_m
           smoking_f ~~ alc_risk_tot_m
           smoking_f ~~ alc_risk_tot_f
           smoking_f ~~ alc_prob_m
           alc_risk_tot_m ~~ alc_risk_tot_f
           alc_risk_tot_m ~~ alc_prob_m
           alc_risk_tot_f ~~ alc_prob_m
           '
  
  
  
# tot: LGM with covariates and predictors, slope only ####
  
  modelTs <-  
           '
# growth parameters (latent variables)
  
           i =~ 1*tot1 + 1*tot2 + 1*tot3
           s =~ -3.5*tot1 + -2*tot2 + 0*tot3
  
# obs variable variances
  
           tot1 ~~ tot1
           tot2 ~~ tot2
           tot3 ~~ tot3
  
# growth parameter (co)variances
  
           i ~~ i
           s ~~ s
           i ~~ s
  
# obs variable intercepts (fixed to 0)
  
           tot1 ~ 0*1
           tot2 ~ 0*1
           tot3 ~ 0*1
  
# growth parameter intercepts (freely estimated)
  
           i ~ 1 
           s ~ 1
  
# time-invariant covariate (effects)
  
           i ~ 0*sex
           s ~ sex
           i ~ 0*parity
           s ~ parity

# time-varying covariates (effects for observed variables)

           tot1 ~ age1
           tot2 ~ age2
           tot3 ~ age3

# time-invariant covariate (intercept & variance)
  
           sex ~~ sex
           sex ~ 1
           parity ~~ parity
           parity ~ 1
          
# time-varying covariates (intercept & variance)

           age1 ~~ age1
           age2 ~~ age2
           age3 ~~ age3
           age1 ~ 1
           age2 ~ 1
           age3 ~ 1

# covariances among covariates

           age1 ~~ age2
           age2 ~~ age3
           age1 ~~ age3
           age1 ~~ sex
           age2 ~~ sex
           age3 ~~ sex
           age1 ~~ parity
           age2 ~~ parity
           age3 ~~ parity
  
# predictors (effects)
  
           s ~ distress_m + pre_distress_m + pre_distress_f + pre_stress_m + 
               post_stress_m + stress_f + post_dep_m + relation_m + relation_f +
               education + income + smoking_m + smoking_f + 
               alc_risk_tot_m + alc_risk_tot_f + alc_prob_m
           i ~ 0*distress_m + 0*pre_distress_m + 0*pre_distress_f + 0*pre_stress_m + 
               0*post_stress_m + 0*stress_f + 0*post_dep_m + 0*relation_m + 0*relation_f +
               0*education + 0*income + 0*smoking_m + 0*smoking_f + 
               0*alc_risk_tot_m + 0*alc_risk_tot_f + 0*alc_prob_m
                   
# predictors (intercept & variance)

           distress_m ~~ distress_m
           pre_distress_m ~~ pre_distress_m
           pre_distress_f ~~ pre_distress_f
           pre_stress_m ~~ pre_stress_m
           post_stress_m ~~ post_stress_m
           stress_f ~~ stress_f
           post_dep_m ~~ post_dep_m
           relation_m ~~ relation_m
           relation_f ~~ relation_f
           education ~~ education
           income ~~ income
           smoking_m ~~ smoking_m
           smoking_f ~~ smoking_f
           alc_risk_tot_m ~~ alc_risk_tot_m
           alc_risk_tot_f ~~ alc_risk_tot_f
           alc_prob_m ~~ alc_prob_m
           distress_m ~ 1
           pre_distress_m ~ 1
           pre_distress_f ~ 1
           pre_stress_m ~ 1
           post_stress_m ~ 1
           stress_f ~ 1
           post_dep_m ~ 1
           relation_m ~ 1
           relation_f ~ 1
           education ~ 1
           income ~ 1
           smoking_m ~ 1
           smoking_f ~ 1
           alc_risk_tot_m ~ 1
           alc_risk_tot_f ~ 1
           alc_prob_m ~ 1

# covariances

           sex ~~ education
           sex ~~ income
           sex ~~ smoking_m
           sex ~~ smoking_f
           sex ~~ alc_risk_tot_m
           sex ~~ alc_risk_tot_f
           sex ~~ distress_m
           sex ~~ pre_distress_m
           sex ~~ pre_distress_f
           sex ~~ pre_stress_m
           sex ~~ post_stress_m
           sex ~~ stress_f
           sex ~~ post_dep_m
           sex ~~ relation_m
           sex ~~ relation_f
           sex ~~ alc_prob_m
           parity ~~ education
           parity ~~ income
           parity ~~ smoking_m
           parity ~~ smoking_f
           parity ~~ alc_risk_tot_m
           parity ~~ alc_risk_tot_f
           parity ~~ distress_m
           parity ~~ pre_distress_m
           parity ~~ pre_distress_f
           parity ~~ pre_stress_m
           parity ~~ post_stress_m
           parity ~~ stress_f
           parity ~~ post_dep_m
           parity ~~ relation_m
           parity ~~ relation_f
           parity ~~ alc_prob_m
           distress_m ~~ pre_distress_f
           distress_m ~~ pre_distress_m
           distress_m ~~ pre_stress_m
           distress_m ~~ post_stress_m
           distress_m ~~ stress_f
           distress_m ~~ post_dep_m
           distress_m ~~ relation_m
           distress_m ~~ relation_f
           distress_m ~~ education
           distress_m ~~ income
           distress_m ~~ smoking_m
           distress_m ~~ smoking_f
           distress_m ~~ alc_risk_tot_m
           distress_m ~~ alc_risk_tot_f
           distress_m ~~ alc_prob_m
           pre_distress_m ~~ pre_distress_f
           pre_distress_m ~~ pre_stress_m
           pre_distress_m ~~ post_stress_m
           pre_distress_m ~~ stress_f
           pre_distress_m ~~ post_dep_m
           pre_distress_m ~~ relation_m
           pre_distress_m ~~ relation_f
           pre_distress_m ~~ education
           pre_distress_m ~~ income
           pre_distress_m ~~ smoking_m
           pre_distress_m ~~ smoking_f
           pre_distress_m ~~ alc_risk_tot_m
           pre_distress_m ~~ alc_risk_tot_f
           pre_distress_m ~~ alc_prob_m
           pre_distress_f ~~ pre_stress_m
           pre_distress_f ~~ post_stress_m
           pre_distress_f ~~ stress_f
           pre_distress_f ~~ post_dep_m
           pre_distress_f ~~ relation_m
           pre_distress_f ~~ relation_f
           pre_distress_f ~~ education
           pre_distress_f ~~ income
           pre_distress_f ~~ smoking_m
           pre_distress_f ~~ smoking_f
           pre_distress_f ~~ alc_risk_tot_m
           pre_distress_f ~~ alc_risk_tot_f
           pre_distress_f ~~ alc_prob_m
           pre_stress_m ~~ post_stress_m
           pre_stress_m ~~ stress_f
           pre_stress_m ~~ post_dep_m
           pre_stress_m ~~ relation_m
           pre_stress_m ~~ relation_f
           pre_stress_m ~~ education
           pre_stress_m ~~ income
           pre_stress_m ~~ smoking_m
           pre_stress_m ~~ smoking_f
           pre_stress_m ~~ alc_risk_tot_m
           pre_stress_m ~~ alc_risk_tot_f
           pre_stress_m ~~ alc_prob_m
           post_stress_m ~~ stress_f
           post_stress_m ~~ post_dep_m
           post_stress_m ~~ relation_m
           post_stress_m ~~ relation_f
           post_stress_m ~~ education
           post_stress_m ~~ income
           post_stress_m ~~ smoking_m
           post_stress_m ~~ smoking_f
           post_stress_m ~~ alc_risk_tot_m
           post_stress_m ~~ alc_risk_tot_f
           post_stress_m ~~ alc_prob_m
           stress_f ~~ post_dep_m
           stress_f ~~ relation_m
           stress_f ~~ relation_f
           stress_f ~~ education
           stress_f ~~ income
           stress_f ~~ smoking_m
           stress_f ~~ smoking_f
           stress_f ~~ alc_risk_tot_m
           stress_f ~~ alc_risk_tot_f
           stress_f ~~ alc_prob_m
           post_dep_m ~~ relation_m
           post_dep_m ~~ relation_f
           post_dep_m ~~ education
           post_dep_m ~~ income
           post_dep_m ~~ smoking_m
           post_dep_m ~~ smoking_f
           post_dep_m ~~ alc_risk_tot_m
           post_dep_m ~~ alc_risk_tot_f
           post_dep_m ~~ alc_prob_m
           relation_m ~~ relation_f
           relation_m ~~ education
           relation_m ~~ income
           relation_m ~~ smoking_m
           relation_m ~~ smoking_f
           relation_m ~~ alc_risk_tot_m
           relation_m ~~ alc_risk_tot_f
           relation_m ~~ alc_prob_m
           relation_f ~~ education
           relation_f ~~ income
           relation_f ~~ smoking_m
           relation_f ~~ smoking_f
           relation_f ~~ alc_risk_tot_m
           relation_f ~~ alc_risk_tot_f
           relation_f ~~ alc_prob_m
           education ~~ income
           education ~~ smoking_m
           education ~~ smoking_f
           education ~~ alc_risk_tot_m
           education ~~ alc_risk_tot_f
           education ~~ alc_prob_m
           income ~~ smoking_m
           income ~~ smoking_f
           income ~~ alc_risk_tot_m
           income ~~ alc_risk_tot_f
           income ~~ alc_prob_m
           smoking_m ~~ smoking_f
           smoking_m ~~ alc_risk_tot_m
           smoking_m ~~ alc_risk_tot_f
           smoking_m ~~ alc_prob_m
           smoking_f ~~ alc_risk_tot_m
           smoking_f ~~ alc_risk_tot_f
           smoking_f ~~ alc_prob_m
           alc_risk_tot_m ~~ alc_risk_tot_f
           alc_risk_tot_m ~~ alc_prob_m
           alc_risk_tot_f ~~ alc_prob_m
           '
  
  

