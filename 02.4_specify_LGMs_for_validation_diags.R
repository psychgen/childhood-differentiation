# 02.4_specify_LGMs_for_validation_diags.R

# intercept and slope effects for both diff and tot

model <-  
          '
# growth parameters (latent variables)
  
          i1 =~ 1*diff1 + 1*diff2 + 1*diff3
          s1 =~ -3.5*diff1 + -2*diff2 + 0*diff3
          i2 =~ 1*tot1 + 1*tot2 + 1*tot3
          s2 =~ -3.5*tot1 + -2*tot2 + 0*tot3
  
# obs variable variances

          diff1 ~~ diff1
          diff2 ~~ diff2
          diff3 ~~ diff3
          tot1 ~~ tot1
          tot2 ~~ tot2
          tot3 ~~ tot3
  
# growth parameter (co)variances
  
          i1 ~~ i1
          s1 ~~ s1
          i1 ~~ s1
          i2 ~~ i2
          s2 ~~ s2
          i2 ~~ s2
          i1 ~~ i2
          i1 ~~ s2
          s1 ~~ i2
          s1 ~~ s2
  
# obs variable intercepts (fixed to 0)
  
          diff1 ~ 0*1
          diff2 ~ 0*1
          diff3 ~ 0*1
          tot1 ~ 0*1
          tot2 ~ 0*1
          tot3 ~ 0*1
  
# growth parameter intercepts (freely estimated)
  
          i1 ~ 1 
          s1 ~ 1
          i2 ~ 1 
          s2 ~ 1
          
# time-invariant covariate (effects)
  
          i1 + s1 ~ sex
          i2 + s2 ~ sex
          i1 + s1 ~ parity
          i2 + s2 ~ parity
           
# time-varying covariates (effects for observed variables)

          diff1 ~ age1
          diff2 ~ age2
          diff3 ~ age3
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
          '
  
  
  
  