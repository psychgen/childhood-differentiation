# 02.1_specify_LGMs_for_validation.R

### steps followed:

# 0. specify p factor model
# 1. specify LGM with effects of both growth factors
# 2. specify LGM with effects of intercept only
# 3. specify LGM with effects of slope only
# 4. specify LGM with no effects of growth factors

# 0 - p factor model - poor model fit shows the need to deviate from
# pre-registration in terms of how outcomes are handled

  model0 <-
   
          '
# p factor (latent variable)
  
          p =~ inat_8yr + hyp_8yr + cd_8yr + odd_8yr + dep_8yr + anx_8yr

# observed variable variances
  
          inat_8yr ~~ inat_8yr
          hyp_8yr ~~ hyp_8yr
          cd_8yr ~~ cd_8yr
          odd_8yr ~~ odd_8yr
          dep_8yr ~~ dep_8yr
          anx_8yr ~~ anx_8yr
 
# means of outcomes (freely estimated)
  
          inat_8yr ~ 1
          hyp_8yr ~ 1
          cd_8yr ~ 1
          odd_8yr ~ 1
          dep_8yr ~ 1
          anx_8yr ~ 1
  
# covariances of outcomes (fixed to zero)
  
          inat_8yr ~~ 0*hyp_8yr + 0*cd_8yr + 0*odd_8yr + 0*dep_8yr + 0*anx_8yr
          hyp_8yr ~~ 0*cd_8yr + 0*odd_8yr + 0*dep_8yr + 0*anx_8yr
          cd_8yr ~~ 0*odd_8yr + 0*dep_8yr + 0*anx_8yr
          odd_8yr ~~ 0*dep_8yr + 0*anx_8yr
          dep_8yr ~~ 0*anx_8yr
          '
  
  

# 1 - both intercept and slope effects 

  model1 <-  
          '
# growth parameters (latent variables)
  
          i =~ 1*cbcl1 + 1*cbcl2 + 1*cbcl3
          s =~ -3.5*cbcl1 + -2*cbcl2 + 0*cbcl3
  
# obs variable variances
  
          cbcl1 ~~ cbcl1
          cbcl2 ~~ cbcl2
          cbcl3 ~~ cbcl3
          inat_8yr ~~ inat_8yr
          hyp_8yr ~~ hyp_8yr
          cd_8yr ~~ cd_8yr
          odd_8yr ~~ odd_8yr
          dep_8yr ~~ dep_8yr
          anx_8yr ~~ anx_8yr
  
# growth parameter (co)variances
  
          i ~~ i
          s ~~ s
          i ~~ s
  
# obs variable intercepts (fixed to 0)
  
          cbcl1 ~ 0*1
          cbcl2 ~ 0*1
          cbcl3 ~ 0*1
  
# growth parameter intercepts (freely estimated)
  
          i ~ 1 
          s ~ 1
  
# outcomes
  
          inat_8yr + hyp_8yr + cd_8yr + odd_8yr + dep_8yr + anx_8yr ~ i + s
  
# means of outcomes (freely estimated)
  
          inat_8yr ~ 1
          hyp_8yr ~ 1
          cd_8yr ~ 1
          odd_8yr ~ 1
          dep_8yr ~ 1
          anx_8yr ~ 1
  
# covariances of outcomes (freely estimated)
  
          inat_8yr ~~ hyp_8yr + cd_8yr + odd_8yr + dep_8yr + anx_8yr
          hyp_8yr ~~ cd_8yr + odd_8yr + dep_8yr + anx_8yr
          cd_8yr ~~ odd_8yr + dep_8yr + anx_8yr
          odd_8yr ~~ dep_8yr + anx_8yr
          dep_8yr ~~ anx_8yr
  
# covariances of cbcl scores with outcomes (fixed to zero)
  
          cbcl1 ~~ 0*inat_8yr + 0*hyp_8yr + 0*cd_8yr + 0*odd_8yr + 0*dep_8yr + 0*anx_8yr
          cbcl2 ~~ 0*inat_8yr + 0*hyp_8yr + 0*cd_8yr + 0*odd_8yr + 0*dep_8yr + 0*anx_8yr
          cbcl3 ~~ 0*inat_8yr + 0*hyp_8yr + 0*cd_8yr + 0*odd_8yr + 0*dep_8yr + 0*anx_8yr
          
# time-invariant covariates (effects)
  
          i + s ~ sex
          i + s ~ parity

# time-invariant covariates (covariances with outcomes)

          sex ~~ inat_8yr + hyp_8yr + cd_8yr + odd_8yr + dep_8yr + anx_8yr
          parity ~~ inat_8yr + hyp_8yr + cd_8yr + odd_8yr + dep_8yr + anx_8yr
           
# time-varying covariates (effects for observed variables)

          cbcl1 ~ age1
          cbcl2 ~ age2
          cbcl3 ~ age3
          
# time-invariant covariates (intercept & variance)
  
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
  
  
# 2 - intercept only ####
  
  model2 <-  
          '
# growth parameters (latent variables)
  
          i =~ 1*cbcl1 + 1*cbcl2 + 1*cbcl3
          s =~ -3.5*cbcl1 + -2*cbcl2 + 0*cbcl3
  
# obs variable variances
  
          cbcl1 ~~ cbcl1
          cbcl2 ~~ cbcl2
          cbcl3 ~~ cbcl3
          inat_8yr ~~ inat_8yr
          hyp_8yr ~~ hyp_8yr
          cd_8yr ~~ cd_8yr
          odd_8yr ~~ odd_8yr
          dep_8yr ~~ dep_8yr
          anx_8yr ~~ anx_8yr
  
# growth parameter (co)variances
  
          i ~~ i
          s ~~ s
          i ~~ s
  
# obs variable intercepts (fixed to 0)
  
          cbcl1 ~ 0*1
          cbcl2 ~ 0*1
          cbcl3 ~ 0*1
  
# growth parameter intercepts (freely estimated)
  
          i ~ 1 
          s ~ 1
  
# outcomes
  
          inat_8yr + hyp_8yr + cd_8yr + odd_8yr + dep_8yr + anx_8yr ~ i + 0*s
          
  
# means of outcomes (freely estimated)
  
          inat_8yr ~ 1
          hyp_8yr ~ 1
          cd_8yr ~ 1
          odd_8yr ~ 1
          dep_8yr ~ 1
          anx_8yr ~ 1
  
# covariances of outcomes (fixed to zero)
  
          inat_8yr ~~ hyp_8yr + cd_8yr + odd_8yr + dep_8yr + anx_8yr
          hyp_8yr ~~ cd_8yr + odd_8yr + dep_8yr + anx_8yr
          cd_8yr ~~ odd_8yr + dep_8yr + anx_8yr
          odd_8yr ~~ dep_8yr + anx_8yr
          dep_8yr ~~ anx_8yr
  
# covariances of cbcl scores with outcomes (fixed to zero)
  
          cbcl1 ~~ 0*inat_8yr + 0*hyp_8yr + 0*cd_8yr + 0*odd_8yr + 0*dep_8yr + 0*anx_8yr
          cbcl2 ~~ 0*inat_8yr + 0*hyp_8yr + 0*cd_8yr + 0*odd_8yr + 0*dep_8yr + 0*anx_8yr
          cbcl3 ~~ 0*inat_8yr + 0*hyp_8yr + 0*cd_8yr + 0*odd_8yr + 0*dep_8yr + 0*anx_8yr
          
# time-invariant covariates (effects)
  
          i + s ~ sex
          i + s ~ parity

# time-invariant covariates (covariances with outcomes)

          sex ~~ inat_8yr + hyp_8yr + cd_8yr + odd_8yr + dep_8yr + anx_8yr
          parity ~~ inat_8yr + hyp_8yr + cd_8yr + odd_8yr + dep_8yr + anx_8yr
           
# time-varying covariates (effects for observed variables)

          cbcl1 ~ age1
          cbcl2 ~ age2
          cbcl3 ~ age3
          
# time-invariant covariates (intercept & variance)
  
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

  
# 3 - slope only ####
  
  model3 <-  
          '
# growth parameters (latent variables)
  
          i =~ 1*cbcl1 + 1*cbcl2 + 1*cbcl3
          s =~ -3.5*cbcl1 + -2*cbcl2 + 0*cbcl3
  
# obs variable variances
  
          cbcl1 ~~ cbcl1
          cbcl2 ~~ cbcl2
          cbcl3 ~~ cbcl3
          inat_8yr ~~ inat_8yr
          hyp_8yr ~~ hyp_8yr
          cd_8yr ~~ cd_8yr
          odd_8yr ~~ odd_8yr
          dep_8yr ~~ dep_8yr
          anx_8yr ~~ anx_8yr
  
# growth parameter (co)variances
  
          i ~~ i
          s ~~ s
          i ~~ s
  
# obs variable intercepts (fixed to 0)
  
          cbcl1 ~ 0*1
          cbcl2 ~ 0*1
          cbcl3 ~ 0*1
  
# growth parameter intercepts (freely estimated)
  
          i ~ 1 
          s ~ 1
  
# outcomes
  
          inat_8yr + hyp_8yr + cd_8yr + odd_8yr + dep_8yr + anx_8yr ~ s + 0*i
  
# means of outcomes (freely estimated)
  
          inat_8yr ~ 1
          hyp_8yr ~ 1
          cd_8yr ~ 1
          odd_8yr ~ 1
          dep_8yr ~ 1
          anx_8yr ~ 1
  
# covariances of outcomes (freely estimated)
  
          inat_8yr ~~ hyp_8yr + cd_8yr + odd_8yr + dep_8yr + anx_8yr
          hyp_8yr ~~ cd_8yr + odd_8yr + dep_8yr + anx_8yr
          cd_8yr ~~ odd_8yr + dep_8yr + anx_8yr
          odd_8yr ~~ dep_8yr + anx_8yr
          dep_8yr ~~ anx_8yr
  
# covariances of cbcl scores with outcomes (fixed to zero)
  
          cbcl1 ~~ 0*inat_8yr + 0*hyp_8yr + 0*cd_8yr + 0*odd_8yr + 0*dep_8yr + 0*anx_8yr
          cbcl2 ~~ 0*inat_8yr + 0*hyp_8yr + 0*cd_8yr + 0*odd_8yr + 0*dep_8yr + 0*anx_8yr
          cbcl3 ~~ 0*inat_8yr + 0*hyp_8yr + 0*cd_8yr + 0*odd_8yr + 0*dep_8yr + 0*anx_8yr
          
# time-invariant covariates (effects)
  
          i + s ~ sex
          i + s ~ parity

# time-invariant covariates (covariances with outcomes)

          sex ~~ inat_8yr + hyp_8yr + cd_8yr + odd_8yr + dep_8yr + anx_8yr
          parity ~~ inat_8yr + hyp_8yr + cd_8yr + odd_8yr + dep_8yr + anx_8yr
           
# time-varying covariates (effects for observed variables)

          cbcl1 ~ age1
          cbcl2 ~ age2
          cbcl3 ~ age3
          
# time-invariant covariates (intercept & variance)
  
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


# 4 - comparison model with no intercept or slope effects 
  
  model4 <-  
          '
# growth parameters (latent variables)
  
          i =~ 1*cbcl1 + 1*cbcl2 + 1*cbcl3
          s =~ -3.5*cbcl1 + -2*cbcl2 + 0*cbcl3
  
# obs variable variances
  
          cbcl1 ~~ cbcl1
          cbcl2 ~~ cbcl2
          cbcl3 ~~ cbcl3
          inat_8yr ~~ inat_8yr
          hyp_8yr ~~ hyp_8yr
          cd_8yr ~~ cd_8yr
          odd_8yr ~~ odd_8yr
          dep_8yr ~~ dep_8yr
          anx_8yr ~~ anx_8yr
  
# growth parameter (co)variances
  
          i ~~ i
          s ~~ s
          i ~~ s
  
# obs variable intercepts (fixed to 0)
  
          cbcl1 ~ 0*1
          cbcl2 ~ 0*1
          cbcl3 ~ 0*1
  
# growth parameter intercepts (freely estimated)
  
          i ~ 1 
          s ~ 1
  
# outcomes
  
          inat_8yr + hyp_8yr + cd_8yr + odd_8yr + dep_8yr + anx_8yr ~ 0*i + 0*s
  
# means of outcomes (freely estimated)
  
          inat_8yr ~ 1
          hyp_8yr ~ 1
          cd_8yr ~ 1
          odd_8yr ~ 1
          dep_8yr ~ 1
          anx_8yr ~ 1
  
# covariances of outcomes (freely estimated)
  
          inat_8yr ~~ hyp_8yr + cd_8yr + odd_8yr + dep_8yr + anx_8yr
          hyp_8yr ~~ cd_8yr + odd_8yr + dep_8yr + anx_8yr
          cd_8yr ~~ odd_8yr + dep_8yr + anx_8yr
          odd_8yr ~~ dep_8yr + anx_8yr
          dep_8yr ~~ anx_8yr
  
# covariances of cbcl scores with outcomes (fixed to zero)
  
          cbcl1 ~~ 0*inat_8yr + 0*hyp_8yr + 0*cd_8yr + 0*odd_8yr + 0*dep_8yr + 0*anx_8yr
          cbcl2 ~~ 0*inat_8yr + 0*hyp_8yr + 0*cd_8yr + 0*odd_8yr + 0*dep_8yr + 0*anx_8yr
          cbcl3 ~~ 0*inat_8yr + 0*hyp_8yr + 0*cd_8yr + 0*odd_8yr + 0*dep_8yr + 0*anx_8yr
          
# time-invariant covariates (effects)
  
          i + s ~ sex
          i + s ~ parity

# time-invariant covariate (covariances with outcomes)

          sex ~~ inat_8yr + hyp_8yr + cd_8yr + odd_8yr + dep_8yr + anx_8yr
          parity ~~ inat_8yr + hyp_8yr + cd_8yr + odd_8yr + dep_8yr + anx_8yr
           
# time-varying covariates (effects for observed variables)

          cbcl1 ~ age1
          cbcl2 ~ age2
          cbcl3 ~ age3
          
# time-invariant covariates (intercept & variance)
  
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
  
  
  
# Create models object ####
  
  models <- c("model1" = model1,
              "model2" = model2,
              "model3" = model3,
              "model4" = model4)
  
  
  
  
  