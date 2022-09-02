# 02.2_specify_combined_LGMs_for_validation.R

### steps followed for the combined diff + tot LGMs:

# 1. specify LGM with effects of both growth factors
# 2. specify LGM with effects of intercept only
# 3. specify LGM with effects of slope only
# 4. specify LGM with no effects of growth factors
# 5. specify LGM with effects of differentiation only
# 6. specify LGM with effects of total only


# 1 - intercept and slope effects for both diff and tot

  model1 <-  
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
          inat_8yr ~~ inat_8yr
          hyp_8yr ~~ hyp_8yr
          cd_8yr ~~ cd_8yr
          odd_8yr ~~ odd_8yr
          dep_8yr ~~ dep_8yr
          anx_8yr ~~ anx_8yr
  
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
  
# outcomes
  
          inat_8yr + hyp_8yr + cd_8yr + odd_8yr + dep_8yr + anx_8yr ~ i1 + s1
          inat_8yr + hyp_8yr + cd_8yr + odd_8yr + dep_8yr + anx_8yr ~ i2 + s2
  
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
  
          diff1 ~~ 0*inat_8yr + 0*hyp_8yr + 0*cd_8yr + 0*odd_8yr + 0*dep_8yr + 0*anx_8yr
          diff2 ~~ 0*inat_8yr + 0*hyp_8yr + 0*cd_8yr + 0*odd_8yr + 0*dep_8yr + 0*anx_8yr
          diff3 ~~ 0*inat_8yr + 0*hyp_8yr + 0*cd_8yr + 0*odd_8yr + 0*dep_8yr + 0*anx_8yr
          tot1 ~~ 0*inat_8yr + 0*hyp_8yr + 0*cd_8yr + 0*odd_8yr + 0*dep_8yr + 0*anx_8yr
          tot2 ~~ 0*inat_8yr + 0*hyp_8yr + 0*cd_8yr + 0*odd_8yr + 0*dep_8yr + 0*anx_8yr
          tot3 ~~ 0*inat_8yr + 0*hyp_8yr + 0*cd_8yr + 0*odd_8yr + 0*dep_8yr + 0*anx_8yr
          
# time-invariant covariate (effects)
  
          i1 + s1 ~ sex
          i2 + s2 ~ sex
          i1 + s1 ~ parity
          i2 + s2 ~ parity

# time-invariant covariate (covariances with outcomes)

          sex ~~ inat_8yr + hyp_8yr + cd_8yr + odd_8yr + dep_8yr + anx_8yr
          parity ~~ inat_8yr + hyp_8yr + cd_8yr + odd_8yr + dep_8yr + anx_8yr
           
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
  
  
  
# 2 - intercepts only for diff and tot
  
  model2 <-  
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
          inat_8yr ~~ inat_8yr
          hyp_8yr ~~ hyp_8yr
          cd_8yr ~~ cd_8yr
          odd_8yr ~~ odd_8yr
          dep_8yr ~~ dep_8yr
          anx_8yr ~~ anx_8yr
  
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
  
# outcomes
  
          inat_8yr + hyp_8yr + cd_8yr + odd_8yr + dep_8yr + anx_8yr ~ i1 + 0*s1
          inat_8yr + hyp_8yr + cd_8yr + odd_8yr + dep_8yr + anx_8yr ~ i2 + 0*s2
  
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
  
          diff1 ~~ 0*inat_8yr + 0*hyp_8yr + 0*cd_8yr + 0*odd_8yr + 0*dep_8yr + 0*anx_8yr
          diff2 ~~ 0*inat_8yr + 0*hyp_8yr + 0*cd_8yr + 0*odd_8yr + 0*dep_8yr + 0*anx_8yr
          diff3 ~~ 0*inat_8yr + 0*hyp_8yr + 0*cd_8yr + 0*odd_8yr + 0*dep_8yr + 0*anx_8yr
          tot1 ~~ 0*inat_8yr + 0*hyp_8yr + 0*cd_8yr + 0*odd_8yr + 0*dep_8yr + 0*anx_8yr
          tot2 ~~ 0*inat_8yr + 0*hyp_8yr + 0*cd_8yr + 0*odd_8yr + 0*dep_8yr + 0*anx_8yr
          tot3 ~~ 0*inat_8yr + 0*hyp_8yr + 0*cd_8yr + 0*odd_8yr + 0*dep_8yr + 0*anx_8yr
          
# time-invariant covariate (effects)
  
          i1 + s1 ~ sex
          i2 + s2 ~ sex
          i1 + s1 ~ parity
          i2 + s2 ~ parity

# time-invariant covariate (covariances with outcomes)

          sex ~~ inat_8yr + hyp_8yr + cd_8yr + odd_8yr + dep_8yr + anx_8yr
          parity ~~ inat_8yr + hyp_8yr + cd_8yr + odd_8yr + dep_8yr + anx_8yr
           
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
  

  
  
# 3 - slope only for diff and tot
  
  model3 <-  
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
          inat_8yr ~~ inat_8yr
          hyp_8yr ~~ hyp_8yr
          cd_8yr ~~ cd_8yr
          odd_8yr ~~ odd_8yr
          dep_8yr ~~ dep_8yr
          anx_8yr ~~ anx_8yr
  
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
  
# outcomes
  
          inat_8yr + hyp_8yr + cd_8yr + odd_8yr + dep_8yr + anx_8yr ~ 0*i1 + s1
          inat_8yr + hyp_8yr + cd_8yr + odd_8yr + dep_8yr + anx_8yr ~ 0*i2 + s2
  
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
  
          diff1 ~~ 0*inat_8yr + 0*hyp_8yr + 0*cd_8yr + 0*odd_8yr + 0*dep_8yr + 0*anx_8yr
          diff2 ~~ 0*inat_8yr + 0*hyp_8yr + 0*cd_8yr + 0*odd_8yr + 0*dep_8yr + 0*anx_8yr
          diff3 ~~ 0*inat_8yr + 0*hyp_8yr + 0*cd_8yr + 0*odd_8yr + 0*dep_8yr + 0*anx_8yr
          tot1 ~~ 0*inat_8yr + 0*hyp_8yr + 0*cd_8yr + 0*odd_8yr + 0*dep_8yr + 0*anx_8yr
          tot2 ~~ 0*inat_8yr + 0*hyp_8yr + 0*cd_8yr + 0*odd_8yr + 0*dep_8yr + 0*anx_8yr
          tot3 ~~ 0*inat_8yr + 0*hyp_8yr + 0*cd_8yr + 0*odd_8yr + 0*dep_8yr + 0*anx_8yr
          
# time-invariant covariate (effects)
  
          i1 + s1 ~ sex
          i2 + s2 ~ sex
          i1 + s1 ~ parity
          i2 + s2 ~ parity

# time-invariant covariate (covariances with outcomes)

          sex ~~ inat_8yr + hyp_8yr + cd_8yr + odd_8yr + dep_8yr + anx_8yr
          parity ~~ inat_8yr + hyp_8yr + cd_8yr + odd_8yr + dep_8yr + anx_8yr
           
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
  
  
  
  
  
# 4 - no effects for diff or tot
  
  model4 <-  
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
          inat_8yr ~~ inat_8yr
          hyp_8yr ~~ hyp_8yr
          cd_8yr ~~ cd_8yr
          odd_8yr ~~ odd_8yr
          dep_8yr ~~ dep_8yr
          anx_8yr ~~ anx_8yr
  
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
  
# outcomes
  
          inat_8yr + hyp_8yr + cd_8yr + odd_8yr + dep_8yr + anx_8yr ~ 0*i1 + 0*s1
          inat_8yr + hyp_8yr + cd_8yr + odd_8yr + dep_8yr + anx_8yr ~ 0*i2 + 0*s2
  
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
  
          diff1 ~~ 0*inat_8yr + 0*hyp_8yr + 0*cd_8yr + 0*odd_8yr + 0*dep_8yr + 0*anx_8yr
          diff2 ~~ 0*inat_8yr + 0*hyp_8yr + 0*cd_8yr + 0*odd_8yr + 0*dep_8yr + 0*anx_8yr
          diff3 ~~ 0*inat_8yr + 0*hyp_8yr + 0*cd_8yr + 0*odd_8yr + 0*dep_8yr + 0*anx_8yr
          tot1 ~~ 0*inat_8yr + 0*hyp_8yr + 0*cd_8yr + 0*odd_8yr + 0*dep_8yr + 0*anx_8yr
          tot2 ~~ 0*inat_8yr + 0*hyp_8yr + 0*cd_8yr + 0*odd_8yr + 0*dep_8yr + 0*anx_8yr
          tot3 ~~ 0*inat_8yr + 0*hyp_8yr + 0*cd_8yr + 0*odd_8yr + 0*dep_8yr + 0*anx_8yr
          
# time-invariant covariate (effects)
  
          i1 + s1 ~ sex
          i2 + s2 ~ sex
          i1 + s1 ~ parity
          i2 + s2 ~ parity

# time-invariant covariate (covariances with outcomes)

          sex ~~ inat_8yr + hyp_8yr + cd_8yr + odd_8yr + dep_8yr + anx_8yr
          parity ~~ inat_8yr + hyp_8yr + cd_8yr + odd_8yr + dep_8yr + anx_8yr
           
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
  
  
  

# 5 - intercept and slope effects for diff only
  
  model5 <-  
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
          inat_8yr ~~ inat_8yr
          hyp_8yr ~~ hyp_8yr
          cd_8yr ~~ cd_8yr
          odd_8yr ~~ odd_8yr
          dep_8yr ~~ dep_8yr
          anx_8yr ~~ anx_8yr
  
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
  
# outcomes
  
          inat_8yr + hyp_8yr + cd_8yr + odd_8yr + dep_8yr + anx_8yr ~ i1 + s1
          inat_8yr + hyp_8yr + cd_8yr + odd_8yr + dep_8yr + anx_8yr ~ 0*i2 + 0*s2
  
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
  
          diff1 ~~ 0*inat_8yr + 0*hyp_8yr + 0*cd_8yr + 0*odd_8yr + 0*dep_8yr + 0*anx_8yr
          diff2 ~~ 0*inat_8yr + 0*hyp_8yr + 0*cd_8yr + 0*odd_8yr + 0*dep_8yr + 0*anx_8yr
          diff3 ~~ 0*inat_8yr + 0*hyp_8yr + 0*cd_8yr + 0*odd_8yr + 0*dep_8yr + 0*anx_8yr
          tot1 ~~ 0*inat_8yr + 0*hyp_8yr + 0*cd_8yr + 0*odd_8yr + 0*dep_8yr + 0*anx_8yr
          tot2 ~~ 0*inat_8yr + 0*hyp_8yr + 0*cd_8yr + 0*odd_8yr + 0*dep_8yr + 0*anx_8yr
          tot3 ~~ 0*inat_8yr + 0*hyp_8yr + 0*cd_8yr + 0*odd_8yr + 0*dep_8yr + 0*anx_8yr
          
# time-invariant covariate (effects)
  
          i1 + s1 ~ sex
          i2 + s2 ~ sex
          i1 + s1 ~ parity
          i2 + s2 ~ parity

# time-invariant covariate (covariances with outcomes)

          sex ~~ inat_8yr + hyp_8yr + cd_8yr + odd_8yr + dep_8yr + anx_8yr
          parity ~~ inat_8yr + hyp_8yr + cd_8yr + odd_8yr + dep_8yr + anx_8yr
           
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
  
  
  
  
  
# 6 - intercept and slope effects for tot only
  
  model6 <-  
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
          inat_8yr ~~ inat_8yr
          hyp_8yr ~~ hyp_8yr
          cd_8yr ~~ cd_8yr
          odd_8yr ~~ odd_8yr
          dep_8yr ~~ dep_8yr
          anx_8yr ~~ anx_8yr
  
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
  
# outcomes
  
          inat_8yr + hyp_8yr + cd_8yr + odd_8yr + dep_8yr + anx_8yr ~ 0*i1 + 0*s1
          inat_8yr + hyp_8yr + cd_8yr + odd_8yr + dep_8yr + anx_8yr ~ i2 + s2
  
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
  
          diff1 ~~ 0*inat_8yr + 0*hyp_8yr + 0*cd_8yr + 0*odd_8yr + 0*dep_8yr + 0*anx_8yr
          diff2 ~~ 0*inat_8yr + 0*hyp_8yr + 0*cd_8yr + 0*odd_8yr + 0*dep_8yr + 0*anx_8yr
          diff3 ~~ 0*inat_8yr + 0*hyp_8yr + 0*cd_8yr + 0*odd_8yr + 0*dep_8yr + 0*anx_8yr
          tot1 ~~ 0*inat_8yr + 0*hyp_8yr + 0*cd_8yr + 0*odd_8yr + 0*dep_8yr + 0*anx_8yr
          tot2 ~~ 0*inat_8yr + 0*hyp_8yr + 0*cd_8yr + 0*odd_8yr + 0*dep_8yr + 0*anx_8yr
          tot3 ~~ 0*inat_8yr + 0*hyp_8yr + 0*cd_8yr + 0*odd_8yr + 0*dep_8yr + 0*anx_8yr
          
# time-invariant covariate (effects)
  
          i1 + s1 ~ sex
          i2 + s2 ~ sex
          i1 + s1 ~ parity
          i2 + s2 ~ parity

# time-invariant covariate (covariances with outcomes)

          sex ~~ inat_8yr + hyp_8yr + cd_8yr + odd_8yr + dep_8yr + anx_8yr
          parity ~~ inat_8yr + hyp_8yr + cd_8yr + odd_8yr + dep_8yr + anx_8yr
           
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
  
  
  