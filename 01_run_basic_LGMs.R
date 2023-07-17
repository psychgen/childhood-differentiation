# 01_run_basic_LGMs.R

# this script runs the latent growth models (LGMs) by sourcing:
# 01.1_specify_basic_LGMs.R

  library(tidyverse)
  library(lavaan)
  library(DiagrammeR)

# read in processed data from 00_data_preparation.R

  load("./data/processed_data_new.RData")

# specify basic LGMs

  source("./scripts/01.1_specify_basic_LGMs.R")

# diff: run basic LGMs and compare ####

# diff: run basic LGM with linear slope
  
  fit_modelA_diff <- sem(modelA_diff, 
                         missing = "ML", 
                         estimator = "MLR", 
                         data = alldata_new, 
                         cluster = "m_id",
                         fixed.x = F)

# provide output for the model
  
  summary(fit_modelA_diff, fit.measures = TRUE, std = TRUE)
  standardizedSolution(fit_modelA_diff, type = "std.all", se = TRUE, 
                       pvalue = TRUE, ci = TRUE, level = 0.95)

# diff: run basic LGM without slope
  
  fit_modelB_diff <- sem(modelB_diff, 
                         missing = "ML", 
                         estimator = "MLR", 
                         data = alldata_new, 
                         cluster = "m_id",
                         fixed.x = F)

# provide output for the model
  
  summary(fit_modelB_diff, fit.measures = TRUE, std = TRUE)

# compare the models
  
  diff_fit <- anova(fit_modelA_diff, fit_modelB_diff)
  
  diff_fit

# tot: run basic LGMs and compare ####

# tot: run basic LGM with linear slope
  
  fit_modelA_tot <- sem(modelA_tot, 
                        missing = "ML", 
                        estimator = "MLR", 
                        data = alldata_new, 
                        cluster = "m_id",
                        fixed.x = F)

# provide output for the model
  
  summary(fit_modelA_tot, fit.measures = TRUE, std = TRUE)

# tot: run basic LGM without slope
  
  fit_modelB_tot <- sem(modelB_tot, 
                        missing = "ML", 
                        estimator = "MLR", 
                        data = alldata_new, 
                        cluster = "m_id",
                        fixed.x = F)

# provide output for the model
  
  summary(fit_modelB_tot, fit.measures = TRUE, std = TRUE)

# compare the models
  
  diff_tot <- anova(fit_modelA_tot, fit_modelB_tot)
  
  diff_tot
  
  save(diff_fit,diff_tot, file="./output/stable5.RData")

# For each best-fitting model, test whether the age covariate effects can be
# dropped: 

# diff: run basic LGM with linear slope, no age effects
  
  fit_modelA_diff_noage <- sem(modelA_diff_noage, 
                               missing = "ML", 
                               estimator = "MLR", 
                               data = alldata_new, 
                               cluster = "m_id",
                               fixed.x = F)

# provide output for the model
  
  summary(fit_modelA_diff_noage, fit.measures = TRUE, std = TRUE)

# compare the models
  
  lavTestLRT(fit_modelA_diff, fit_modelA_diff_noage)

# tot: run basic LGM with linear slope, no age effects
  
  fit_modelA_tot_noage <- sem(modelA_tot_noage, 
                              missing = "ML", 
                              estimator = "MLR", 
                              data = alldata_new, 
                              cluster="m_id",
                              fixed.x=F)
# compare the models
  
  lavTestLRT(fit_modelA_tot, fit_modelA_tot_noage)

# to take forward:
# modelA_diff and fit_modelA_tot
  
# basic LGM with both
  
  fit_modelB_both <- sem(modelB_both, 
                         missing = "ML", 
                         estimator = "MLR", 
                         data = alldata_new, 
                         cluster = "m_id",
                         fixed.x = F)

# provide output for the model
  
  summary(fit_modelB_both, fit.measures = TRUE, std = TRUE)
  
# create basic LGM plot (for suppl.) based on the selected diff model ####
  
# convert path data-frame to node and edge data-frame
  fit_modelA_diff_noage %>% parameterestimates %>% sample_n(5)
  
# to describe edges, we need the origin, path-type, endpoint, and label.
  paths <- fit_modelA_diff_noage %>%
    parameterestimates %>%
    select(lhs, op, rhs, est)
  
# latent variables are left-hand side of "=~" lines
  latent <- paths %>%
    filter(op == "=~") %>%
    select(nodes = lhs) %>%
    distinct %>%
    mutate(shape = "circle")
  
# this is a node data-frame now
  latent
  
# manifest variables are not latent variables
  `%not_in%` <- Negate(`%in%`)
  manifest <- paths %>%
    filter(op != "~1", lhs %not_in% latent$nodes) %>%
    select(nodes = lhs) %>%
    distinct %>%
    mutate(shape = "square")
  
# nodes are prepared
  node_set <- combine_nodes(latent, manifest)
  node_set
  
# edges will be labeled by the parameter estimates
  all_paths <- paths %>%
    filter(op != "~1") %>%
    mutate(label = round(est, 2)) %>%
    select(-est)
  
# factor loadings are the paths in the "=~" lines
  loadings <- all_paths %>%
    filter(op == "=~") %>%
    mutate(edge_from = lhs, edge_to = rhs, style = "dashed") %>%
    select(edge_from, edge_to, style, label)
  
# this is now an edge dataframe
  loadings
  
# regressions are the paths in the "~" lines
  regressions <- all_paths %>%
    filter(op == "~") %>%
    rename(edge_to = lhs, edge_from = rhs) %>%
    mutate(style = "solid") %>%
    select(edge_from, edge_to, style, label)
  
  edge_set <- combine_edges(loadings, regressions)
  edge_set
  
# combine edges and nodes
  my_graphA_diff <- graphviz_graph(
    nodes = node_set,
    edges_df = edge_set,
    graph_attrs = c("ranksep = 1"))
  
# plot the graph
  graphviz_render(my_graphA_diff)
  
# extract dot code from this diagram
  cat(my_graphA_diff$dot_code)
  
# tweak basic plot (also drop age)
  
  grViz("
  digraph {
    
    graph [ranksep = 1]
    rankdir = 'BT';

    
    'i' [shape = 'circle', fixedsize = TRUE] 
    's' [shape = 'circle', fixedsize = TRUE] 
    'diff1' [shape = 'square', fixedsize = TRUE] 
    'diff2' [shape = 'square', fixedsize = TRUE] 
    'diff3' [shape = 'square', fixedsize = TRUE]
    'i'->'diff1' [style = 'dashed', label = '1'] 
    'i'->'diff2' [style = 'dashed', label = '1'] 
    'i'->'diff3' [style = 'dashed', label = '1'] 
    's'->'diff1' [style = 'dashed', label = '-3.5'] 
    's'->'diff2' [style = 'dashed', label = '-2'] 
    's'->'diff3' [style = 'dashed', label = '0']
    's'->'i' [dir = 'both', style = 'solid']
    
    # additional constraints on the graph
    diff1 -> diff2 -> diff3 [style = 'invis']
    s -> i [style = 'invis']
    {rank = 'max'; diff1; diff2; diff3}
    {rank = 'same'; i; s}
  }
  ")

  