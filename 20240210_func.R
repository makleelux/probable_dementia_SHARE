#############################################################
#                                                           #
#         Function Definitions                              #
#                                                           #
#                                                           #
#                                           maklee 20220810 #
#############################################################

adapt_lw_per_country = function(country_string, traindat, testdat){
  ### derives lw adaptations per country
  ### requires:
  ### country_string: string indicating country of interest
  ### traindat: training data from which to draw thresholds
  ### testdat: test data for which to derive classifications
  
  # compute sum of recall and iadl variables per country
  # for training data
  traindat = traindat %>% 
    # perform analysis per country
    filter(country %in% country_string) %>% 
    mutate(
      # sum scores based on immediate and delayed recall / 9 IADLs
      recall_sum = cf016tot + cf008tot,
      iadl_sum = 
        iadl_meal + iadl_groceries + iadl_phone + iadl_medi + iadl_money + 
        iadl_map + iadl_leaving + iadl_garden + iadl_laundry)
  
  # for testing data
  testdat = testdat %>% 
    # perform analysis per country
    filter(country %in% country_string) %>% 
    mutate(
      # sum scores based on immediate and delayed recall / 9 IADLs
      recall_sum = cf016tot + cf008tot,
      iadl_sum = 
        iadl_meal + iadl_groceries + iadl_phone + iadl_medi + iadl_money + 
        iadl_map + iadl_leaving + iadl_garden + iadl_laundry)
      
    # derive classifications for testing data
    outdat = testdat %>% 
      mutate(
        
        ## LW classification (criterion 1)
        # store recall cutoff
        recall_cutoff = stats::quantile(traindat$recall_sum, probs = c(.025)),
        # criterion 1: recall score below 2.5th percentile
        LW = ifelse(recall_sum <= recall_cutoff, 1, 0),
        
        ## LW iadl classification (criterion 1 & 2)
        # store IADL cutoff; if Q3 > 0 --> outliers, if Q3 = 0 --> 0
        Q3 = stats::quantile(traindat$iadl_sum, probs = c(.75)),
        Q3_greater_zero = ifelse(Q3 > 0, "yes", "no"),
        IADL_cutoff = ifelse(Q3_greater_zero == "yes", Q3 + 1.5*IQR(traindat$iadl_sum), 0),
        
        # criterion 2: iadl sum outlying or greater 1
        LW_iadl = ifelse(LW == 1 & iadl_sum > IADL_cutoff, 1, 0),
        
        ## LW classification (criterion 1* based on country prevalence)
        # store recall cutoff prev
        recall_cutoff_prev = stats::quantile(traindat$recall_sum, probs = c(testdat$dementia_prev[1])),
        # criterion 1*: recall score below dementia prev percentile
        LW_prev = ifelse(recall_sum <= recall_cutoff_prev, 1, 0),
        
        ## LW iadl classification (criterion 1 & 2)
        # criterion 2: iadl sum outlying or greater 1
        LW_iadl_prev = ifelse(LW_prev == 1 & iadl_sum > IADL_cutoff, 1, 0),
    
        # transform to factors
        probdem_LW = factor(ifelse(LW == 1, "dementia", "no dementia")),
        probdem_LW_iadl = factor(ifelse(LW_iadl == 1, "dementia", "no dementia")),
        probdem_LW_prev = factor(ifelse(LW_prev == 1, "dementia", "no dementia")),
        probdem_LW_iadl_prev = factor(ifelse(LW_iadl_prev == 1, "dementia", "no dementia")))
    
  return(outdat)
}
pvalue <- function(x, ...) {
  ### conducts significance tests for table 1.
  ### requires no input
  ### adapted from: https://cran.r-project.org/web/packages/table1/vignettes/table1-examples.html
  
  # construct vectors of data y, and groups (strata) g
  y <- unlist(x)
  g <- factor(rep(1:length(x), times=sapply(x, length)))
  if (is.numeric(y)) {
    # for numeric variables, perform a standard 2-sample t-test
    p <- t.test(y ~ g)$p.value
    p <- round(p, 3)
  } else {
    # for categorical variables, perform a chi-squared test of independence
    p <- chisq.test(table(y, g))$p.value
    p <- round(p, 3)
  }
  # format the p-value, using an HTML entity for the less-than sign.
  # the initial empty string places the output on the line below the variable label.
  # additionally remove leading zero.
  c("", p <- sub("^(-?)0.", "\\1.", 
                 sub("<", "&lt;", 
                     format.pval(p, digits=2, eps=0.001))))
}
tidy_model_comparison = function(probdem, data, metrics){
  ### computes performance table with metrics for classifications
  ### requires:
  ### probdem: string or vector of strings indicating classification algorithms of interest
  ### data: data including classifications, preferably test data
  ### metrics: string or vector of strings with metrics to be included (need to be embedded in confusionMatrix)
  
  tidy_res = data.frame()
  for(i in 1:length(probdem)){
    probdem_ = data
    col = probdem[i]
    probdem_$ref = unlist(data[col]) %>% as.vector() %>% as.factor()
    conf_mat_temp = confusionMatrix(probdem_$ref, data$dementia) %>% 
      tidy() %>% 
      filter(term %in% metrics) %>% 
      select(term, estimate) %>% 
      mutate(alg = probdem[i]) %>% 
      pivot_wider(names_from = term, values_from = estimate) %>% 
      select(alg, accuracy, balanced_accuracy, sensitivity, specificity, precision, f1) %>% 
      mutate(alg = alg %>% str_replace("probdem_", ""))
    tidy_res = rbind(tidy_res, conf_mat_temp)
  } 
  return(tidy_res)
}
rename_alg = function(x) {
  ### renames classification algorithms to a plot friendly form
  ### requires:
  ### x: string of classification algorithm names
  
  x = x %>% str_replace("dementia", "Self-report")
  x = x %>% str_replace("probdem_", "")
  x = x %>% str_replace("LW", "LW (Recall)")
  x = x %>% str_replace("LW [(]Recall[)]_iadl", "LW (Recall & IADL)")
  x = x %>% str_replace("_prev", "^'P'")
  x = x %>% str_replace("xgb", "XGBoost")
  x = x %>% str_replace("glm", "Logistic Regression")
  x = x %>% str_replace("ranger", "Random Forest")
  x = x %>% str_replace("_smote", " SMOTE")
  x = x %>% str_replace("_down", " DOWN")
  x = x %>% str_replace("_iv", "")
  x = x %>% str_replace("_w", " weighted")
  x = factor(x)
  
  return(x)
}
rename_alg_short = function(x) {
  ### renames classification algorithms to a plot friendly short form
  ### requires:
  ### x: string of classification algorithm names
  
  x = x %>% str_replace("dementia", "Self-report")
  x = x %>% str_replace("probdem_", "")
  x = x %>% str_replace("LW", "'LW (Recall)'")
  x = x %>% str_replace("[']LW [(]Recall[)][']_iadl", "'LW (Recall & IADL)'")
  x = x %>% str_replace("_prev", "^'P'")
  x = x %>% str_replace("glm", "GLM")
  x = x %>% str_replace("GLM_smote", "'GLM SMOTE'")
  x = x %>% str_replace("GLM_down", "'GLM DOWN'")
  x = x %>% str_replace("ranger", "RF")
  x = x %>% str_replace("RF_smote", "'RF SMOTE'")
  x = x %>% str_replace("RF_down", "'RF DOWN'")
  x = x %>% str_replace("xgb", "XGB")
  x = x %>% str_replace("XGB_smote", "'XGB SMOTE'")
  x = x %>% str_replace("XGB_down", "'XGB DOWN'")
  x = x %>% str_replace("_iv", "")
  x = x %>% str_replace("GLM_w", "'GLM weighted'")
  x = factor(x)
  
  return(x)
}
weighted_class_prev_per_country = function(dat_with_probdem, dat_with_country){
  ### derives population-level prevalence per country weighted by cciw_w7 for Self-reports, LW (Recall & IADL)*, GLM (weighted) IV, Random Forest SMOTE IV and XGBoost SMOTE IV
  ### requires:
  ### dat_with_probdem: df including classifications, preferably in test set
  ### dat_with_country: df including country information for instances in dat_with_probdem

  # attach country information
  test_dat_baked_temp = dat_with_probdem %>% 
    left_join(dat_with_country %>% select(mergeid, country)) %>% 
    # drop missing weights
    drop_na(cciw_w7)
  
  # set survey design
  dsgn = svydesign(ids = ~1, weights = ~cciw_w7, data = test_dat_baked_temp)
  
  # create tables with weighted prev per country
  dementia_tab = data.frame(prop.table(svytable(design = dsgn, formula = ~dementia + country), margin = 2)) %>% rename(`Self-report` = Freq)
  probdem_LW_tab = data.frame(prop.table(svytable(design = dsgn, formula = ~probdem_LW_iadl_prev + country), margin = 2)) %>% rename(`LW (Recall & IADL)^'P'` = Freq, dementia = probdem_LW_iadl_prev)
  probdem_glm_w_tab = data.frame(prop.table(svytable(design = dsgn, formula = ~probdem_glm_w_iv + country), margin = 2)) %>% rename(`Logistic Regression weighted` = Freq, dementia = probdem_glm_w_iv)
  probdem_ranger_tab = data.frame(prop.table(svytable(design = dsgn, formula = ~probdem_ranger_smote_iv + country), margin = 2)) %>% rename(`Random Forest SMOTE` = Freq, dementia = probdem_ranger_smote_iv)
  probdem_xgb_tab = data.frame(prop.table(svytable(design = dsgn, formula = ~probdem_xgb_smote_iv + country), margin = 2)) %>% rename(`XGBoost SMOTE` = Freq, dementia = probdem_xgb_smote_iv)
  
  SHARE_prevalence_weighted = dementia_tab %>% 
    left_join(probdem_LW_tab) %>% 
    left_join(probdem_glm_w_tab) %>% 
    left_join(probdem_ranger_tab) %>% 
    left_join(probdem_xgb_tab) %>% 
    filter(dementia == "dementia") %>% 
    left_join(test_dat_baked_temp %>% group_by(country) %>% summarise(n = n(), dementia_prev = first(dementia_prev))) %>%  
    mutate(Ndemexpected = dementia_prev*n,
           Ndemfactual_dementia = `Self-report`*n,
           Ndemfactual_LW = `LW (Recall & IADL)^'P'`*n,
           Ndemfactual_GLM_w = `Logistic Regression weighted`*n,
           Ndemfactual_Ranger = `Random Forest SMOTE`*n,
           Ndemfactual_XGB = `XGBoost SMOTE`*n)
  
  return(SHARE_prevalence_weighted)
}
create_world = function(weighted_class_prev_per_country){
  ### create world map with country information attached
  ### requires:
  ### weighted_class_prev_per_country: data with country information as provided by function weighted_class_prev_per_country
  
  world = ne_countries(scale = "medium", returnclass = "sf") %>% 
    # align naming
    mutate(country = name) %>% 
    # join info
    left_join(weighted_class_prev_per_country %>% 
                # align naming
                mutate(country = recode(country, "Czech Republic" = "Czech Rep."),
                       # transform to percent
                       `Self-report` = 100*`Self-report`, 
                       OECD = 100*dementia_prev, 
                       `LW (Recall & IADL)^'P'` = 100*`LW (Recall & IADL)^'P'`,
                       `GLM (weighted)` = 100*`Logistic Regression weighted`,
                       `RF SMOTE` = 100*`Random Forest SMOTE`,
                       `XGB SMOTE` = 100*`XGBoost SMOTE`))
  
  return(world)
}
plot_prev_per_country = function(prev_source, Europe, prefix){
  ### plots dementia prevalence per country on map
  ### if prevalence is greater 10, country will be illustrated in green
  ### countries without prevalence information (i.e. not included are depicted in white)
  ### requires:
  ### prev_source: string indicating source classification
  ### Europe: data set containing participating countries, their geometry and prevalences as per classification
  
  plot_Europe = ggplot(Europe, aes(fill = !!sym(prev_source), colour = "white", label = prev_source, prefix = "")) +
    geom_sf() +
    scale_color_manual(values = "white", guide = "none") +
    coord_sf(xlim = c(-10,35), ylim = c(25,70), expand = TRUE) +
    theme_bw() +
    scale_fill_viridis_c(limits=c(0, 10), na.value = "lightgrey", name = "Dementia Prevalence", option = "magma", direction = -1) + # old na #7FC97F
    theme(axis.line = element_blank(), panel.background = element_blank(), 
          axis.text = element_blank(), axis.ticks = element_blank(), panel.grid = element_blank(), 
          legend.text = element_text(size = 12),
          legend.title = element_text(size = 20),
          legend.key.width = unit(2, 'cm'),
          plot.margin = unit(c(.01,.01,.25,.01), "in"),
          plot.caption = element_text(size = 20, hjust = .5),
          plot.caption.position = 'panel') +
    labs(caption = ifelse(grepl("Recall", prev_source), 
                          parse(text = "'LW (Recall & IADL)'^'P'"), 
                          paste0(prev_source)))
  
  return(plot_Europe)
}
means = function(variables, data, status){
  ### calculates mean values with 95% CIs for every variable in self-reported dementia classifications
  ### requires:
  ### variables: vector of strings of variables of interest
  ### data: data containing respective variables and self-reports
  ### status: string or vector of strings indicating dementia status of interest
  
  limit = c("upper", "lower")
  dem_mean = data.frame()
  for(i in 1:length(variables)){
    for(j in 1:length(status)){
      df_temp = data %>% 
        filter(dementia == "dementia") %>% 
        summarise(variable = variables[i],
                  status = status[j],
                  CImin = mean_cl_normal(!!sym(variables[i]), na.rm = T)[[2]],
                  CImax = mean_cl_normal(!!sym(variables[i]), na.rm = T)[[3]]) %>% 
        mutate(value = 1)
      
      dem_mean = rbind(dem_mean, df_temp) 
    }
  }
  
  return(dem_mean)
}
integer_breaks <- function(n = 5, ...) {
  ### helper function for nice integer breaks in facetted plot
  ### requires no input
  
  breaker <- breaks_pretty(n, ...)
  function(x) {
    breaks <- breaker(x)
    breaks[breaks == floor(breaks)]
  }
}
sub_figure_6 = function(dem_stat, data, variables){
  ### plots subset of figure two for one dementia status
  ### requires:
  ### dem_stat: string indicating dementia status of interest
  ### data: data containing variables of interest and dementia classifications
  ### variables: character string indicating variables of interest
  
  # compute mean values and CIs for self-reported dementia
  means_dem = means(variables = variables, 
                    data = data, 
                    status = c("dementia", "no dementia"))
  
  # create plot data
  plot_dat = data %>% 
    select(Age, `Grip Strength (kg)`, `Euro-Depression Scale`, `Orientation to Date`, 
           `Numeracy Performance`, `Verbal Fluency`, contains("dem"), 
           -dementia_dbl, -dementia_prev) %>% 
    pivot_longer(cols = contains("dem"), 
                 names_to = "Classification", 
                 values_to = "status") %>% 
    filter(Classification %in% probdem_include[c(1:5, 7, 12, 15)], status == dem_stat) %>% 
    pivot_longer(cols = variables, 
                 names_to = "var", # not variable; necessary due to bug in stat_pvalue_manual
                 values_to = "value") %>%
    mutate(Classification = factor(Classification, levels = 
                                     c("dementia",
                                       "probdem_LW_prev", "probdem_LW_iadl_prev",
                                       "probdem_LW", "probdem_LW_iadl", 
                                       "probdem_glm_w_iv",
                                       "probdem_ranger_smote_iv", 
                                       "probdem_xgb_smote_iv")),
           value = as.double(value)) %>% 
    drop_na(value)
  
  stats_tmp <- plot_dat %>% 
    filter(status == "dementia") %>% 
    group_by(var) %>% 
    rstatix::t_test(value ~ Classification) %>% 
    rstatix::add_significance() %>% 
    rstatix::adjust_pvalue(method = "bonferroni") %>% 
    rstatix::add_xy_position(x = "Classification", fun = "mean_ci") %>%
    filter(p.adj.signif != "ns", group1 == "dementia")
  
  stats_imp = stats_tmp %>% mutate(y.position = c(90, 95, 100, 40, 45, 50, 55, 3.5, 3.8, 4.2, 4.6))
  
  # create plot
  plot_dat %>% 
    ggplot(aes(x = Classification, y = value)) +
    stat_summary(geom = "point", fun.data = "mean_cl_normal", size = 7, aes(colour = Classification, fill = Classification, label = Classification, shape = Classification)) +
    stat_summary(geom = "errorbar", width = .5, fun.data = "mean_cl_normal", size = 1.5, aes(colour = Classification, fill = Classification, label = Classification, shape = Classification)) +
    facet_wrap(~var, scales = "free_y", nrow = 2, shrink = F) +
    scale_shape_manual(" ",
                         values=c("dementia" = 1,
                                  "probdem_LW_prev" = 25, "probdem_LW_iadl_prev" = 24, 
                                  "probdem_LW" = 6, "probdem_LW_iadl" = 2, 
                                  "probdem_glm_w_iv" = 23, "probdem_ranger_smote_iv" = 22, 
                                  "probdem_xgb_smote_iv" = 21),
                         labels=c("Self-report",
                                  parse(text = "'LW (Recall)'^'P'"), parse(text = "'LW (Recall & IADL)'^'P'"), 
                                  "LW (Recall)", "LW (Recall & IADL)", 
                                  "GLM (weighted)", "RF SMOTE", "XGB SMOTE")) +
    scale_colour_manual(" ", 
                        values=c("dementia" = "#50C878",
                                 "probdem_LW_prev" = "#053061", "probdem_LW_iadl_prev" = "#2166AC", 
                                 "probdem_LW" = "#4393C3", "probdem_LW_iadl" = "#92C5DE", 
                                 "probdem_glm_w_iv" = "#B2182B", "probdem_ranger_smote_iv" = "#800026", 
                                 "probdem_xgb_smote_iv" = "#D6604D"),
                        labels = c("Self-report",
                                   parse(text = "'LW (Recall)'^'P'"), parse(text = "'LW (Recall & IADL)'^'P'"), 
                                   "LW (Recall)", "LW (Recall & IADL)", 
                                   "GLM (weighted)", "RF SMOTE", "XGB SMOTE")) +
    scale_fill_manual(" ",
                      values=c("dementia" = "#50C878",
                               "probdem_LW_prev" = "#053061", "probdem_LW_iadl_prev" = "#2166AC", 
                               "probdem_LW" = "#4393C3", "probdem_LW_iadl" = "#92C5DE", 
                               "probdem_glm_w_iv" = "#B2182B", "probdem_ranger_smote_iv" = "#800026", 
                               "probdem_xgb_smote_iv" = "#D6604D"),
                      labels = c("Self-report",
                                 parse(text = "'LW (Recall)'^'P'"), parse(text = "'LW (Recall & IADL)'^'P'"), 
                                 "LW (Recall)", "LW (Recall & IADL)", 
                                 "GLM (weighted)", "RF SMOTE", "XGB SMOTE")) +
    scale_alpha_continuous(guide = "none") +
    ylab("") + xlab("") +
    theme_bw() + 
    theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), 
          panel.grid.major.x = element_blank(), plot.title = element_text(face = "bold"),
          text = element_text(size = 25), legend.position = "bottom", legend.text.align = c(0),
          legend.key.width = unit(.25, "cm"),
          legend.key.size = unit(1.5, "cm"),
          legend.text = element_text(size = 23)) +
    stat_pvalue_manual(stats_imp %>% filter(group1 == "dementia"), hide.ns = T, size = 10) +
    geom_label(data = plot_dat %>% 
                 group_by(Classification, var) %>% 
                 summarise(n = n(), ypos = mean(value) - sd(value) + 1),
               inherit.aes = FALSE, aes(Classification, ypos, label = n),
               size = 6)
}
AUC_func = function(data, probdem_include){
  ### computes AUC for every tested algorithm
  ### requires:
  ### data: data frame containing all classifications, actual dementia status and predicted probabilities
  ### probdem_include: character vector with algorithms to compute AUC for
  
  # compute AUC and bind to df
  AUC = c()
  for(i in 1:length(probdem_include)){
    AUC_temp = ifelse(grepl("LW", probdem_include[i]),
                      AUC(ifelse(data[,probdem_include[i]] == "dementia", 1, 0), data$dementia_dbl),
                      AUC(data[,str_replace(probdem_include[i], "probdem_", "pred_")], data$dementia_dbl))
    AUC = c(AUC, AUC_temp)
  }
  
  AUC_df = data.frame(
    alg = rename_alg_short(probdem_include),
    AUC = AUC)
  
  return(AUC_df)
}
