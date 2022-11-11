ci.adjusted_MrJerryTAO <- function(
    
  data, 
  # data should be a data frame in the long form with each subject 
  # measured under different conditions on separate rows, 
  # but multiple outcome measures by different columns are allowed. 
  
  level = "individual", 
  # "individual" for individual measures, like personal raw scores
  # "model" for model estimates, like coefficients from bootstrapped samples
  # "group" for group summary statistics where one or more among 
  # CI, SE, SD, size or mean of the group needs to be provided. 
  
  varname = NULL, subject = NULL, between = NULL, repeated = NULL, 
  # varname = c(), subject, between, repeated can be variable names without 
  # quotation marks. varnam can take multiple outcome measures in c()
  # To specify units, groups, and levels defined by multiple variables, use 
  # interaction() or list() to include intersections such as 
  # between = interaction(var1, var2, sep = ":") or 
  # repeated = list(var1, var2, var3)
  # In the returned object, these grouping variables will be factors
  
  cutoff = NULL, relation = NULL, inclusive = NULL, 
  # cutoff transform original values to I(value ? cutoff)
  # relation = "greater"/"less", default is greater
  # inclusive = T/F about the cutoff point, default is T
  
  minimum = NULL, maximum = NULL, tolerance = 1e-10, 
  # lower = c(), upper = c() for range-preserving CIs must be numeric vectors 
  # with length and positions matching varname
  # If null or NA, lower and upper theoretical limits of possible CIs 
  # are take from the minimum and maximum of raw scores. 
  # Group means that can lie exactly at these limit will result in NaN
  # of range-preserving CIs, for which a small tolerance is recommended
  # such as minimum = 0 - 1e-10, maximum = 100 + 1
  
  alpha = 0.05, bonferroni = F, 
  # Significance level and whether its adjustment for family-wise 
  # mutliple comparisons should be implemented
  
  # The following only apply to level = "group", where argument subject = ,
  # repeated = , cutoff = , relation = , and inclusive = are ignored
  estimate = NULL, 
  # If not provided, it will be calculated as midpoint of CI.
  se = NULL, 
  # If not provided, it will be calculated as SD/sqrt(n) if sd = and size = 
  # are available or half of CI width divided by t statistic. 
  lcl = NULL, ucl = NULL, 
  # If not provided, both will be calculated as estimate ± t × SE
  sd = NULL, 
  # Provide to calculate SE
  size = NULL 
  # Provide to adjust degree of freedom, or normal distribution assumed. 
  
) {
  options(dplyr.summarise.inform = FALSE)
  output <- data.frame()
  
  if(level %in% c("individual", "model")) {
    output <- data %>%
      pivot_longer(
        cols = {{varname}}, names_to = "varname", values_to = "value") %>%
      mutate(
        value = {if (is.null({{cutoff}})) {value}
          else if (length({{cutoff}}) == 2 && (
            is.null({{inclusive}}) || {{inclusive}} == T)) {
            value >= {{cutoff}}[1] & value <= {{cutoff}}[2]}
          else if (length({{cutoff}}) == 2 && {{inclusive}} == F) {
            value > {{cutoff}}[1] & value < {{cutoff}}[2]}
          else if (is.null({{relation}})) {
            value >= {{cutoff}} }
          else if ({{relation}} == "less" && (
            is.null({{inclusive}}) || {{inclusive}} == T)) {
            value <= {{cutoff}} }
          else if ({{relation}} == "less" && {{inclusive}} == F) {
            value < {{cutoff}} }
          else if ({{relation}} == "greater" && (
            is.null({{inclusive}}) || {{inclusive}} == T)) {
            value >= {{cutoff}}}
          else if ({{relation}} == "greater" && {{inclusive}} == F) {
            value > {{cutoff}} } },
        subject = if (is.null({{subject}})) {row_number()} else {
          interaction({{subject}}, sep = ":") },
        between = if (is.null({{between}})) {factor("none")} else {
          interaction({{between}}, sep = ":") },
        repeated = if (is.null({{repeated}})) {factor("none")} else {
          interaction({{repeated}}, sep = ":") } ) %>%
      group_by(varname) %>%
      mutate( #  n_groups(.)
        nrepeated = n_distinct(repeated),
        nbetween = n_distinct(between),
        nrepeatedbetween = nrow(distinct(cur_data(), repeated, between)),
        min_lower = {if(
          is.null({{minimum}}) || is.na({{minimum}}[cur_group_id()]))
          min(value, na.rm = T) - {{tolerance}}
          else {{minimum}}[cur_group_id()] },
        max_upper = {if(
          is.null({{maximum}}) || is.na({{maximum}}[cur_group_id()]))
          max(value, na.rm = T) + {{tolerance}}
          else {{maximum}}[cur_group_id()] },
      ) %>%
      group_by(varname, between) %>%                 # mean centering
      mutate(grandmean = mean(value, na.rm = T)) %>% # grand mean per group
      group_by(varname, between, subject) %>%
      mutate(                               # remove between_subject errors
        value_norm = value - mean(value, na.rm = T) + grandmean) %>%
      group_by(varname, between, repeated) %>%
      summarise(
        min_lower = mean(min_lower, na.rm = T),
        max_upper = mean(max_upper, na.rm = T),
        
        n_subject = length(unique(subject)),
        n_observation = n(),
        n_missing = sum(is.na(value)),
        n_measured = sum(!is.na(value)),
        n_repeated = mean(nrepeated),
        n_between = mean(nbetween),
        n_repeatedbetween = mean(nrepeatedbetween),
        t_regular = qt(1-{{alpha}}/2, df = n_measured-1),
        
        mean_regular = mean(value, na.rm = T), # for mean value
        mean_norm = mean(value_norm, na.rm = T),
        sd_regular = sd(value, na.rm = T),
        sd_norm = sd(value_norm, na.rm = T) *
          ifelse(n_repeated <= 1, 1, sqrt(n_repeated/(n_repeated-1))),
        se_regular = if ({{level}} == "summary") sd_regular else
          sd_regular/sqrt(n_measured),
        se_norm = if ({{level}} == "summary") sd_norm else
          sd_norm/sqrt(n_measured),
        ci_regular = t_regular * ifelse(se_regular<=0, NA, se_regular),
        ci_norm = t_regular * ifelse(se_norm<=0, NA, se_norm),
        ratio_senorm = ifelse(
          n_repeated <= 1 | se_regular == 0 | se_norm == 0, 1,
          se_norm/se_regular),
        
        # for x >= a, both lower and upper higher than regular
        sda_regular = sd_regular / (mean_regular - min_lower),
        sda_norm = sd_norm / (mean_regular - min_lower),
        sea_regular = se_regular / (mean_regular - min_lower),
        sea_norm = se_norm / (mean_regular - min_lower),
        cia_regular = t_regular * ifelse(sea_regular<=0, NA, sea_regular),
        cia_norm = t_regular * ifelse(sea_norm<=0, NA, sea_norm),
        ratioa_senorm = ifelse(
          n_repeated <= 1 | sea_regular == 0 | sea_norm == 0, 1,
          sea_norm/sea_regular),
        
        # for x <= b, both lower and upper lower than regular
        sdb_regular = sd_regular / (max_upper - mean_regular),
        sdb_norm = sd_norm / (max_upper - mean_regular),
        seb_regular = se_regular / (max_upper - mean_regular),
        seb_norm = se_norm / (max_upper - mean_regular),
        cib_regular = t_regular * ifelse(seb_regular<=0, NA, seb_regular),
        cib_norm = t_regular * ifelse(seb_norm<=0, NA, seb_norm),
        ratiob_senorm = ifelse(
          n_repeated <= 1 | seb_regular == 0 | seb_norm == 0, 1,
          seb_norm/seb_regular),
        
        # for a <= x <= b, will drag both limits towards center
        sdab_regular = sd_regular * (max_upper - min_lower) / (
          (mean_regular - min_lower) *(max_upper - mean_regular)),
        sdab_norm = sd_norm * (max_upper - min_lower) / (
          (mean_regular - min_lower) *(max_upper - mean_regular)),
        seab_regular = if ({{level}} == "summary") sdab_regular else
          sdab_regular/sqrt(n_measured),
        seab_norm = if ({{level}} == "summary") sdab_norm else
          sdab_norm/sqrt(n_measured),
        ciab_regular = t_regular * ifelse(seab_regular<=0, NA, seab_regular),
        ciab_norm = t_regular * ifelse(seab_norm<=0, NA, seab_norm),
        ratioab_senorm = ifelse(
          n_repeated <= 1 | seab_regular == 0 | seab_norm == 0, 1,
          seab_norm/seab_regular),
        
        # Single group CI: lower_regular upper_regular as in t.test()
        # If sd=0, then CI is NA
        lower_regular = mean_regular - ci_regular,
        upper_regular = mean_regular + ci_regular,
        lowera_regular = min_lower + (
          mean_regular - min_lower) * exp(-cia_regular),
        uppera_regular = min_lower + (
          mean_regular - min_lower) * exp(cia_regular),
        lowerb_regular = max_upper - (
          max_upper - mean_regular) * exp(cib_regular),
        upperb_regular = max_upper - (
          max_upper - mean_regular) * exp(-cib_regular),
        lowerab_regular = (
          min_lower + max_upper *
            (mean_regular - min_lower)/(max_upper - mean_regular) *
            exp(-ciab_regular)) /
          (1 + (mean_regular - min_lower)/(max_upper - mean_regular) *
             exp(-ciab_regular)),
        upperab_regular = (
          min_lower + max_upper *
            (mean_regular - min_lower)/(max_upper - mean_regular) *
            exp(ciab_regular)) /
          (1 + (mean_regular - min_lower)/(max_upper - mean_regular) *
             exp(ciab_regular)),
        
        # Single group CI for proportions
        mean_prop = mean(value, na.rm = T), # for wilson prop
        mean_proptest = (
          mean_prop + t_regular^2 /(2 * n_measured) ) / (
            1 + t_regular^2/n_measured),
        se_proptest = sqrt( # doable with 0 count in 1 subject
          mean_prop * (1 - mean_prop)/n_measured +
            t_regular^2/(2 * n_measured)^2) / (
              1 + t_regular^2/n_measured),
        ci_proptest = t_regular*se_proptest,
        # Wilson prop CI: lower_proptest upper_proptest as in prop.test()
        lower_proptest = mean_proptest - ci_proptest,
        upper_proptest = mean_proptest + ci_proptest,
      ) %>%
      group_by(varname) %>%
      mutate(
        # Adjust the number of groups under comparison so that
        # groups with only one measurement is not considered (missing se)
        n_repeatedbetween = n_repeatedbetween - sum(n_measured<=1),
        pt_adjalpha = 1-alpha/(2 * (
          if ({{bonferroni}}) {n_repeatedbetween} else {1})) ,
        ratio_adjdf = qt(pt_adjalpha, df = case_when( # adjust df
          n_repeatedbetween == 1 ~ n_measured-1,
          n_repeated <= 1 ~ (sum(se_regular^2, na.rm = T))^2/
            sum(se_regular^4/(n_measured-1), na.rm = T),
          TRUE ~ (sum(se_norm^2, na.rm = T))^2/
            sum(se_norm^4/(n_measured-1), na.rm = T) )) / t_regular,
        ratio_tryon = case_when( # adjust SE ratio of normalized SE
          n_repeatedbetween == 1 ~ 1,
          n_repeated <= 1 ~ sqrt(2)/2 * sqrt(n_repeatedbetween) *
            sqrt(sum(se_regular^2, na.rm = T))/sum(se_regular, na.rm = T),
          TRUE ~ sqrt(2)/2 * sqrt(n_repeatedbetween) *
            sqrt(sum(se_norm^2, na.rm = T))/sum(se_norm, na.rm = T) ),
        # Difference-adjusted between-group CI: lower_tryon upper_tryon
        # Comparable to Welch t.test()
        lower_tryon = mean_regular - ci_regular*ratio_adjdf*ratio_tryon,
        upper_tryon = mean_regular + ci_regular*ratio_adjdf*ratio_tryon,
        # Difference-adjusted repeated-measures: lower_norm upper_norm
        # Comparable to repeated measures linear models
        # Same as using ratio_senorm to adjust ci_regular*ratio_senorm
        lower_norm = mean_regular - ci_norm*ratio_adjdf*ratio_tryon,
        upper_norm = mean_regular + ci_norm*ratio_adjdf*ratio_tryon,
        
        # for x >= a, generally wider than without range limiting
        ratioa_adjdf = qt(pt_adjalpha, df = case_when( # for x > a
          n_repeatedbetween == 1 ~ n_measured-1,
          n_repeated <= 1 ~ (sum(sea_regular^2, na.rm = T))^2/
            sum(sea_regular^4/(n_measured-1), na.rm = T),
          TRUE ~ (sum(sea_norm^2, na.rm = T))^2/
            sum(sea_norm^4/(n_measured-1), na.rm = T) )) / t_regular,
        ratioa_tryon = case_when( # adjust SE ratio of normalized SE
          n_repeatedbetween == 1 ~ 1,
          n_repeated <= 1 ~ sqrt(2)/2 * sqrt(n_repeatedbetween) *
            sqrt(sum(sea_regular^2, na.rm = T))/sum(sea_regular, na.rm = T),
          TRUE ~ sqrt(2)/2 * sqrt(n_repeatedbetween) *
            sqrt(sum(sea_norm^2, na.rm = T))/sum(sea_norm, na.rm = T) ),
        lowera_tryon = min_lower + (mean_regular - min_lower) *
          exp(-cia_regular*ratioa_adjdf*ratioa_tryon),
        uppera_tryon = min_lower + (mean_regular - min_lower) *
          exp(cia_regular*ratioa_adjdf*ratioa_tryon),
        lowera_norm = min_lower + (mean_regular - min_lower) *
          exp(-cia_norm*ratioa_adjdf*ratioa_tryon),
        uppera_norm = min_lower + (mean_regular - min_lower) *
          exp(cia_norm*ratioa_adjdf*ratioa_tryon),
        
        # for x <= b, generally wider than without range limiting
        ratiob_adjdf = qt(pt_adjalpha, df = case_when( # for x > a
          n_repeatedbetween == 1 ~ n_measured-1,
          n_repeated <= 1 ~ (sum(seb_regular^2, na.rm = T))^2/
            sum(seb_regular^4/(n_measured-1), na.rm = T),
          TRUE ~ (sum(seb_norm^2, na.rm = T))^2/
            sum(seb_norm^4/(n_measured-1), na.rm = T) )) / t_regular,
        ratiob_tryon = case_when( # adjust SE ratio of normalized SE
          n_repeatedbetween == 1 ~ 1,
          n_repeated <= 1 ~ sqrt(2)/2 * sqrt(n_repeatedbetween) *
            sqrt(sum(seb_regular^2, na.rm = T))/sum(seb_regular, na.rm = T),
          TRUE ~ sqrt(2)/2 * sqrt(n_repeatedbetween) *
            sqrt(sum(seb_norm^2, na.rm = T))/sum(seb_norm, na.rm = T) ),
        lowerb_tryon = max_upper - (max_upper - mean_regular) *
          exp(cib_regular*ratiob_adjdf*ratiob_tryon),
        upperb_tryon = max_upper - (max_upper - mean_regular) *
          exp(-cib_regular*ratiob_adjdf*ratiob_tryon),
        lowerb_norm = max_upper - (max_upper - mean_regular) *
          exp(cib_norm*ratiob_adjdf*ratiob_tryon),
        upperb_norm = max_upper - (max_upper - mean_regular) *
          exp(-cib_norm*ratiob_adjdf*ratiob_tryon),
        
        # for a <= x <= b, generally wider than without range limiting
        # compared to uppernorm_proptest, drag limits more extreme to 0 and 1
        ratioab_adjdf = qt(pt_adjalpha, df = case_when( # for x > a
          n_repeatedbetween == 1 ~ n_measured-1,
          n_repeated <= 1 ~ (sum(seab_regular^2, na.rm = T))^2/
            sum(seab_regular^4/(n_measured-1), na.rm = T),
          TRUE ~ (sum(seab_norm^2, na.rm = T))^2/
            sum(seab_norm^4/(n_measured-1), na.rm = T) )) / t_regular,
        ratioab_tryon = case_when( # adjust SE ratio of normalized SE
          n_repeatedbetween == 1 ~ 1,
          n_repeated <= 1 ~ sqrt(2)/2 * sqrt(n_repeatedbetween) *
            sqrt(sum(seab_regular^2, na.rm = T))/sum(seab_regular, na.rm = T),
          TRUE ~ sqrt(2)/2 * sqrt(n_repeatedbetween) *
            sqrt(sum(seab_norm^2, na.rm = T))/sum(seab_norm, na.rm = T) ),
        lowerab_tryon = (
          min_lower + max_upper *
            (mean_regular - min_lower)/(max_upper - mean_regular) *
            exp(-ciab_regular*ratioab_adjdf*ratioab_tryon)) /
          (1 + (mean_regular - min_lower)/(max_upper - mean_regular) *
             exp(-ciab_regular*ratioab_adjdf*ratioab_tryon)),
        upperab_tryon = (
          min_lower + max_upper *
            (mean_regular - min_lower)/(max_upper - mean_regular) *
            exp(ciab_regular*ratioab_adjdf*ratioab_tryon)) /
          (1 + (mean_regular - min_lower)/(max_upper - mean_regular) *
             exp(ciab_regular*ratioab_adjdf*ratioab_tryon)),
        lowerab_norm = (
          min_lower + max_upper *
            (mean_regular - min_lower)/(max_upper - mean_regular) *
            exp(-ciab_norm*ratioab_adjdf*ratioab_tryon)) /
          (1 + (mean_regular - min_lower)/(max_upper - mean_regular) *
             exp(-ciab_norm*ratioab_adjdf*ratioab_tryon)),
        upperab_norm = (
          min_lower + max_upper *
            (mean_regular - min_lower)/(max_upper - mean_regular) *
            exp(ciab_norm*ratioab_adjdf*ratioab_tryon)) /
          (1 + (mean_regular - min_lower)/(max_upper - mean_regular) *
             exp(ciab_norm*ratioab_adjdf*ratioab_tryon)),
        
        # For prop adjustment
        ratio_dfprop = qt(pt_adjalpha, df = case_when(
          n_repeatedbetween == 1 ~ n_measured-1,
          n_repeated <= 1 ~ (sum(se_proptest^2, na.rm = T))^2/
            sum(se_proptest^4/(n_measured-1), na.rm = T),
          TRUE ~ (sum((se_proptest*ratio_senorm)^2, na.rm = T))^2/
            sum((se_proptest*ratio_senorm)^4/(n_measured-1),
                na.rm = T) )) / t_regular,
        ratio_tryonprop = case_when(
          n_repeatedbetween == 1 ~ 1,
          n_repeated <= 1 ~ sqrt(2)/2 * sqrt(n_repeatedbetween) *
            sqrt(sum(se_proptest^2, na.rm = T))/sum(se_proptest, na.rm = T),
          TRUE ~ sqrt(2)/2 * sqrt(n_repeatedbetween) * sqrt(sum(
            (se_proptest*ratio_senorm)^2, na.rm = T))/
            sum(se_proptest*ratio_senorm, na.rm = T)),
        # Wilson prop CI : lower_tryon upper_tryon
        # adjust tryon ratio based on proptest SE
        lowernorm_proptest = mean_proptest -
          ci_proptest*ratio_senorm*ratio_dfprop*ratio_tryonprop,
        uppernorm_proptest = mean_proptest +
          ci_proptest*ratio_senorm*ratio_dfprop*ratio_tryonprop,
        # adjust tryon ratio based on original score SE, wider CI
        lowernorm_proptest2 = mean_proptest -
          ci_proptest*ratio_senorm*ratio_adjdf*ratio_tryon,
        uppernorm_proptest2 = mean_proptest +
          ci_proptest*ratio_senorm*ratio_adjdf*ratio_tryon,
        
        # confine prop CI in [0, mean, 1]
        lower_proptest = case_when(
          lower_proptest < 0 ~ 0,
          lower_proptest > mean_prop ~ mean_prop,
          T ~ lower_proptest),
        upper_proptest = case_when(
          upper_proptest > 1 ~ 1,
          upper_proptest < mean_prop ~ mean_prop,
          T ~ upper_proptest),
        lowernorm_proptest = case_when(
          lowernorm_proptest < 0 ~ 0,
          lowernorm_proptest > mean_prop ~ mean_prop,
          T ~ lowernorm_proptest),
        uppernorm_proptest = case_when(
          uppernorm_proptest > 1 ~ 1,
          uppernorm_proptest < mean_prop ~ mean_prop,
          T ~ uppernorm_proptest),
        lowernorm_proptest2 = case_when(
          lowernorm_proptest2 < 0 ~ 0,
          lowernorm_proptest2 > mean_prop ~ mean_prop,
          T ~ lowernorm_proptest2),
        uppernorm_proptest2 = case_when(
          uppernorm_proptest2 > 1 ~ 1,
          uppernorm_proptest2 < mean_prop ~ mean_prop,
          T ~ uppernorm_proptest2),
      )
  }
  
  if(level == "group") {
    output <- data %>% 
      mutate(
        varname = if (is.null({{varname}})) {"none"} else {
          interaction({{varname}}, sep = ":") }, 
        between = if (is.null({{between}})) {"none"} else {
          interaction({{between}}, sep = ":") } ) %>%
      group_by(varname) %>%
      mutate(n_between = n_distinct(between)) %>% 
      group_by(varname, between) %>%
      mutate(n_between = mean(n_between)) %>%
      group_by(varname) %>%
      mutate(
        
        t_regular = {if(is.null({{size}})) 
          qnorm(1-{{alpha}}/2) 
          else qt(1-{{alpha}}/2, df = {{size}}-1) },
        
        estimate = {if(!is.null({{estimate}})) {{estimate}} 
          else if(!is.null({{lcl}}) && !is.null({{ucl}})) {
            ({{ucl}} + {{lcl}})/2 } 
          else as.numeric(NA)},
        
        se_regular = {if(!is.null({{se}})) {{se}}
          else if(!is.null({{sd}}) && !is.null({{size}})) {
            {{sd}} / sqrt({{size}}) }
          else if(!is.null({{lcl}}) && !is.null({{ucl}})) {
            ({{ucl}} - {{lcl}})/2/t_regular }
          else as.numeric(NA) },
        
        ci_lower = {if(!is.null({{estimate}}) && !is.null({{lcl}})) {
          {{estimate}} - {{lcl}} }
          else if(!is.null({{lcl}}) & !is.null({{ucl}}))
            ({{ucl}} - {{lcl}})/2
          else if(!all(is.na(se_regular))) {t_regular * se_regular}
          else as.numeric(NA) },
        
        ci_upper = {if(!is.null({{estimate}}) && !is.null({{ucl}})) {
          {{ucl}} - {{estimate}} }
          else if(!is.null({{lcl}}) & !is.null({{ucl}}))
            ({{ucl}} - {{lcl}})/2
          else if(!all(is.na(se_regular))) {t_regular * se_regular}
          else as.numeric(NA) },
        
        # Range-preserving CI
        min_lower = {if(
          is.null({{minimum}}) || is.na({{minimum}}[cur_group_id()])) 
          min(estimate, na.rm = T) - {{tolerance}}
          else {{minimum}}[cur_group_id()] }, 
        max_upper = {if(
          is.null({{maximum}}) || is.na({{maximum}}[cur_group_id()])) 
          max(estimate, na.rm = T) + {{tolerance}}
          else {{maximum}}[cur_group_id()] }, 
        
        # for x >= a, both lower and upper higher than regular
        sea_regular = se_regular / (estimate - min_lower),
        cia_lower = ci_lower * sea_regular / se_regular, 
        cia_upper = ci_upper * sea_regular / se_regular, 
        
        # for x <= b, both lower and upper lower than regular
        seb_regular = se_regular / (max_upper - estimate),
        cib_lower = ci_lower * seb_regular / se_regular, 
        cib_upper = ci_upper * seb_regular / se_regular, 
        
        # for a <= x <= b, will drag both limits towards center
        seab_regular = se_regular * (max_upper - min_lower) / (
          (estimate - min_lower) *(max_upper - estimate)),
        ciab_lower = ci_lower * seab_regular / se_regular, 
        ciab_upper = ci_upper * seab_regular / se_regular, 
        
        # Single group CI: lower_regular upper_regular as in t.test(x)
        # If sd=0, then CI is NA
        lower_regular = estimate - ci_lower, 
        upper_regular = estimate + ci_upper, 
        lowera_regular = min_lower + (
          estimate - min_lower) * exp(-cia_lower), 
        uppera_regular = min_lower + (
          estimate - min_lower) * exp(cia_upper), 
        lowerb_regular = max_upper - (
          max_upper - estimate) * exp(cib_lower), 
        upperb_regular = max_upper - (
          max_upper - estimate) * exp(-cib_upper), 
        lowerab_regular = (
          min_lower + max_upper * 
            (estimate - min_lower)/(max_upper - estimate) * 
            exp(-ciab_lower)) / 
          (1 + (estimate - min_lower)/(max_upper - estimate) * 
             exp(-ciab_lower)), 
        upperab_regular = (
          min_lower + max_upper * 
            (estimate - min_lower)/(max_upper - estimate) * 
            exp(ciab_upper)) / 
          (1 + (estimate - min_lower)/(max_upper - estimate) * 
             exp(ciab_upper)), 
        
        # Difference-adjusted between-group CI: lower_tryon upper_tryon
        pt_adjalpha = 1-alpha/(2 * (
          if({{bonferroni}}) {n_between} else {1})) ,
        ratio_adjdf = {if(is.null({{size}})) {1}
          else qt(pt_adjalpha, df = case_when( # adjust df normalized SE
            n_between == 1 ~ {{size}} - 1,
            TRUE ~ (sum(se_regular^2))^2/sum(se_regular^4/({{size}}-1)) )
          ) / t_regular },
        ratio_tryon = case_when( # adjust SE ratio of normalized SE
          n_between == 1 ~ 1,
          TRUE ~ sqrt(2)/2 * sqrt(n_between) * 
            sqrt(sum(se_regular^2))/sum(se_regular) ),
        
        # Comparable to Welch t.test(x~g)
        lower_tryon = estimate - ci_lower*ratio_adjdf*ratio_tryon,
        upper_tryon = estimate + ci_upper*ratio_adjdf*ratio_tryon,
        
        # for x >= a, generally wider than without range limiting
        ratioa_adjdf = {if(is.null({{size}})) {1}
          else qt(pt_adjalpha, df = case_when( # adjust df normalized SE
            n_between == 1 ~ {{size}} - 1,
            TRUE ~ (sum(sea_regular^2))^2/sum(sea_regular^4/({{size}}-1)) )
          ) / t_regular },
        ratioa_tryon = case_when( # adjust SE ratio of normalized SE
          n_between == 1 ~ 1,
          TRUE ~ sqrt(2)/2 * sqrt(n_between) * 
            sqrt(sum(sea_regular^2))/sum(sea_regular) ),
        lowera_tryon = min_lower + (estimate - min_lower) * 
          exp(-cia_lower*ratioa_adjdf*ratioa_tryon), 
        uppera_tryon = min_lower + (estimate - min_lower) * 
          exp(cia_upper*ratioa_adjdf*ratioa_tryon), 
        
        # for x <= b, generally wider than without range limiting
        ratiob_adjdf = {if(is.null({{size}})) {1}
          else qt(pt_adjalpha, df = case_when( # adjust df normalized SE
            n_between == 1 ~ {{size}} - 1,
            TRUE ~ (sum(seb_regular^2))^2/sum(seb_regular^4/({{size}}-1)) )
          ) / t_regular },
        ratiob_tryon = case_when( # adjust SE ratio of normalized SE
          n_between == 1 ~ 1,
          TRUE ~ sqrt(2)/2 * sqrt(n_between) * 
            sqrt(sum(seb_regular^2))/sum(seb_regular) ),
        lowerb_tryon = max_upper - (max_upper - estimate) * 
          exp(cib_lower*ratiob_adjdf*ratiob_tryon), 
        upperb_tryon = max_upper - (max_upper - estimate) * 
          exp(-cib_upper*ratiob_adjdf*ratiob_tryon), 
        
        # for a <= x <= b, generally wider than without range limiting
        # compared to uppernorm_proptest, drag limits more extreme to 0 and 1
        ratioab_adjdf = {if(is.null({{size}})) {1}
          else qt(pt_adjalpha, df = case_when( # adjust df normalized SE
            n_between == 1 ~ {{size}} - 1,
            TRUE ~ (sum(seab_regular^2))^2/sum(
              seab_regular^4/({{size}}-1)) )
          ) / t_regular },
        ratioab_tryon = case_when( # adjust SE ratio of normalized SE
          n_between == 1 ~ 1,
          TRUE ~ sqrt(2)/2 * sqrt(n_between) * 
            sqrt(sum(seab_regular^2))/sum(seab_regular) ),
        lowerab_tryon = (
          min_lower + max_upper * 
            (estimate - min_lower)/(max_upper - estimate) * 
            exp(-ciab_lower*ratioab_adjdf*ratioab_tryon)) / 
          (1 + (estimate - min_lower)/(max_upper - estimate) * 
             exp(-ciab_lower*ratioab_adjdf*ratioab_tryon)), 
        upperab_tryon = (
          min_lower + max_upper * 
            (estimate - min_lower)/(max_upper - estimate) * 
            exp(ciab_upper*ratioab_adjdf*ratioab_tryon)) / 
          (1 + (estimate - min_lower)/(max_upper - estimate) * 
             exp(ciab_upper*ratioab_adjdf*ratioab_tryon)), 
      )
  }
  return(output %>% ungroup())
}