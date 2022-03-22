# functions and preferences
# for the handgrip-WM project
# written by shelby bachman, sbachman@usc.edu


# thresholds --------------------------------------------------------------

thresh <- 0.7  # threshold for excluding individual pupil segments based on missingness


# parameters --------------------------------------------------------------

nperm_aov <- 10000  # number of permutations for permuted ANOVAs
nboot <- 1000       # number of bootstrap samples for bootstrapped comparisons


# figure settings ---------------------------------------------------------

# color scheme based on group (handgrip/control)
cols_group <- c('Handgrip' = '#332288', 'Control' = '#999933')

# rotate x-axis labels
theme_rotatex <- theme(axis.text.x = element_text(angle = 45, hjust = 1))

# set text size
theme_font <- theme(text = element_text(size = 16))

# set text size for manuscript-ready figures
theme_font_pub <- theme(text = element_text(size = 12),
                        axis.text.x = element_text(size = 9),
                        axis.text.y = element_text(size = 9),
                        axis.title.y = element_text(size = 10),
                        axis.title.x = element_text(size = 10))

# dpi to save figures
fig_dpi <- 600


# function to remove outliers using MAD-median rule -----------------------

rm_outliers_mad <- function(x) {
  sample_median <- median(x, na.rm = TRUE)
  sample_mad <- mad(x, na.rm = TRUE)
  mad_med <- ifelse(is.na(x), NA, ((x - sample_median) / (1.483 * sample_mad)))
  x_rmout <- ifelse(mad_med > 2.24, NA, x)
  n_out <- sum(mad_med > 2.24, na.rm = TRUE)
  
  result <- vector(mode = 'list', length = 2)
  result[[1]] <- x_rmout
  result[[2]] <- n_out
  return(result)
}


# function to create half-violin plot -------------------------------------

# load function for geom_violin_flat():
source("https://raw.githubusercontent.com/datavizpyr/data/master/half_flat_violinplot.R")


# function to create cross-bar (used with half-violin plot) ---------------

stat_sum_df <- function(fun, geom="crossbar", ...) {
  stat_summary(fun.data = fun, colour = "black", alpha = 0.6, geom = geom, width = 0.1, ...)
}


# functions to format ANOVA output for tables -----------------------------

format_aov_ez <- function(aov_ez_obj) {
  aov_df <- aov_ez_obj$anova_table %>%
    as.data.frame()
  
  # extract effect names
  aov_df$Effect <- rownames(aov_df)
  
  # remove row names
  rownames(aov_df) <- c()
  
  # format columns of interest
  aov_df <- aov_df %>%
    mutate(df = str_c(round(`num Df`, digits = 2), ', ', round(`den Df`, digits = 2)),
           Effect = fix_effect_name(Effect)) %>%
    select(Effect, df, `F`, pes, `p` = `Pr(>F)`)
  
  # rounding
  aov_df <- aov_df %>%
    mutate(`F` = round(`F`, digits = 2),
           pes = round(pes, digits = 3),
           `p` = fix_pval_table(`p`))
  
  return(aov_df)
}

format_aovperm <- function(aovperm_obj) {
  aov_df <- aovperm_obj$table
  aov_df$Effect <- rownames(aov_df)
  # add partial eta-squared as column
  eta2_vals <- F_to_eta2(f = aov_df$F, 
                         df = aov_df$dfn,
                         df_error = aov_df$dfd)
  aov_df$pes <- eta2_vals$Eta2_partial
  
  # fix p-values
  aov_df$`Parametric p` <- fix_pval_table(aov_df$`parametric P(>F)`)
  aov_df$`Permutation p` <- fix_pval_table(aov_df$`permutation P(>F)`)
  
  rownames(aov_df) <- NULL
  aov_df <- aov_df %>%
    rowwise() %>%
    mutate(`F` = sprintf('%.2f', round(`F`, digits = 2)),
           `df` = paste(dfn, ', ', dfd, sep = ''),
           pes = round(pes, digits = 3)) %>%
    mutate(Effect = fix_effect_name(Effect)) %>%
    select(Effect, df, `F`, pes, `Parametric p`, `Permutation p`)
  
  return(as.data.frame(aov_df)) 
}


# function to format effect names for tables ------------------------------

fix_effect_name <- function(eff_name) {
  eff_name_fixed <- str_replace_all(eff_name, ':', ' x ')
  eff_name_fixed <- case_when(
    eff_name == 'age_group' ~ 'Age group',
    eff_name_fixed == 'group' ~ 'Group',
    eff_name_fixed == 'event' ~ 'Phase',
    eff_name_fixed == 'condition' ~ 'Load',
    eff_name_fixed == 'group x condition' ~ 'Group x Load',
    eff_name_fixed == 'group x age_group' ~ 'Group x Age group',
    eff_name_fixed == 'group x event' ~ 'Group x Phase',
    eff_name_fixed == 'age_group x event' ~ 'Age group x Phase',
    eff_name_fixed == 'group x age_group x event' ~ 'Group x Age group x Phase',
    eff_name_fixed == 'sample' ~ 'Timepoint',
    eff_name_fixed == 'sample x group' ~ 'Timepoint x Group',
    eff_name_fixed == 'sample x age_group' ~ 'Timepoint x Age group',
    eff_name_fixed == 'sample x group x age_group' ~ 'Timepoint x Group x Age group',
    eff_name_fixed == 'block_recoded' ~ 'Block',
    eff_name_fixed == 'group x block_recoded' ~ 'Group x Block'
  )  
  return(eff_name_fixed)
}


# function to format a column of p-values for a table ---------------------

# converts p-values to be 3 digits or <.001 as needed, for text and tables
fix_pval_table <- function(p, digits = 3) {
  new_p <- vector(mode = 'character', length = length(p))
  for (ii in 1:length(p)) {
    if (p[ii] < .001) {
      new_p[ii] <- '<.001' 
    } else {
      new_p[ii] <- as.character(printnum(p[ii], digits = digits))
    }
  }
  
  return(new_p)
}


# function to compute bootstrapped p-values -------------------------------

# get p-values from the output of a call to Boot()
boot_pvalues <- function(x, side = c(0, -1, 1)) {
  # Based on:
  # https://blogs.sas.com/content/iml/2011/11/02/how-to-compute-p-values-for-a-bootstrap-distribution.html
  side <- side[1]
  x <- as.data.frame(x$t)
  ps <- sapply(x, function(.x) {
    s <- na.omit(.x)
    s0 <- 0
    N <- length(s)
    if (side == 0) {
      min((1 + sum(s >= s0)) / (N + 1),
          (1 + sum(s <= s0)) / (N + 1)) * 2
    } else if (side < 0) {
      (1 + sum(s <= s0)) / (N + 1)
    } else if (side > 0) {
      (1 + sum(s >= s0)) / (N + 1)
    }
  })
  
  setNames(ps,colnames(x))
}


# function to take two permuted ANOVAs and summarize p's ------------------

# for a given effect, summarize p's across two permuated ANOVAs
# only useful in cases where both p's are <.001
# or when both p's are >= .05
summarize_p_across_aovperms <- function(aovperm1, aovperm2, effect) {
  p1 <- aovperm1$table$`permutation P(>F)`[rownames(aovperm1$table) == effect]
  p2 <- aovperm2$table$`permutation P(>F)`[rownames(aovperm1$table) == effect]
  
  # case when both p's are < .001
  if (p1 < 0.001 & p2 < .001) {
    return('< .001')
  # case when both p's are >= .05
  } else if (p1 >= 0.05 & p2 >= 0.05) {
    return( str_c('>= ', round(min(p1, p2), digits = 3), sep = '') )
  }
}


# function to take parametric ANOVAs and summarize p's --------------------

# summarize p's across a set of parametric ANOVAs
# for effect(s) of interest
summarize_p_across_aovs <- function(list_aovs, effects) {
  #p1 <- aov1$anova_table$`Pr(>F)`[rownames(aov1$anova_table) == effect]
  #p2 <- aov2$anova_table$`Pr(>F)`[rownames(aov2$anova_table) == effect]
  p <- NULL
  for (jj in 1:length(list_aovs)) {
    for (hh in 1:length(effects)) {
      p <- append(p, list_aovs[[jj]]$anova_table$`Pr(>F)`[rownames(list_aovs[[jj]]$anova_table) == effects[hh]])
    }
  }
  
  # case when both p's are < .001
  if (all(p < 0.001)) {
    return('< .001')
    # case when both p's are >= .05
  } else if (all(p >= 0.05)) {
    return( str_c('>= ', round(min(p), digits = 3), sep = '') )
    # cases when some p's are between .001 and 05
  } else if (all(p < 0.05)) {
    return ( str_c('<= ', round(max(p), digits = 3), sep = '') )
  }
}


# function to take a set of pairwise comparisons and summarize p's --------

summarize_p_across_tests <- function(p_list) {

  # case when all p's are < .00
  if (all(p_list < 0.001)) {
    return('< .001')
    # case when all p's are >= .05
  } else if (all(p_list >= 0.05)) {
    return( str_c('>= ', round(min(p_list), digits = 3), sep = '') )
  }
}


# function to take a set of (formatted) comparisons and summarize p's --------

summarize_p_across_formatted_tests <- function(p_list) {
  
  p_new <- vector(mode = 'numeric', length = length(p_list))
  # handle p's where p<.001
  for (ii in 1:length(p_list)) {
    if (p_list[ii] == '<.001') {
      p_new[ii] <- 0.00001
    } else {
      p_new[ii] <- as.numeric(p_list[ii])
    }
  }
  
  # case when all p's are < .001
  if (all(p_new < .001)) {
    return('< .001')
    # case when all p's are >= .05
  } else if (all(p_new >= 0.05)) {
    return( str_c('>= ', round(min(p_new), digits = 3), sep = '') )
    # case when some p's are between 0.001 and 0.05
  } else if (all(p_new < 0.05)) {
    return ( str_c('<= ', round(max(p_new), digits = 3), sep = '') )
  }
}
