# Functions used across multiple files

#--- --- --- --- --- --- --- --- --- 
# Recode Xpert results
#--- --- --- --- --- --- --- --- --- 

# Recodes raw Ultra results 
# (Xpert_result, pool_resultt; 1=pos, 2=neg, 3=trace, 4=invalid, 5=error, 6=noresult) into factors with fixed levels: pos, neg, invalid, error, noresult.

# Trace handling is configurable: xpert_trace ∈ {as_pos, as_neg, drop} maps Xpert “trace” to positive, negative, or NA.
# For pooling, pool_trace ∈ {as_pos_xpert, as_neg, drop}: on pool trace, use the already-recoded individual Xpert (as_pos_xpert), force negative (as_neg), or drop (NA).
# Invalid/error/no-result pass through unchanged to invalid/error/noresult.

recode_xpert_pool_results <- function(data,
                                      xpert_result_col = "Xpert_result",
                                      pool_result_col  = "pool_resultt",
                                      xpert_trace = c("as_pos", "as_neg", "drop"),
                                      pool_trace  = c("as_pos_xpert", "as_neg", "drop")) {
  xpert_trace <- rlang::arg_match(xpert_trace)
  pool_trace  <- rlang::arg_match(pool_trace)
  
  lvl <- c("pos", "neg", "invalid", "error", "noresult")
  
  data %>%
    mutate(
      # Xpert recode with configurable trace handling
      xpert = case_when(
        .data[[xpert_result_col]] == 1 ~ "pos",
        .data[[xpert_result_col]] == 2 ~ "neg",
        .data[[xpert_result_col]] == 3 ~ switch(xpert_trace,
                                                as_pos = "pos",
                                                as_neg = "neg",
                                                drop   = NA_character_),
        .data[[xpert_result_col]] == 4 ~ "invalid",
        .data[[xpert_result_col]] == 5 ~ "error",
        .data[[xpert_result_col]] == 6 ~ "noresult",
        TRUE ~ NA_character_
      ),
      # Pooling recode with configurable trace handling
      pooling = case_when(
        .data[[pool_result_col]] == 1 ~ xpert,  # adopt individual xpert (after its trace rule)
        .data[[pool_result_col]] == 2 ~ "neg",
        .data[[pool_result_col]] == 3 ~ switch(pool_trace,
                                               as_pos_xpert = xpert, # treat as positive via xpert
                                               as_neg       = "neg",
                                               drop         = NA_character_),
        .data[[pool_result_col]] == 4 ~ xpert,  
        .data[[pool_result_col]] == 5 ~ xpert,  
        .data[[pool_result_col]] == 6 ~ xpert,  
        TRUE ~ NA_character_
      )
    ) %>%
    mutate(
      across(c(xpert, pooling), ~ factor(., levels = lvl))
    )
}

#--- --- --- --- --- --- --- --- --- --- 
# Create MTB Grade subset for Positive
#--- --- --- --- --- --- --- --- --- --- 

mtb_grade_posify <- function(df) {
  levs <- levels(factor(df$mtb_grade))
  df %>%
    mutate(
      mtb_grade_pos = factor(
        if_else(xpert == "Positive", as.character(mtb_grade), NA_character_),
        levels = levs
      )
    )
}
#--- --- --- --- --- --- --- --- --- --- 
# Recode xpert, pooling, culture for outputs
#--- --- --- --- --- --- --- --- --- --- 

recode_xpert_culture <- function(df, xpert_col = xpert, culture_col = culture, pooling_col = pooling) {
  df %>%
    mutate(
      {{ xpert_col }} := fct_recode(factor({{ xpert_col }}),
                                    "Positive" = "pos", "Negative" = "neg",
                                    "Invalid"  = "invalid", "Error" = "error",
                                    "No result" = "noresult"
      ),
      {{ culture_col }} := fct_recode(factor({{ culture_col }}),
                                      "Positive" = "pos", "Negative" = "neg",
                                      "Contaminated" = "contaminated"
      ),
      {{ pooling_col }} := fct_recode(factor({{ pooling_col }}),
                                      "Positive" = "pos", "Negative" = "neg",
                                      "Invalid"  = "invalid", "Error" = "error",
                                      "No result" = "noresult"
      )
    )
}