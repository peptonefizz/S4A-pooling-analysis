pacman::p_load(tidyverse, here)
pacman::p_load(grid,forestploter)

eff_perf <- readRDS(here("Outputs","Intermediate outputs","efficiency_sen_spec.rds"))
forest_culture_H <- readRDS(here("Outputs","Intermediate outputs","forest_culture_stacked.rds")) 
# %>% 
#   filter(!is.na(Characteristic))

# lookups for sensitivity and specificity based on imported eff_perf
pool_params <- eff_perf %>%
  filter(test == "pooling", !is.na(poolsize)) %>%
  transmute(poolsize = as.integer(poolsize),
            SeP = sensitivity,
            SpP = specificity) %>% 
  filter(is.na(poolsize)) #overall

xpert_params <- eff_perf %>%
  filter(test == "xpert") %>%
  summarise(SeI = first(sensitivity), SpI = first(specificity))

###########

dt <- forest_culture_H %>% 
  pivot_wider(
    names_from = test,
    values_from = c(tp, fp, tn, fn, total,
                    sensitivity, sen_ciL, sen_ciH,
                    specificity, spec_ciL, spec_ciH),
    names_sep = "_"
  ) %>% 
mutate(
    sensitivity_pooling_est = sprintf(
      "%.1f%% (%.1f–%.1f)",
      sensitivity_pooling * 100,
      sen_ciL_pooling * 100,
      sen_ciH_pooling * 100
    ),
    specificity_pooling_est = sprintf(
      "%.1f%% (%.1f–%.1f)",
      specificity_pooling * 100,
      spec_ciL_pooling * 100,
      spec_ciH_pooling * 100
    ),
    sensitivity_xpert_est = sprintf(
      "%.1f%% (%.1f–%.1f)",
      sensitivity_xpert * 100,
      sen_ciL_xpert * 100,
      sen_ciH_xpert * 100
    ),
    specificity_xpert_est = sprintf(
      "%.1f%% (%.1f–%.1f)",
      specificity_xpert * 100,
      spec_ciL_xpert * 100,
      spec_ciH_xpert * 100
    )
  ) %>%
  rename(
    TP = tp_pooling,
    FN = fn_pooling,
    TN = tn_pooling,
    FP = fp_pooling,
    N = total_pooling,
    "Pooled testing sensitivity (95% CI)" = sensitivity_pooling_est,
    "Pooled testing specificity (95% CI)" = specificity_pooling_est,
    "Individual Xpert Ultra sensitivity (95% CI)" = sensitivity_xpert_est,
    "Individual Xpert Ultra specificity (95% CI)" = specificity_xpert_est
  )

dt <- dt %>%
  mutate(
    Characteristic = if_else(
      Subgroup == "MTB grade",
      factor(Characteristic, levels = c("High", "Medium", "Low", "Very Low", "Trace", "MTB not detected")) %>% as.character(),
      Characteristic
    )
  ) %>%
  arrange(match(Subgroup, unique(Subgroup)), 
          if_else(Subgroup == "MTB grade", 
                  match(Characteristic, c("High", "Medium", "Low", "Very Low", "Trace", "MTB not detected")), 
                  row_number()))

dt <- dt %>% 
  mutate(
  Characteristic = dplyr::case_match(
    Characteristic,
    "ACF" ~ "CBCF",
    "District" ~ "DHC",
    .default = Characteristic
  ))


# Adding blank rows and rearraging rows
dt <- dt %>%
  mutate(.i = row_number()) %>%
  group_split(Subgroup, keep = TRUE) %>%
  map_dfr(function(g) {
    sg <- unique(g$Subgroup)
    if (!is.na(sg) && !tolower(sg) %in% "overall") {
      idx <- g$.i[1]
      header <- g[1, ]
      header[] <- NA
      header$Subgroup <- sg
      header$Characteristic <- sg
      header$.i <- idx - 0.5
      bind_rows(header, g)
    } else {
      g
    }
  }) %>%
  arrange(.i) %>%
  select(-.i)

# # All NA to blanks
cols_to_blank <- c("Characteristic","N","TP","FN","TN","FP",
                   "Pooled testing sensitivity (95% CI)","Pooled testing specificity (95% CI)",
                   "Individual Xpert Ultra sensitivity (95% CI)","Individual Xpert Ultra specificity (95% CI)")
dt <- dt %>% mutate(across(all_of(cols_to_blank), ~ dplyr::coalesce(as.character(.x), "")))

# total n for mtb_grade subgroup
n_mtb_grade <- dt %>% 
  filter(Subgroup == "MTB grade", !is.na(total_xpert)) %>% 
  summarise(n = sum(total_xpert, na.rm = TRUE)) %>% 
  pull(n)

# formatting for MTB grade rows
dt <- dt %>%
  mutate(
    # hides warning NAs introduced by coercion
    FP_i  = suppressWarnings(as.integer(FP)),
    TN_i  = suppressWarnings(as.integer(TN)),
    is_header = N == "" & TP == "" & FN == "" & TN == "" & FP == "" & Characteristic != "",
    n_neg = dplyr::coalesce(TN_i, 0L) + dplyr::coalesce(FP_i, 0L),
    
    FP = dplyr::if_else(is_header, "", dplyr::if_else(n_neg == 0L, "—", as.character(FP_i))),
    TN = dplyr::if_else(is_header, "", dplyr::if_else(n_neg == 0L, "—", as.character(TN_i))),
    
    `Pooled testing specificity (95% CI)` = stringr::str_replace(`Pooled testing specificity (95% CI)`, "^NA% \\(NA.?NA\\)$", "—"),
    `Individual Xpert Ultra specificity (95% CI)`   = stringr::str_replace(`Individual Xpert Ultra specificity (95% CI)`,   "^NA% \\(NA.?NA\\)$", "—"),
    Characteristic = dplyr::if_else(
      as.character(Characteristic) == "MTB grade",
      paste0("MTB grade (culture positive only, n=", n_mtb_grade, ")"),
      as.character(Characteristic)
    )
  )

# MTB grade: label last row
# dt <- dt %>%
#   mutate(Characteristic = if_else(
#     Subgroup == "MTB grade" & row_number() == max(row_number()[Subgroup == "MTB grade"]),
#     "MTB not detected",
#     Characteristic
#   ))

dt <- dt %>%
  mutate(
    Characteristic = if_else(
      Subgroup == "MTB grade" & row_number() == max(row_number()[Subgroup == "MTB grade"]),
      "MTB not detected",
      Characteristic
    ),
    `Pooled testing sensitivity (95% CI)` = if_else(
      Subgroup == "MTB grade" & row_number() == max(row_number()[Subgroup == "MTB grade"]),
      "—",
      as.character(`Pooled testing sensitivity (95% CI)`)
    )
  )


# MTB grade: Individual Xpert Ultra sensitivity CIs and sensitivity for Xpert = NA
dt <- dt %>%
  mutate(`Individual Xpert Ultra sensitivity (95% CI)` = if_else(
    Subgroup == "MTB grade", "—", as.character(`Individual Xpert Ultra sensitivity (95% CI)`)
  )) %>% 
  mutate(
    across(c(sensitivity_xpert, sen_ciL_xpert, sen_ciH_xpert),
           ~ if_else(Subgroup == "MTB grade", NA_real_, .x))
  )

categorylabel <- c(1, 2, 10, 14, 19, 23)

dt <- dt %>%
  mutate(Characteristic = if_else(
    is.na(Characteristic) | row_number() %in% categorylabel,
    as.character(Characteristic),
    paste0(strrep(" ", 3), as.character(Characteristic))
  ))

########################
##### forestplot code
########################

# remove Specificity usage and add a spacer column
dt$"  " <- paste(rep(" ", 40), collapse = " ")
dt$"   " <- paste(rep(" ", 40), collapse = " ")
dt$" " <- paste(rep(" ", 5), collapse = " ")  # wider gap

# Set-up theme
tm <- forest_theme(base_size = 10,
                   refline_gp = gpar(lty="blank"),
                   ci_pch = c(15),
                   ci_col = c("steelblue"),
                   footnote_gp = grid::gpar(col = "darkgreen",fontsize=8),
                   vertline_lty = c("dashed"),
                   vertline_col = c("darkred", "darkblue"),
                   legend_name = NULL,
                   legend_position = "bottom",
                   legend_value = c("Pooled testing", "Individual Xpert Ultra"),
                   xlab_adjust = c("center"),
                   xlab_gp = gpar(fontsize = 10,fontface = "bold"),
                   
)

p <- forest(dt[,c("Characteristic",
                  "N","TP","FN", "TN","FP",
                  "Pooled testing sensitivity (95% CI)","Individual Xpert Ultra sensitivity (95% CI)",
                  "  "," ","   ",
                  "Pooled testing specificity (95% CI)","Individual Xpert Ultra specificity (95% CI)")],
            
            est = list(dt$sensitivity_pooling, dt$specificity_pooling,
                       dt$sensitivity_xpert, dt$specificity_xpert),
            
            lower = list(dt$sen_ciL_pooling, dt$spec_ciL_pooling,
                         dt$sen_ciL_xpert, dt$spec_ciL_xpert),
            
            upper = list(dt$sen_ciH_pooling, dt$spec_ciH_pooling,
                         dt$sen_ciH_xpert, dt$spec_ciH_xpert),
            
            
            ci_column = c(9, 11),
            ref_line = 1,
            vert_line = list(c(pool_params$SeP,xpert_params$SeI),
                             c(pool_params$SpP,xpert_params$SpI)),
            nudge_y = 0.4,
            theme = tm,
            xlim = list(c(0.2, 1), c(0.9, 1)),
            ticks_at = list(c(0.2,0.5,0.8,0.9,1),
                            c(0.9,0.95,1)),
            ticks_minor	= list(c(0.3,0.4,0.5,0.6,0.7,0.8,0.85,0.9,0.95,1),
                               c(0.9,0.95,0.96,0.97,0.98,0.99,1)),
            xlab = c("Sensitivity","Specificity")
) %>% 
  edit_plot(row = categorylabel, gp = gpar(fontface = "bold")) %>%
  insert_text(text = "Pooled testing result",
              col = 2:6,
              part = "header",
              gp = gpar(fontface = "italic", fontsize = 10)) %>% 
  add_border(part = c("body"), row = categorylabel,
             where = c("bottom"),
             gp = gpar(lwd = 0.7)) %>%
  add_border(part = c("body"), row = categorylabel,
             where = c("top"),
             gp = gpar(lwd = 0.7)) %>%
  
  add_border(part = c("header"), row = 1, 
             where = c("top"),
             gp = gpar(lwd = 1.3)) %>% 
  add_border(part = c("header"), row = 2, 
             where = c("bottom"),
             gp = gpar(lwd = 1.5)) %>% 
  add_border(part = "header", row = 1, col = 2:6, 
             gp = gpar(lwd = 1.3)) %>% 
  
  edit_plot(row = categorylabel, which = "background",
            gp = gpar(fill = "lightgray")) %>% 
  edit_plot(row = setdiff(seq_len(nrow(dt)), categorylabel),
            which = "background",
            gp = gpar(fill = "white"))


# print(p)

ggplot2::ggsave(filename = here("Outputs","Viz_forest","figure1forest.png"), 
                plot = p,
                dpi = 300, width = 25, height = 9, units = "in")

