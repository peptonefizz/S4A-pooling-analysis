pacman::p_load(tidyverse, here)
pacman::p_load(grid,forestploter)

forest_ppa <- readRDS(here("Outputs","Intermediate outputs","forest_ppa_stacked.rds"))

# lookups for PPA of pooling vs. xpert
pooling_ppa <- forest_ppa %>%
  dplyr::filter(Subgroup == "Overall", Characteristic == "Overall") %>%
  dplyr::pull(sensitivity) 

###########

dt <- forest_ppa %>% 
  mutate(
    `Pooled testing PPA (95% CI)` = sprintf(
      "%.1f%% (%.1f–%.1f)",
      sensitivity * 100,
      sen_ciL * 100,
      sen_ciH * 100
    )
  ) %>% 
  rename(
    TP = tp,
    FN = fn,
    TN = tn,
    FP = fp,
    N  = total
  ) %>% 
  filter(!(Subgroup == "MTB grade" & Characteristic == "Overall"))


# Adding blank rows and rearraging rows
dt <- dt %>%
  mutate(.i = row_number()) %>%
  group_split(Subgroup, .keep = TRUE) %>%
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


dt <- dt %>% 
  mutate(
    Characteristic = dplyr::case_match(
      Characteristic,
      "ACF" ~ "CBCF",
      "District" ~ "DHC",
      .default = Characteristic
    ))

# All NA to blanks
cols_to_blank <- c("Characteristic","N","TP","FN","TN","Pooled testing PPA (95% CI)")
dt <- dt %>% mutate(across(all_of(cols_to_blank), ~ dplyr::coalesce(as.character(.x), "")))

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

# add a spacer column for plotting CI
dt$"  " <- paste(rep(" ", 40), collapse = " ")

# Set-up theme
tm <- forest_theme(base_size = 10,
                   refline_gp = gpar(lty="blank"),
                   ci_pch = c(15),
                   ci_col = c("steelblue"),
                   footnote_gp = grid::gpar(col = "darkgreen",fontsize=8),
                   vertline_lty = c("dashed"),
                   vertline_col = c("darkred", "darkblue"),
                   legend_name = NULL,
                   legend_position = NULL,
                   legend_value = NULL,
                   xlab_adjust = c("center"),
                   xlab_gp = gpar(fontsize = 9,fontface = "bold",lineheight = 1.3)
                   )

# header/subgroup table passed to forest()
p <- forest(dt[, c("Characteristic",
                   "N","TP","FN","TN",
                   "Pooled testing PPA (95% CI)","  ")],
            est   = list(dt$sensitivity),
            lower = list(dt$sen_ciL),
            upper = list(dt$sen_ciH),
            ci_column = c(7),
            ref_line  = 1,
            vert_line = c(pooling_ppa),
            nudge_y = 0,
            theme = tm,
            xlim = c(0, 1),
            xlab = "Positive percent agreement (Pooled testing result vs Individual Xpert Ultra)"
            ) %>% 
  
  edit_plot(row = categorylabel, gp = gpar(fontface = "bold")) %>%
  add_border(part = c("body"), row = categorylabel, 
             where = c("bottom"),
             gp = gpar(lwd = 0.7)) %>% 
  add_border(part = c("body"), row = categorylabel, 
             where = c("top"),
             gp = gpar(lwd = 0.7)) %>% 
  
  add_border(part = c("header"), row = 1, 
             where = c("top"),
             gp = gpar(lwd = 1.3)) %>% 
  add_border(part = c("header"), row = 1, 
             where = c("bottom"),
             gp = gpar(lwd = 1.5)) %>% 
  
  edit_plot(row = categorylabel, which = "background",
            gp = gpar(fill = "lightgray")) %>% 
  edit_plot(row = setdiff(seq_len(nrow(dt)), categorylabel),
            which = "background",
            gp = gpar(fill = "white"))


print(p)

ggplot2::ggsave(filename = here("Outputs","Viz_forest","figure2forest_PPA.png"), 
                plot = p,
                dpi = 300, width = 13, height = 9, units = "in")
