pacman::p_load(tidyverse, here)
pacman::p_load(grid, forestploter)

forest_ppa <- readRDS(here("Outputs", "Intermediate outputs", "forest_ppa_stacked.rds"))

# overall PPA for vertical reference line
pooling_ppa <- forest_ppa %>%
  filter(Subgroup == "Overall", Characteristic == "Overall") %>%
  pull(sensitivity)

# prepare analysis table
base_dt <- forest_ppa %>%
  mutate(
    `Pooled testing PPA (95% CI)` = sprintf(
      "%.1f%% (%.1f–%.1f)",
      sensitivity * 100,
      sen_ciL * 100,
      sen_ciH * 100
    )
  ) %>%
  rename(
    CP = tp,
    DN = fn,
    CN = tn,
    N  = total
  ) %>%
  filter(!(Subgroup == "MTB grade" & Characteristic == "Overall")) %>%
  mutate(
    Characteristic = dplyr::case_match(
      Characteristic,
      "ACF" ~ "CBCF",
      "District" ~ "DHC",
      .default = Characteristic
    )
  ) %>% 
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

# total n in MTB grade subgroup (individual Xpert Ultra-positive only)
n_mtb_grade <- base_dt %>%
  filter(Subgroup == "MTB grade") %>%
  summarise(n = sum(N, na.rm = TRUE)) %>%
  pull(n)

# add subgroup header rows
dt <- base_dt %>%
  mutate(.i = row_number()) %>%
  group_split(Subgroup, .keep = TRUE) %>%
  map_dfr(function(g) {
    sg <- unique(g$Subgroup)
    if (!is.na(sg) && tolower(sg) != "overall") {
      header <- g[1, ]
      header[] <- NA
      header$Subgroup <- sg
      header$Characteristic <- sg
      header$.i <- min(g$.i) - 0.5
      bind_rows(header, g)
    } else {
      g
    }
  }) %>%
  arrange(.i) %>%
  select(-.i) %>%
  mutate(
    is_header = is.na(N) & is.na(CP) & is.na(DN) & is.na(CN) & !is.na(Characteristic),
    Characteristic = if_else(
      Characteristic == "MTB grade",
      paste0(
        "MTB grade (individual Xpert Ultra positive only, n = ",
        format(n_mtb_grade, big.mark = ",", scientific = FALSE),
        ")"
      ),
      Characteristic
    )
  )


# rows to highlight (overall row + subgroup headers)
categorylabel <- which(dt$is_header | dt$Characteristic == "Overall")

# display formatting
dt <- dt %>%
  mutate(
    Characteristic = as.character(Characteristic),
    N  = dplyr::coalesce(as.character(N), ""),
    CP = dplyr::coalesce(as.character(CP), ""),
    DN = dplyr::coalesce(as.character(DN), ""),
    CN = case_when(
      Subgroup == "MTB grade" & !is_header ~ "—",
      TRUE ~ dplyr::coalesce(as.character(CN), "")
    ),
    `Pooled testing PPA (95% CI)` = dplyr::coalesce(as.character(`Pooled testing PPA (95% CI)`), ""),
    Characteristic = if_else(
      row_number() %in% categorylabel,
      Characteristic,
      paste0(strrep(" ", 3), Characteristic)
    )
  )

########################
##### forestplot code
########################

# spacer column for CI plot
dt$"  " <- paste(rep(" ", 40), collapse = " ")

# theme
tm <- forest_theme(
  base_size = 10,
  refline_gp = gpar(lty = "blank"),
  ci_pch = 15,
  ci_col = "steelblue",
  footnote_gp = grid::gpar(col = "darkgreen", fontsize = 8),
  vertline_lty = "dashed",
  vertline_col = c("darkred", "darkblue"),
  legend_name = NULL,
  legend_position = NULL,
  legend_value = NULL,
  xlab_adjust = "center",
  xlab_gp = gpar(fontsize = 9, fontface = "bold", lineheight = 1.3)
)

p <- forest(
  dt[, c(
    "Characteristic",
    "N", "CP", "DN", "CN",
    "Pooled testing PPA (95% CI)", "  "
  )],
  est = list(dt$sensitivity),
  lower = list(dt$sen_ciL),
  upper = list(dt$sen_ciH),
  ci_column = 7,
  ref_line = 1,
  vert_line = pooling_ppa,
  nudge_y = 0,
  theme = tm,
  xlim = c(0, 1),
  xlab = "Positive percent agreement (Pooled testing vs individual Xpert Ultra)"
) %>%
  edit_plot(row = categorylabel, gp = gpar(fontface = "bold")) %>%
  add_border(
    part = "body", row = categorylabel,
    where = "bottom",
    gp = gpar(lwd = 0.7)
  ) %>%
  add_border(
    part = "body", row = categorylabel,
    where = "top",
    gp = gpar(lwd = 0.7)
  ) %>%
  add_border(
    part = "header", row = 1,
    where = "top",
    gp = gpar(lwd = 1.3)
  ) %>%
  add_border(
    part = "header", row = 1,
    where = "bottom",
    gp = gpar(lwd = 1.5)
  ) %>%
  edit_plot(
    row = categorylabel, which = "background",
    gp = gpar(fill = "lightgray")
  ) %>%
  edit_plot(
    row = setdiff(seq_len(nrow(dt)), categorylabel),
    which = "background",
    gp = gpar(fill = "white")
  )

print(p)

ggplot2::ggsave(
  filename = here("Outputs", "Viz_forest", "figure2forest_PPA.png"),
  plot = p,
  dpi = 300,
  width = 12,
  height = 7,
  units = "in"
)
