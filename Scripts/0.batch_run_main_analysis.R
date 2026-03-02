suppressMessages(if (!require("pacman")) install.packages("pacman"))
pacman::p_load(rmarkdown,here, ggplot2)

# Patch ggsave to auto-create directories
unlockBinding("ggsave", as.environment("package:ggplot2"))
assign("ggsave", function(filename, ...) {
  dir.create(dirname(filename), recursive = TRUE, showWarnings = FALSE)
  ggplot2::ggsave(filename, ...)
}, envir = as.environment("package:ggplot2"))
lockBinding("ggsave", as.environment("package:ggplot2"))

folder_name <- "Phase 1 V4 (complete) 21-01-2026"
data_format_with_ct <- "data format with grade & ct-values & rif (single xpt & pool) 020226.csv"

rmd_files <- c(
  "0.functions.R",
  "1.cleaning.Rmd",
  "2.STARD_main.qmd",
  "3.table1.qmd",
  "4.performance.Rmd",
  "5.efficiency.Rmd",
  "6.missed.qmd",
  # "7.ct-value.Rmd",
  "viz_performance_forest_culture.R",
  "viz_performance_forest_PPA_xpert.R",
  "viz_sankey_pool_population.R",
  "viz_sankey_pool.R"
  )

for (file in rmd_files) {
  rmd_path <- here("Scripts", file)
  message("Running: ", rmd_path)
  
  render(
    input = rmd_path,
    output_format = "all",
    output_file = tempfile(),
    clean = TRUE,
    envir = new.env()
  )
}


