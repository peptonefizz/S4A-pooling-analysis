# 🧪 Diagnostic performance and cartridge savings of pooled testing for molecular diagnosis of tuberculosis: a diagnostic accuracy study across seven countries

**Manuscript status**: drafting

This repository contains an R/Quarto analysis workflow for evaluating pooled sputum testing using Xpert MTB/RIF Ultra (Xpert Ultra) for tuberculosis (TB) diagnosis. The workflow supports comparisons of:

-   Performance of pooled test and individual Xpert Ultra vs culture (reference standard)
-   Pooled Xpert Ultra vs individual Xpert Ultra
-   Cartridge use and savings under a pooled testing workflow

| 🔒 [Data note]{.underline}: This repo is designed to be public without sharing any primary study data. A fully synthetic dummy dataset is generated locally so readers can run the code end-to-end.

------------------------------------------------------------------------

## 🌍 Start4All (Start Taking Action for TB Diagnosis)

This analysis supports evidence generation for Start4All, a four-year, Unitaid-funded programme led by the Liverpool School of Tropical Medicine (LSTM) to improve access to TB screening, diagnosis, and linkage to treatment across multiple high-burden countries.

👉 Learn more on the official project page: [**Start4All (LSTM)**](https://www.lstmed.ac.uk/start4all)

------------------------------------------------------------------------

## 📁 Repository structure

-   `Scripts/`\
    Analysis scripts used to generate outputs.
-   `Data/`\
    Dummy data is under `Data/DummyData/`. Primary data ignored by git.
-   `Outputs/` \
    Generated tables/figures/reports (ignored by git)

------------------------------------------------------------------------

## 🚀 Quick start (run using dummy data)

From the repository root in R/RStudio:

``` r
source("Scripts/0.batch_run_dummy_analysis.R")
```

This will use the synthetic dummy dataset in `Data/DummyData/` to run the full analysis pipeline using the dummy CSV inputs.

------------------------------------------------------------------------

## 🧩 Running with real data (local only)

1.  Place your private input files under `Data/<YOUR_DATA_FOLDER>/` using the same naming/structure expected by the scripts.
2.  Set the pipeline `folder_name <- "<YOUR_DATA_FOLDER>"` (see the batch-run script pattern).
3.  Run the pipeline scripts.

------------------------------------------------------------------------

## 📦 Outputs

Outputs are written to subfolders under `Outputs/` as configured in the scripts.

------------------------------------------------------------------------

## 📝 Citation

If you use or adapt this code, please cite the associated manuscript and acknowledge this repository.
