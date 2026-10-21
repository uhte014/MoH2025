# Registered deaths and excess nonviolent mortality in Gaza in 2025

Replication files for: Spagat, M. (2026). Registered deaths and excess nonviolent
mortality in Gaza in 2025. Submitted to *Demographic Research*.

## Running the code

From this folder:  `Rscript R/analysis.R`
(requires the R packages dplyr, tidyr, readr, ggplot2 and knitr)

## Contents

- `reports/`              the two Ministry of Health reports the analysis uses (see below)
- `data/inputs.csv`       single values, each with its source report and printed page number
- `data/cause_shares.csv` published cause-of-death percentages by group, with page numbers
- `data/cause_rates.csv`  published cause-specific death rates per 100,000 (2025 report, Table 3)
- `R/analysis.R`          builds Tables 1-4 as printed in the paper, with their notes,
                          plus figures quoted in the text, consistency checks and a supplementary figure
- `outputs/`              tables.md (paper tables, notes, text figures, checks), table_notes.txt,
                          table*.csv (unrounded values with rounding ranges), supplementary_figure.png/.pdf

## Source reports

The reports in `reports/` are included unaltered so that every input can be checked
against its source. They are not covered by this repository's licence.

- `MoH_Gaza_Annual_Mortality_Report_2025.pdf`: Ministry of Health, Gaza (2026).
  Annual Mortality Report, Gaza Strip 2025. Downloaded 30 September 2026 from
  https://www.moh.gov.ps/portal/wp-content/uploads/2026/09/Annual_Mortality_Report_Gaza_2025_EN.pdf
  SHA-256: 012f120f9bcca9e97adcb7c94d0534cdc7d4d636af8818cd941b2baf45b39187
- `MoH_Gaza_Annual_Report_2022.pdf`: Ministry of Health, Gaza (2023).
  Annual Report 2022: Southern Governorates - Gaza Strip. Downloaded 30 September 2026 from
  https://www.moh.gov.ps/portal/annual-report-moh-gaza-2023/
  SHA-256: f6d6585973fea8426f553abba67994cc25882d1976bb60362a7cf35f5aa8e9b9

GMS figures are taken from Spagat et al. (2026), The Lancet Global Health,
doi:10.1016/S2214-109X(25)00522-4.

## Notes on the data

1. Many counts are published only as percentages of group totals, rounded to one decimal
   place. Counts derived from them carry the rounding through as lower and upper bounds.
2. The 2025 report's cause-of-death charts for ages 5+ appear to include "other accidents"
   with war deaths: the war bars by age sum to about 18,880, close to war plus other accidents.
   For those ages, accidents are excluded in both years.
3. 2022 accidents are derived from the published rate of 8.9 per 100,000, which may include
   the few war deaths of that year.
4. 2022 stillbirths: 302 (MoH hospitals, consistent with the published 2022 perinatal rate)
   are used; 327 (all hospitals) gives 5.66 rather than 5.23 per 1,000 total births.
5. Neonatal and post-neonatal rates include the 141 infant war deaths, whose ages are not reported.
6. The US Census Bureau baseline (6,043) is a pre-war projection and makes no allowance for
   emigration, so it is probably somewhat high.

## Licence

Code and data files: MIT (see LICENSE). The reports in `reports/` are excluded.
