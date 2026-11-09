**Overview**\
The code in this replication package constructs the figure and tables for "Public preferences for ecological indicators used in Everglades restoration" by Stainback et al. using R. One main file (run_all.R) run all of the code to generate the data for the 1 figure and 3 necessary tables in the paper. The replicator should expect the code to run for about 10 minutes.

Stainback GA, Lai JH, Pienaar EF, Adam DC, Wiederholt R, et al. (2020) Public preferences for ecological indicators used in Everglades restoration. PLOS ONE 15(6): e0234051. https://doi.org/10.1371/journal.pone.0234051

**Data**\
Florida Public Preferences Everglades Restoration Data come from a publicly available 2016 Attribute Choice Experiment Survey by the Everglades Foundation
Data was downloaded 8/5/26

https://doi.org/10.17605/OSF.IO/9AG7Z

I certify that the author(s) of the manuscript have legitimate access to and permission to use the data used in this manuscript.

**Running Code**\
Analysis was run in R version 4.6.1 (2026-06-24)

Running the replication requires the following packages:\
  tidyverse\
  haven\
  knitr\
  survival\
  mlogit\
  logitr\
  psych\
  gt\
The beginning of the file "run_all.R" will install all packages (latest version) if not already installed.

To run the replication, run "run_all.R". Outputs are saved to the output/ folder: fig_1.png, tab_1.csv/.md, tab_6.csv/.md, tab_7.csv, tab_8.csv.

**Controlled Randomness**\
  The analysis relies on random number generation. The seed (420) is set at the top of run_all.R and passed explicitly to mlogit() in the mixed logit section.

**File Dictionary**\
  README.Rmd            : this file\
  run_all.R             : installs necessary packages, runs all analysis and exports figure/tables to output/\
  LCM_4classes_data.dta : survey data (OSF, downloaded 8/5/26)\

**Memory, Runtime, Storage Requirements**\
Runtime is appoximately 10 minutes.\
This replication package requires around 5 MB of storage on the installed device

Not included in this replication package is the code for the latent class model. We also recognize that the code for the Mixed Logit Regression in WTP space is not accurate to the paper, but we have included it to be honest to the replication attempt.
The WTP-space mixed logit (end of run_all.R) does not converge. logitr returns Exit Status -1 (generic failure), all standard errors are NaN, and the coefficients are implausible (e.g., optout = 6,522, cost scale = -11.8). The log-likelihood is -12,775.6, and the reported null log-likelihood of -592,075 is also clearly wrong. These results should not be used. This model is not part of the five requested outputs.


**AI Usage Disclosure**\
  AI was used sparingly to write and debug code, and format output
