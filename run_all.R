##### INSTALL LIBRARIES AND LOAD IN DATA #####
start_time <- Sys.time()
pkgs <- c("tidyverse", "haven", "knitr", "survival",
          "mlogit", "logitr", "psych", "gt")

to_install <- setdiff(pkgs, rownames(installed.packages()))
if (length(to_install) > 0) install.packages(to_install)
library(tidyverse)
library(haven)
library(knitr)
library(survival)
library(mlogit)
library(logitr)
library(psych)
library(gt)
set.seed(420)  # global seed; mlogit() below also takes seed = 420 explicitly
everglades <- read_dta("LCM_4classes_data.dta")
#############################################1########################################################
############################## Histogram of Aggregate NEP Scores #####################################
######################################################################################################

fig_1 <- everglades |>
  ggplot(mapping = aes(x = nep)) +
  geom_histogram(fill = "grey80", color = "black", bins = 21) +
  theme_bw() +
  labs(
    x = "NEP score",
    y = "Number of Respondents"
  )

##############################################2#######################################################
##################################### Summary Statistics #############################################
######################################################################################################

sum_stat1 <- everglades |> 
  summarize(
    Respondents = n_distinct(respondentid),
    "Choice Sets" = n_distinct(groupid),
    Alternatives = n(),
    "Alternatives Per Set" = Alternatives / `Choice Sets`,
    "Chosen Options" = sum(choice)
  ) |> 
  pivot_longer(cols = everything(),
               names_to = "Variable") |> 
  rename(
    "Number of Observations" = "value"
  )

sum_stat2 <- everglades %>%
  group_by(option) %>%
  summarise(
    n = n(),
    chosen = sum(choice),
    choice_share = mean(choice)
  )

respondents <- everglades %>%
  distinct(
    respondentid,
    female, age, income, education, college, race, spanish,
    nep, nocost, difficultchoice, nogovt, nopay, politics
  ) |> 
  mutate(
    female = as.integer(female),
    age = as.integer(age),
    income = as.numeric(income),
    college = as.integer(college),
    race = as.integer(race),
    spanish = as.integer(spanish),
    nep = as.integer(nep),
    nocost = as.integer(nocost),
    nopay = as.integer(nopay),
  )

tab_8 <- respondents %>%
  select(
    female, age, income, college, race, spanish,
    nep, nocost, difficultchoice, nogovt, nopay, politics
  ) %>%
  pivot_longer(
    cols = everything(),
    names_to = "Variable",
    values_to = "Value",
    names_repair = "minimal"
  ) |> 
  group_by(Variable) %>%
  summarise(
    `Mean / Share` = mean(Value, na.rm = TRUE),
    SD             = sd(Value, na.rm = TRUE),
    Min            = min(Value, na.rm = TRUE),
    Max            = max(Value, na.rm = TRUE),
    .groups = "drop"
  )

######################################################################################################
################################ NEP Correlation Analysis ############################################
######################################################################################################

# some of the scores are reverse coded so create a sign variable
signs = c(1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1)

#summarize nep scores for each respondent so only 1 obs per respondent
nep_data <- everglades |> 
  group_by(respondentid) |> 
  summarize(
    nep1 = mean(nep1), #"mean" isnt really taking the mean of anything here
    nep2 = mean(nep2), #just condensing the observations
    nep3 = mean(nep3),
    nep4 = mean(nep4),
    nep5 = mean(nep5),
    nep6 = mean(nep6),
    nep7 = mean(nep7),
    nep8 = mean(nep8),
    nep9 = mean(nep9),
    nep10 = mean(nep10),
    nep11 = mean(nep11),
    nep12 = mean(nep12),
    nep13 = mean(nep13),
    nep14 = mean(nep14),
    nep15 = mean(nep15)
  ) |> 
  select(
    !respondentid ) #get rid of respondentid column so no clashing with alpha

nep_corr_table = nep_data |> psych::alpha(keys = signs)

# Extract the item columns
clean_summary <- nep_corr_table$item.stats[, c("raw.r", "r.drop")]

# Add the overall raw.alpha as a column so it's all in one matrix
clean_summary$overall_raw_alpha <- nep_corr_table$alpha.drop[, "raw_alpha"]

# Reverse-key the items yourself (assuming a 1-5 scale; adjust min/max if different)
scale_min <- 1
scale_max <- 5

neg_items <- c("nep2", "nep4", "nep6", "nep8", "nep10", "nep12", "nep14")

nep_keyed <- nep_data |>
  mutate(across(all_of(neg_items), ~ (scale_max + scale_min) - .x))

cov_mat <- cov(nep_keyed, use = "pairwise.complete.obs")

# Mean off-diagonal covariance
mean_offdiag <- function(m) mean(m[lower.tri(m)])

# Overall (the "Test scale" row)
overall_cov <- mean_offdiag(cov_mat)

# Item-dropped version: for each item, drop it and average the remaining covariances
avg_cov_drop <- sapply(colnames(cov_mat), function(item) {
  mean_offdiag(cov_mat[colnames(cov_mat) != item, colnames(cov_mat) != item])
})

clean_summary$avg_interitem_cov <- avg_cov_drop

#clean up the table !!
tab_7 <- clean_summary |> 
  rename(
    "item-test correlation" = raw.r,
    "item-rest correlation"= r.drop,
    "alpha" = overall_raw_alpha,
    "average interitem covariance" = avg_interitem_cov
  )  |> 
  mutate(
    Obs. = 2100,
    Sign = if_else(row_number() %% 2 != 0, "+", "-")
  )  |> relocate(
    alpha, .after = "average interitem covariance") |> 
  relocate(
    Sign, .before = "item-test correlation") |> 
  relocate(
    Obs., .before = Sign
  ) |>  
  mutate(across(where(is.numeric), round, digits = 3))

######################################################################################################
########################## Conditional Logit Regression ###############################
######################################################################################################
everglades <- everglades |>
  mutate(
    black = as.integer(race == 2),
    other_race = as.integer(!race %in% c(1, 2)),
    hispanic = spanish
  )

attrs <- c(
  "bird10e2", "bird50e2", "bird75e2",
  "gator10e2", "gator50e2", "gator75e2",
  "snail10e2", "snail50e2", "snail75e2",
  "trout10e2", "trout50e2", "trout75e2",
  "water10e2", "water50e2", "water75e2"
)

recode_everglades <- everglades |>
  mutate(across(
    matches("^(bird|gator|snail|trout|water)(10|50|75)e2$"),
    ~ as.integer(.x == 1),
    .names = "{sub('e2$', '', .col)}"
  ))

everglades_mlogit <- mlogit.data(
  data    = recode_everglades,
  choice  = "choice",
  shape   = "long",
  chid.var = "groupid",
  alt.var  = "option",
  id.var   = "respondentid"
)

clogit_model <- mlogit(
  choice ~ cdum +
    bird10 + bird50 + bird75 +
    gator10 + gator50 + gator75 +
    snail10 + snail50 + snail75 +
    trout10 + trout50 + trout75 +
    water10 + water50 + water75 +
    cost | 0,
  data = everglades_mlogit
)

s  <- summary(clogit_model)
ct <- s$CoefTable
b_cost <- coef(clogit_model)["cost"]

b <- coef(clogit_model)
attrs <- setdiff(names(b), c("cdum", "cost"))

# Paper's significance star usage: *** 1%, ** 5%, * 10%
stars <- function(p) case_when(p < 0.01 ~ "***",
                               p < 0.05 ~ "**",
                               p < 0.10 ~ "*",
                               TRUE     ~ "")

wtp_table <- tibble(
  Variable    = rownames(ct),
  Coefficient = paste0(formatC(ct[, "Estimate"], format = "f", digits = 3),
                       stars(ct[, "Pr(>|z|)"])),
  `Std. Error` = formatC(ct[, "Std. Error"], format = "f", digits = 3),
  WTP = ifelse(Variable %in% c("cdum", "cost"),
               "",
               paste0("$", formatC(-ct[, "Estimate"] / b_cost,
                                   format = "f", digits = 2)))
) |>
  mutate(Variable = recode(Variable, cdum = "optout"))

# summary rows
ll     <- as.numeric(logLik(clogit_model))
n_rows <- nrow(everglades_mlogit)
n_ch   <- n_rows / 3                      # choice occasions (12,599)
ll0    <- n_ch * log(1/3)                 # null model: equal shares
lr     <- 2 * (ll - ll0)
df     <- length(coef(clogit_model))     # 17, as in the paper
pval   <- pchisq(lr, df = df, lower.tail = FALSE)
r2     <- 1 - ll / ll0                    

stat_rows <- tibble(
  Variable     = c("Log Likelihood", "N", paste0("LR chi2(", df, ")"),
                   "Prob>chi2", "Pseudo R2"),
  Coefficient  = c(formatC(ll,   format = "f", digits = 3, big.mark = ","),
                   formatC(n_rows, format = "d", big.mark = ","),
                   formatC(lr,   format = "f", digits = 2, big.mark = ","),
                   formatC(pval, format = "f", digits = 4),
                   formatC(r2,   format = "f", digits = 4)),
  `Std. Error` = "",
  WTP          = ""
)

final_table <- bind_rows(wtp_table, stat_rows)
tab_6 <- knitr::kable(final_table, align = c("l", "r", "r", "r"))

######################################################################################################
########################## Random Parameter (Mixed) Logit Regression ###############################
######################################################################################################

# negative cost, so a positive lognormal coefficient means disutility from cost
recode_everglades$negcost <- -recode_everglades$cost

everglades_mlogit2 <- mlogit.data(
  data     = recode_everglades,
  choice   = "choice",
  shape    = "long",
  chid.var = "groupid",
  alt.var  = "option",
  id.var   = "respondentid" #MAKE CORRELATION = FALSE
)

rpar_spec <- c(setNames(rep("n", length(attrs) + 1), c("cdum", attrs)),
               negcost = "ln")

f <- as.formula(paste("choice ~ cdum +", paste(attrs, collapse = " + "),
                      "+ negcost | 0"))


rpl <- mlogit(f, data = everglades_mlogit2, rpar = rpar_spec,
              R = 500, halton = NA, panel = TRUE,
              start = c(coef(clogit_model)[c("cdum", attrs)],
                        negcost = log(0.0102),
                        rep(0.1, length(attrs) + 2)),
              correlation = FALSE,
              seed = 420)

s2  <- summary(rpl)
ct2 <- s2$CoefTable
b2  <- coef(rpl)

attribute_vars <- c(
  "bird10", "bird50", "bird75",
  "gator10", "gator50", "gator75",
  "snail10", "snail50", "snail75",
  "trout10", "trout50", "trout75",
  "water10", "water50", "water75"
)

# rpl uses negcost = -cost, estimated as log-normal.
# This is the same denominator you used successfully by hand.
cost_scale <- exp(b2["negcost"])

wtp_values <- b2[attribute_vars] / cost_scale
round(wtp_values, 2)

# Paper's significance star usage: *** 1%, ** 5%, * 10%
stars <- function(p) case_when(p < 0.01 ~ "***",
                               p < 0.05 ~ "**",
                               p < 0.10 ~ "*",
                               TRUE     ~ "")

mean_vars <- c("cdum", attribute_vars, "negcost")

wtp_table2 <- tibble(
  Variable = mean_vars,
  Coefficient = paste0(
    formatC(ct2[mean_vars, "Estimate"], format = "f", digits = 3),
    stars(ct2[mean_vars, "Pr(>|z|)"])
  ),
  `Std. Error` = formatC(
    ct2[mean_vars, "Std. Error"],
    format = "f",
    digits = 3
  ),
  WTP = case_when(
    Variable %in% attribute_vars ~ paste0(
      "$",
      formatC(
        b2[Variable] / cost_scale,
        format = "f",
        digits = 2
      )
    ),
    TRUE ~ ""
  )
)

# summary rows
ll2     <- as.numeric(logLik(rpl))
n_rows2 <- nrow(everglades_mlogit2)
n_ch2   <- n_rows2 / 3                    # choice occasions
ll02    <- n_ch2 * log(1/3)               # null model: equal shares
lr2     <- 2 * (ll2 - ll02)
df2     <- length(coef(rpl))
pval2   <- pchisq(lr2, df = df2, lower.tail = FALSE)
r22     <- 1 - ll2 / ll02                  

stat_rows2 <- tibble(
  Variable = c("Log Likelihood", "N", paste0("LR chi2(", df2, ")"),
               "Prob>chi2", "Pseudo R2"),
  Coefficient  = c(formatC(ll2,   format = "f", digits = 3, big.mark = ","),
                   formatC(n_rows2, format = "d", big.mark = ","),
                   formatC(lr2,   format = "f", digits = 2, big.mark = ","),
                   formatC(pval2, format = "f", digits = 4),
                   formatC(r22,   format = "f", digits = 4)),
  `Std. Error` = "",
  WTP          = ""
)

final_table2 <- bind_rows(wtp_table2, stat_rows2)
tab_1 <- knitr::kable(final_table2, align = c("l", "r", "r", "r"))

######################################################################################################
################################# EXPORT FIGURES AND TABLES ##########################################
######################################################################################################

out_dir <- "output"
dir.create(out_dir, showWarnings = FALSE)

# Figure 1 (ggplot)
ggsave(file.path(out_dir, "fig_1.png"), plot = fig_1,
       width = 6, height = 4, dpi = 300)

# Tables 6 and 1 are knitr::kable objects -> save as markdown, plus csv of the underlying data
writeLines(as.character(tab_6), file.path(out_dir, "tab_6.md"))
writeLines(as.character(tab_1), file.path(out_dir, "tab_1.md"))
write_csv(final_table,  file.path(out_dir, "tab_6.csv"))
write_csv(final_table2, file.path(out_dir, "tab_1.csv"))

# Table 7 has item names as row names, so keep them as a column
tab_7 |>
  tibble::rownames_to_column("Item") |>
  write_csv(file.path(out_dir, "tab_7.csv"))

# Table 8
write_csv(tab_8, file.path(out_dir, "tab_8.csv"))

######################################################################################################
################### Random Parameter (Mixed) Logit Regression in the WTP Space #######################
######################################################################################################

attr_vars <- c(
  "bird10", "bird50", "bird75",
  "gator10", "gator50", "gator75",
  "snail10", "snail50", "snail75",
  "trout10", "trout50", "trout75",
  "water10", "water50", "water75"
)

pars <- c("optout", attr_vars)

wtp_data <- recode_everglades |>
  mutate(
    # Convert Stata value labels to their displayed labels first
    option = haven::as_factor(option),
    
    # Original underlying coding is already 0/1
    choice = as.integer(choice),
    
    # Now this correctly identifies alternative C
    optout = case_when(
      option == "C" ~ 1L,
      option %in% c("A", "B") ~ 0L,
      TRUE ~ NA_integer_
    ),
    
    obsID = as.integer(groupid)
  ) |>
  arrange(obsID, option)

# These checks should NOT show any missing values:
table(wtp_data$option, wtp_data$optout, useNA = "ifany")

colSums(is.na(wtp_data[, c("choice", "optout", "obsID", "cost", attr_vars)]))


# Fit this BEFORE creating start_wtp

wtp_mnl <- logitr(
  data = wtp_data,
  outcome = "choice",
  obsID = "obsID",
  pars = pars,
  scalePar = "cost"
)

mnl_b <- coef(wtp_mnl)
lambda0 <- unname(mnl_b["scalePar"])

stopifnot(
  is.finite(lambda0),
  lambda0 > 0,
  all(is.finite(mnl_b[pars]))
)

# Use MNL means, but begin heterogeneity essentially at zero.
# This is much more stable than starting with large SDs.
start_wtp <- c(
  log(lambda0),             # log of positive cost-scale mean
  unname(mnl_b[pars]),      # WTP means
  0.01,                     # SD of log cost scale
  rep(0.10, length(pars))   # SDs of WTP coefficients
)

stopifnot(length(start_wtp) == 34)

# Diagnostic WTP-space mixed logit: run only one well-defined start first

set.seed(420)

wtp_fit_check <- logitr(
  data = wtp_data,
  outcome = "choice",
  obsID = "obsID",
  panelID = "respondentid",
  
  pars = pars,
  scalePar = "cost",
  randPars = setNames(rep("n", length(pars)), pars),
  randScale = "ln",
  
  correlation = FALSE,
  drawType = "halton",
  numDraws = 500,
  
  startVals = start_wtp,
  numMultiStarts = 1,
  numThreads = 1
)

summary(wtp_fit_check)
logLik(wtp_fit_check)

message("Total runtime: ", round(difftime(Sys.time(), start_time, units = "mins"), 1), " minutes")

print(sessionInfo())




