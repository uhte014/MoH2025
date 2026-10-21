# =============================================================================
# Gaza 2025: registered deaths, under-registration and excess nonviolent deaths
# Builds Tables 1-4 of the paper (with their notes) from the files in data/,
# plus a supplementary figure and consistency checks that are not in the paper.
#
# Run from the project root:   Rscript R/analysis.R
#
# Conventions
#  - "Nonviolent" = all deaths other than operations of war; accidents count
#    as nonviolent.
#  - Counts derived from rounded percentages carry +/-0.05 percentage points
#    per share used; these bounds are reported as *_lo / *_hi where they matter.
#  - 2022 counts are scaled to the 2025 population (factor pop25/pop22) when
#    compared with 2025 counts; infant measures use live births instead.
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(readr)
  library(ggplot2)
  library(knitr)
})

dir.create("outputs", showWarnings = FALSE)

inputs <- read_csv("data/inputs.csv", show_col_types = FALSE,
                   col_types = cols(year = col_character()))
shares <- read_csv("data/cause_shares.csv", show_col_types = FALSE,
                   col_types = cols(year = col_character()))

# --- helpers ------------------------------------------------------------------

v <- function(id, year) {
  out <- inputs$value[inputs$id == id & inputs$year == as.character(year)]
  if (length(out) != 1) stop("Input not found or not unique: ", id, " ", year)
  out
}

# Share (in %) of a cause within a group; returns central, lo, hi
s <- function(year, group, cause) {
  r <- shares[shares$year == as.character(year) & shares$group == group &
                shares$cause == cause, ]
  if (nrow(r) != 1) stop("Share not found: ", year, " ", group, " ", cause)
  lo <- ifelse(is.na(r$pct_lo), r$pct - 0.05, r$pct_lo)
  hi <- ifelse(is.na(r$pct_hi), r$pct + 0.05, r$pct_hi)
  c(mid = r$pct, lo = max(lo, 0), hi = hi) / 100
}

# Sum of several shares (central and bounds add)
s_sum <- function(year, group, causes) {
  Reduce(`+`, lapply(causes, function(cz) s(year, group, cz)))
}

pct_change <- function(new, old) 100 * (new / old - 1)
r0 <- function(x) round(x)
r1 <- function(x) round(x, 1)

# --- core quantities ----------------------------------------------------------

pop22 <- v("pop", 2022); pop25 <- v("pop", 2025)
scale <- pop25 / pop22
births22 <- v("births", 2022); births25 <- v("births", 2025)

tot22 <- v("deaths_total", 2022)
tot25 <- v("deaths_total", 2025)
war25 <- v("war", 2025)
acc25 <- v("accidents", 2025)
acc22 <- v("accidents_rate_per100k", 2022) * pop22 / 1e5

nv25        <- tot25 - war25           # nonviolent, incl. accidents
nv25_exacc  <- nv25 - acc25
nv22        <- tot22                   # 2022: no separate war category (see note)
nv22_exacc  <- tot22 - acc22

# =============================================================================
# TABLE 1. Registered deaths and registered nonviolent excess
# =============================================================================

exp_nv25   <- nv22 * scale             # expected nonviolent at 2022 rates
exp_acc25  <- acc22 * scale
exp_ex25   <- nv22_exacc * scale

baselines <- tibble(
  baseline = c("2022 crude death rate (main)",
               "2018-19 average crude death rate",
               "US Census Bureau projection (as used in the GMS)"),
  expected = c(exp_nv25,
               mean(c(v("cdr_per1000", 2018), v("cdr_per1000", 2019))) * pop25 / 1000,
               v("gms_baseline_2025", 2025))
) %>%
  mutate(registered_excess = nv25 - expected)

table1 <- tibble(
  item = c("Population",
           "Live births",
           "All registered deaths",
           "Violent (operations of war)",
           "Nonviolent: accidents",
           "Nonviolent: all other causes",
           "Nonviolent: total",
           "Expected nonviolent deaths at 2022 rates",
           "  of which accidents",
           "  of which all other causes",
           "Registered nonviolent excess",
           "  of which accidents",
           "  of which all other causes"),
  y2022 = c(pop22, births22, tot22, NA, acc22, nv22_exacc, nv22,
            NA, NA, NA, NA, NA, NA),
  y2025 = c(pop25, births25, tot25, war25, acc25, nv25_exacc, nv25,
            exp_nv25, exp_acc25, exp_ex25,
            nv25 - exp_nv25, acc25 - exp_acc25, nv25_exacc - exp_ex25)
)

reg_excess <- nv25 - exp_nv25

# =============================================================================
# TABLE 2. Nonviolent deaths excluding accidents, by age and sex
# =============================================================================

age_row <- function(group) {
  # ---- 2022 -------------------------------------------------------------
  total22 <- if (group == "<1") v("age_total", "2022:<1") else
    v("age_share_pct", paste0("2022:", group)) / 100 * tot22
  acc_causes22 <- shares %>%
    filter(year == "2022", group == !!group,
           cause %in% c("other_accidents", "transport", "accidents_all")) %>%
    pull(cause)
  a22 <- if (length(acc_causes22)) s_sum(2022, group, acc_causes22) else c(mid = 0, lo = 0, hi = 0)
  ex22 <- total22 * (1 - a22[c("mid", "hi", "lo")])  # more accidents -> fewer left
  names(ex22) <- c("mid", "lo", "hi")

  # ---- 2025 -------------------------------------------------------------
  total25 <- v("age_total", paste0("2025:", group))
  if (group == "<1") {
    war_n <- total25 * s(2025, "<1", "war")
    acc_n <- total25 * s(2025, "<1", "other_accidents")
    ex25 <- total25 - war_n[c("mid", "hi", "lo")] - acc_n[c("mid", "hi", "lo")]
  } else {
    rm_causes <- shares %>%
      filter(year == "2025", group == !!group,
             cause %in% c("war", "war_incl_other_accidents",
                          "other_accidents", "transport")) %>%
      pull(cause)
    rm_share <- s_sum(2025, group, rm_causes)
    ex25 <- total25 * (1 - rm_share[c("mid", "hi", "lo")])
  }
  names(ex25) <- c("mid", "lo", "hi")

  unlisted22 <- any(!is.na(shares$pct_lo[shares$year == "2022" & shares$group == group &
                                          shares$cause %in% acc_causes22]))
  if (group == "<1") {
    # infants: compare per 1,000 live births
    tibble(group = "Under 1 (per 1,000 live births)",
           y2022 = ex22["mid"] / births22 * 1000,
           y2022_lo = ex22["lo"] / births22 * 1000, y2022_hi = ex22["hi"] / births22 * 1000,
           y2025 = ex25["mid"] / births25 * 1000,
           y2025_lo = ex25["lo"] / births25 * 1000, y2025_hi = ex25["hi"] / births25 * 1000,
           unlisted22 = unlisted22)
  } else {
    tibble(group = paste("Ages", group),
           y2022 = ex22["mid"] * scale,
           y2022_lo = ex22["lo"] * scale, y2022_hi = ex22["hi"] * scale,
           y2025 = ex25["mid"], y2025_lo = ex25["lo"], y2025_hi = ex25["hi"],
           unlisted22 = unlisted22)
  }
}

ages <- c("<1", "1-4", "5-19", "20-59", "60+")
table2_age <- bind_rows(lapply(ages, age_row))

# Sex: 2022 accidents are not available by sex, so compare INCLUDING accidents
sex_row <- function(sex) {
  d22 <- v(paste0("deaths_", sex), 2022)
  d25 <- v(paste0("deaths_", sex), 2025)
  w <- s(2025, sex, "war")
  ex25 <- d25 * (1 - w[c("mid", "hi", "lo")]); names(ex25) <- c("mid", "lo", "hi")
  tibble(group = paste0(ifelse(sex == "male", "Men", "Women"),
                        ", all ages (including accidents)"),
         y2022 = d22 * scale, y2022_lo = d22 * scale, y2022_hi = d22 * scale,
         y2025 = ex25["mid"], y2025_lo = ex25["lo"], y2025_hi = ex25["hi"],
         unlisted22 = FALSE)
}
table2_sex <- bind_rows(sex_row("female"), sex_row("male"))

table2 <- bind_rows(table2_age, table2_sex) %>%
  mutate(change_pct = pct_change(y2025, y2022))

table2_75 <- tibble(
  group = "All-cause death rate aged 75+ (per 1,000; 2025 incl. war)",
  y2022 = v("asdr_75plus", 2022), y2025 = v("asdr_75plus", 2025)
) %>% mutate(change_pct = pct_change(y2025, y2022))

# Parity floor: deaths needed to bring each age group (1+ years) and infants
# (per birth) back up to 2022 levels. Age groups only -- sex rows overlap.
parity <- table2_age %>%
  mutate(
    n_exp_mid = ifelse(grepl("Under 1", group), y2022 * births25 / 1000, y2022),
    n_exp_lo  = ifelse(grepl("Under 1", group), y2022_lo * births25 / 1000, y2022_lo),
    n_exp_hi  = ifelse(grepl("Under 1", group), y2022_hi * births25 / 1000, y2022_hi),
    n_obs     = ifelse(grepl("Under 1", group), y2025 * births25 / 1000, y2025),
    short_mid = pmax(0, n_exp_mid - n_obs),
    short_lo  = pmax(0, n_exp_lo - n_obs),
    short_hi  = pmax(0, n_exp_hi - n_obs)
  )
parity_floor <- c(mid = sum(parity$short_mid), lo = sum(parity$short_lo),
                  hi = sum(parity$short_hi))

# =============================================================================
# TABLE 3. Perinatal, infant and maternal indicators
# =============================================================================

inf22 <- v("age_total", "2022:<1"); inf25 <- v("age_total", "2025:<1")
sb22 <- v("stillbirths_moh", 2022); sb22_all <- v("stillbirths_all", 2022)
sb25 <- v("stillbirths", 2025)

per_birth <- function(n, births) n / births * 1000
comp <- function(share_id, year, infants, births)
  per_birth(v(share_id, year) / 100 * infants, births)

cause_rate <- function(year, causes, infants, births)
  per_birth(s_sum(year, "<1", causes)["mid"] * infants, births)

mat22 <- v("maternal_deaths", 2022)
mat25 <- v("deaths_female", 2025) * s(2025, "female", "pregnancy_obstetric")

table3 <- tibble(
  indicator = c(
    "Stillbirths",
    "Stillbirth rate (per 1,000 total births)",
    "Early neonatal mortality, 0-6 days (per 1,000 live births)",
    "Late neonatal mortality, 7-27 days",
    "Post-neonatal mortality, 28-364 days",
    "Infant mortality, all causes",
    "Infant mortality, excl. war and accidents",
    "Infant deaths from prematurity / low birth weight (per 1,000 live births)",
    "Infant deaths from infection incl. septicaemia",
    "Infant deaths from congenital malformations",
    "Infant deaths from SIDS / unknown cause",
    "Maternal deaths",
    "Maternal mortality ratio (per 100,000 live births)"),
  y2022 = c(
    sb22,
    sb22 / (births22 + sb22) * 1000,
    comp("infdeath_share_0_6d", 2022, inf22, births22),
    comp("infdeath_share_7_27d", 2022, inf22, births22),
    comp("infdeath_share_28_364d", 2022, inf22, births22),
    per_birth(inf22, births22),
    per_birth(inf22 * (1 - s(2022, "<1", "other_accidents")["mid"]), births22),
    cause_rate(2022, "prematurity_lbw", inf22, births22),
    cause_rate(2022, "infectious_incl_sepsis", inf22, births22),
    cause_rate(2022, "congenital_malformations", inf22, births22),
    cause_rate(2022, "sids_unknown", inf22, births22),
    mat22,
    mat22 / births22 * 1e5),
  y2025 = c(
    sb25,
    sb25 / (births25 + sb25) * 1000,
    comp("infdeath_share_0_6d", 2025, inf25, births25),
    comp("infdeath_share_7_27d", 2025, inf25, births25),
    comp("infdeath_share_28_364d", 2025, inf25, births25),
    per_birth(inf25, births25),
    per_birth(inf25 * (1 - s_sum(2025, "<1", c("war", "other_accidents"))["mid"]), births25),
    cause_rate(2025, "prematurity_lbw", inf25, births25),
    cause_rate(2025, c("infectious_diseases", "septicaemia",
                       "diarrhoea_gastroenteritis", "other_infectious"), inf25, births25),
    cause_rate(2025, "congenital_malformations", inf25, births25),
    cause_rate(2025, "sids_unknown", inf25, births25),
    mat25["mid"],
    mat25["mid"] / births25 * 1e5)
) %>% mutate(change_pct = pct_change(y2025, y2022))

# internal consistency checks against published rates
checks <- tibble(
  check = c("2022 perinatal rate reproduced (MoH stillbirths)",
            "2025 perinatal rate reproduced",
            "2022 neonatal rate (report: 6.6)",
            "2025 neonatal rate (report: 9.8)",
            "2022 post-neonatal rate (report: 4.2)",
            "2025 post-neonatal rate (report: 6.0)",
            "2025 war deaths by sex sum (report: 17,286)",
            "2025 war + other accidents implied by age charts"),
  value = c(
    (sb22 + v("infdeath_share_0_6d", 2022) / 100 * inf22) / (births22 + sb22) * 1000,
    (sb25 + v("infdeath_share_0_6d", 2025) / 100 * inf25) / (births25 + sb25) * 1000,
    comp("infdeath_share_0_6d", 2022, inf22, births22) + comp("infdeath_share_7_27d", 2022, inf22, births22),
    comp("infdeath_share_0_6d", 2025, inf25, births25) + comp("infdeath_share_7_27d", 2025, inf25, births25),
    comp("infdeath_share_28_364d", 2022, inf22, births22),
    comp("infdeath_share_28_364d", 2025, inf25, births25),
    v("deaths_male", 2025) * s(2025, "male", "war")["mid"] +
      v("deaths_female", 2025) * s(2025, "female", "war")["mid"],
    inf25 * s(2025, "<1", "war")["mid"] +
      v("age_total", "2025:1-4") * s(2025, "1-4", "war")["mid"] +
      v("age_total", "2025:5-19") * s(2025, "5-19", "war_incl_other_accidents")["mid"] +
      v("age_total", "2025:20-59") * s(2025, "20-59", "war_incl_other_accidents")["mid"] +
      v("age_total", "2025:60+") * s(2025, "60+", "war_incl_other_accidents")["mid"])
)

# =============================================================================
# TABLE 4. Registration completeness and implied nonviolent excess
# =============================================================================

gms_days <- v("gms_days", 2023)
gms_ann <- c(lo = v("gms_excess_lo", 2023), mid = v("gms_excess", 2023),
             hi = v("gms_excess_hi", 2023)) * 365 / gms_days
need_c <- function(excess) nv25 / (exp_nv25 + excess)

scen <- tibble(
  panel = c(rep("A. Illustrative scenarios", 5), rep("B. Completeness implied by the GMS estimate", 3)),
  scenario = c("Registration complete",
               "No age group below its 2022 level",
               "90% complete",
               sprintf("Home deaths missed at their 2022 share (%.1f%%)", 100 * v("home_death_share", 2022)),
               "75% complete",
               "GMS excess, lower 95% bound",
               "GMS excess, central estimate",
               "GMS excess, upper 95% bound"),
  completeness = c(1,
                   nv25 / (nv25 + parity_floor["mid"]),
                   0.90,
                   1 - v("home_death_share", 2022),
                   0.75,
                   need_c(gms_ann["lo"]), need_c(gms_ann["mid"]), need_c(gms_ann["hi"]))
) %>%
  mutate(implied_nonviolent = nv25 / completeness,
         implied_excess = implied_nonviolent - exp_nv25,
         multiple_of_registered = implied_excess / reg_excess)

gms_rates <- tibble(
  measure = c("GMS nonviolent death rate, Oct 2023-Jan 2025 (published, per person-time)",
              "  lower 95% bound", "  upper 95% bound",
              "MoH registered nonviolent death rate, 2025",
              "Ratio GMS / MoH (central; lower; upper)"),
  value = c(v("gms_nv_rate", 2023), v("gms_nv_rate_lo", 2023), v("gms_nv_rate_hi", 2023),
            nv25 / pop25 * 1000, NA)
)
gms_ratio <- c(v("gms_nv_rate", 2023), v("gms_nv_rate_lo", 2023), v("gms_nv_rate_hi", 2023)) /
  (nv25 / pop25 * 1000)

# =============================================================================
# TABLE 2, PANEL B. Selected cause-specific death rates (per 100,000)
# =============================================================================

cause_rates <- read_csv("data/cause_rates.csv", show_col_types = FALSE)
unk <- tibble(
  cause = "Unknown and unspecified",
  y2022 = v("unknown_cause_share_pct", 2022) / 100 * tot22 / pop22 * 1e5,
  y2025 = v("unknown_cause_share_pct", 2025) / 100 * tot25 / pop25 * 1e5
)
table2b <- cause_rates %>% select(cause, y2022, y2025) %>%
  bind_rows(tibble(cause = "Sum of the five causes above",
                   y2022 = sum(cause_rates$y2022), y2025 = sum(cause_rates$y2025))) %>%
  bind_rows(unk) %>%
  mutate(change_pct = pct_change(y2025, y2022))

hosp_share22 <- v("hospital_deliveries", 2022) / births22

# =============================================================================
# SUPPLEMENTARY FIGURE (not in the paper). Change 2022 -> 2025 by group (ratio, log scale), faceted by
#           where deaths in the group mostly occur
# =============================================================================

t3 <- function(lbl) table3 %>% filter(indicator == lbl)
t2 <- function(lbl) table2 %>% filter(group == lbl)

fig <- tibble(
  label = c("Stillbirths (per birth)",
            "Prematurity deaths (per birth)",
            "Infant infection deaths (per birth)",
            "Infant deaths excl. war and accidents (per birth)",
            "Ages 1-4",
            "Ages 5-19",
            "Ages 20-59",
            "Ages 60+",
            "Women, all ages (incl. accidents)"),
  ratio = c(t3("Stillbirth rate (per 1,000 total births)")$y2025 /
              t3("Stillbirth rate (per 1,000 total births)")$y2022,
            t3("Infant deaths from prematurity / low birth weight (per 1,000 live births)")$y2025 /
              t3("Infant deaths from prematurity / low birth weight (per 1,000 live births)")$y2022,
            t3("Infant deaths from infection incl. septicaemia")$y2025 /
              t3("Infant deaths from infection incl. septicaemia")$y2022,
            t3("Infant mortality, excl. war and accidents")$y2025 /
              t3("Infant mortality, excl. war and accidents")$y2022,
            t2("Ages 1-4")$y2025 / t2("Ages 1-4")$y2022,
            t2("Ages 5-19")$y2025 / t2("Ages 5-19")$y2022,
            t2("Ages 20-59")$y2025 / t2("Ages 20-59")$y2022,
            t2("Ages 60+")$y2025 / t2("Ages 60+")$y2022,
            t2("Women, all ages (including accidents)")$y2025 / t2("Women, all ages (including accidents)")$y2022),
  setting = factor(c(rep("Stillbirths and infants", 4), rep("Ages 1 and over", 5)),
                   levels = c("Stillbirths and infants", "Ages 1 and over"))
) %>%
  arrange(setting, ratio) %>%
  mutate(label = factor(label, levels = unique(label)))

p <- ggplot(fig, aes(x = ratio, y = label)) +
  geom_vline(xintercept = 1, colour = "grey55", linewidth = 0.4) +
  geom_segment(aes(x = 1, xend = ratio, yend = label), colour = "grey75", linewidth = 0.5) +
  geom_point(size = 2.6, colour = "grey15") +
  geom_text(aes(label = sprintf("x%.2f", ratio),
                hjust = ifelse(ratio >= 1, -0.35, 1.35)),
            size = 3, colour = "grey25") +
  scale_x_log10(breaks = c(0.5, 0.75, 1, 1.5, 2, 3, 5, 8),
                labels = c("x0.5", "x0.75", "x1", "x1.5", "x2", "x3", "x5", "x8"),
                expand = expansion(mult = c(0.12, 0.12))) +
  facet_grid(setting ~ ., scales = "free_y", space = "free_y", switch = "y") +
  labs(x = "Registered 2025 level relative to 2022 (log scale)", y = NULL) +
  theme_minimal(base_size = 11, base_family = "sans") +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank(),
        strip.placement = "outside",
        strip.text.y.left = element_text(angle = 0, hjust = 1, face = "bold"),
        panel.spacing.y = unit(0.8, "lines"))

ggsave("outputs/supplementary_figure.png", p, width = 8, height = 5, dpi = 200, bg = "white")
ggsave("outputs/supplementary_figure.pdf", p, width = 8, height = 5)

# =============================================================================
# Write outputs: the paper's tables, with the paper's titles, labels and notes
# =============================================================================

en   <- function(x) gsub("-", "–", x)                 # en dash for ranges/labels
mn   <- function(x) sub("^-", "−", x)                  # true minus sign
n0   <- function(x) formatC(round(x), format = "d", big.mark = ",")
pc   <- function(x) ifelse(round(x) == 0, "0%", mn(sprintf("%+.0f%%", x)))
d1   <- function(x) ifelse(x < 1, sprintf("%.2f", x), sprintf("%.1f", x))

notes <- list(
  t1 = paste0(
    "Sources: MoH (2023, 2026); Spagat et al. (2026) for the Census Bureau projection. ",
    sprintf("2022 accidents from the published rate of %.1f per 100,000, which may include the few war deaths of that year. ",
            v("accidents_rate_per100k", 2022)),
    sprintf("2018–19 baseline: average of the 2018 and 2019 crude death rates (%.2f and %.2f per 1,000) applied to the 2025 population.",
            v("cdr_per1000", 2018), v("cdr_per1000", 2019))),
  t2 = paste0(
    "Sources: MoH (2023, 2026); ranges for 2022 reflect accident shares not listed in the 2022 report; ",
    "ranges from rounding of published shares are given in the replication files. ",
    "2022 accidents are not reported by sex, so the rows for women and men include accidents in both years."),
  t3 = paste0(
    "Sources: MoH (2023, 2026). Neonatal and post-neonatal rates include the ",
    round(inf25 * s(2025, "<1", "war")["mid"]),
    " infant war deaths, whose ages are not reported. ",
    sprintf("2022 stillbirths are those in MoH hospitals (%d), consistent with the 2022 perinatal rate; ", sb22),
    sprintf("all hospitals report %d (%.1f per 1,000).", sb22_all, sb22_all / (births22 + sb22_all) * 1000)),
  t4 = paste0(
    "Implied nonviolent deaths = ", n0(nv25), " ÷ completeness; implied excess = implied nonviolent deaths − ",
    n0(exp_nv25), ". In panel A, the second row adds the deaths needed to bring each age group back to its 2022 level ",
    sprintf("(range %s–%s). ", n0(parity_floor["lo"]), n0(parity_floor["hi"])),
    "Panel B gives the completeness at which implied nonviolent deaths would match the GMS excess for ",
    "7 October 2023 to 5 January 2025, expressed per year. Completeness is measured relative to 2022 registration. ",
    "If 2022 registration was itself incomplete, true deaths in 2022 and 2025 are both higher by the same proportion.")
)

# ---- Table 1 ---------------------------------------------------------------
bl <- function(i) baselines$registered_excess[i]
t1_paper <- tibble(
  ` ` = c("Population (estimate)", "Live births", "All registered deaths",
          "Violent (operations of war)", "Nonviolent: accidents", "Nonviolent: all other causes",
          "Nonviolent: total", "Expected nonviolent deaths at 2022 rates",
          "Registered excess, 2022 baseline", "Excess in accidents", "Excess in all other causes",
          "Registered excess, 2018–19 baseline", "Registered excess, US Census Bureau baseline"),
  `2022` = c(n0(pop22), n0(births22), n0(tot22), "–", n0(acc22), n0(nv22_exacc), n0(nv22),
             rep("", 6)),
  `2025` = c(n0(pop25), n0(births25), n0(tot25), n0(war25), n0(acc25), n0(nv25_exacc), n0(nv25),
             n0(exp_nv25), n0(reg_excess), n0(acc25 - exp_acc25), n0(nv25_exacc - exp_ex25),
             n0(bl(2)), n0(bl(3)))
)

# ---- Table 2 ---------------------------------------------------------------
cell2 <- function(m, lo, hi, show_range, infant) {
  if (infant) return(sprintf("%.1f", m))
  if (show_range) paste0(n0(lo), "–", n0(hi)) else n0(m)
}
chg2 <- function(y25, lo22, hi22, mid22, show_range) {
  if (!show_range) return(pc(pct_change(y25, mid22)))
  a <- pc(pct_change(y25, hi22)); b <- pc(pct_change(y25, lo22))
  if (a == b) a else paste(a, "to", b)
}
t2a_paper <- table2 %>%
  rowwise() %>%
  mutate(infant = grepl("Under 1", group),
         `2022` = cell2(y2022, y2022_lo, y2022_hi, unlisted22, infant),
         `2025` = cell2(y2025, y2025_lo, y2025_hi, FALSE, infant),
         Change = chg2(y2025, y2022_lo, y2022_hi, y2022, unlisted22),
         group = en(group)) %>%
  ungroup() %>%
  select(` ` = group, `2022`, `2025`, Change) %>%
  bind_rows(tibble(` ` = "All-cause death rate at 75+, per 1,000 (2025 includes war deaths)",
                   `2022` = sprintf("%.1f", table2_75$y2022), `2025` = sprintf("%.1f", table2_75$y2025),
                   Change = pc(table2_75$change_pct)))

t2b_paper <- table2b %>%
  mutate(cause = recode(cause, "Malignant neoplasm" = "Malignant neoplasms",
                        "Sum of the five causes above" = "Sum of the five causes")) %>%
  transmute(` ` = cause, `2022` = sprintf("%.1f", y2022), `2025` = sprintf("%.1f", y2025),
            Change = pc(change_pct))

# ---- Table 3 ---------------------------------------------------------------
t3row <- function(lbl, ind) {
  r <- table3 %>% filter(indicator == ind)
  tibble(` ` = lbl, `2022` = d1(r$y2022), `2025` = d1(r$y2025), Change = pc(r$change_pct))
}
mmr25 <- mat25 / births25 * 1e5
t3_paper <- bind_rows(
  t3row("Stillbirths per 1,000 total births", "Stillbirth rate (per 1,000 total births)"),
  t3row("Early neonatal deaths (0–6 days) per 1,000 live births", "Early neonatal mortality, 0-6 days (per 1,000 live births)"),
  t3row("Late neonatal deaths (7–27 days) per 1,000 live births", "Late neonatal mortality, 7-27 days"),
  t3row("Post-neonatal deaths (28–364 days) per 1,000 live births", "Post-neonatal mortality, 28-364 days"),
  t3row("Infant deaths, excluding war and accidents, per 1,000 live births", "Infant mortality, excl. war and accidents"),
  t3row("Infant deaths from prematurity or low birth weight, per 1,000 live births", "Infant deaths from prematurity / low birth weight (per 1,000 live births)"),
  t3row("Infant deaths from infections (including septicaemia), per 1,000 live births", "Infant deaths from infection incl. septicaemia"),
  t3row("Infant deaths from congenital malformations, per 1,000 live births", "Infant deaths from congenital malformations"),
  t3row("Infant deaths of unknown cause (including SIDS), per 1,000 live births", "Infant deaths from SIDS / unknown cause"),
  tibble(` ` = "Maternal deaths per 100,000 live births",
         `2022` = sprintf("%.1f", mat22 / births22 * 1e5),
         `2025` = sprintf("%.0f (%.0f–%.0f)", mmr25["mid"], mmr25["lo"], mmr25["hi"]),
         Change = "too few to compare")
)

# ---- Table 4 ---------------------------------------------------------------
t4_paper <- scen %>%
  transmute(panel, Scenario = scenario,
            Completeness = sprintf("%.0f%%", 100 * completeness),
            `Implied nonviolent deaths` = n0(implied_nonviolent),
            `Implied excess` = n0(implied_excess),
            `Multiple of registered excess` = sprintf("%.1f", multiple_of_registered))
t4_print <- bind_rows(lapply(split(t4_paper, t4_paper$panel), function(d)
  bind_rows(tibble(Scenario = d$panel[1]), d %>% select(-panel)))) %>%
  mutate(across(everything(), ~ ifelse(is.na(.), "", .)))

# ---- write ------------------------------------------------------------------
sink("outputs/tables.md")
cat("# Tables for the paper (generated by R/analysis.R)\n\n")
cat("## Table 1. Registered deaths in Gaza, 2022 and 2025, and registered excess nonviolent deaths\n\n")
print(kable(t1_paper, align = "lrr")); cat("\n", notes$t1, "\n\n", sep = "")
cat("## Table 2. Registered nonviolent deaths, 2022 and 2025\n\n")
cat("Panel A. By age and sex (excluding accidents unless stated; 2022 scaled to the 2025 population)\n\n")
print(kable(t2a_paper, align = "lrrr"))
cat("\nPanel B. Selected causes, death rates per 100,000 population, all ages\n\n")
print(kable(t2b_paper, align = "lrrr")); cat("\n", notes$t2, "\n\n", sep = "")
cat("## Table 3. Perinatal, infant and maternal indicators, 2022 and 2025\n\n")
print(kable(t3_paper, align = "lrrr")); cat("\n", notes$t3, "\n\n", sep = "")
cat("## Table 4. Registration completeness and implied excess nonviolent deaths in 2025 (2022 baseline)\n\n")
print(kable(t4_print, align = "lrrrr")); cat("\n", notes$t4, "\n\n", sep = "")

cat("---\n\n# Figures quoted in the text but not tabulated\n\n")
cat(sprintf("- Accident death rate 2025: %.1f per 100,000 (%.1f times the 2022 rate of %.1f)\n",
            acc25 / pop25 * 1e5, (acc25 / pop25) / (acc22 / pop22), v("accidents_rate_per100k", 2022)))
male_acc <- v("deaths_male", 2025) * s_sum(2025, "male", c("other_accidents", "transport"))["mid"]
fem_acc  <- v("deaths_female", 2025) * s_sum(2025, "female", c("other_accidents", "transport"))["mid"]
cat(sprintf("- Male share of 2025 accidents: %.0f%%\n", 100 * male_acc / (male_acc + fem_acc)))
cat(sprintf("- Share of 2022 births delivered in hospital: %.1f%%\n", 100 * hosp_share22))
cat(sprintf("- 2022 deaths at home: %.1f%%; elsewhere outside hospital: 2.1%%\n", 100 * v("home_death_share", 2022)))
cat(sprintf("- GMS excess per year: %s (95%% CI %s–%s)\n", n0(gms_ann["mid"]), n0(gms_ann["lo"]), n0(gms_ann["hi"])))
cat(sprintf("- Nonviolent death rate per 1,000: GMS %.1f (%.1f–%.1f); MoH registered 2025 %.2f; ratio %.1f (%.1f–%.1f)\n",
            v("gms_nv_rate", 2023), v("gms_nv_rate_lo", 2023), v("gms_nv_rate_hi", 2023),
            nv25 / pop25 * 1000, gms_ratio[1], gms_ratio[2], gms_ratio[3]))
cat(sprintf("- Implied deaths at 83%% completeness exceed registered by %.0f%%\n",
            100 * (1 / scen$completeness[4] - 1)))

cat("\n# Consistency checks (not in the paper)\n\n")
print(kable(checks %>% mutate(value = sprintf("%.2f", value)), align = "lr"))
sink()

write_csv(table1, "outputs/table1.csv")
write_csv(baselines, "outputs/table1_baselines.csv")
write_csv(table2, "outputs/table2.csv")
write_csv(table2b, "outputs/table2b.csv")
write_csv(table3, "outputs/table3.csv")
write_csv(scen, "outputs/table4.csv")
writeLines(unlist(notes), "outputs/table_notes.txt")

cat(readLines("outputs/tables.md"), sep = "\n")
