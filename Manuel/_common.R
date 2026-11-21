# _common.R — réglages partagés : source("_common.R") en tête de chaque chapitre.
suppressPackageStartupMessages(library(ggplot2))
source("_moteur.R", encoding = "UTF-8")
knitr::opts_chunk$set(echo = FALSE, message = FALSE, warning = FALSE, fig.align = "center",
                      out.width = "85%", fig.width = 7.5, fig.height = 4, dpi = 200, dev = "ragg_png")
nb  <- function(x, d = 3) sub("^-", "−", fmt(x, d))
nbm <- function(x, d = 3) gsub(",", "{,}", fmt(x, d), fixed = TRUE)
pct <- function(x, d = 1) paste0(fmt(100 * x, d), " %")
fmt_axe <- function(v) ifelse(is.na(v), NA_character_, sub("\\.", ",", format(v, trim = TRUE, drop0trailing = TRUE)))
beta_txt <- function(ab) paste0("bêta(", ab[1], ", ", ab[2], ")")
COUL <- c("Uniforme" = "#125c88", "Plutôt équilibrée" = "#a45f00", "Face favorisée" = "#9b3374", "Pile favorisée" = "#28834c")
theme_set(theme_minimal(base_size = 12) + theme(panel.grid.minor = element_blank(), legend.position = "bottom",
  plot.title = element_text(face = "bold", size = 12)))

# ── Fil rouge : graine 2026, probabilité connue 0,70, a priori uniforme ────
# Clics : Lancer (10), Lancer (10), puis trois fois Lancer (100).
G <- 2026L; THETA <- 0.70
FIL <- experience(G, "connue", THETA)
for (lot in c(10, 10, 100, 100, 100)) FIL$lancer(lot)
H <- FIL$etat()$hist
