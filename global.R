library(shiny)
library(bslib)
library(DT)
library(htmltools)
library(jsonlite)
library(DBI)
library(httr2)
library(uuid)
library(brand.yml)
library(openssl)

# Source R/ with brand_colors.R and app_config.R first (vasper pattern).
.r_files <- list.files("R", pattern = "\\.R$", full.names = TRUE)
.r_first <- file.path(
  "R",
  c("brand_colors.R", "app_config.R", "rules_engine.R")
)
invisible(lapply(c(.r_first, setdiff(.r_files, .r_first)), source))
rm(.r_files, .r_first)

app_theme <- bs_theme(version = 5, preset = "shiny", brand = "_brand.yml")
