library(testthat)
# brand_colors.R reads _brand.yml by relative path; run from the project root.
source(file.path("R", "app_config.R"))
source(file.path("R", "brand_colors.R"))

# WCAG 2.1 relative luminance and contrast ratio.
.hex_rgb <- function(hex) strtoi(substring(sub("^#", "", hex), c(1, 3, 5), c(2, 4, 6)), 16L)
.rel_lum <- function(hex) {
  srgb <- .hex_rgb(hex) / 255
  lin <- ifelse(srgb <= 0.03928, srgb / 12.92, ((srgb + 0.055) / 1.055)^2.4)
  sum(lin * c(0.2126, 0.7152, 0.0722))
}
contrast_ratio <- function(a, b) {
  l <- sort(c(.rel_lum(a), .rel_lum(b)), decreasing = TRUE)
  (l[1] + 0.05) / (l[2] + 0.05)
}

test_that("every required brand colour resolves to a hex string", {
  required <- c("primary", "primary_light", "secondary", "success",
                "warning", "warning_light", "danger", "danger_light",
                "foreground", "background", "surface", "light", "dark", "rule_line")
  for (key in required) {
    val <- BRAND_COLORS[[key]]
    expect_true(!is.null(val), info = paste("missing brand colour:", key))
    expect_match(val, "^#[0-9A-Fa-f]{6}$", info = paste("not a hex colour:", key))
  }
})

test_that("agriculture-app leftovers are gone", {
  expect_null(BRAND_COLORS$usda_nass_navy)
  expect_null(BRAND_COLORS$accent)
})

test_that("body text meets WCAG AA on both surfaces", {
  expect_gte(contrast_ratio(BRAND_COLORS$foreground, BRAND_COLORS$background), 4.5)
  expect_gte(contrast_ratio(BRAND_COLORS$secondary,  BRAND_COLORS$surface),    4.5)
  expect_gte(contrast_ratio("#FFFFFF",               BRAND_COLORS$primary),    4.5)
  expect_gte(contrast_ratio(BRAND_COLORS$danger,     BRAND_COLORS$background), 4.5)
})
