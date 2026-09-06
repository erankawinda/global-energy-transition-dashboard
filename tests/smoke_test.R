#!/usr/bin/env Rscript

# Run from the repository root: Rscript tests/smoke_test.R
app_environment <- new.env(parent = globalenv())
sys.source("app.R", envir = app_environment)

energy <- app_environment$energy
mean_change <- app_environment$paired_country_mean_change

stopifnot(
  nrow(energy) > 0,
  all(c(
    "country", "iso_code", "year", "renewables_share_elec",
    "co2_intensity", "region"
  ) %in% names(energy)),
  !anyDuplicated(energy[c("country", "year")]),
  min(energy$year) == 2000,
  max(energy$year) == 2024,
  all(energy$renewables_share_elec >= 0),
  all(energy$renewables_share_elec <= 100),
  is.na(mean_change(energy, 2000)),
  is.finite(mean_change(energy, 2024))
)

latest <- energy[energy$year == max(energy$year), ]
stopifnot(
  nrow(latest) > 0,
  sum(!is.na(latest$co2_intensity) & latest$co2_intensity > 0) >= 2
)

carbon_counts <- energy |>
  dplyr::filter(!is.na(co2_intensity), co2_intensity > 0) |>
  dplyr::count(year)
annual_changes <- vapply(2000:2024, function(year) mean_change(energy, year), numeric(1))
stopifnot(
  all(2000:2024 %in% carbon_counts$year),
  min(carbon_counts$n) >= 2,
  is.na(annual_changes[[1]]),
  all(is.finite(annual_changes[-1]))
)

cat("Dashboard smoke test passed.\n")
