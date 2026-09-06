# Global Energy Transition Dashboard

[Open the live dashboard](https://01976317-6787-6e0a-0975-c8b07e3a7905.share.connect.posit.cloud/)

An interactive R Shiny dashboard for exploring national electricity mixes from
2000 to 2024. It provides:

- a country-level map of renewable electricity shares;
- time-series and since-2000 comparisons for selected countries;
- a descriptive comparison of renewable share and electricity carbon
  intensity; and
- unweighted regional summaries.

This was developed as a data-visualisation coursework project. It is an
exploratory dashboard, not a causal study, forecast, or energy-system model.

## How the analysis works

`app.R` joins country records to UN regions using ISO-3 codes from the bundled
boundary file. The selected-year headline is the unweighted mean across
countries with data. The annual-change statistic first matches countries
observed in both the selected and preceding year, then averages their
country-level changes. This avoids treating a change in country coverage as a
change in renewable share.

The renewables-and-carbon-intensity view fits a simple linear model to the log
of carbon intensity for the selected year. It is shown only as a descriptive
summary of cross-country association; it does not identify a causal effect.

## Run locally

The deployed manifest uses R 4.4.1. Install the direct application packages:

```r
install.packages(c(
  "shiny", "shinydashboard", "plotly", "dplyr", "readr",
  "sf", "tidyr"
))
```

From the repository root, run:

```r
shiny::runApp()
```

The app reads only committed files, so no account, API key, or network request
is needed at runtime.

## Data and provenance

- `data/owid-energy-data.csv` and `data/owid-energy-codebook.csv` are a
  committed snapshot of the [Our World in Data Energy
  dataset](https://github.com/owid/energy-data). The snapshot contains records
  through 2024, although coverage differs by indicator, country, and year. The
  exact upstream commit and download time were not recorded in the original
  coursework project.
- `data/world-countries.json` supplies ISO-3 and UN-region fields used by the
  app. Its schema is derived from Natural Earth country-boundary data. The
  original download URL and release were not recorded, so it should not be
  treated as a current source for disputed boundaries.
- `data/README.md` records file roles and SHA-256 checksums for the committed
  snapshot.

[Our World in Data's reuse guidance](https://ourworldindata.org/faqs) asks users
to credit both OWID and the underlying sources listed in its codebook. Its own
data and visualisations are available under CC BY; third-party fields retain
their original terms. Natural Earth data are public domain under its [terms of
use](https://www.naturalearthdata.com/about/terms-of-use/).

## Interpretation limits

- Country and regional means are not population- or electricity-weighted.
- Changing indicator coverage can affect comparisons across years.
- The fitted line summarises association, not causation.
- Values reflect the committed historical snapshot, not live upstream data.

## Deployment

`manifest.json` records the R version, package dependencies, and application
files required by Posit Connect Cloud. After changing app files or
dependencies, regenerate it from a working R 4.4.1 environment:

```r
rsconnect::writeManifest(appDir = ".", appMode = "shiny")
```

Commit the regenerated manifest with the related change. Account tokens and
deployment credentials must remain outside the repository.

## Repository structure

```text
app.R                 Shiny user interface, data preparation, and server logic
www/style.css         Responsive dashboard styling
data/                 Committed data snapshot and provenance notes
tests/smoke_test.R    Data and transformation checks
manifest.json         Posit Connect Cloud dependency manifest
```

Run the smoke test from the repository root with:

```bash
Rscript tests/smoke_test.R
```

## Licence

The application source code is provided under the BSD 3-Clause licence in
`LICENSE`. Bundled upstream data retain their own terms as described above.
