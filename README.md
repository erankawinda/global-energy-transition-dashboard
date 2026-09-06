# Global Energy Transition Dashboard

An interactive R Shiny dashboard for exploring changes in national electricity
mixes since 2000. It includes a choropleth, country time series, a descriptive
renewables-versus-emissions view, and regional electricity-mix summaries.

This was developed as a data-visualisation coursework project. It is an
exploratory dashboard, not a causal study or an energy-system forecasting tool.

## Run locally

Install the required R packages:

```r
install.packages(c(
  "shiny", "shinydashboard", "plotly", "dplyr", "readr",
  "sf", "scales", "tidyr"
))
```

Then, from the repository root:

```r
shiny::runApp()
```

## Data

- `data/owid-energy-data.csv` and `data/owid-energy-codebook.csv` are a bundled
  snapshot of the [Our World in Data Energy dataset](https://github.com/owid/energy-data).
  The codebook records the upstream sources and units for individual variables.
- `data/world-countries.json` supplies country boundaries and contains Natural
  Earth attribution metadata. The original download URL and version were not
  recorded in this coursework repository.
- `data/ISO 3166 country codes.csv` supplies two-letter country codes.

The bundled files make the original dashboard reproducible, but anyone reusing
the data should check the current upstream versions and licence terms.

## Interpretation

The dashboard presents descriptive relationships. Country averages are not
population- or electricity-weighted unless a chart explicitly says otherwise,
and the plots should not be read as evidence that one variable caused another.

## Repository hygiene

R session files, editor state, deployment metadata, and operating-system files
are intentionally excluded. Deployment credentials must be configured outside
the repository.

## Licence

The source code is provided under the BSD 3-Clause licence in `LICENSE`.
Upstream datasets and geographic boundaries retain their own terms.
