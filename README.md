# reprokit-app

Shiny front end for [reprokit](https://github.com/OWNER/reprokit). It runs in
the browser (shinylive/webR) and is hosted on GitHub Pages.

| Tab | For |
|---|---|
| I can't share my data | authors: triage a fitted model, see what readers lose |
| Does this paper add up? | readers: consistency checks (planned) |
| Class scenarios | teaching: one YAML file per scenario in `app/scenarios/` |

## Rule: no statistics in the app

Every number comes from a `reprokit::` function. If the app needs a new
calculation, add it to the package first (with tests), then call it here.
`tests/testthat/test-no-statistics.R` catches direct calls such as `mean()` or `lm()`.

## Develop

```r
pak::local_install_deps()   # installs shiny, bslib, yaml and reprokit (from GitHub)
shiny::runApp("app")
```

Test from the repo root:

```bash
Rscript tests/testthat.R
```

## Deploy

`deploy-shinylive.yaml` runs `shinylive::export("app", "site")` on every push to
`main`. webR installs packages from r-universe, not GitHub, so reprokit must be
published there before the hosted app works. Locally, the app uses whichever
reprokit is installed.
