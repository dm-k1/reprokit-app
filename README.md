# reprokit-app

Shiny front end for [reprokit](https://github.com/dm-k1/reprokit). It runs in
the browser (shinylive/webR) and is hosted on GitHub Pages.

| Tab | For |
|---|---|
| Start here | everyone: what the app does, the two directions, what happens to the data |
| I can't share my data | authors: data (or a practice dataset), test, reason -> verdict, recomputed result, appendix (.md), what readers lose |
| Does this paper add up? | readers: type reported numbers -> consistency, coverage level, status per number, variants that fit |
| Practice examples | learning: fictional psychology studies in `app/practice/`, three papers to check and three datasets to share |
| Help: How the App works | how each part works, the glossary, what every result means, and `reprokit::coverage_table()` |

## Where things live

| Path | What |
|---|---|
| `app/app.R` | the page and the routing between tabs |
| `app/R/mod_*.R` | one module per tab |
| `app/R/text_explanations.R` | every explanation the app shows, the test catalogue and the worked examples |
| `app/R/utils_ui.R` | the theme and shared building blocks |
| `app/R/reprokit_bridge.R` | the only file that touches reprokit internals (see the TODO there) |
| `app/www/app.css` | the stylesheet |
| `app/practice/` | the practice examples, one YAML file each (see its README) |
| `tools/make-practice-data.R` | makes the fictional practice datasets |

Colours: purple for the app itself, and the Okabe-Ito colours (blue, sky blue,
orange, vermillion) for results, so they stay distinguishable with colour
blindness. No result relies on colour alone; each also has an icon and a word.

## Rule: no statistics in the app

Every number comes from a `reprokit::` function. If the app needs a new
calculation, add it to the package first (with tests), then call it here.
`tests/testthat/test-no-statistics.R` catches direct calls such as `mean()` or `lm()`
(it reads the parsed code, so the words inside help texts are not flagged).

## Develop

```r
pak::local_install_deps()   # installs shiny, bslib, yaml and reprokit (from GitHub)
shiny::runApp("app")
```

The practice datasets are made by a script, so they can be remade or changed:

```bash
Rscript tools/make-practice-data.R
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
