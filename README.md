# reprokit-app

Shiny front end for [reprokit](https://github.com/dm-k1/reprokit). It runs in
the browser (shinylive/webR) and is hosted on GitHub Pages.

The app is one page: a title bar, a short explanation, and three tabs.

| Tab | For |
|---|---|
| Check Paper (opens first) | readers: reported numbers -> consistency, coverage level, status per number, choices that fit |
| Publish data | authors who cannot share data: data, test, reason -> verdict, what to publish, appendix (.md), what readers lose |
| Help | how it works, practice examples (three papers to check and three datasets to publish), what the results mean, the glossary, `reprokit::coverage_table()`, APA and data sharing, data safety |

The **Dark mode** button in the title bar switches to a dark scheme in OSF's
navy and blue; the choice is remembered in the browser.

Each tool works like an app: the steps sit in a sidebar and open one at a
time, three value boxes summarise the result, and the details sit in tabs
that appear once there is a result.

Tests offered (from `reprokit::handlers()`):

* **Compare means:** independent-samples t (Student and Welch), paired t,
  one-sample t, one-way ANOVA, two-way ANOVA.
* **Relationships:** Pearson correlation, simple, multiple and moderation
  regression (check only; share a regression as a fitted `lm` in `.rds`),
  simple mediation.
* **Counts:** chi-squared test of independence and goodness of fit.

## Where things live

| Path | What |
|---|---|
| `app/app.R` | starts the app |
| `app/R/layout.R` | the page, the help panel and the routing between tabs |
| `app/R/mod_*.R` | one module per tab |
| `app/R/check_form.R` | the number-entry form, built from the test catalogue |
| `app/R/reprokit_calls.R` | every call to reprokit, in one place |
| `app/R/tests.R`, `app/R/text.R` | reading the catalogue and the words |
| `app/R/ui_parts.R` | the theme and shared building blocks |
| `app/text/en.yml` | every word the app shows |
| `app/text/tests.yml` | the test catalogue: menus, formulas, form fields |
| `app/text/examples.yml` | the worked example behind each test's "See a practice example" |
| `app/practice/` | the practice examples, one YAML file each (see its README) |
| `app/www/app.css` | the stylesheet |
| `tools/make-practice-data.R` | makes the fictional practice datasets |

To change wording, edit `app/text/en.yml`; no R is needed. To add a test that
reprokit supports, add an entry to `app/text/tests.yml` and a worked example
to `app/text/examples.yml`; the tests check that the example is consistent.

## Rules

* **No statistics in the app.** Every number comes from a `reprokit::`
  function. If the app needs a new calculation, add it to the package first
  (with tests), then call it here. `tests/testthat/test-no-statistics.R`
  catches direct calls such as `mean()` or `lm()`.
* **No emoji.** Icons come from Font Awesome (`shiny::icon()`);
  `tests/testthat/test-text.R` checks every file and every icon name.
* **Tidyverse style.** `lintr` with its default (tidyverse) linters, lines up
  to 80 characters; CI fails on any lint.
* **Colours.** The Open Science Framework's palette (from OSF's own
  stylesheets). Results use blue, red and orange, which stay distinguishable
  with the common forms of colour blindness, and never rely on colour alone.

## Develop

```r
pak::local_install_deps()   # installs shiny, bslib, yaml and reprokit (GitHub)
shiny::runApp("app")
```

The practice datasets are made by a script, so they can be remade:

```bash
Rscript tools/make-practice-data.R
```

Test and lint from the repo root:

```bash
Rscript tests/testthat.R
Rscript -e 'lintr::lint_dir("app")'
```

## Deploy

`deploy-shinylive.yaml` runs `shinylive::export("app", "site")` on every push
to `main`. webR installs packages from r-universe, not GitHub, so reprokit
must be published there before the hosted app works. Locally, the app uses
whichever reprokit is installed.
