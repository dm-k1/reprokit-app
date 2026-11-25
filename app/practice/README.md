# Practice examples

One YAML file per example. The app lists them in file-name order, in two
groups:

* `check-*.yml` (`type: check`): a fictional paper. "Check these numbers"
  loads `check$reported` into Check Paper for `check$test`, one
  of the ids in `app/text/tests.yml`. `check$args` holds test arguments such
  as `mu`, and `check$claims` what the paper says it did. `expected` records
  what reprokit should say (`checked`, `consistent`); a test holds the file
  to it.
* `share-*.yml` (`type: share`): a fictional dataset in `data/`, made by
  `tools/make-practice-data.R`. "Try it" loads it into Publish data with the analysis in `analysis` (`test`, `version`, `columns`).
  `expected_verdict` must be the verdict of `reprokit::sufficient_stats()`;
  a test checks it.

Rules:

* Quote every reported number (`"2.85"`), so its decimals are kept. A bare
  `n:` key is fine: the loader keeps `n` and `y` as names instead of reading
  them as yes/no. A table of counts or cell values is a list of rows.
* `share-*.yml` cards show `plan` under `reason_label` when the authors do
  share their data; `sharing_reason` is what gets typed into step 3 when the
  dataset is loaded.
* The cards never say which kind of example they are. Keep the verdict and
  the lesson in `answer`, which stays folded until a student opens it.
* No emoji: a test checks every file.
* Real examples must be verified at source resolution first (project plan,
  section 8).
