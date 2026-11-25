# Practice examples

One YAML file per example. The app lists them in file-name order, in two groups:

* `check-*.yml` (`type: check`): a fictional paper. The card has a button that loads
  `check$reported` into "Does this paper add up?". `expected` records what reprokit
  should say (`checked`, `consistent`); a test holds the file to it.
* `share-*.yml` (`type: share`): a fictional dataset in `data/`, made by
  `tools/make-practice-data.R`. The card loads it into "I can't share my data" with the
  analysis in `analysis`. `expected_verdict` must be what `reprokit::triage()` returns;
  a test checks it.

Rules:

* Quote every reported number (`"2.85"`), so its decimals are kept. A bare `n:` key is
  fine: the loader keeps `n` and `y` as names instead of reading them as yes/no.
* `share-*.yml` cards show `plan` under `reason_label` when the authors do share their
  data; `sharing_reason` is what gets typed into step 3 when the dataset is loaded.
* The cards never say which kind of example they are. Keep the verdict and the lesson
  in `answer`, which stays folded until a student opens it.
* Real examples must be verified at source resolution first (project plan, section 8).
