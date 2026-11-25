# All the explanatory text the app shows, in one place, so wording can be
# reviewed (and translated) without touching the logic. Words follow the
# project plan: "consistent / inconsistent with the reported statistics", never
# "wrong" or "error"; the tool implements an APA obligation, it does not go
# against APA. This file holds words only, no statistics.

# The tests the app offers --------------------------------------------------
# One entry per reprokit handler that the app exposes. `share` says which
# columns the author picks and how they become reprokit's formula; `check` says
# which reported numbers the form asks for.

test_catalogue <- function() {
  list(
    t_independent = list(
      title = "Compare two separate groups",
      menu = "Independent-Samples t-Test (Compare 2 separate groups)",
      technical = "Independent-samples t-test (Student or Welch)",
      example = "Do students taught with method A score higher than students taught with method B?",
      share = list(columns = c(outcome = "Outcome (the numbers you compared)",
                               group = "Group (a column with exactly two groups)"),
                   tests = c("Welch's t (does not assume equal variances)" = "welch",
                             "Student's t (assumes equal variances)" = "student")),
      check = list(per_cell = c("n", "m", "sd"), cells = 2L,
                   cell_names = c("Group 1", "Group 2"),
                   single = c("t", "df", "p", "d", "ci_lower", "ci_upper")),
      has_mu = FALSE, declarable = c("test", "tail")
    ),
    t_paired = list(
      title = "Compare two measurements of the same people",
      menu = "Paired-Samples t-Test (Compare 2 measurements of the same people)",
      technical = "Paired-samples t-test",
      example = "Did the same participants sleep longer after the intervention than before?",
      share = list(columns = c(first = "First measurement (e.g. before)",
                               second = "Second measurement (e.g. after)"),
                   tests = c("Paired t-test" = "paired")),
      check = list(per_cell = c("m", "sd"), cells = 2L,
                   cell_names = c("Condition 1", "Condition 2"),
                   single = c("n", "r", "t", "df", "p", "d", "ci_lower", "ci_upper")),
      has_mu = TRUE, declarable = "tail"
    ),
    t_one_sample = list(
      title = "Compare one group's mean with a fixed value",
      menu = "One-Sample t-Test (Compare 1 group's mean with a fixed value)",
      technical = "One-sample t-test",
      example = "Is the average score in this sample different from the scale midpoint of 4?",
      share = list(columns = c(outcome = "Outcome (the numbers you compared)"),
                   tests = c("One-sample t-test" = "one_sample")),
      check = list(per_cell = c("m", "sd"), cells = 1L, cell_names = "Sample",
                   single = c("n", "t", "df", "p", "d", "ci_lower", "ci_upper")),
      has_mu = TRUE, declarable = "tail"
    ),
    cor_pearson = list(
      title = "How strongly two numeric variables go together",
      menu = "Pearson Correlation (How strongly 2 numeric variables go together)",
      technical = "Pearson correlation",
      example = "Do people who sleep more also report less stress?",
      share = list(columns = c(x = "First variable", y = "Second variable"),
                   tests = c("Pearson correlation" = "pearson")),
      check = list(per_cell = c("m", "sd"), cells = 2L,
                   cell_names = c("Variable 1", "Variable 2"),
                   single = c("n", "r", "df", "p", "ci_lower", "ci_upper")),
      has_mu = FALSE, declarable = "tail"
    ),
    anova_oneway = list(
      title = "Compare three or more separate groups",
      menu = "One-Way Between Subjects ANOVA (Compare 3 or more separate groups)",
      technical = "One-way between-subjects ANOVA",
      example = paste("Does anxiety differ between pupils in a CBT programme, a mindfulness",
                      "programme and a waiting list?"),
      share = list(columns = c(outcome = "Outcome (the numbers you compared)",
                               group = "Group (a column with two or more groups)"),
                   tests = c("One-way ANOVA (classical F test)" = "oneway")),
      check = list(per_cell = c("n", "m", "sd"), cells = NA_integer_,
                   cell_names = NULL,
                   single = c("f", "df1", "df2", "p", "eta2")),
      has_mu = FALSE, declarable = character()
    )
  )
}

# Choices for the test drop-down menus: the test's name first, then in brackets
# what it is for, e.g. "One-Way Between Subjects ANOVA (Compare 3 or more
# separate groups)". The wording is the `menu` field of each entry above.
test_choices <- function() {
  tests <- test_catalogue()
  stats::setNames(names(tests), vapply(tests, `[[`, character(1), "menu"))
}

# The example behind each test's "See a practice example" button: a short
# story and the numbers as a paper would print them. Every one of them is
# consistent according to reprokit (a test checks this); the Practice examples
# tab holds the harder cases.
check_examples <- function() {
  list(
    t_independent = list(
      note = paste("Two groups of 10 volunteers each took a different sleep aid; the outcome is",
                   "how many extra hours they slept. Reported with Welch's t."),
      reported = list(n = c("10", "10"), m = c("0.75", "2.33"), sd = c("1.79", "2.00"),
                      t = "-1.86", df = "17.8", p = ".079")
    ),
    t_paired = list(
      note = paste("Twenty students rated their stress from 1 to 7 before and after four",
                   "weeks with a relaxation app. The paper does not report the correlation",
                   "between the two ratings."),
      reported = list(n = "20", m = c("5.1", "4.6"), sd = c("1.2", "1.1"),
                      t = "2.85", df = "19", p = ".010")
    ),
    t_one_sample = list(
      note = paste("Twenty volunteers' extra hours of sleep on a new sleep aid, compared with",
                   "a test value of 0.5 hours (the gain a placebo usually gives)."),
      reported = list(n = "20", m = "1.54", sd = "2.02", t = "2.30", df = "19", p = ".033"),
      mu = 0.5
    ),
    cor_pearson = list(
      note = paste("Sleep duration and wellbeing in a sample of students, reported the short",
                   "APA way: r(28) = .41, p = .024, without n, means or SDs."),
      reported = list(r = ".41", df = "28", p = ".024")
    ),
    anova_oneway = list(
      note = paste("Thirty participants rated their mood from 1 to 10 after watching a sad,",
                   "a neutral or a happy film clip, ten per clip."),
      reported = list(n = c("10", "10", "10"), m = c("5.03", "4.66", "5.53"),
                      sd = c("0.58", "0.79", "0.44"), f = "4.85", df1 = "2", df2 = "27",
                      eta2 = "0.26", p = ".016")
    )
  )
}

# Quantities ------------------------------------------------------------------

quantity_info <- function() {
  list(
    n = c("Sample size (n)",
          "How many people (or cases) the result rests on. Type it as a whole number."),
    m = c("Mean (M)", "The average, exactly as printed in the paper."),
    sd = c("Standard deviation (SD)", "How spread out the scores are, exactly as printed."),
    r = c("Correlation (r)",
          paste("For a paired test: the correlation between the two measurements.",
                "Papers rarely report it.")),
    t = c("t value", "The test statistic, e.g. the 2.85 in t(19) = 2.85."),
    f = c("F value", "The test statistic, e.g. the 4.85 in F(2, 27) = 4.85."),
    df = c("Degrees of freedom (df)", "The number in brackets, e.g. the 19 in t(19)."),
    df1 = c("df, first number", "The first number in F(2, 27): groups minus one."),
    df2 = c("df, second number", "The second number in F(2, 27)."),
    p = c("p value", "As printed: \".010\", \"0.01\", \"< .001\" all work."),
    d = c("Effect size (d or g)",
          "Cohen's d, Hedges' g or similar. The app tries every common way of computing it."),
    eta2 = c("Eta squared (\u03B7\u00B2)", "The effect size of the ANOVA."),
    ci_lower = c("Confidence interval, lower bound", "The first number in [0.21, 1.40]."),
    ci_upper = c("Confidence interval, upper bound", "The second number in [0.21, 1.40].")
  )
}

quantity_label <- function(q) {
  info <- quantity_info()
  vapply(q, function(x) if (is.null(info[[x]])) x else info[[x]][1L], character(1),
         USE.NAMES = FALSE)
}

quantity_help <- function(q) {
  info <- quantity_info()
  if (is.null(info[[q]])) "" else info[[q]][2L]
}

# Relations, in words. Unknown names fall back to "x from y".
relation_words <- function(name) {
  words <- c(
    df_from_n = "df from the sample size",
    df1_from_n = "first df from the number of groups",
    df2_from_n = "second df from the sample sizes",
    t_from_descriptives = "t recomputed from n, M and SD",
    f_from_descriptives = "F recomputed from n, M and SD",
    p_from_t = "p from t and df",
    p_from_f = "p from F and df",
    t_from_r = "t from r and df",
    eta2_from_f = "eta squared from F and df",
    ci_lower_from_descriptives = "CI lower bound from n, M and SD",
    ci_upper_from_descriptives = "CI upper bound from n, M and SD",
    ci_lower_from_r = "CI lower bound from r and n",
    ci_upper_from_r = "CI upper bound from r and n",
    d_from_descriptives = "d from M and SD",
    d_av_from_descriptives = "d (average-SD versions) from M and SD"
  )
  vapply(name, function(x) {
    if (x %in% names(words)) words[[x]] else gsub("_", " ", x, fixed = TRUE)
  }, character(1), USE.NAMES = FALSE)
}

# Turn "p_from_t; t_from_descriptives (needs r)" into readable words.
checked_against_words <- function(text) {
  if (is.na(text) || !nzchar(text)) return("\u2014")
  if (startsWith(text, "no ")) {
    return(paste("every common formula was tried;", sub(" \\(.*$", "", text)))
  }
  parts <- strsplit(text, "; ", fixed = TRUE)[[1L]]
  readable <- vapply(parts, function(part) {
    name <- sub(" .*$", "", part)
    rest <- if (grepl(" ", part, fixed = TRUE)) sub("^[^ ]+ ", "", part) else ""
    trimws(paste(relation_words(name), rest))
  }, character(1), USE.NAMES = FALSE)
  paste(readable, collapse = "; ")
}

# Statuses of a reported number ---------------------------------------------------
# icon, colour class, one-line meaning. Colour is never the only signal.

status_info <- function() {
  list(
    "verified" = c("\u2714", "success",
                   paste("Recomputed from more basic numbers (for example t from n, M and SD)",
                         "and it matches.")),
    "coherent" = c("\u2713", "info",
                   paste("Fits the other numbers, but was only used as an input, or checked",
                         "against other derived numbers.")),
    "inconsistent" = c("\u2716", "danger",
                       paste("Cannot be correct together with the other numbers, as reported.",
                             "This does not say which number is off, or why.")),
    "not reproducible" = c("?", "warning",
                           paste("No common convention reproduces it (typical for effect sizes",
                                 "whose formula is not stated). A remark, not an accusation.")),
    "unconfirmed" = c("\u2026", "secondary",
                      "Could not be checked because another number it needs is not reported."),
    "not checkable" = c("\u2013", "light",
                        "No formula of this test connects it to the other numbers.")
  )
}

# Triage verdicts (plan section 3). ------------------------------------------------

verdict_info <- function() {
  list(
    summaries_suffice = list(
      icon = "\u2705",
      title = "Summary statistics suffice",
      class = "success",
      meaning = paste("Readers can reproduce your result exactly from a small set of summary",
                      "statistics. The app writes the appendix that lists them.")
    ),
    synthetic_possible = list(
      icon = "\U0001F501",
      title = "Synthetic data possible",
      class = "warning",
      meaning = paste("Summaries cannot reproduce this result exactly. A synthetic dataset",
                      "could reproduce it approximately. Share the raw data if you can.")
    ),
    needs_raw_data = list(
      icon = "\U0001F512",
      title = "Needs the raw data",
      class = "danger",
      meaning = paste("Summaries are not enough, or they would be so close to the raw data",
                      "that they protect no one. Readers need the raw data.")
    )
  )
}

standard_reminder <- function() {
  paste("Sharing the raw data remains the standard. The APA Publication Manual",
        "(7th ed., \u00A71.14)",
        "requires authors to make their data available, and the APA Ethics Code (Standard 8.14)",
        "prohibits withholding them from qualified researchers who want to verify the results.",
        "That is APA policy, not this app's opinion.")
}

inconsistency_caveat <- function() {
  paste("An inconsistency shows that the numbers cannot all be correct as reported. It does not",
        "show which one is off, or why: a typo, a different test, different rounding and a real",
        "mistake all look the same.")
}

# Coverage levels, in plain words, for the level ladder. -----------------------------

level_steps_text <- function() {
  c("Nothing could be checked.",
    "The statistic, df, p and CI agree with each other.",
    "The test statistic was recomputed from the descriptives (n, M, SD).",
    "Everything needed to reproduce the analysis is reported.")
}

# Sharing reasons (plan section 1, Chambers 2019 ch. 4). -------------------------

sharing_reason_help <- function() {
  tagList(
    p("You have to give a reason before the app produces anything. This keeps the app from",
      "becoming a respectable excuse for not sharing: your reason is printed in the appendix,",
      "next to the list of what readers lose."),
    tags$p(tags$b("Reasons that often hold up:"), "participants did not consent to sharing;",
           "the data could identify people even without names (small groups, rare",
           "combinations); a data-use agreement with a third party forbids it."),
    tags$p(tags$b("Reasons worth a second look:"), "\"it is a lot of work\", \"others might",
           "find mistakes\", \"we collected it, so it is ours\". Most refusals to share are",
           "not legal reasons (Chambers, 2019, ch. 4), and papers whose authors are",
           "reluctant to share tend to contain more reporting errors (Wicherts et al., 2011).",
           "Could you share under a data-use agreement instead?")
  )
}

# Glossary (follows vignette("concepts") in reprokit). -------------------------------

glossary <- function() {
  list(
    "Sufficient set" = paste("The smallest set of numbers from which the whole result of a test",
                             "can be recomputed exactly. For a t-test with two separate groups:",
                             "n, mean and SD of each group. For a Pearson correlation: n and r."),
    "APA set" = paste("The numbers APA style asks a paper to report for a test (for two separate",
                      "groups: n, M and SD per group, t, df, p, d and a confidence interval)."),
    "Missing numbers" = paste("Numbers in the sufficient set that APA does not ask for. If a test",
                              "has any, a paper that follows APA style still does not let readers",
                              "reproduce the result. For the paired t-test it is the correlation",
                              "between the two measurements."),
    "Choices to declare" = paste("Decisions the numbers do not reveal but the result depends",
                                 "on: one- or two-sided, confidence level, Student or Welch, and",
                                 "how the effect size was computed. They are choices, not",
                                 "missing numbers."),
    "Verdict" = paste("Whether summaries suffice, synthetic data could work, or the raw data are",
                      "needed. Advice about what is enough, not a judgement on the author."),
    "Disclosure check" = paste("A check that no group has fewer than 5 cases and that the total is",
                               "at least 10. It lowers the risk that summaries identify someone;",
                               "it does not remove it, and it is not a legal guarantee."),
    "Rounding interval" = paste("A printed number stands for every value that rounds to it:",
                                "\"2.85\" means anything from 2.845 to 2.855. That is why you",
                                "type numbers exactly as printed, and why 2.850 and 2.85 are",
                                "different claims."),
    "Relation" = paste("A formula that links reported numbers, such as p from t and df. The check",
                       "evaluates every relation whose numbers are reported."),
    "Variant" = paste("One way of making the analysis choices (for example Welch's t, two-sided,",
                      "95%). The check tries them all and reports which fit. All the numbers of",
                      "one result must fit the same variant."),
    "Coverage level" = paste("How much of the result the reported numbers let anyone check, from 0",
                             "(nothing) to 3 (everything needed to reproduce it is reported)."),
    "Implied range" = paste("When a number is not reported but the others constrain it, the range",
                            "it must lie in. A derived value, not a claim about the true value.")
  )
}
