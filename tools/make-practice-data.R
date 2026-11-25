# Makes the fictional practice datasets in app/practice/data/.
# The data are invented: simulated with a fixed seed, then rounded to the
# precision a questionnaire or task would give. Re-run from the repo root:
#   Rscript tools/make-practice-data.R
# This script lives outside app/ on purpose: it computes things, and the app
# itself contains no statistics. Keep the order of the random draws, or the
# files (and the practice answers) change.

out <- file.path("app", "practice", "data")
dir.create(out, recursive = TRUE, showWarnings = FALSE)
set.seed(20261124)

clamp <- function(x, lower, upper) pmin(pmax(x, lower), upper)

write_data <- function(data, name) {
  utils::write.csv(data, file.path(out, name), row.names = FALSE)
}

# 1. Anxiety programmes for young adolescents: 3 groups of 15, one-way ANOVA.
programmes <- rep(c("CBT group", "Mindfulness", "Waiting list"), each = 15)
anxiety <- data.frame(
  pupil = sprintf("P%02d", seq_along(programmes)),
  programme = programmes,
  age = sample(12:15, length(programmes), replace = TRUE),
  anxiety_before = round(clamp(rnorm(45, 34, 7), 0, 60)),
  anxiety_after = round(
    clamp(rnorm(45, rep(c(24, 27, 32), each = 15), 7), 0, 60)
  )
)
write_data(anxiety, "school-anxiety-programmes.csv")

# 2. Face-memory training in developmental prosopagnosia: 7 adults.
before <- round(clamp(rnorm(7, 38, 5), 0, 72))
faces <- data.frame(
  participant = sprintf("DP%d", 1:7),
  age = c(24, 31, 45, 29, 52, 38, 27),
  face_memory_before = before,
  face_memory_after = round(clamp(before + rnorm(7, 5, 3), 0, 72))
)
write_data(faces, "face-memory-training.csv")

# 3. Night shifts and attention lapses: 16 day-shift, 4 night-shift nurses.
shift <- rep(c("day", "night"), c(16, 4))
nurses <- data.frame(
  nurse = sprintf("N%02d", seq_along(shift)),
  shift = shift,
  years_on_ward = sample(1:20, length(shift), replace = TRUE),
  lapses = round(
    clamp(rnorm(20, ifelse(shift == "day", 4, 9), 2.5), 0, Inf)
  )
)
write_data(nurses, "night-shift-attention.csv")

files <- list.files(out, pattern = "\\.csv$")
cat("Wrote", length(files), "files to", out, "\n")
