# ============================================================
# Summary stats for the fastball-streak DFA.
# Run from the repo root after src/fastball_automaton.R:
#   Rscript src/streak_summary.R [path/to/pitches.csv]
# ============================================================

args <- commandArgs(trailingOnly = TRUE)
input_file   <- if (length(args) >= 1) args[1] else "data/septemberpitch.csv"
atbat_file   <- file.path("output", "fastball_streak_by_atbat.csv")
fastball_ids <- c("FF")   # must match fastball_automaton.R

con <- file(input_file, open = "r", encoding = "UTF-8-BOM")
pitches <- read.csv(con, stringsAsFactors = FALSE); close(con)
names(pitches) <- sub("^X\\.+", "", names(pitches))
pitches <- pitches[order(pitches$pitcher, pitches$game_pk,
                         pitches$at_bat_number, pitches$pitch_number), ]
pitches$symbol   <- ifelse(pitches$pitch_type %in% fastball_ids, 1L, 0L)
pitches$atbat_id <- paste(pitches$pitcher, pitches$game_pk, pitches$at_bat_number, sep = "_")

ab <- read.csv(atbat_file, stringsAsFactors = FALSE)
pitches <- pitches[pitches$atbat_id %in% ab$atbat_id, ]

cat("Game dates:", min(pitches$game_date), "to", max(pitches$game_date), "\n")
cat("Plate appearances:", nrow(ab), "  accepted:", sum(ab$accepted),
    sprintf("(%.1f%%)\n", 100 * mean(ab$accepted)))

# Mark the pitches in each PA's first run of 3+ fastballs (the run that sends
# the DFA to q3). Four accepted PAs contain a second qualifying run; it is not
# counted, so each accepted PA contributes exactly one streak.
first_streak <- function(s) {
  r <- rle(s); keep <- rep(FALSE, length(s))
  k <- which(r$values == 1 & r$lengths >= 3)[1]
  if (!is.na(k)) { end <- cumsum(r$lengths)[k]; keep[(end - r$lengths[k] + 1):end] <- TRUE }
  keep
}
longest_run <- function(s) {
  r <- rle(s); if (any(r$values == 1)) max(r$lengths[r$values == 1]) else 0L
}

ids <- factor(pitches$atbat_id, levels = unique(pitches$atbat_id))
pitches$in_streak <- unlist(lapply(split(pitches$symbol, ids), first_streak))

ab$longest_run <- as.integer(tapply(pitches$symbol, pitches$atbat_id, longest_run)[ab$atbat_id])
top <- ab[ab$longest_run == max(ab$longest_run), c("atbat_id", "player_name", "game_pk", "result")]
top <- merge(top, pitches[!duplicated(pitches$atbat_id), c("atbat_id", "batter")], by = "atbat_id")
cat("\nLongest streak:", max(ab$longest_run), "straight fastballs\n")
print(top[, -1], row.names = FALSE)

cat("\nMost accepted PAs by pitcher:\n")
print(head(sort(table(ab$player_name[ab$accepted]), decreasing = TRUE), 6))

ff <- pitches$release_speed[pitches$in_streak]
cat(sprintf("\nFastballs in the streaks: %d, mean %.2f mph, range %.1f-%.1f\n",
            length(ff), mean(ff, na.rm = TRUE), min(ff, na.rm = TRUE), max(ff, na.rm = TRUE)))
