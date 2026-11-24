# ============================================================
# Fastball Sequencing Project — via a Deterministic Finite Automaton
#
# Alphabet:   Sigma = {0, 1}
#             1 = the pitch was a 4-seam fastball (FF)
#             0 = anything else
#
# Automaton:  Q  = {q0, q1, q2, q3}
#             q0 = start state
#             F  = {q3}   (accepting: "saw 3+ fastballs in a row")
#
# Transition table (delta):
#         0     1
#   q0    q0    q1
#   q1    q0    q2
#   q2    q0    q3
#   q3    q3    q3      <- absorbing: once accepted, stays accepted
#
# For each at-bat, the pitch sequence a1 a2 ... an is fed through
# the automaton one symbol at a time:
#   q_i = delta(q_{i-1}, a_i),   accept iff q_n in F
# ============================================================

# ---- 1. Settings ------------------------------------------------
args <- commandArgs(trailingOnly = TRUE)
input_file <- if (length(args) >= 1) args[1] else "data/septemberpitch.csv"
out_dir <- "output"
dir.create(out_dir, showWarnings = FALSE)
fastball_ids <- c("FF")              # 4-seam fastball only

# ---- 2. Define the automaton -------------------------------------
# delta[state, symbol] -> next state. Symbols are columns "0" and "1".
delta <- matrix(
  c("q0", "q1",   # from q0: on 0 -> q0, on 1 -> q1
    "q0", "q2",   # from q1: on 0 -> q0, on 1 -> q2
    "q0", "q3",   # from q2: on 0 -> q0, on 1 -> q3
    "q3", "q3"),  # from q3: on 0 -> q3, on 1 -> q3 (absorbing accept state)
  nrow = 4, ncol = 2, byrow = TRUE,
  dimnames = list(c("q0", "q1", "q2", "q3"), c("0", "1"))
)
start_state <- "q0"
accept_states <- c("q3")

# Run the automaton on a 0/1 sequence for one at-bat.
# Returns TRUE (accept) / FALSE (reject): start at q0, read left to right,
# follow delta, accept when the final state is accepting.
run_automaton <- function(symbols) {
  state <- start_state
  for (a in symbols) {
    state <- delta[state, as.character(a)]
  }
  state %in% accept_states
}

# ---- 3. Load data -------------------------------------------------
# Statcast exports are UTF-8 (accented names like "Soriano, José"), so read
# them in a UTF-8 locale.
suppressWarnings(invisible(try(Sys.setlocale("LC_ALL", "en_US.UTF-8"), silent = TRUE)))
suppressWarnings(invisible(try(Sys.setlocale("LC_ALL", "C.UTF-8"), silent = TRUE)))

con <- file(input_file, open = "r", encoding = "UTF-8-BOM")
pitches <- read.csv(con, stringsAsFactors = FALSE)
close(con)
names(pitches) <- sub("^X\\.+", "", names(pitches))  # strip stray BOM prefix if present

required_cols <- c("pitcher", "player_name", "game_pk",
                    "at_bat_number", "pitch_number", "pitch_type")
missing_cols <- setdiff(required_cols, names(pitches))
if (length(missing_cols) > 0) {
  stop("Missing expected column(s): ", paste(missing_cols, collapse = ", "))
}

# ---- 4. Order pitches correctly within each at-bat -----------------
pitches <- pitches[order(pitches$pitcher, pitches$game_pk,
                          pitches$at_bat_number, pitches$pitch_number), ]

# This is the input alphabet symbol for each pitch: 1 if fastball, 0 otherwise.
pitches$symbol <- ifelse(pitches$pitch_type %in% fastball_ids, 1L, 0L)

pitches$atbat_id <- paste(pitches$pitcher, pitches$game_pk,
                           pitches$at_bat_number, sep = "_")

# ---- 4b. Validate that each at-bat's sequence is trustworthy ---------
# We can only trust "3 fastballs in a row" if:
#   (a) pitch_number forms a clean 1..n run (no duplicate/missing pitch
#       numbers, which would mean we can't be sure of true order), and
#   (b) the at-bat actually finished (the last pitch has a non-blank
#       'events' value) -- an at-bat with no recorded result means our
#       data was cut off mid-PA, so the true sequence might continue
#       past what we have.
validate_atbat <- function(sub) {
  sub <- sub[order(sub$pitch_number), ]
  n <- nrow(sub)
  clean_sequence <- identical(sub$pitch_number, seq_len(n))
  last_event_present <- !is.na(sub$events[n]) && nzchar(sub$events[n])
  multiple_batters <- length(unique(sub$batter)) > 1  # informational only
  data.frame(
    clean_sequence = clean_sequence,
    complete_atbat = last_event_present,
    multiple_batters = multiple_batters
  )
}

validation <- do.call(rbind, lapply(split(pitches, pitches$atbat_id), validate_atbat))
validation$atbat_id <- rownames(validation)

n_bad_sequence <- sum(!validation$clean_sequence)
n_incomplete   <- sum(!validation$complete_atbat)
n_multi_batter <- sum(validation$multiple_batters)

cat("Data validation:\n")
cat(" - At-bats with a malformed pitch_number sequence:", n_bad_sequence, "\n")
cat(" - At-bats with no recorded result (likely cut off mid-PA):", n_incomplete, "\n")
cat(" - At-bats with a mid-PA batter change (informational, not excluded):",
    n_multi_batter, "\n")

# Exclude anything we can't trust the full sequence for.
valid_ids <- validation$atbat_id[validation$clean_sequence & validation$complete_atbat]
excluded  <- validation[!(validation$clean_sequence & validation$complete_atbat), ]
if (nrow(excluded) > 0) {
  write.csv(excluded, file.path(out_dir, "excluded_at_bats.csv"), row.names = FALSE)
  cat(" -> Excluded", nrow(excluded),
      "at-bat(s) from analysis; see output/excluded_at_bats.csv\n")
}
cat("\n")

pitches <- pitches[pitches$atbat_id %in% valid_ids, ]

# ---- 5. Run the automaton on every at-bat ---------------------------
symbol_split <- split(pitches$symbol, pitches$atbat_id)
accepted <- sapply(symbol_split, run_automaton)

atbat_info <- pitches[!duplicated(pitches$atbat_id),
                       c("atbat_id", "pitcher", "player_name",
                         "game_pk", "at_bat_number")]
atbat_info$accepted <- accepted[atbat_info$atbat_id]

# Attach the at-bat's result (Statcast's 'events' field, e.g. "strikeout",
# "single", "walk", ...), taken from each at-bat's last pitch.
last_pitch <- pitches[order(pitches$atbat_id, pitches$pitch_number), ]
last_pitch <- last_pitch[!duplicated(last_pitch$atbat_id, fromLast = TRUE), ]
result_lookup <- setNames(last_pitch$events, last_pitch$atbat_id)
atbat_info$result <- result_lookup[atbat_info$atbat_id]

# ---- 6. Trace one example at-bat ------------------------------------
example_id <- names(symbol_split)[which(accepted)[1]]
if (!is.na(example_id)) {
  seq_example <- symbol_split[[example_id]]
  cat("Example trace (at-bat", example_id, "):\n")
  cat("Input:  ", paste(seq_example, collapse = ""), "\n")
  state <- start_state
  trace <- state
  for (a in seq_example) {
    state <- delta[state, as.character(a)]
    trace <- c(trace, state)
  }
  cat("States: ", paste(trace, collapse = " -> "), "\n")
  cat("Result: ", ifelse(state %in% accept_states, "ACCEPT", "REJECT"), "\n\n")
}

# ---- 7. Output -------------------------------------------------------
cat("Total at-bats analyzed:", nrow(atbat_info), "\n")
cat("Total at-bats accepted (3+ FF in a row):", sum(atbat_info$accepted), "\n")

write.csv(atbat_info, file.path(out_dir, "fastball_streak_by_atbat.csv"), row.names = FALSE)
cat("\nSaved: output/fastball_streak_by_atbat.csv\n")
