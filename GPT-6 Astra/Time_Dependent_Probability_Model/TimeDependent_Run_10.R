# Step 1: Store supplied inputs without printing.
# Currency and price year have not yet been specified.
inputs <- list(
  states = c("stable", "progressive", "death"),
  utilities = list(
    stable = 0.743,
    progressive = 0.731,
    decrement_per_consecutive_month_progression = 0.02
  ),
  costs = list(
    standard_tmz = list(
      stable_months_1_3 = 1944,
      stable_months_4_6 = 2453,
      stable_subsequent_months = 350,
      progressive = 2720,
      death = 0
    ),
    long_term_tmz = list(
      stable_months_1_3 = 1944,
      stable_months_4_6 = 2453,
      stable_subsequent_months = 2453,
      progressive = 2720,
      death = 0
    )
  ),
  design = list(
    cycle_length_months = 1,
    time_horizon_months = 60
  )
)

# Standalone Standard TMZ calculations; no other script is sourced or changed.
# Decimal commas in supplied data are represented as decimal points in R.
# Interpret the requested formula as 1 - exp(log(y) / t).
# IMPORTANT: These are the requested transformations, not validated conditional
# Markov transition probabilities. OS includes both living states; OS - PFS
# is progressive-state occupancy, not survival conditional on progression.

OS_standard <- c(
  0.988, 0.970, 0.950, 0.913, 0.888, 0.863, 0.813, 0.782, 0.733, 0.675,
  0.650, 0.611, 0.550, 0.525, 0.488, 0.450, 0.430, 0.394, 0.350, 0.325,
  0.313, 0.300, 0.275, 0.265, 0.240, 0.225, 0.213, 0.213, 0.213, 0.213,
  0.188, 0.188, 0.188, 0.188, 0.188, 0.188, 0.188, 0.188, 0.188, 0.188,
  0.094, 0.087, 0.081, 0.075, 0.070, 0.065, 0.060, 0.056, 0.052, 0.048,
  0.045, 0.041, 0.038, 0.036, 0.033, 0.030, 0.028, 0.026, 0.024, 0.022
)

PFS_standard <- c(
  0.978, 0.968, 0.800, 0.738, 0.643, 0.539, 0.493, 0.459, 0.417, 0.368,
  0.314, 0.269, 0.259, 0.228, 0.227, 0.211, 0.197, 0.184, 0.174, 0.157,
  0.146, 0.136, 0.126, 0.107, 0.097, 0.097, 0.097, 0.093, 0.093, 0.093,
  0.071, 0.057, 0.033, 0.033, 0.033, 0.033, 0.024, 0.022, 0.020, 0.018,
  0.016, 0.014, 0.013, 0.011, 0.010, 0.009, 0.008, 0.007, 0.007, 0.006,
  0.005, 0.005, 0.004, 0.004, 0.003, 0.003, 0.003, 0.003, 0.002, 0.002
)

cycle_length_months <- 1
time_horizon_months <- 60
t <- seq_len(time_horizon_months / cycle_length_months)

stopifnot(
  length(OS_standard) == length(t),
  length(PFS_standard) == length(t),
  all(is.finite(OS_standard)), all(is.finite(PFS_standard)),
  all(OS_standard > 0 & OS_standard <= 1),
  all(PFS_standard > 0 & PFS_standard <= OS_standard),
  all(OS_standard - PFS_standard > 0)
)

# Each row corresponds to a cycle, with t starting at 1.
# Assignments store results without printing them.
transition_probs_standard <- data.frame(
  cycle = t,
  stable_to_death = 1 - exp(log(OS_standard) / t),
  stable_to_progressive = 1 - exp(log(PFS_standard) / t),
  progressive_to_death = 1 - exp(log(OS_standard - PFS_standard) / t)
)

# Calculate remaining-in-state probabilities separately.
# Assumption: no transition from progressive back to stable.
stable_to_stable_standard <- 1 -
  transition_probs_standard$stable_to_death -
  transition_probs_standard$stable_to_progressive

progressive_to_progressive_standard <- 1 -
  transition_probs_standard$progressive_to_death

# Append both calculated vectors to the existing data frame.
transition_probs_standard$stable_to_stable <- stable_to_stable_standard
transition_probs_standard$progressive_to_progressive <-
  progressive_to_progressive_standard

# Standalone long-term TMZ calculations.
# Decimal commas in supplied data are represented as decimal points in R.
# Interpret the requested formula as 1 - exp(log(y) / t).
# These requested transformations are not validated conditional Markov
# transition probabilities. OS includes both living states; OS - PFS
# is progressive-state occupancy, not survival conditional on progression.

OS_long_term <- c(
  1.000, 1.000, 0.982, 0.930, 0.895, 0.860, 0.825, 0.789, 0.737, 0.702,
  0.667, 0.649, 0.614, 0.579, 0.544, 0.526, 0.491, 0.456, 0.421, 0.404,
  0.368, 0.351, 0.351, 0.351, 0.351, 0.333, 0.333, 0.316, 0.316, 0.296,
  0.296, 0.296, 0.296, 0.296, 0.296, 0.269, 0.241, 0.215, 0.215, 0.215,
  0.215, 0.215, 0.215, 0.215, 0.215, 0.215, 0.215, 0.215, 0.215, 0.215,
  0.215, 0.215, 0.215, 0.215, 0.215, 0.215, 0.215, 0.215, 0.215, 0.215
)

PFS_long_term <- c(
  0.947, 0.912, 0.877, 0.754, 0.699, 0.607, 0.497, 0.478, 0.421, 0.402,
  0.364, 0.325, 0.306, 0.306, 0.306, 0.287, 0.268, 0.268, 0.268, 0.268,
  0.268, 0.268, 0.268, 0.268, 0.268, 0.268, 0.268, 0.268, 0.268, 0.244,
  0.244, 0.244, 0.244, 0.244, 0.244, 0.244, 0.203, 0.203, 0.203, 0.203,
  0.203, 0.203, 0.203, 0.203, 0.203, 0.203, 0.203, 0.152, 0.152, 0.152,
  0.152, 0.152, 0.152, 0.152, 0.152, 0.152, 0.152, 0.152, 0.152, 0.152
)

cycle_length_months <- 1
time_horizon_months <- 60
t <- seq_len(time_horizon_months / cycle_length_months)

stopifnot(
  length(OS_long_term) == length(t),
  length(PFS_long_term) == length(t),
  all(is.finite(OS_long_term)), all(is.finite(PFS_long_term)),
  all(OS_long_term > 0 & OS_long_term <= 1),
  all(PFS_long_term > 0 & PFS_long_term <= OS_long_term),
  all(OS_long_term - PFS_long_term > 0)
)

# Store one row per cycle without printing results.
transition_probs_long_term <- data.frame(
  cycle = t,
  stable_to_death = 1 - exp(log(OS_long_term) / t),
  stable_to_progressive = 1 - exp(log(PFS_long_term) / t),
  progressive_to_death = 1 - exp(log(OS_long_term - PFS_long_term) / t)
)

stable_to_stable_long_term <- 1 -
  transition_probs_long_term$stable_to_death -
  transition_probs_long_term$stable_to_progressive

progressive_to_progressive_long_term <- 1 -
  transition_probs_long_term$progressive_to_death

transition_probs_long_term$stable_to_stable <-
  stable_to_stable_long_term

transition_probs_long_term$progressive_to_progressive <-
  progressive_to_progressive_long_term

# Markov traces: exactly 60 rows, labelled cycles 1 through 60.
# Reuse previously calculated transition tables when present in this session.
# Otherwise load the companion scripts without modifying them.
# Keep all three scripts in the same directory. Run with Rscript, or source
# this file with that directory as the working directory (or with chdir = TRUE).
script_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
script_dir <- if (length(script_arg) > 0L) {
  dirname(normalizePath(sub("^--file=", "", script_arg[1L])))
} else {
  getwd()
}

if (!exists("transition_probs_standard")) {
  source(file.path(script_dir, "standard_tmz_transitions.R"), local = TRUE)
}
if (!exists("transition_probs_long_term")) {
  source(file.path(script_dir, "long_term_tmz_transitions.R"), local = TRUE)
}

# Assumption: all patients begin in the stable state.
initial_distribution <- c(stable = 1, progressive = 0, death = 0)
n_cycles <- 60L

# Explicit requested mapping: no transition at cycle 1; probability row 1
# at cycles 2 and 3; probability row (cycle - 2) at cycles 4 through 60.
transition_cycle_used <- c(NA_integer_, pmax(1L, (2L:n_cycles) - 2L))

calculate_markov_trace <- function(probs, initial, mapping) {
  states <- c("stable", "progressive", "death")
  required <- c("stable_to_stable", "stable_to_progressive", "stable_to_death",
                "progressive_to_progressive", "progressive_to_death")
  stopifnot(all(required %in% names(probs)),
            all(states %in% names(initial)),
            all(is.finite(initial)), all(initial >= 0),
            abs(sum(initial) - 1) < 1e-10,
            nrow(probs) >= max(mapping, na.rm = TRUE))
  trace <- matrix(0, nrow = length(mapping), ncol = 3L,
                  dimnames = list(NULL, states))
  trace[1L, ] <- initial[states]
  
  for (cycle in 2L:length(mapping)) {
    p <- probs[mapping[cycle], , drop = FALSE]
    # Rows are origin states; columns are destination states.
    # No return from progressive to stable; death is absorbing.
    transition_matrix <- matrix(c(
      p$stable_to_stable, p$stable_to_progressive, p$stable_to_death,
      0, p$progressive_to_progressive, p$progressive_to_death,
      0, 0, 1
    ), nrow = 3L, byrow = TRUE, dimnames = list(states, states))
    stopifnot(all(is.finite(transition_matrix)),
              all(transition_matrix >= 0 & transition_matrix <= 1),
              all(abs(rowSums(transition_matrix) - 1) < 1e-10))
    trace[cycle, ] <- as.numeric(trace[cycle - 1L, ] %*% transition_matrix)
  }
  
  stopifnot(all(is.finite(trace)), all(trace >= -1e-10),
            all(trace <= 1 + 1e-10),
            all(abs(rowSums(trace) - 1) < 1e-10))
  data.frame(cycle = seq_len(length(mapping)), trace, row.names = NULL)
}

markov_trace_standard <- calculate_markov_trace(
  transition_probs_standard, initial_distribution, transition_cycle_used
)
markov_trace_long_term <- calculate_markov_trace(
  transition_probs_long_term, initial_distribution, transition_cycle_used
)

# Both objects have 60 rows: cycle 1 is the initial distribution, followed
# by 59 transitions. No cycle 0 or cycle 61 is included.
# The supplied probability transformations and requested lag are preserved;
# these traces do not establish reproduction of the supplied OS/PFS curves.

# Calculate QALMs using existing inputs and Markov traces; no data redefinition.
# Keep this script with tmz_model.R, tmz_markov_traces.R, and both transition
# scripts. Run via Rscript, or source with their directory as the working
# directory (or use source(..., chdir = TRUE)).
script_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
script_dir <- if (length(script_arg) > 0L) {
  dirname(normalizePath(sub("^--file=", "", script_arg[1L])))
} else {
  getwd()
}
if (!exists("inputs")) {
  source(file.path(script_dir, "tmz_model.R"), local = TRUE)
}
if (!exists("markov_trace_standard") || !exists("markov_trace_long_term")) {
  source(file.path(script_dir, "tmz_markov_traces.R"), local = TRUE)
}

# Implement the explicitly requested calendar-cycle schedule, not individual
# time since progression. Interpret the decrement as utility points per month,
# consistent with the supplied health-utility input, rather than annual QALYs.
# Death contributes zero QALMs.
calculate_qalms <- function(trace, model_inputs) {
  stopifnot(nrow(trace) == 60L, identical(as.integer(trace$cycle), 1:60),
            model_inputs$design$cycle_length_months == 1,
            all(c("stable", "progressive", "death") %in% names(trace)))
  decrements <- pmin(pmax(trace$cycle - 2L, 0L), 23L)
  progressive_utility <- model_inputs$utilities$progressive -
    model_inputs$utilities$decrement_per_consecutive_month_progression * decrements
  
  stable_qalms <- trace$stable * model_inputs$utilities$stable *
    model_inputs$design$cycle_length_months
  progressive_qalms <- trace$progressive * progressive_utility *
    model_inputs$design$cycle_length_months
  death_qalms <- numeric(nrow(trace))
  qalms <- stable_qalms + progressive_qalms + death_qalms
  
  data.frame(
    cycle = trace$cycle,
    stable = trace$stable,
    progressive = trace$progressive,
    death = trace$death,
    progressive_utility = progressive_utility,
    stable_qalms = stable_qalms,
    progressive_qalms = progressive_qalms,
    death_qalms = death_qalms,
    qalms = qalms,
    cumulative_qalms = cumsum(qalms)
  )
}

qalms_standard <- calculate_qalms(markov_trace_standard, inputs)
qalms_long_term <- calculate_qalms(markov_trace_long_term, inputs)
total_qalms_standard <- sum(qalms_standard$qalms)
total_qalms_long_term <- sum(qalms_long_term$qalms)

total_qalms <- data.frame(
  treatment = c("Standard TMZ", "Long-term TMZ"),
  total_qalms = c(total_qalms_standard, total_qalms_long_term)
)

# Sum all 60 cycles, including the initial distribution at cycle 1.
# No discounting or half-cycle correction has been requested or applied.
cat("Standard TMZ: QALMs by cycle\n")
print(qalms_standard, row.names = FALSE, max = nrow(qalms_standard) * ncol(qalms_standard))
cat("\nLong-term TMZ: QALMs by cycle\n")
print(qalms_long_term, row.names = FALSE, max = nrow(qalms_long_term) * ncol(qalms_long_term))
cat("\nTotal QALMs\n")
print(total_qalms, row.names = FALSE)

total_qalys_standard <- total_qalms_standard / 12
total_qalys_long_term <- total_qalms_long_term / 12

# Calculate costs from existing inputs and Markov traces without redefining data.
# Keep this script beside tmz_model.R, tmz_markov_traces.R, and both transition
# scripts. Run via Rscript, or source with their directory as the working
# directory (or use source(..., chdir = TRUE)).
script_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
script_dir <- if (length(script_arg) > 0L) {
  dirname(normalizePath(sub("^--file=", "", script_arg[1L])))
} else {
  getwd()
}
if (!exists("inputs")) {
  source(file.path(script_dir, "tmz_model.R"), local = TRUE)
}
if (!exists("markov_trace_standard") || !exists("markov_trace_long_term")) {
  source(file.path(script_dir, "tmz_markov_traces.R"), local = TRUE)
}

calculate_cycle_costs <- function(trace, unit_costs) {
  stopifnot(nrow(trace) == 60L, identical(as.integer(trace$cycle), 1:60),
            all(c("stable", "progressive", "death") %in% names(trace)))
  # Cycle 1 is the initial distribution but is charged explicitly as requested.
  # Cycles 2-4 correspond to months 1-3, cycles 5-7 to months 4-6,
  # and cycles 8-60 to subsequent months.
  stable_unit_cost <- ifelse(
    trace$cycle <= 4L, unit_costs$stable_months_1_3,
    ifelse(trace$cycle <= 7L, unit_costs$stable_months_4_6,
           unit_costs$stable_subsequent_months)
  )
  stable_cost <- trace$stable * stable_unit_cost
  progressive_cost <- trace$progressive * unit_costs$progressive
  death_cost <- trace$death * unit_costs$death
  cycle_cost <- stable_cost + progressive_cost + death_cost
  
  data.frame(
    cycle = trace$cycle,
    month = trace$cycle - 1L,
    stable = trace$stable,
    progressive = trace$progressive,
    death = trace$death,
    stable_unit_cost = stable_unit_cost,
    progressive_unit_cost = unit_costs$progressive,
    death_unit_cost = unit_costs$death,
    stable_cost = stable_cost,
    progressive_cost = progressive_cost,
    death_cost = death_cost,
    cycle_cost = cycle_cost,
    cumulative_cost = cumsum(cycle_cost)
  )
}

costs_standard <- calculate_cycle_costs(
  markov_trace_standard, inputs$costs$standard_tmz
)
costs_long_term <- calculate_cycle_costs(
  markov_trace_long_term, inputs$costs$long_term_tmz
)
total_cost_standard <- sum(costs_standard$cycle_cost)
total_cost_long_term <- sum(costs_long_term$cycle_cost)

total_costs <- data.frame(
  treatment = c("Standard TMZ", "Long-term TMZ"),
  total_cost = c(total_cost_standard, total_cost_long_term)
)

# Costs are per patient in the initial cohort, in the supplied currency units.
# No discounting or half-cycle correction has been requested or applied.
# Rounding is for display only; stored values and totals retain full precision.
cat("Standard TMZ: costs by cycle\n")
print(costs_standard, row.names = FALSE,
      max = nrow(costs_standard) * ncol(costs_standard))
cat("\nLong-term TMZ: costs by cycle\n")
print(costs_long_term, row.names = FALSE,
      max = nrow(costs_long_term) * ncol(costs_long_term))
cat("\nTotal costs\n")
print(transform(total_costs, total_cost = round(total_cost, 2)), row.names = FALSE)

# Incremental cost-effectiveness: long-term TMZ versus standard TMZ.
# Reuse existing full-precision results; otherwise run the companion scripts.
# Keep all previously generated scripts in this directory. Run via Rscript,
# or source with this directory as the working directory (or chdir = TRUE).
script_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
script_dir <- if (length(script_arg) > 0L) {
  dirname(normalizePath(sub("^--file=", "", script_arg[1L])))
} else {
  getwd()
}

# Suppress the companion scripts' cycle-level output when loading results.
if (!exists("total_qalms_standard") || !exists("total_qalms_long_term")) {
  invisible(capture.output(source(file.path(script_dir, "tmz_qalms.R"),
                                  local = TRUE)))
}
if (!exists("total_cost_standard") || !exists("total_cost_long_term")) {
  invisible(capture.output(source(file.path(script_dir, "tmz_costs.R"),
                                  local = TRUE)))
}

stopifnot(all(is.finite(c(total_cost_standard, total_cost_long_term,
                          total_qalms_standard, total_qalms_long_term))))

total_qalys_standard <- total_qalms_standard / 12
total_qalys_long_term <- total_qalms_long_term / 12
incremental_cost <- total_cost_long_term - total_cost_standard
incremental_qalms <- total_qalms_long_term - total_qalms_standard
incremental_qalys <- total_qalys_long_term - total_qalys_standard

if (incremental_qalms == 0 || incremental_qalys == 0) {
  stop("ICER is undefined because incremental effectiveness is zero.")
}

icer_per_qalm <- incremental_cost / incremental_qalms
icer_per_qaly <- incremental_cost / incremental_qalys

# Preserve full precision for subsequent calculations.
icer_results <- data.frame(
  outcome = c("QALM", "QALY"),
  incremental_cost = rep(incremental_cost, 2),
  incremental_effectiveness = c(incremental_qalms, incremental_qalys),
  icer = c(icer_per_qalm, icer_per_qaly)
)

# Display ICERs rounded to the nearest whole currency unit, without decimals.
cat("Long-term TMZ versus Standard TMZ\n")
cat(sprintf("ICER per QALM: %.0f\n", icer_per_qalm))
cat(sprintf("ICER per QALY: %.0f\n", icer_per_qaly))