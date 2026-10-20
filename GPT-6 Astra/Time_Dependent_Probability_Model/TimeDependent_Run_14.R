# Step 1: Store supplied inputs silently.
# Costs retain the supplied units; currency and price year are unspecified.
# Duration-dependent progression utilities will be implemented in a later step.

inputs <- list(
  states = c("stable", "progressive", "death"),
  utility = list(
    stable = 0.743,
    progressive = 0.731,
    decrement_per_consecutive_month_progressive = 0.02
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

# Standard TMZ: standalone calculation from supplied monthly survival data.
# Decimal commas in the source data are represented as decimal points in R.

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

t <- seq_len(60L)

stopifnot(
  length(OS_standard) == length(t),
  length(PFS_standard) == length(t),
  all(is.finite(OS_standard)), all(is.finite(PFS_standard)),
  all(OS_standard > 0 & OS_standard <= 1),
  all(PFS_standard > 0 & PFS_standard <= 1),
  all(OS_standard > PFS_standard)
)

# Interpret the requested formula as 1 - exp(log(y) / t).
# log() is the natural logarithm in R; calculations are vectorised by cycle.
transition_probs_standard <- data.frame(
  cycle = t,
  stable_to_death = 1 - exp(log(OS_standard) / t),
  stable_to_progressive = 1 - exp(log(PFS_standard) / t),
  progressive_to_death = 1 - exp(log(OS_standard - PFS_standard) / t)
)

# Modelling qualification:
# These columns implement the requested formula and labels exactly.
# OS and PFS are cumulative survival probabilities, not state-specific
# conditional survival probabilities. OS - PFS is progressive-state occupancy,
# not survival conditional on progression. These calculations alone therefore
# do not establish valid cycle-specific Markov transition probabilities.
# No output is printed and no transition matrix is constructed here.

# Calculate stay probabilities separately (no progressive-to-stable transition).
stable_to_stable_standard <- 1 -
  transition_probs_standard$stable_to_death -
  transition_probs_standard$stable_to_progressive

progressive_to_progressive_standard <- 1 -
  transition_probs_standard$progressive_to_death

# Append the two calculated vectors to the existing data frame.
transition_probs_standard$stable_to_stable <- stable_to_stable_standard
transition_probs_standard$progressive_to_progressive <-
  progressive_to_progressive_standard

# Long-term TMZ: standalone calculation from supplied monthly survival data.
# Decimal commas in the source data are represented as decimal points in R.

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

t <- seq_len(60L)

stopifnot(
  length(OS_long_term) == length(t),
  length(PFS_long_term) == length(t),
  all(is.finite(OS_long_term)), all(is.finite(PFS_long_term)),
  all(OS_long_term > 0 & OS_long_term <= 1),
  all(PFS_long_term > 0 & PFS_long_term <= 1),
  all(OS_long_term > PFS_long_term)
)

# Interpret the requested formula as 1 - exp(log(y) / t).
# log() is the natural logarithm in R; calculations are vectorised by cycle.
transition_probs_long_term <- data.frame(
  cycle = t,
  stable_to_death = 1 - exp(log(OS_long_term) / t),
  stable_to_progressive = 1 - exp(log(PFS_long_term) / t),
  progressive_to_death = 1 - exp(log(OS_long_term - PFS_long_term) / t)
)

# Modelling qualification:
# These columns implement the requested formula and labels exactly.
# OS and PFS are cumulative survival probabilities, not state-specific
# conditional survival probabilities. OS - PFS is progressive-state occupancy,
# not survival conditional on progression. These calculations alone therefore
# do not establish valid cycle-specific Markov transition probabilities.
# No output is printed and no transition matrix is constructed here.

# Calculate stay probabilities separately (no progressive-to-stable transition).
stable_to_stable_long_term <- 1 -
  transition_probs_long_term$stable_to_death -
  transition_probs_long_term$stable_to_progressive

progressive_to_progressive_long_term <- 1 -
  transition_probs_long_term$progressive_to_death

# Append the two calculated vectors to the existing data frame.
transition_probs_long_term$stable_to_stable <- stable_to_stable_long_term
transition_probs_long_term$progressive_to_progressive <-
  progressive_to_progressive_long_term

# Markov traces for Standard and Long-term TMZ: exactly cycles 1 through 60.
# Run after the two transition scripts in the same R session, for example:
# source("standard_tmz_transition_probs.R")
# source("long_term_tmz_transition_probs.R")
# source("tmz_markov_traces.R")
# Existing transition_probs_standard and transition_probs_long_term are reused.

# Assumption: the entire cohort starts stable.
initial_distribution <- c(stable = 1, progressive = 0, death = 0)

build_markov_trace <- function(transition_probs, initial_distribution) {
  states <- c("stable", "progressive", "death")
  required_columns <- c(
    "stable_to_stable", "stable_to_progressive", "stable_to_death",
    "progressive_to_progressive", "progressive_to_death"
  )
  stopifnot(
    all(required_columns %in% names(transition_probs)),
    all(states %in% names(initial_distribution)),
    length(initial_distribution) == 3L,
    all(is.finite(initial_distribution)),
    all(initial_distribution >= 0),
    abs(sum(initial_distribution) - 1) < 1e-10
  )
  
  # Cycle 1 has no transition. Cycles 2 and 3 both use probability cycle 1.
  # Cycle 60 uses probability cycle 58; cycles 59 and 60 are not applied.
  transition_cycle_used <- c(NA_integer_, pmax(1L, (2:60) - 2L))
  needed_cycles <- 1:58
  stopifnot(
    "cycle" %in% names(transition_probs),
    !anyDuplicated(transition_probs$cycle),
    all(needed_cycles %in% transition_probs$cycle)
  )
  
  occupancy <- matrix(
    0, nrow = 60L, ncol = 3L,
    dimnames = list(NULL, states)
  )
  occupancy[1L, ] <- initial_distribution[states]
  
  for (cycle in 2:60) {
    probability_cycle <- transition_cycle_used[cycle]
    p <- transition_probs[
      match(probability_cycle, transition_probs$cycle), , drop = FALSE
    ]
    # Rows: origin state. Columns: destination state.
    # Progression is irreversible and death is absorbing.
    transition_matrix <- matrix(
      c(
        p$stable_to_stable, p$stable_to_progressive, p$stable_to_death,
        0, p$progressive_to_progressive, p$progressive_to_death,
        0, 0, 1
      ),
      nrow = 3L, byrow = TRUE,
      dimnames = list(states, states)
    )
    stopifnot(
      all(is.finite(transition_matrix)),
      all(transition_matrix >= 0 & transition_matrix <= 1),
      all(abs(rowSums(transition_matrix) - 1) < 1e-10)
    )
    occupancy[cycle, ] <- as.numeric(
      occupancy[cycle - 1L, ] %*% transition_matrix
    )
  }
  
  stopifnot(
    all(is.finite(occupancy)),
    all(occupancy >= -1e-10 & occupancy <= 1 + 1e-10),
    all(abs(rowSums(occupancy) - 1) < 1e-10)
  )
  data.frame(
    cycle = seq_len(60L),
    stable = occupancy[, "stable"],
    progressive = occupancy[, "progressive"],
    death = occupancy[, "death"]
  )
}

if (!exists("transition_probs_standard") ||
    !exists("transition_probs_long_term")) {
  stop("First run both transition probability scripts in this R session.")
}

markov_trace_standard <- build_markov_trace(
  transition_probs_standard, initial_distribution
)
markov_trace_long_term <- build_markov_trace(
  transition_probs_long_term, initial_distribution
)

# Traces are stored silently. No cycle 0 or cycle 61 is included.
# These traces inherit the modelling limitations of the supplied transitions.

# Calculate QALMs using existing inputs and Markov traces.
# Run after tmz_model.R and tmz_markov_traces.R (and its prerequisites).
# No input data or Markov traces are redefined.

if (!exists("inputs") || !exists("markov_trace_standard") ||
    !exists("markov_trace_long_term")) {
  stop("Load inputs and both Markov traces before running this script.")
}

calculate_qalms <- function(trace, inputs) {
  stopifnot(
    nrow(trace) == 60L,
    identical(as.integer(trace$cycle), seq_len(60L)),
    all(c("stable", "progressive", "death") %in% names(trace)),
    inputs$design$cycle_length_months == 1,
    inputs$design$time_horizon_months == 60
  )
  proportions <- as.matrix(trace[, c("stable", "progressive", "death")])
  stopifnot(
    all(is.finite(proportions)),
    all(proportions >= -1e-10 & proportions <= 1 + 1e-10),
    all(abs(rowSums(proportions) - 1) < 1e-10)
  )
  
  # Calendar-cycle schedule requested by the user:
  # cycles 1-2: zero decrements; cycles 3-25: 1 through 23 decrements;
  # cycles 26-60: retain 23 decrements (no further decrease or reset).
  decrement_count <- pmin(pmax(trace$cycle - 2L, 0L), 23L)
  progressive_utility <- inputs$utility$progressive -
    inputs$utility$decrement_per_consecutive_month_progressive * decrement_count
  
  # The supplied 0.02 is interpreted as a utility decrement, not an annual
  # QALY loss to be converted into monthly units. Utility x months = QALMs.
  # This schedule applies to everyone progressive in a given model cycle;
  # individual time since progression is not available in aggregate traces.
  stable_qalms <- trace$stable * inputs$utility$stable *
    inputs$design$cycle_length_months
  progressive_qalms <- trace$progressive * progressive_utility *
    inputs$design$cycle_length_months
  death_qalms <- trace$death * 0  # Assumption: utility of death is zero.
  
  data.frame(
    cycle = trace$cycle,
    stable = trace$stable,
    progressive = trace$progressive,
    death = trace$death,
    progressive_decrement_count = decrement_count,
    progressive_utility = progressive_utility,
    stable_qalms = stable_qalms,
    progressive_qalms = progressive_qalms,
    death_qalms = death_qalms,
    total_qalms = stable_qalms + progressive_qalms + death_qalms
  )
}

qalms_standard <- calculate_qalms(markov_trace_standard, inputs)
qalms_long_term <- calculate_qalms(markov_trace_long_term, inputs)

total_qalms_standard <- sum(qalms_standard$total_qalms)
total_qalms_long_term <- sum(qalms_long_term$total_qalms)

# Side-by-side per-cycle results, including all 60 cycles.
qalms_by_cycle <- data.frame(
  cycle = qalms_standard$cycle,
  progressive_utility = qalms_standard$progressive_utility,
  standard_stable_qalms = qalms_standard$stable_qalms,
  standard_progressive_qalms = qalms_standard$progressive_qalms,
  standard_death_qalms = qalms_standard$death_qalms,
  standard_total_qalms = qalms_standard$total_qalms,
  long_term_stable_qalms = qalms_long_term$stable_qalms,
  long_term_progressive_qalms = qalms_long_term$progressive_qalms,
  long_term_death_qalms = qalms_long_term$death_qalms,
  long_term_total_qalms = qalms_long_term$total_qalms
)

total_qalms <- data.frame(
  treatment = c("Standard TMZ", "Long-term TMZ"),
  total_qalms = c(total_qalms_standard, total_qalms_long_term)
)

# Undiscounted, per-patient QALMs; no half-cycle correction is applied.
# Cycle 1 contributes one full month, as requested for the 60-row trace.
print(qalms_by_cycle, row.names = FALSE, max = nrow(qalms_by_cycle) * ncol(qalms_by_cycle))
print(total_qalms, row.names = FALSE)

total_qalys_standard <- total_qalms_standard / 12
total_qalys_long_term <- total_qalms_long_term / 12

# Per-cycle and total costs using existing inputs and Markov traces.
# Run after tmz_model.R and tmz_markov_traces.R (and its prerequisites).
# No input data or traces are redefined.

if (!exists("inputs") || !exists("markov_trace_standard") ||
    !exists("markov_trace_long_term")) {
  stop("Load inputs and both Markov traces before running this script.")
}

calculate_costs <- function(trace, unit_costs) {
  stopifnot(
    nrow(trace) == 60L,
    identical(as.integer(trace$cycle), seq_len(60L)),
    all(c("stable", "progressive", "death") %in% names(trace))
  )
  proportions <- as.matrix(trace[, c("stable", "progressive", "death")])
  stopifnot(
    all(is.finite(proportions)),
    all(proportions >= -1e-10 & proportions <= 1 + 1e-10),
    all(abs(rowSums(proportions) - 1) < 1e-10)
  )
  
  # Cycle 1 is the initial distribution but is charged explicitly.
  # Cycle 2 = month 1; cycle 60 = month 59.
  # Stable cost bands: cycles 1-4 (4), 5-7 (3), and 8-60 (53).
  stable_unit_cost <- ifelse(
    trace$cycle <= 4L,
    unit_costs$stable_months_1_3,
    ifelse(trace$cycle <= 7L,
           unit_costs$stable_months_4_6,
           unit_costs$stable_subsequent_months)
  )
  
  stable_cost <- trace$stable * stable_unit_cost
  progressive_cost <- trace$progressive * unit_costs$progressive
  death_cost <- trace$death * unit_costs$death
  
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
    total_cost = stable_cost + progressive_cost + death_cost
  )
}

costs_standard <- calculate_costs(
  markov_trace_standard, inputs$costs$standard_tmz
)
costs_long_term <- calculate_costs(
  markov_trace_long_term, inputs$costs$long_term_tmz
)

total_cost_standard <- sum(costs_standard$total_cost)
total_cost_long_term <- sum(costs_long_term$total_cost)

costs_by_cycle <- data.frame(
  cycle = costs_standard$cycle,
  month = costs_standard$month,
  standard_stable_unit_cost = costs_standard$stable_unit_cost,
  standard_stable_cost = costs_standard$stable_cost,
  standard_progressive_cost = costs_standard$progressive_cost,
  standard_death_cost = costs_standard$death_cost,
  standard_total_cost = costs_standard$total_cost,
  long_term_stable_unit_cost = costs_long_term$stable_unit_cost,
  long_term_stable_cost = costs_long_term$stable_cost,
  long_term_progressive_cost = costs_long_term$progressive_cost,
  long_term_death_cost = costs_long_term$death_cost,
  long_term_total_cost = costs_long_term$total_cost
)

total_costs <- data.frame(
  treatment = c("Standard TMZ", "Long-term TMZ"),
  total_cost = c(total_cost_standard, total_cost_long_term)
)

# Undiscounted costs per patient in the supplied, unspecified currency.
# No half-cycle correction; all 60 rows are charged as requested.
# Calculations retain full precision; rounding is only for printed output.
print(round(costs_by_cycle, 2), row.names = FALSE,
      max = nrow(costs_by_cycle) * ncol(costs_by_cycle))
print(transform(total_costs, total_cost = round(total_cost, 2)), row.names = FALSE)

# ICERs: Long-term TMZ compared with Standard TMZ.
# Run after tmz_qalms.R and tmz_costs.R in the same R session.
# Reuse full-precision totals; do not redefine input data.

required_totals <- c(
  "total_cost_standard", "total_cost_long_term",
  "total_qalms_standard", "total_qalms_long_term"
)
if (!all(vapply(required_totals, exists, logical(1), inherits = TRUE))) {
  stop("Run the QALM and cost scripts before calculating ICERs.")
}
stopifnot(all(vapply(
  mget(required_totals, inherits = TRUE),
  function(x) is.numeric(x) && length(x) == 1L && is.finite(x),
  logical(1)
)))

# Convert QALMs to QALYs using 12 months per year.
total_qalys_standard <- total_qalms_standard / 12
total_qalys_long_term <- total_qalms_long_term / 12

incremental_cost <- total_cost_long_term - total_cost_standard
incremental_qalms <- total_qalms_long_term - total_qalms_standard
incremental_qalys <- total_qalys_long_term - total_qalys_standard

if (incremental_qalms == 0 || incremental_qalys == 0) {
  stop("ICER is undefined because incremental health benefit is zero.")
}

icer_per_qalm <- incremental_cost / incremental_qalms
icer_per_qaly <- incremental_cost / incremental_qalys

# Numeric results retain full precision.
icer_results <- data.frame(
  comparison = "Long-term TMZ versus Standard TMZ",
  outcome = c("QALM", "QALY"),
  incremental_cost = incremental_cost,
  incremental_effect = c(incremental_qalms, incremental_qalys),
  icer = c(icer_per_qalm, icer_per_qaly)
)

# Display only: round to whole currency units, with no decimal places.
icer_display <- data.frame(
  comparison = icer_results$comparison,
  measure = c("Cost per QALM gained", "Cost per QALY gained"),
  ICER = formatC(icer_results$icer, format = "f", digits = 0, big.mark = ",")
)
print(icer_display, row.names = FALSE)