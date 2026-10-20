# Step 1: Store model inputs without printing
inputs <- list(
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
    states = c("stable", "progressive", "death"),
    cycle_length_months = 1,
    time_horizon_months = 60
  )
)

inputs$design$n_cycles <- with(
  inputs$design,
  time_horizon_months / cycle_length_months
)

# Standalone calculation for Standard TMZ therapy, cycles 1-60.
# Formula interpreted as 1 - exp(log(y) / t), equivalent to 1 - y^(1/t).
# All assignments are silent; no packages or existing scripts are required.

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

stopifnot(
  length(OS_standard) == 60L,
  length(PFS_standard) == 60L,
  all(is.finite(OS_standard)), all(is.finite(PFS_standard)),
  all(OS_standard > 0 & OS_standard <= 1),
  all(PFS_standard > 0 & PFS_standard <= OS_standard),
  all(OS_standard - PFS_standard > 0)
)

t <- seq_len(60L)

transition_probs_standard <- data.frame(
  cycle = t,
  stable_to_death = 1 - exp(log(OS_standard) / t),
  stable_to_progressive = 1 - exp(log(PFS_standard) / t),
  progressive_to_death = 1 - exp(log(OS_standard - PFS_standard) / t)
)

# Modelling limitation: these are the requested formula outputs, not validated
# conditional monthly Markov transition estimates. OS - PFS is the proportion
# alive with progression, not survival conditional on being progressive.
# Additional assumptions are needed to identify state-specific transitions.

# Calculate the remaining probabilities separately
stable_to_stable <- 1 -
  transition_probs_standard$stable_to_death -
  transition_probs_standard$stable_to_progressive

# Assumes no transition from progressive back to stable
progressive_to_progressive <- 1 -
  transition_probs_standard$progressive_to_death

# Append to the existing data frame without printing
transition_probs_standard$stable_to_stable <- stable_to_stable
transition_probs_standard$progressive_to_progressive <- progressive_to_progressive

# Standalone calculation for long-term TMZ therapy, cycles 1-60.
# Formula interpreted as 1 - exp(log(y) / t), equivalent to 1 - y^(1/t).
# All assignments are silent; no packages or existing scripts are required.

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

stopifnot(
  length(OS_long_term) == 60L,
  length(PFS_long_term) == 60L,
  all(is.finite(OS_long_term)), all(is.finite(PFS_long_term)),
  all(OS_long_term > 0 & OS_long_term <= 1),
  all(PFS_long_term > 0 & PFS_long_term <= OS_long_term),
  all(OS_long_term - PFS_long_term > 0)
)

t <- seq_len(60L)

transition_probs_long_term <- data.frame(
  cycle = t,
  stable_to_death = 1 - exp(log(OS_long_term) / t),
  stable_to_progressive = 1 - exp(log(PFS_long_term) / t),
  progressive_to_death = 1 - exp(log(OS_long_term - PFS_long_term) / t)
)

# Modelling limitation: these are the requested formula outputs, not validated
# conditional monthly Markov transition estimates. OS - PFS is the proportion
# alive with progression, not survival conditional on being progressive.
# Additional assumptions are needed to identify state-specific transitions.

# Calculate the remaining probabilities separately
stable_to_stable <- 1 -
  transition_probs_long_term$stable_to_death -
  transition_probs_long_term$stable_to_progressive

# Assumes no transition from progressive back to stable
progressive_to_progressive <- 1 -
  transition_probs_long_term$progressive_to_death

# Append to the existing data frame without printing
transition_probs_long_term$stable_to_stable <- stable_to_stable
transition_probs_long_term$progressive_to_progressive <- progressive_to_progressive

# Reuse transition_probs_standard and transition_probs_long_term in the
# current R session, including the previously calculated self-transitions.
# Cycle 1 is the initial distribution; there is no cycle 0 or cycle 61.
# Initial-state assumption: all patients start stable.
# The traces inherit the modelling limitations of the supplied probabilities.

initial_distribution <- c(stable = 1, progressive = 0, death = 0)
n_cycles <- 60L

calculate_markov_trace <- function(transition_probs, initial, n_cycles = 60L) {
  states <- c("stable", "progressive", "death")
  required <- c("stable_to_stable", "stable_to_progressive", "stable_to_death",
                "progressive_to_progressive", "progressive_to_death")
  if (!all(required %in% names(transition_probs))) {
    stop("Transition table must include all five previously calculated probabilities.")
  }
  stopifnot(n_cycles == 60L, all(states %in% names(initial)))
  initial <- initial[states]
  stopifnot(all(is.finite(initial)), all(initial >= 0),
            abs(sum(initial) - 1) < 1e-10)
  
  # Destination cycles 2, 3, 4, ..., 60 use probability cycles 1, 1, 2, ..., 58.
  probability_cycle <- pmax(1L, seq.int(2L, n_cycles) - 2L)
  if (!"cycle" %in% names(transition_probs) ||
      anyDuplicated(transition_probs$cycle)) {
    stop("Transition table must have unique cycle identifiers.")
  }
  probability_rows <- match(probability_cycle, transition_probs$cycle)
  if (anyNA(probability_rows)) stop("Missing required transition probability cycles.")
  
  trace <- matrix(0, nrow = n_cycles, ncol = length(states),
                  dimnames = list(NULL, states))
  trace[1L, ] <- initial
  
  for (cycle in seq.int(2L, n_cycles)) {
    p <- transition_probs[probability_rows[cycle - 1L], , drop = FALSE]
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
  
  stopifnot(nrow(trace) == 60L, all(trace >= 0),
            all(abs(rowSums(trace) - 1) < 1e-10))
  data.frame(cycle = seq_len(n_cycles), trace, row.names = NULL)
}

if (!exists("transition_probs_standard") || !exists("transition_probs_long_term")) {
  stop("Load both previously calculated transition tables into this R session first.")
}

# Store both traces silently.
markov_trace_standard <- calculate_markov_trace(
  transition_probs_standard, initial_distribution, n_cycles
)
markov_trace_long_term <- calculate_markov_trace(
  transition_probs_long_term, initial_distribution, n_cycles
)

# Requires the existing inputs, markov_trace_standard and
# markov_trace_long_term objects. No input data are redefined.
# Implements the explicitly requested calendar-cycle utility schedule.
# This does not track each patient's time since progression: all progressive
# patients in a given cycle receive the same utility, including new entrants.
# The supplied 0.02 is interpreted as a utility decrement, not 0.02 QALYs.
# No discounting or half-cycle correction is applied.

if (!exists("inputs") || !exists("markov_trace_standard") ||
    !exists("markov_trace_long_term")) {
  stop("Load the existing inputs and both 60-cycle Markov traces first.")
}

calculate_qalms <- function(trace, inputs) {
  stopifnot(
    all(c("cycle", "stable", "progressive", "death") %in% names(trace)),
    nrow(trace) == 60L,
    all(trace$cycle == seq_len(60L)),
    inputs$design$cycle_length_months == 1
  )
  proportions <- as.matrix(trace[, c("stable", "progressive", "death")])
  stopifnot(all(is.finite(proportions)), all(proportions >= 0),
            all(abs(rowSums(proportions) - 1) < 1e-10))
  
  # Cycles 1-2: zero decrements; cycles 3-25: 1-23 decrements;
  # cycles 26-60: retain all 23 decrements.
  decrement_count <- pmin(pmax(trace$cycle - 2L, 0L), 23L)
  progressive_utility <- inputs$utilities$progressive -
    inputs$utilities$decrement_per_consecutive_month_progression * decrement_count
  
  stable_qalms <- trace$stable * inputs$utilities$stable *
    inputs$design$cycle_length_months
  progressive_qalms <- trace$progressive * progressive_utility *
    inputs$design$cycle_length_months
  death_qalms <- trace$death * 0 # Death has zero utility.
  cycle_qalms <- stable_qalms + progressive_qalms + death_qalms
  
  data.frame(
    cycle = trace$cycle,
    stable = trace$stable,
    progressive = trace$progressive,
    death = trace$death,
    progressive_utility = progressive_utility,
    stable_qalms = stable_qalms,
    progressive_qalms = progressive_qalms,
    death_qalms = death_qalms,
    cycle_qalms = cycle_qalms,
    cumulative_qalms = cumsum(cycle_qalms)
  )
}

qalms_standard <- calculate_qalms(markov_trace_standard, inputs)
qalms_long_term <- calculate_qalms(markov_trace_long_term, inputs)

total_qalms_standard <- sum(qalms_standard$cycle_qalms)
total_qalms_long_term <- sum(qalms_long_term$cycle_qalms)

qalms_summary <- data.frame(
  treatment = c("Standard TMZ", "Long-term TMZ"),
  total_qalms = c(total_qalms_standard, total_qalms_long_term)
)

# Display all 60 cycles for each treatment, then the totals.
print(qalms_standard, row.names = FALSE, max = nrow(qalms_standard) * ncol(qalms_standard))
print(qalms_long_term, row.names = FALSE, max = nrow(qalms_long_term) * ncol(qalms_long_term))
print(qalms_summary, row.names = FALSE)

total_qalys_standard <- total_qalms_standard / 12
total_qalys_long_term <- total_qalms_long_term / 12

data.frame(
  treatment = c("Standard TMZ", "Long-term TMZ"),
  total_qalys = c(total_qalys_standard, total_qalys_long_term)
)

# Requires existing inputs, markov_trace_standard and markov_trace_long_term.
# Reuses all supplied data without redefining them.
# Cycle 1 is the initial distribution but incurs costs as explicitly requested.
# Costs are undiscounted, with no half-cycle correction.

if (!exists("inputs") || !exists("markov_trace_standard") ||
    !exists("markov_trace_long_term")) {
  stop("Load the existing inputs and both 60-cycle Markov traces first.")
}

calculate_cycle_costs <- function(trace, costs) {
  stopifnot(
    all(c("cycle", "stable", "progressive", "death") %in% names(trace)),
    nrow(trace) == 60L,
    all(trace$cycle == seq_len(60L))
  )
  required_costs <- c("stable_months_1_3", "stable_months_4_6",
                      "stable_subsequent_months", "progressive", "death")
  stopifnot(all(required_costs %in% names(costs)))
  cost_values <- unlist(costs[required_costs], use.names = FALSE)
  stopifnot(length(cost_values) == 5L, all(is.finite(cost_values)),
            all(cost_values >= 0))
  proportions <- as.matrix(trace[, c("stable", "progressive", "death")])
  stopifnot(all(is.finite(proportions)), all(proportions >= 0),
            all(abs(rowSums(proportions) - 1) < 1e-10))
  
  stable_unit_cost <- ifelse(
    trace$cycle <= 4L, costs$stable_months_1_3,
    ifelse(trace$cycle <= 7L, costs$stable_months_4_6,
           costs$stable_subsequent_months)
  )
  
  stable_cost <- trace$stable * stable_unit_cost
  progressive_cost <- trace$progressive * costs$progressive
  death_cost <- trace$death * costs$death
  cycle_cost <- stable_cost + progressive_cost + death_cost
  
  data.frame(
    cycle = trace$cycle,
    month = trace$cycle - 1L, # Month 0 denotes the initial distribution.
    stable = trace$stable,
    progressive = trace$progressive,
    death = trace$death,
    stable_unit_cost = stable_unit_cost,
    progressive_unit_cost = costs$progressive,
    death_unit_cost = costs$death,
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

total_costs_standard <- sum(costs_standard$cycle_cost)
total_costs_long_term <- sum(costs_long_term$cycle_cost)

costs_summary <- data.frame(
  treatment = c("Standard TMZ", "Long-term TMZ"),
  total_cost = c(total_costs_standard, total_costs_long_term)
)

# Display all 60 cycles for each treatment and the totals without rounding
# the stored results. Currency is not specified in the supplied inputs.
print(costs_standard, row.names = FALSE, max = nrow(costs_standard) * ncol(costs_standard))
print(costs_long_term, row.names = FALSE, max = nrow(costs_long_term) * ncol(costs_long_term))
print(costs_summary, row.names = FALSE)

# Incremental comparison: long-term TMZ minus standard TMZ.
# Reuse full-precision cost and QALM totals already calculated in this session.
required_objects <- c("total_costs_standard", "total_costs_long_term",
                      "total_qalms_standard", "total_qalms_long_term")
if (!all(vapply(required_objects, exists, logical(1), inherits = TRUE))) {
  stop("Calculate both treatments' total costs and total QALMs first.")
}

incremental_cost <- total_costs_long_term - total_costs_standard
incremental_qalms <- total_qalms_long_term - total_qalms_standard
incremental_qalys <- incremental_qalms / 12

stopifnot(length(incremental_cost) == 1L,
          length(incremental_qalms) == 1L,
          is.finite(incremental_cost), is.finite(incremental_qalms))
if (incremental_qalms == 0) {
  stop("ICER is undefined because the incremental health benefit is zero.")
}

icer_per_qalm <- incremental_cost / incremental_qalms
icer_per_qaly <- incremental_cost / incremental_qalys

# Retain numeric ICERs at full precision; round only their displayed values.
icer_results <- data.frame(
  outcome = c("Cost per QALM gained", "Cost per QALY gained"),
  ICER = c(icer_per_qalm, icer_per_qaly)
)
icer_display <- icer_results
icer_display$ICER <- formatC(icer_results$ICER, format = "f", digits = 0,
                             big.mark = ",")
print(icer_display, row.names = FALSE)