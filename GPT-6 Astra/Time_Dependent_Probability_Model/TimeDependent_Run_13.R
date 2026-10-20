# Health utility inputs
utilities <- list(
  stable = 0.743,
  progressive = 0.731,
  decrement_per_consecutive_month_progression = 0.02
)

# Cost inputs
costs <- list(
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
)

# Model design
cycle_length_months <- 1L
time_horizon_months <- 60L
n_cycles <- time_horizon_months / cycle_length_months

health_states <- c("stable", "progressive", "death")

# Standalone script: inputs and requested Standard TMZ calculations.
# No inputs or results are printed. No existing script is sourced or modified.

utilities <- list(
  stable = 0.743,
  progressive = 0.731,
  decrement_per_consecutive_month_progression = 0.02
)

costs <- list(
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
)

cycle_length_months <- 1L
time_horizon_months <- 60L
n_cycles <- time_horizon_months / cycle_length_months
health_states <- c("stable", "progressive", "death")

# Monthly survival observations for cycles 1 to 60; decimal commas converted
# to decimal points for R. No cycle-zero observation is included.
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
  length(OS_standard) == n_cycles,
  length(PFS_standard) == n_cycles,
  all(is.finite(OS_standard)),
  all(is.finite(PFS_standard)),
  all(OS_standard > 0 & OS_standard <= 1),
  all(PFS_standard > 0 & PFS_standard <= 1),
  all(PFS_standard < OS_standard),
  all(diff(OS_standard) <= 0),
  all(diff(PFS_standard) <= 0)
)

t <- seq_len(n_cycles)

# Interpret the requested formula as 1 - exp(log(y) / t), i.e. 1 - y^(1/t).
# These are the user-specified transformations, not validated Markov estimates.
# For survival y(t), this gives a constant monthly equivalent through time t,
# rather than the conditional probability specifically during month t.
# OS is overall survival, not survival conditional on being stable.
# PFS includes both progression and death as events.
# OS - PFS is progressive-state occupancy, not post-progression survival.
# Thus the labels below follow the requested mapping only; additional
# assumptions/data are required before using them in an HTA Markov model.
transition_probs_standard <- data.frame(
  cycle = t,
  stable_to_death = 1 - exp(log(OS_standard) / t),
  stable_to_progressive = 1 - exp(log(PFS_standard) / t),
  progressive_to_death = 1 - exp(log(OS_standard - PFS_standard) / t)
)

# Silent numerical checks only; these do not validate the clinical model.
stopifnot(
  nrow(transition_probs_standard) == n_cycles,
  all(is.finite(as.matrix(transition_probs_standard[-1]))),
  all(as.matrix(transition_probs_standard[-1]) >= 0),
  all(as.matrix(transition_probs_standard[-1]) <= 1)
)

# Calculate separately
stable_to_stable <- 1 -
  transition_probs_standard$stable_to_progressive -
  transition_probs_standard$stable_to_death

progressive_to_progressive <- 1 -
  transition_probs_standard$progressive_to_death

# Append to the existing data frame
transition_probs_standard$stable_to_stable <- stable_to_stable
transition_probs_standard$progressive_to_progressive <- progressive_to_progressive

# Standalone long-term TMZ calculations for 60 monthly cycles.
# No existing script is sourced or modified; no data or results are printed.

cycle_length_months <- 1L
time_horizon_months <- 60L
n_cycles <- time_horizon_months / cycle_length_months

# Observations at the end of months 1 to 60 (no month-zero observation).
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

# Silent input checks.
stopifnot(
  length(OS_long_term) == n_cycles,
  length(PFS_long_term) == n_cycles,
  all(is.finite(OS_long_term)),
  all(is.finite(PFS_long_term)),
  all(OS_long_term > 0 & OS_long_term <= 1),
  all(PFS_long_term > 0 & PFS_long_term <= 1),
  all(PFS_long_term < OS_long_term),
  all(diff(OS_long_term) <= 0),
  all(diff(PFS_long_term) <= 0)
)

t <- seq_len(n_cycles)

# Formula interpretation: 1 - exp(log(y) / t), equivalent to 1 - y^(1/t).
# Labels below implement the requested mapping, not validated Markov estimates.
# A survival value transformed this way gives a constant monthly equivalent
# through month t, rather than a conditional probability during month t.
# OS is not survival conditional on being stable; PFS includes death as well
# as progression. OS - PFS is progressive-state occupancy, not survival after
# progression. Further assumptions/data are needed before HTA model use.
transition_probs_long_term <- data.frame(
  cycle = t,
  stable_to_death = 1 - exp(log(OS_long_term) / t),
  stable_to_progressive = 1 - exp(log(PFS_long_term) / t),
  progressive_to_death = 1 - exp(log(OS_long_term - PFS_long_term) / t)
)

# Silent numerical checks; these do not validate the clinical model.
stopifnot(
  nrow(transition_probs_long_term) == n_cycles,
  all(is.finite(as.matrix(transition_probs_long_term[-1]))),
  all(as.matrix(transition_probs_long_term[-1]) >= 0),
  all(as.matrix(transition_probs_long_term[-1]) <= 1)
)

# Calculate separately
stable_to_stable <- 1 -
  transition_probs_long_term$stable_to_progressive -
  transition_probs_long_term$stable_to_death

progressive_to_progressive <- 1 -
  transition_probs_long_term$progressive_to_death

# Append to the existing data frame
transition_probs_long_term$stable_to_stable <- stable_to_stable
transition_probs_long_term$progressive_to_progressive <- progressive_to_progressive

# Calculate traces from transition probabilities already in the R session.
# Run after creating transition_probs_standard and transition_probs_long_term.
# No previous script or transition table is modified. Nothing is printed.
# The limitations of the previously specified transition estimates still apply.

if (!exists("transition_probs_standard") ||
    !exists("transition_probs_long_term")) {
  stop("Create both transition probability objects in this R session first.")
}

# Assumption: all patients start stable at cycle 1.
initial_state_distribution <- c(stable = 1, progressive = 0, death = 0)
n_trace_cycles <- 60L

build_markov_trace <- function(transition_probs, initial_distribution) {
  states <- c("stable", "progressive", "death")
  tolerance <- 1e-10
  required <- c("stable_to_death", "stable_to_progressive",
                "progressive_to_death")
  if (!all(required %in% names(transition_probs))) {
    stop("The transition object is missing required probability columns.")
  }
  # Local copy supports either a data frame or a named list of vectors.
  probs <- as.data.frame(transition_probs)
  if (nrow(probs) != 60L) stop("Expected 60 transition probability rows.")
  if ("cycle" %in% names(probs) &&
      !all(probs$cycle == seq_len(60L))) {
    stop("Transition rows must be ordered by cycles 1 through 60.")
  }
  
  # Reuse appended stay probabilities if present. Otherwise derive their
  # complements locally from the existing probabilities, as previously done.
  if (!"stable_to_stable" %in% names(probs)) {
    probs$stable_to_stable <- 1 - probs$stable_to_progressive -
      probs$stable_to_death
  }
  if (!"progressive_to_progressive" %in% names(probs)) {
    probs$progressive_to_progressive <- 1 - probs$progressive_to_death
  }
  
  probability_columns <- c(required, "stable_to_stable",
                           "progressive_to_progressive")
  probability_values <- as.matrix(probs[probability_columns])
  if (!is.numeric(probability_values) ||
      any(!is.finite(probability_values)) ||
      any(probability_values < 0 | probability_values > 1)) {
    stop("Transition probabilities must be finite and between zero and one.")
  }
  if (any(abs(probs$stable_to_stable + probs$stable_to_progressive +
              probs$stable_to_death - 1) > tolerance) ||
      any(abs(probs$progressive_to_progressive +
              probs$progressive_to_death - 1) > tolerance)) {
    stop("Outgoing transition probabilities must sum to one.")
  }
  if (!all(states %in% names(initial_distribution))) {
    stop("Initial distribution must name stable, progressive and death.")
  }
  initial_distribution <- initial_distribution[states]
  stopifnot(all(is.finite(initial_distribution)),
            all(initial_distribution >= 0),
            abs(sum(initial_distribution) - 1) <= tolerance)
  
  trace <- matrix(0, nrow = 60L, ncol = 3L,
                  dimnames = list(NULL, states))
  trace[1L, ] <- initial_distribution
  
  for (t in 2L:60L) {
    probability_cycle <- max(1L, t - 2L)
    p <- probs[probability_cycle, ]
    
    # Rows are origin states; columns are destination states.
    # No return from progressive to stable; death is absorbing.
    transition_matrix <- matrix(
      c(p$stable_to_stable, p$stable_to_progressive, p$stable_to_death,
        0, p$progressive_to_progressive, p$progressive_to_death,
        0, 0, 1),
      nrow = 3L, byrow = TRUE,
      dimnames = list(states, states)
    )
    trace[t, ] <- as.numeric(trace[t - 1L, ] %*% transition_matrix)
  }
  
  stopifnot(nrow(trace) == 60L,
            all(is.finite(trace)),
            all(trace >= -tolerance & trace <= 1 + tolerance),
            all(abs(rowSums(trace) - 1) <= tolerance))
  data.frame(cycle = seq_len(60L), trace, row.names = NULL)
}

# Explicit mapping for audit: cycle 1 has no transition; cycles 2 and 3 use
# probability cycle 1; cycle 4 uses 2; ...; cycle 60 uses 58.
# Probability cycles 59 and 60 are unused under the requested convention.
transition_cycle_mapping <- data.frame(
  cycle = seq_len(n_trace_cycles),
  probability_cycle = c(NA_integer_, pmax(1L, (2L:60L) - 2L))
)

markov_trace_standard <- build_markov_trace(
  transition_probs_standard, initial_state_distribution
)
markov_trace_long_term <- build_markov_trace(
  transition_probs_long_term, initial_state_distribution
)

# QALMs from existing Markov traces and utility inputs.
# Run after the input and Markov trace scripts. No input data are redefined.
# Assumptions: death utility is zero; no discounting or half-cycle correction.
# Each of the 60 trace rows receives one monthly reward, including cycle 1.
# The requested 0.02 is interpreted as a utility decrement, not an absolute
# loss of 0.02 QALYs (which would equal 0.24 QALMs).
# This is the requested model-cycle schedule, not a patient-level calculation
# of consecutive months since progression. The latter requires tracking
# progression duration or entry cohorts beyond the aggregate state trace.

required_objects <- c("utilities", "cycle_length_months",
                      "markov_trace_standard", "markov_trace_long_term")
if (!all(vapply(required_objects, exists, logical(1), inherits = TRUE))) {
  stop("Load utilities, cycle_length_months and both Markov traces first.")
}

stopifnot(
  cycle_length_months == 1,
  all(c("stable", "progressive",
        "decrement_per_consecutive_month_progression") %in% names(utilities))
)

calculate_qalms <- function(trace, utility_inputs, cycle_months) {
  required_columns <- c("cycle", "stable", "progressive", "death")
  stopifnot(
    all(required_columns %in% names(trace)),
    nrow(trace) == 60L,
    all(trace$cycle == seq_len(60L))
  )
  state_proportions <- as.matrix(trace[c("stable", "progressive", "death")])
  stopifnot(
    all(is.finite(state_proportions)),
    all(state_proportions >= 0 & state_proportions <= 1),
    all(abs(rowSums(state_proportions) - 1) < 1e-10)
  )
  
  # Cycles 1-2: 0 decrements; cycles 3-25: 1-23 decrements;
  # cycles 26-60: retain all 23 decrements, with no further reduction.
  decrement_count <- pmin(pmax(trace$cycle - 2L, 0L), 23L)
  progressive_utility <- utility_inputs$progressive -
    utility_inputs$decrement_per_consecutive_month_progression * decrement_count
  
  stable_qalms <- trace$stable * utility_inputs$stable * cycle_months
  progressive_qalms <- trace$progressive * progressive_utility * cycle_months
  death_qalms <- trace$death * 0 * cycle_months
  cycle_qalms <- stable_qalms + progressive_qalms + death_qalms
  
  # Verify the decrement boundaries and the fixed utility after cycle 25.
  stopifnot(
    all(decrement_count[1:2] == 0L),
    all(decrement_count[3:25] == seq_len(23L)),
    all(decrement_count[26:60] == 23L),
    all(progressive_utility[26:60] == progressive_utility[25]),
    all(is.finite(cycle_qalms))
  )
  
  data.frame(
    cycle = trace$cycle,
    stable = trace$stable,
    progressive = trace$progressive,
    death = trace$death,
    stable_utility = utility_inputs$stable,
    progressive_decrement_count = decrement_count,
    progressive_utility = progressive_utility,
    death_utility = 0,
    stable_qalms = stable_qalms,
    progressive_qalms = progressive_qalms,
    death_qalms = death_qalms,
    cycle_qalms = cycle_qalms,
    cumulative_qalms = cumsum(cycle_qalms)
  )
}

qalms_standard <- calculate_qalms(
  markov_trace_standard, utilities, cycle_length_months
)
qalms_long_term <- calculate_qalms(
  markov_trace_long_term, utilities, cycle_length_months
)

total_qalms_standard <- sum(qalms_standard$cycle_qalms)
total_qalms_long_term <- sum(qalms_long_term$cycle_qalms)

qalms_summary <- data.frame(
  treatment = c("Standard TMZ", "Long-term TMZ"),
  total_qalms = c(total_qalms_standard, total_qalms_long_term)
)

# Display results for all 60 cycles of each treatment, followed by totals.
print(qalms_standard, row.names = FALSE, max = nrow(qalms_standard) * ncol(qalms_standard))
print(qalms_long_term, row.names = FALSE, max = nrow(qalms_long_term) * ncol(qalms_long_term))
print(qalms_summary, row.names = FALSE)

total_qalys_standard <- total_qalms_standard / 12
total_qalys_long_term <- total_qalms_long_term / 12

data.frame(
  treatment = c("Standard TMZ", "Long-term TMZ"),
  total_qalys = c(total_qalys_standard, total_qalys_long_term)
)

# Costs from existing inputs and Markov traces; no data are redefined.
# Run after creating costs, markov_trace_standard and markov_trace_long_term.
# Costs are undiscounted, with no half-cycle correction.
# Cycle 1 is the initial distribution but receives a full cycle cost as requested.

required_objects <- c("costs", "markov_trace_standard", "markov_trace_long_term")
if (!all(vapply(required_objects, exists, logical(1), inherits = TRUE))) {
  stop("Load costs and both Markov traces before running this script.")
}

calculate_cycle_costs <- function(trace, cost_inputs) {
  required_columns <- c("cycle", "stable", "progressive", "death")
  required_costs <- c("stable_months_1_3", "stable_months_4_6",
                      "stable_subsequent_months", "progressive", "death")
  stopifnot(
    all(required_columns %in% names(trace)),
    all(required_costs %in% names(cost_inputs)),
    nrow(trace) == 60L,
    all(trace$cycle == seq_len(60L))
  )
  proportions <- as.matrix(trace[c("stable", "progressive", "death")])
  unit_costs <- unlist(cost_inputs[required_costs], use.names = FALSE)
  stopifnot(
    length(unit_costs) == length(required_costs),
    all(is.finite(unit_costs)), all(unit_costs >= 0),
    all(is.finite(proportions)),
    all(proportions >= 0 & proportions <= 1),
    all(abs(rowSums(proportions) - 1) < 1e-10)
  )
  
  stable_unit_cost <- ifelse(
    trace$cycle <= 4L, cost_inputs$stable_months_1_3,
    ifelse(trace$cycle <= 7L, cost_inputs$stable_months_4_6,
           cost_inputs$stable_subsequent_months)
  )
  
  stable_cost <- trace$stable * stable_unit_cost
  progressive_cost <- trace$progressive * cost_inputs$progressive
  death_cost <- trace$death * cost_inputs$death
  cycle_cost <- stable_cost + progressive_cost + death_cost
  
  data.frame(
    cycle = trace$cycle,
    month = trace$cycle - 1L,
    stable = trace$stable,
    progressive = trace$progressive,
    death = trace$death,
    stable_unit_cost = stable_unit_cost,
    progressive_unit_cost = cost_inputs$progressive,
    death_unit_cost = cost_inputs$death,
    stable_cost = stable_cost,
    progressive_cost = progressive_cost,
    death_cost = death_cost,
    cycle_cost = cycle_cost,
    cumulative_cost = cumsum(cycle_cost)
  )
}

cycle_costs_standard <- calculate_cycle_costs(
  markov_trace_standard, costs$standard_tmz
)
cycle_costs_long_term <- calculate_cycle_costs(
  markov_trace_long_term, costs$long_term_tmz
)

total_cost_standard <- sum(cycle_costs_standard$cycle_cost)
total_cost_long_term <- sum(cycle_costs_long_term$cycle_cost)

cost_summary <- data.frame(
  treatment = c("Standard TMZ", "Long-term TMZ"),
  total_cost = c(total_cost_standard, total_cost_long_term)
)

# Display every cycle for each treatment and the total expected cost per patient.
print(cycle_costs_standard, row.names = FALSE,
      max = nrow(cycle_costs_standard) * ncol(cycle_costs_standard))
print(cycle_costs_long_term, row.names = FALSE,
      max = nrow(cycle_costs_long_term) * ncol(cycle_costs_long_term))
print(cost_summary, row.names = FALSE)

# Incremental cost-effectiveness: long-term TMZ versus standard TMZ.
# Uses existing undiscounted cost and QALM totals; no input data are redefined.

required_objects <- c("total_cost_standard", "total_cost_long_term",
                      "total_qalms_standard", "total_qalms_long_term")
if (!all(vapply(required_objects, exists, logical(1), inherits = TRUE))) {
  stop("Calculate total costs and total QALMs for both treatments first.")
}
for (object_name in required_objects) {
  value <- get(object_name, inherits = TRUE)
  if (!is.numeric(value) || length(value) != 1L || !is.finite(value)) {
    stop(paste(object_name, "must be a single finite numeric value."))
  }
}

incremental_cost <- total_cost_long_term - total_cost_standard
incremental_qalms <- total_qalms_long_term - total_qalms_standard
incremental_qalys <- incremental_qalms / 12

if (incremental_qalms == 0) {
  stop("ICER is undefined because incremental health benefit is zero.")
}

# Keep full precision for calculations; round only the displayed results.
icer_per_qalm <- incremental_cost / incremental_qalms
icer_per_qaly <- incremental_cost / incremental_qalys

icer_results <- data.frame(
  comparison = "Long-term TMZ versus Standard TMZ",
  measure = c("Cost per QALM gained", "Cost per QALY gained"),
  ICER = c(icer_per_qalm, icer_per_qaly)
)

icer_display <- icer_results
icer_display$ICER <- formatC(icer_results$ICER, format = "f", digits = 0,
                             big.mark = ",")
print(icer_display, row.names = FALSE)