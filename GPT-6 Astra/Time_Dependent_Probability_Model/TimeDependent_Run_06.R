# Step 1: Store supplied inputs without printing.
# Costs are retained as supplied; currency and price year are unspecified.
inputs <- list(
  utilities = list(
    stable = 0.743,
    progressive = 0.731,
    decrement_per_consecutive_month_of_progression = 0.02
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


# Standard TMZ: supplied monthly OS and PFS, cycles 1 to 60.
# Standalone script; no dependency on or modification of tmz_model.R.
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
  all(is.finite(OS_standard)),
  all(is.finite(PFS_standard)),
  all(OS_standard > 0 & OS_standard <= 1),
  all(PFS_standard > 0 & PFS_standard <= OS_standard),
  all(diff(OS_standard) <= 0),
  all(diff(PFS_standard) <= 0),
  all(OS_standard - PFS_standard > 0)
)

# Interpret the requested formula as 1 - exp(log(y) / t),
# i.e. division by t is inside the exponent, and log() is natural log.
transition_probs_standard <- data.frame(
  cycle = t,
  stable_to_death = 1 - exp(log(OS_standard) / t),
  stable_to_progressive = 1 - exp(log(PFS_standard) / t),
  progressive_to_death = 1 - exp(log(OS_standard - PFS_standard) / t)
)

# These are the requested transformations, not an identified Markov model:
# OS includes survival in both living states; PFS combines progression/death;
# OS - PFS is progressive-state occupancy, not a survival function.
# Do not treat these values as validated conditional transition probabilities.
# Objects are stored silently; no printing or simulation is performed.

# Standard TMZ: supplied monthly OS and PFS, cycles 1 to 60.
# Standalone script; no dependency on or modification of tmz_model.R.
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
  all(is.finite(OS_standard)),
  all(is.finite(PFS_standard)),
  all(OS_standard > 0 & OS_standard <= 1),
  all(PFS_standard > 0 & PFS_standard <= OS_standard),
  all(diff(OS_standard) <= 0),
  all(diff(PFS_standard) <= 0),
  all(OS_standard - PFS_standard > 0)
)

# Interpret the requested formula as 1 - exp(log(y) / t),
# i.e. division by t is inside the exponent, and log() is natural log.
transition_probs_standard <- data.frame(
  cycle = t,
  stable_to_death = 1 - exp(log(OS_standard) / t),
  stable_to_progressive = 1 - exp(log(PFS_standard) / t),
  progressive_to_death = 1 - exp(log(OS_standard - PFS_standard) / t)
)

# These are the requested transformations, not an identified Markov model:
# OS includes survival in both living states; PFS combines progression/death;
# OS - PFS is progressive-state occupancy, not a survival function.
# Do not treat these values as validated conditional transition probabilities.
# Objects are stored silently; no printing or simulation is performed.

# Calculate self-transition probabilities separately.
# Assumption: no transition from progressive back to stable.
stable_to_stable_standard <- 1 -
  transition_probs_standard$stable_to_death -
  transition_probs_standard$stable_to_progressive

progressive_to_progressive_standard <- 1 -
  transition_probs_standard$progressive_to_death

# Append both vectors to the existing data frame.
transition_probs_standard$stable_to_stable <- stable_to_stable_standard
transition_probs_standard$progressive_to_progressive <-
  progressive_to_progressive_standard

# Long-term TMZ: supplied monthly OS and PFS for cycles 1 to 60.
# Standalone script; existing scripts are not modified.
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
  all(is.finite(OS_long_term)),
  all(is.finite(PFS_long_term)),
  all(OS_long_term > 0 & OS_long_term <= 1),
  all(PFS_long_term > 0 & PFS_long_term <= OS_long_term),
  all(diff(OS_long_term) <= 0),
  all(diff(PFS_long_term) <= 0),
  all(OS_long_term - PFS_long_term > 0)
)

# Requested formula interpreted as 1 - exp(log(y) / t).
# Division by t is inside the exponent; log() is natural log.
transition_probs_long_term <- data.frame(
  cycle = t,
  stable_to_death = 1 - exp(log(OS_long_term) / t),
  stable_to_progressive = 1 - exp(log(PFS_long_term) / t),
  progressive_to_death = 1 - exp(log(OS_long_term - PFS_long_term) / t)
)

# These requested transformations are not validated conditional Markov
# transition estimates: OS covers both living states, PFS combines progression
# and death events, and OS - PFS is progressive-state occupancy.
# Objects are stored silently; no printing or simulation is performed.

# Long-term TMZ: supplied monthly OS and PFS for cycles 1 to 60.
# Standalone script; existing scripts are not modified.
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
  all(is.finite(OS_long_term)),
  all(is.finite(PFS_long_term)),
  all(OS_long_term > 0 & OS_long_term <= 1),
  all(PFS_long_term > 0 & PFS_long_term <= OS_long_term),
  all(diff(OS_long_term) <= 0),
  all(diff(PFS_long_term) <= 0),
  all(OS_long_term - PFS_long_term > 0)
)

# Requested formula interpreted as 1 - exp(log(y) / t).
# Division by t is inside the exponent; log() is natural log.
transition_probs_long_term <- data.frame(
  cycle = t,
  stable_to_death = 1 - exp(log(OS_long_term) / t),
  stable_to_progressive = 1 - exp(log(PFS_long_term) / t),
  progressive_to_death = 1 - exp(log(OS_long_term - PFS_long_term) / t)
)

# These requested transformations are not validated conditional Markov
# transition estimates: OS covers both living states, PFS combines progression
# and death events, and OS - PFS is progressive-state occupancy.
# Objects are stored silently; no printing or simulation is performed.

# Calculate self-transition probabilities separately.
# Assumption: no transition from progressive back to stable.
stable_to_stable_long_term <- 1 -
  transition_probs_long_term$stable_to_death -
  transition_probs_long_term$stable_to_progressive

progressive_to_progressive_long_term <- 1 -
  transition_probs_long_term$progressive_to_death

# Append both vectors to the existing data frame without printing.
transition_probs_long_term$stable_to_stable <- stable_to_stable_long_term
transition_probs_long_term$progressive_to_progressive <-
  progressive_to_progressive_long_term

# Markov traces: exactly 60 rows, labelled cycles 1 through 60.
# Reuse the previously calculated transition probability objects.
# If absent, load them from the existing scripts alongside this file.
# Run from the directory containing the three R scripts.
if (!exists("transition_probs_standard")) {
  source("transition_probs_standard.R")
}
if (!exists("transition_probs_long_term")) {
  source("transition_probs_long_term.R")
}

# Assumption: all patients start stable; no recovery from progression.
# Death is absorbing.
initial_state <- c(stable = 1, progressive = 0, death = 0)

calculate_markov_trace <- function(transition_probs, initial_state) {
  states <- c("stable", "progressive", "death")
  required <- c("cycle", "stable_to_stable", "stable_to_progressive",
                "stable_to_death", "progressive_to_progressive",
                "progressive_to_death")
  stopifnot(all(required %in% names(transition_probs)),
            all(states %in% names(initial_state)))
  initial_state <- initial_state[states]
  stopifnot(all(is.finite(initial_state)), all(initial_state >= 0),
            abs(sum(initial_state) - 1) < 1e-10,
            !anyDuplicated(transition_probs$cycle))
  
  trace <- matrix(0, nrow = 60L, ncol = 3L,
                  dimnames = list(NULL, states))
  trace[1L, ] <- initial_state
  probability_cycle <- rep(NA_integer_, 60L)
  
  for (cycle in 2L:60L) {
    # Cycles 2 and 3 use probability cycle 1; thereafter use cycle - 2.
    source_cycle <- max(1L, cycle - 2L)
    probability_cycle[cycle] <- source_cycle
    row_index <- match(source_cycle, transition_probs$cycle)
    stopifnot(!is.na(row_index))
    p <- transition_probs[row_index, , drop = FALSE]
    
    # Rows are origin states; columns are destination states.
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
  
  stopifnot(all(abs(rowSums(trace) - 1) < 1e-10),
            all(trace >= 0 & trace <= 1 + 1e-10))
  data.frame(cycle = seq_len(60L), probability_cycle = probability_cycle,
             trace, row.names = NULL)
}

markov_trace_standard <- calculate_markov_trace(
  transition_probs_standard, initial_state
)
markov_trace_long_term <- calculate_markov_trace(
  transition_probs_long_term, initial_state
)

# Each trace has 60 rows: initial distribution plus 59 transitions.
# Probability cycles used: NA, 1, 1, 2, 3, ..., 58.
# Existing probability cycles 59 and 60 are unused under this timing rule.
# Results are stored without printing.
# These traces inherit the modelling limitations of the supplied probability
# transformations and should not be expected to reproduce the OS/PFS curves.

# Calculate QALMs using existing inputs and Markov traces.
# Run after tmz_model.R and markov_traces.R; no input data are redefined here.
stopifnot(exists("inputs"), exists("markov_trace_standard"),
          exists("markov_trace_long_term"))

cycles <- seq_len(60L)
# Apply the requested calendar-cycle schedule to progressive occupancy.
# Cycle 1-2: zero decrements; cycles 3-25: 1-23 decrements;
# cycles 26-60: retain 23 decrements permanently.
# Interpret 0.02 as a decrement in utility, as specified in the input data,
# rather than an annual QALY amount to be converted into months.
number_of_decrements <- pmin(pmax(cycles - 2L, 0L), 23L)
progressive_utility_by_cycle <- inputs$utilities$progressive -
  inputs$utilities$decrement_per_consecutive_month_of_progression *
  number_of_decrements

# Death contributes zero QALMs. This uses a calendar-cycle utility schedule,
# not individual time since progression; it follows the requested cycle cutoffs.
calculate_qalms <- function(trace) {
  stopifnot(nrow(trace) == 60L,
            all(trace$cycle == cycles),
            all(c("stable", "progressive", "death") %in% names(trace)),
            inputs$design$cycle_length_months == 1)
  proportions <- as.matrix(trace[, c("stable", "progressive", "death")])
  stopifnot(all(is.finite(proportions)), all(proportions >= 0),
            all(abs(rowSums(proportions) - 1) < 1e-10))
  
  stable_qalms <- trace$stable * inputs$utilities$stable *
    inputs$design$cycle_length_months
  progressive_qalms <- trace$progressive * progressive_utility_by_cycle *
    inputs$design$cycle_length_months
  death_qalms <- trace$death * 0
  
  data.frame(
    cycle = cycles,
    stable = trace$stable,
    progressive = trace$progressive,
    death = trace$death,
    stable_utility = inputs$utilities$stable,
    progressive_utility = progressive_utility_by_cycle,
    stable_qalms = stable_qalms,
    progressive_qalms = progressive_qalms,
    death_qalms = death_qalms,
    qalms = stable_qalms + progressive_qalms + death_qalms
  )
}

qalms_standard <- calculate_qalms(markov_trace_standard)
qalms_long_term <- calculate_qalms(markov_trace_long_term)

total_qalms_standard <- sum(qalms_standard$qalms)
total_qalms_long_term <- sum(qalms_long_term$qalms)

qalms_by_cycle <- data.frame(
  cycle = cycles,
  progressive_utility = progressive_utility_by_cycle,
  qalms_standard = qalms_standard$qalms,
  qalms_long_term = qalms_long_term$qalms
)
total_qalms <- data.frame(
  treatment = c("Standard TMZ", "Long-term TMZ"),
  total_qalms = c(total_qalms_standard, total_qalms_long_term)
)

# Sum all 60 rows, including the initial-state row at cycle 1 as requested.
# No discounting or half-cycle correction is applied.
print(qalms_by_cycle, row.names = FALSE)
print(total_qalms, row.names = FALSE)

total_qalys_standard <- total_qalms_standard / 12
total_qalys_long_term <- total_qalms_long_term / 12

# Costs for both treatments using existing inputs and Markov traces.
# Run after tmz_model.R and markov_traces.R; no input data are redefined.
stopifnot(exists("inputs"), exists("markov_trace_standard"),
          exists("markov_trace_long_term"))

calculate_cycle_costs <- function(trace, unit_costs) {
  cycles <- seq_len(60L)
  states <- c("stable", "progressive", "death")
  stopifnot(nrow(trace) == 60L, all(trace$cycle == cycles),
            all(states %in% names(trace)))
  proportions <- as.matrix(trace[, states])
  stopifnot(all(is.finite(proportions)), all(proportions >= 0),
            all(abs(rowSums(proportions) - 1) < 1e-10))
  
  # Cycle 1 is initial occupancy and incurs the months 1-3 cost unit.
  # Cycles 2-60 correspond to months 1-59.
  stable_unit_cost <- ifelse(
    cycles <= 4L, unit_costs$stable_months_1_3,
    ifelse(cycles <= 7L, unit_costs$stable_months_4_6,
           unit_costs$stable_subsequent_months)
  )
  stable_cost <- trace$stable * stable_unit_cost
  progressive_cost <- trace$progressive * unit_costs$progressive
  death_cost <- trace$death * unit_costs$death
  cycle_cost <- stable_cost + progressive_cost + death_cost
  
  data.frame(
    cycle = cycles,
    month = c(NA_integer_, seq_len(59L)),
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

costs_by_cycle <- data.frame(
  cycle = costs_standard$cycle,
  month = costs_standard$month,
  cost_standard = costs_standard$cycle_cost,
  cost_long_term = costs_long_term$cycle_cost
)
total_costs <- data.frame(
  treatment = c("Standard TMZ", "Long-term TMZ"),
  total_cost = c(total_cost_standard, total_cost_long_term)
)

# Undiscounted expected costs per patient across all 60 rows, including cycle 1.
# No half-cycle correction; currency and price year were not supplied.
# Keep full precision when summing; display rounding does not change totals.
print(costs_by_cycle, row.names = FALSE)
print(total_costs, row.names = FALSE)

# Incremental cost-effectiveness: long-term TMZ versus standard TMZ.
# Run after calculate_qalms.R and calculate_costs.R.
# Reuse full-precision totals; round only the displayed ICERs.
stopifnot(exists("total_cost_standard"), exists("total_cost_long_term"),
          exists("total_qalms_standard"), exists("total_qalms_long_term"))

incremental_cost <- total_cost_long_term - total_cost_standard
incremental_qalms <- total_qalms_long_term - total_qalms_standard
incremental_qalys <- incremental_qalms / 12

stopifnot(all(is.finite(c(incremental_cost, incremental_qalms,
                          incremental_qalys))))
if (incremental_qalms == 0) {
  stop("ICER is undefined because incremental effectiveness is zero.")
}

icer_per_qalm <- incremental_cost / incremental_qalms
icer_per_qaly <- incremental_cost / incremental_qalys

# Retain numeric, unrounded results for subsequent analysis.
icers <- data.frame(
  measure = c("Cost per QALM gained", "Cost per QALY gained"),
  icer = c(icer_per_qalm, icer_per_qaly)
)

# Display whole numbers with no decimal places.
icers_display <- data.frame(
  measure = icers$measure,
  ICER = formatC(icers$icer, format = "f", digits = 0, big.mark = ",")
)
cat("Long-term TMZ versus standard TMZ\n")
print(icers_display, row.names = FALSE, right = FALSE)