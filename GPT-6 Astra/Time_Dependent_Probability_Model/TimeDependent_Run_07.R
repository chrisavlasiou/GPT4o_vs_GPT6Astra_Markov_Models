# Standalone input and transition calculation script. No results are printed.
# Formula interpreted as 1 - exp(log(y) / t), with t = cycle number.
# These are the requested transformations, not calibrated conditional Markov
# transitions. OS - PFS is progressive-state occupancy, not survival within
# that state; its transformation does not identify progressive-to-death risk.

cycle_length_months <- 1L
time_horizon_months <- 60L
n_cycles <- time_horizon_months / cycle_length_months

utilities <- list(stable = 0.743, progressive = 0.731,
                  decrement_per_consecutive_month_progression = 0.02)
costs <- list(
  standard = c(stable_months_1_3 = 1944, stable_months_4_6 = 2453,
               stable_subsequent_months = 350, progressive = 2720, death = 0),
  long_term = c(stable_months_1_3 = 1944, stable_months_4_6 = 2453,
                stable_subsequent_months = 2453, progressive = 2720, death = 0)
)

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
  all(OS_standard > 0 & OS_standard <= 1),
  all(PFS_standard > 0 & PFS_standard <= OS_standard),
  all(diff(OS_standard) <= 0),
  all(diff(PFS_standard) <= 0),
  all(OS_standard - PFS_standard > 0)
)

t <- seq_len(n_cycles)
transition_probs_standard <- data.frame(
  cycle = t,
  stable_to_death = 1 - exp(log(OS_standard) / t),
  stable_to_progressive = 1 - exp(log(PFS_standard) / t),
  progressive_to_death = 1 - exp(log(OS_standard - PFS_standard) / t)
)

stopifnot(
  all(is.finite(as.matrix(transition_probs_standard[-1]))),
  all(transition_probs_standard[-1] >= 0),
  all(transition_probs_standard[-1] <= 1),
  all(transition_probs_standard$stable_to_death +
        transition_probs_standard$stable_to_progressive <= 1)
)

# Calculate separately
stable_to_stable <- 1 -
  transition_probs_standard$stable_to_death -
  transition_probs_standard$stable_to_progressive

# Assumes no transition from progressive back to stable
progressive_to_progressive <- 1 -
  transition_probs_standard$progressive_to_death

# Append to the existing data frame
transition_probs_standard$stable_to_stable <- stable_to_stable
transition_probs_standard$progressive_to_progressive <- progressive_to_progressive


# Standalone long-term TMZ calculations; no data or results are printed.
# Requested formula: 1 - exp(log(y) / t), where t is the cycle number.
# These transformations are not calibrated conditional Markov transitions.
# OS - PFS is progressive-state occupancy, not survival within that state.

cycle_length_months <- 1L
time_horizon_months <- 60L
n_cycles <- time_horizon_months / cycle_length_months

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
  length(OS_long_term) == n_cycles,
  length(PFS_long_term) == n_cycles,
  all(OS_long_term > 0 & OS_long_term <= 1),
  all(PFS_long_term > 0 & PFS_long_term <= OS_long_term),
  all(diff(OS_long_term) <= 0),
  all(diff(PFS_long_term) <= 0),
  all(OS_long_term - PFS_long_term > 0)
)

t <- seq_len(n_cycles)
transition_probs_long_term <- data.frame(
  cycle = t,
  stable_to_death = 1 - exp(log(OS_long_term) / t),
  stable_to_progressive = 1 - exp(log(PFS_long_term) / t),
  progressive_to_death = 1 - exp(log(OS_long_term - PFS_long_term) / t)
)

stopifnot(
  all(is.finite(as.matrix(transition_probs_long_term[-1]))),
  all(transition_probs_long_term[-1] >= 0),
  all(transition_probs_long_term[-1] <= 1),
  all(transition_probs_long_term$stable_to_death +
        transition_probs_long_term$stable_to_progressive <= 1)
)

# Calculate separately
stable_to_stable <- 1 -
  transition_probs_long_term$stable_to_death -
  transition_probs_long_term$stable_to_progressive

# Assumes no transition from progressive back to stable
progressive_to_progressive <- 1 -
  transition_probs_long_term$progressive_to_death

# Append to the existing data frame
transition_probs_long_term$stable_to_stable <- stable_to_stable
transition_probs_long_term$progressive_to_progressive <- progressive_to_progressive


# Run after creating transition_probs_standard and transition_probs_long_term.
# Reuses those objects without changing them or recalculating their probabilities.
# Initial distribution assumption: all patients are stable.
# Exactly 60 trace rows: cycle 1 is initial; cycles 2:60 apply 59 transitions.
# Transition rows used: 1, 1, 2, 3, ..., 58.
# Death is absorbing; progressive-to-stable transitions are not allowed.
# Traces inherit the modelling limitations of the supplied transition estimates.

if (!exists("transition_probs_standard") ||
    !exists("transition_probs_long_term")) {
  stop("Create both transition probability objects before running this script.")
}

initial_distribution <- c(stable = 1, progressive = 0, death = 0)

build_markov_trace <- function(transition_probs, initial_distribution) {
  states <- c("stable", "progressive", "death")
  required <- c("stable_to_death", "stable_to_progressive",
                "progressive_to_death")
  if (!all(required %in% names(transition_probs))) {
    stop("Required transition probability columns are missing.")
  }
  if (!all(vapply(transition_probs[required], is.numeric, logical(1))) ||
      any(lengths(transition_probs[required]) < 58L)) {
    stop("Transition columns must be numeric with at least 58 entries.")
  }
  if (!is.null(transition_probs$cycle) &&
      !all(transition_probs$cycle[seq_len(58L)] == seq_len(58L))) {
    stop("Transition rows must be ordered by cycle starting at cycle 1.")
  }
  stopifnot(identical(names(initial_distribution), states),
            all(is.finite(initial_distribution)),
            all(initial_distribution >= 0),
            abs(sum(initial_distribution) - 1) < 1e-10)
  
  occupancy <- matrix(0, nrow = 60L, ncol = 3L,
                      dimnames = list(NULL, states))
  occupancy[1L, ] <- initial_distribution
  
  for (cycle in 2L:60L) {
    probability_cycle <- max(1L, cycle - 2L)
    p_sd <- transition_probs$stable_to_death[probability_cycle]
    p_sp <- transition_probs$stable_to_progressive[probability_cycle]
    p_pd <- transition_probs$progressive_to_death[probability_cycle]
    
    # Use previously appended stay probabilities when available.
    # Otherwise obtain the same complements locally without changing inputs.
    p_ss <- if (!is.null(transition_probs$stable_to_stable)) {
      transition_probs$stable_to_stable[probability_cycle]
    } else {
      1 - p_sd - p_sp
    }
    p_pp <- if (!is.null(transition_probs$progressive_to_progressive)) {
      transition_probs$progressive_to_progressive[probability_cycle]
    } else {
      1 - p_pd
    }
    
    transition_matrix <- matrix(
      c(p_ss, p_sp, p_sd,
        0,    p_pp, p_pd,
        0,    0,    1),
      nrow = 3L, byrow = TRUE,
      dimnames = list(from = states, to = states)
    )
    if (any(!is.finite(transition_matrix)) ||
        any(transition_matrix < 0 | transition_matrix > 1) ||
        any(abs(rowSums(transition_matrix) - 1) > 1e-10)) {
      stop(sprintf("Invalid transition matrix at probability cycle %d.",
                   probability_cycle))
    }
    occupancy[cycle, ] <- occupancy[cycle - 1L, ] %*% transition_matrix
  }
  
  stopifnot(all(is.finite(occupancy)),
            all(occupancy >= 0 & occupancy <= 1 + 1e-10),
            all(abs(rowSums(occupancy) - 1) < 1e-10))
  data.frame(cycle = seq_len(60L), occupancy, row.names = NULL)
}

markov_trace_standard <- build_markov_trace(
  transition_probs_standard, initial_distribution
)
markov_trace_long_term <- build_markov_trace(
  transition_probs_long_term, initial_distribution
)

# Objects are stored without printing. Each contains cycle, stable,
# progressive, and death columns, with exactly 60 rows and no cycle 0 or 61.

# Requires existing markov_trace_standard, markov_trace_long_term,
# utilities, and cycle_length_months. Does not redefine input data.
# The supplied 0.02 is interpreted as a utility decrement per month,
# not an absolute loss of 0.02 QALYs (which would equal 0.24 QALMs).
# Applies the requested calendar-cycle schedule to all progressive patients;
# individual time since progression cannot be recovered from aggregate traces.
# No discounting or half-cycle correction is applied.

required_objects <- c("markov_trace_standard", "markov_trace_long_term",
                      "utilities", "cycle_length_months")
if (!all(vapply(required_objects, exists, logical(1), inherits = TRUE))) {
  stop("Load the existing Markov traces, utilities, and cycle length first.")
}
stopifnot(cycle_length_months == 1,
          is.numeric(utilities$stable), length(utilities$stable) == 1L,
          is.numeric(utilities$progressive), length(utilities$progressive) == 1L,
          is.numeric(utilities$decrement_per_consecutive_month_progression),
          length(utilities$decrement_per_consecutive_month_progression) == 1L,
          all(is.finite(unlist(utilities))))

calculate_qalms <- function(trace) {
  required_columns <- c("cycle", "stable", "progressive", "death")
  stopifnot(all(required_columns %in% names(trace)),
            nrow(trace) == 60L,
            all(trace$cycle == seq_len(60L)))
  occupancy <- as.matrix(trace[c("stable", "progressive", "death")])
  stopifnot(all(is.finite(occupancy)), all(occupancy >= 0),
            all(abs(rowSums(occupancy) - 1) < 1e-10))
  
  # Counts: 0 at cycles 1-2, 1 at cycle 3, 23 at cycle 25,
  # and 23 for every subsequent cycle through cycle 60.
  decrement_count <- pmin(pmax(trace$cycle - 2L, 0L), 23L)
  progressive_utility <- utilities$progressive -
    utilities$decrement_per_consecutive_month_progression * decrement_count
  
  stable_qalms <- trace$stable * utilities$stable * cycle_length_months
  progressive_qalms <- trace$progressive * progressive_utility *
    cycle_length_months
  death_qalms <- trace$death * 0  # Death utility is zero.
  total_qalms <- stable_qalms + progressive_qalms + death_qalms
  
  data.frame(
    cycle = trace$cycle,
    stable = trace$stable,
    progressive = trace$progressive,
    death = trace$death,
    decrement_count = decrement_count,
    progressive_utility = progressive_utility,
    stable_qalms = stable_qalms,
    progressive_qalms = progressive_qalms,
    death_qalms = death_qalms,
    total_qalms = total_qalms,
    cumulative_qalms = cumsum(total_qalms)
  )
}

qalms_standard <- calculate_qalms(markov_trace_standard)
qalms_long_term <- calculate_qalms(markov_trace_long_term)

total_qalms_standard <- sum(qalms_standard$total_qalms)
total_qalms_long_term <- sum(qalms_long_term$total_qalms)

qalms_by_cycle <- rbind(
  data.frame(treatment = "Standard TMZ", qalms_standard),
  data.frame(treatment = "Long-term TMZ", qalms_long_term)
)
qalms_summary <- data.frame(
  treatment = c("Standard TMZ", "Long-term TMZ"),
  total_qalms = c(total_qalms_standard, total_qalms_long_term)
)

# Display every cycle for both treatments, followed by totals.
print(qalms_by_cycle, row.names = FALSE, max = nrow(qalms_by_cycle) *
        ncol(qalms_by_cycle))
print(qalms_summary, row.names = FALSE)

total_qalys_standard <- total_qalms_standard / 12
total_qalys_long_term <- total_qalms_long_term / 12

# Requires existing markov_trace_standard, markov_trace_long_term, and costs.
# Reuses input data without redefining it. Costs are undiscounted and use
# each cycle's state proportions, with no half-cycle correction.
# Cycle 1 is the initial distribution but incurs the first stable cost band.

required_objects <- c("markov_trace_standard", "markov_trace_long_term", "costs")
if (!all(vapply(required_objects, exists, logical(1), inherits = TRUE))) {
  stop("Load both existing Markov traces and the costs object first.")
}

calculate_cycle_costs <- function(trace, treatment_costs) {
  required_columns <- c("cycle", "stable", "progressive", "death")
  required_costs <- c("stable_months_1_3", "stable_months_4_6",
                      "stable_subsequent_months", "progressive", "death")
  stopifnot(all(required_columns %in% names(trace)),
            nrow(trace) == 60L,
            all(trace$cycle == seq_len(60L)),
            all(required_costs %in% names(treatment_costs)),
            all(is.finite(treatment_costs[required_costs])),
            all(treatment_costs[required_costs] >= 0))
  occupancy <- as.matrix(trace[c("stable", "progressive", "death")])
  stopifnot(all(is.finite(occupancy)), all(occupancy >= 0),
            all(abs(rowSums(occupancy) - 1) < 1e-10))
  
  stable_unit_cost <- ifelse(
    trace$cycle <= 4L, treatment_costs[["stable_months_1_3"]],
    ifelse(trace$cycle <= 7L, treatment_costs[["stable_months_4_6"]],
           treatment_costs[["stable_subsequent_months"]])
  )
  stable_cost <- trace$stable * stable_unit_cost
  progressive_cost <- trace$progressive * treatment_costs[["progressive"]]
  death_cost <- trace$death * treatment_costs[["death"]]
  total_cost <- stable_cost + progressive_cost + death_cost
  
  data.frame(
    cycle = trace$cycle,
    month = trace$cycle - 1L,
    stable = trace$stable,
    progressive = trace$progressive,
    death = trace$death,
    stable_unit_cost = stable_unit_cost,
    progressive_unit_cost = treatment_costs[["progressive"]],
    death_unit_cost = treatment_costs[["death"]],
    stable_cost = stable_cost,
    progressive_cost = progressive_cost,
    death_cost = death_cost,
    total_cost = total_cost,
    cumulative_cost = cumsum(total_cost)
  )
}

cycle_costs_standard <- calculate_cycle_costs(markov_trace_standard, costs$standard)
cycle_costs_long_term <- calculate_cycle_costs(markov_trace_long_term, costs$long_term)

total_cost_standard <- sum(cycle_costs_standard$total_cost)
total_cost_long_term <- sum(cycle_costs_long_term$total_cost)

costs_by_cycle <- rbind(
  data.frame(treatment = "Standard TMZ", cycle_costs_standard),
  data.frame(treatment = "Long-term TMZ", cycle_costs_long_term)
)
costs_summary <- data.frame(
  treatment = c("Standard TMZ", "Long-term TMZ"),
  total_cost = c(total_cost_standard, total_cost_long_term)
)

# Display all 60 cycles per treatment and the total costs per initial patient.
# Calculations retain full precision; rounding is for display only.
print(costs_by_cycle, row.names = FALSE,
      max = nrow(costs_by_cycle) * ncol(costs_by_cycle))
print(costs_summary, row.names = FALSE, digits = 12)

# Incremental comparison: long-term TMZ minus standard TMZ.
# Run after the QALM and cost scripts. Existing totals are not redefined.

required_objects <- c("total_cost_standard", "total_cost_long_term",
                      "total_qalms_standard", "total_qalms_long_term")
if (!all(vapply(required_objects, exists, logical(1), inherits = TRUE))) {
  stop("Calculate total costs and total QALMs for both treatments first.")
}
for (object_name in required_objects) {
  value <- get(object_name)
  if (!is.numeric(value) || length(value) != 1L || !is.finite(value)) {
    stop(paste(object_name, "must be a finite numeric scalar."))
  }
}

incremental_cost <- total_cost_long_term - total_cost_standard
incremental_qalms <- total_qalms_long_term - total_qalms_standard
incremental_qalys <- incremental_qalms / 12

if (incremental_qalms == 0) {
  stop("ICERs are undefined because incremental effectiveness is zero.")
}

icer_qalm <- incremental_cost / incremental_qalms
icer_qaly <- incremental_cost / incremental_qalys

# Retain full precision in numeric results; round only displayed values.
icer_results <- data.frame(
  measure = c("Cost per QALM gained", "Cost per QALY gained"),
  ICER = c(icer_qalm, icer_qaly)
)
icer_display <- data.frame(
  measure = icer_results$measure,
  ICER = formatC(icer_results$ICER, format = "f", digits = 0, big.mark = ",")
)
print(icer_display, row.names = FALSE, right = FALSE)