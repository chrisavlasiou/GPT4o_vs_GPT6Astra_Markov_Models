# Health state utilities
utilities <- list(
  stable = 0.743,
  progressive = 0.731,
  decrement_per_consecutive_month_progression = 0.02
)

# Costs per monthly cycle
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
cycle_length_months <- 1
time_horizon_months <- 60
n_cycles <- time_horizon_months / cycle_length_months

health_states <- c("stable", "progressive", "death")

# Standalone calculation using the user-specified formula.
# Decimal commas in the supplied data are represented as decimal points in R.
# No packages are required; inputs and results are stored without printing.

cycle_length_months <- 1
time_horizon_months <- 60
n_cycles <- time_horizon_months / cycle_length_months
t <- seq_len(n_cycles)

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
  all(is.finite(OS_standard)), all(is.finite(PFS_standard)),
  all(OS_standard > 0 & OS_standard <= 1),
  all(PFS_standard > 0 & PFS_standard <= 1),
  all(PFS_standard <= OS_standard),
  all(diff(OS_standard) <= 0), all(diff(PFS_standard) <= 0)
)

# Requested interpretation: 1 - e^(ln(y)/t), equivalent to 1 - y^(1/t).
transition_probability <- function(y, t) {
  stopifnot(length(y) == length(t), all(y >= 0 & y <= 1), all(t > 0))
  1 - exp(log(y) / t)
}

transition_probs_standard <- data.frame(
  cycle = t,
  stable_to_death = transition_probability(OS_standard, t),
  stable_to_progressive = transition_probability(PFS_standard, t),
  progressive_to_death = transition_probability(OS_standard - PFS_standard, t)
)

# Store diagnostic flags without changing the requested probabilities.
invalid_stable_exit_cycles <- with(
  transition_probs_standard,
  cycle[stable_to_death + stable_to_progressive > 1]
)
stopifnot(all(as.matrix(transition_probs_standard[-1]) >= 0),
          all(as.matrix(transition_probs_standard[-1]) <= 1))

# Modelling limitation: these are the requested transformations, not identified
# conditional Markov transition probabilities. The formula uses cumulative
# values at t, rather than survival conditional on the preceding cycle.
# OS - PFS is progressed-alive occupancy, not survival from progression.
# Additional assumptions are needed before using these in an HTA Markov model.

# Calculate separately
stable_to_stable <- with(
  transition_probs_standard,
  1 - stable_to_death - stable_to_progressive
)

progressive_to_progressive <- with(
  transition_probs_standard,
  1 - progressive_to_death
)

# Append to the existing data frame
transition_probs_standard$stable_to_stable <- stable_to_stable
transition_probs_standard$progressive_to_progressive <- progressive_to_progressive

# Standalone long-term TMZ calculation using the requested formula.
# Inputs and results are stored without printing. No packages required.
cycle_length_months <- 1
time_horizon_months <- 60
n_cycles <- time_horizon_months / cycle_length_months
t <- seq_len(n_cycles)

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
  all(is.finite(OS_long_term)), all(is.finite(PFS_long_term)),
  all(OS_long_term > 0 & OS_long_term <= 1),
  all(PFS_long_term > 0 & PFS_long_term <= 1),
  all(PFS_long_term <= OS_long_term),
  all(diff(OS_long_term) <= 0), all(diff(PFS_long_term) <= 0)
)

# Interpretation: 1 - e^(ln(y)/t), equivalent to 1 - y^(1/t).
transition_probability <- function(y, t) {
  stopifnot(length(y) == length(t), all(y >= 0 & y <= 1), all(t > 0))
  1 - exp(log(y) / t)
}

transition_probs_long_term <- data.frame(
  cycle = t,
  stable_to_death = transition_probability(OS_long_term, t),
  stable_to_progressive = transition_probability(PFS_long_term, t),
  progressive_to_death = transition_probability(OS_long_term - PFS_long_term, t)
)

# Store diagnostic flags without modifying the requested results.
invalid_stable_exit_cycles_long_term <- with(
  transition_probs_long_term,
  cycle[stable_to_death + stable_to_progressive > 1]
)
stopifnot(all(as.matrix(transition_probs_long_term[-1]) >= 0),
          all(as.matrix(transition_probs_long_term[-1]) <= 1))

# Modelling limitation: these are user-specified transformations, not identified
# conditional Markov transition probabilities. OS - PFS is progressed-alive
# occupancy, not survival after progression. Further assumptions are needed
# before these values can be used as transitions in an HTA Markov model.

# Calculate separately
stable_to_stable <- with(
  transition_probs_long_term,
  1 - stable_to_death - stable_to_progressive
)

progressive_to_progressive <- with(
  transition_probs_long_term,
  1 - progressive_to_death
)

# Append to the existing data frame
transition_probs_long_term$stable_to_stable <- stable_to_stable
transition_probs_long_term$progressive_to_progressive <- progressive_to_progressive

# Run after creating transition_probs_standard and transition_probs_long_term.
# Reuses the existing probabilities; does not recalculate them from OS/PFS.
# Exactly 60 rows: cycle 1 is the initial distribution (no transition).
# Assumptions: all patients initially stable; no return from progressive to
# stable; death is absorbing. No inputs or results are printed.
# The methodological limitations of the supplied probabilities still apply.

if (!exists("transition_probs_standard") ||
    !exists("transition_probs_long_term")) {
  stop("Create transition_probs_standard and transition_probs_long_term first.")
}

initial_distribution <- c(stable = 1, progressive = 0, death = 0)
n_trace_cycles <- 60L

# The probability cycle used to move INTO each trace cycle.
transition_cycle_used <- c(NA_integer_, pmax(1L, (2:n_trace_cycles) - 2L))

calculate_markov_trace <- function(transition_probs, initial_distribution) {
  states <- c("stable", "progressive", "death")
  required <- c("cycle", "stable_to_death", "stable_to_progressive",
                "progressive_to_death")
  if (!all(required %in% names(transition_probs))) {
    stop("Transition probabilities are missing required columns.")
  }
  probabilities <- as.data.frame(transition_probs)
  if (anyNA(probabilities$cycle) || anyDuplicated(probabilities$cycle)) {
    stop("Probability cycle labels must be non-missing and unique.")
  }
  
  # Support the original objects if the separately supplied append code has
  # not yet been run. These changes affect only this function's local copy.
  if (!"stable_to_stable" %in% names(probabilities)) {
    probabilities$stable_to_stable <- with(
      probabilities, 1 - stable_to_death - stable_to_progressive
    )
  }
  if (!"progressive_to_progressive" %in% names(probabilities)) {
    probabilities$progressive_to_progressive <-
      1 - probabilities$progressive_to_death
  }
  
  stopifnot(identical(names(initial_distribution), states),
            all(is.finite(initial_distribution)),
            all(initial_distribution >= 0),
            abs(sum(initial_distribution) - 1) < 1e-10)
  
  occupancy <- matrix(0, nrow = n_trace_cycles, ncol = length(states),
                      dimnames = list(NULL, states))
  occupancy[1, ] <- initial_distribution
  
  for (trace_cycle in 2:n_trace_cycles) {
    probability_cycle <- transition_cycle_used[trace_cycle]
    row_index <- match(probability_cycle, probabilities$cycle)
    if (is.na(row_index)) stop("Missing required probability cycle.")
    p <- probabilities[row_index, , drop = FALSE]
    
    # Rows are origin states; columns are destination states.
    transition_matrix <- matrix(
      c(p$stable_to_stable, p$stable_to_progressive, p$stable_to_death,
        0, p$progressive_to_progressive, p$progressive_to_death,
        0, 0, 1),
      nrow = 3, byrow = TRUE, dimnames = list(states, states)
    )
    stopifnot(all(is.finite(transition_matrix)),
              all(transition_matrix >= 0 & transition_matrix <= 1),
              all(abs(rowSums(transition_matrix) - 1) < 1e-10))
    
    occupancy[trace_cycle, ] <-
      as.numeric(occupancy[trace_cycle - 1L, ] %*% transition_matrix)
  }
  
  stopifnot(nrow(occupancy) == 60L,
            all(abs(rowSums(occupancy) - 1) < 1e-10),
            all(occupancy >= 0 & occupancy <= 1 + 1e-10),
            all(diff(occupancy[, "death"]) >= -1e-10))
  
  data.frame(cycle = seq_len(n_trace_cycles),
             transition_cycle_used = transition_cycle_used,
             occupancy, row.names = NULL)
}

markov_trace_standard <- calculate_markov_trace(
  transition_probs_standard, initial_distribution
)
markov_trace_long_term <- calculate_markov_trace(
  transition_probs_long_term, initial_distribution
)

# Run after the utility inputs and both 60-cycle Markov traces exist.
# Does not redefine input data or modify the existing traces.
# Interpretation: the supplied 0.02 decrement is a utility decrement, as in
# the original inputs, not an absolute loss of 0.02 QALYs each month.
# Uses the requested model-cycle schedule for everyone in progressive disease;
# it does not track each patient's individual duration since progression.
# Monthly QALMs = state occupancy * utility * cycle length in months.
# No discounting or half-cycle correction is applied.

required_objects <- c("utilities", "cycle_length_months",
                      "markov_trace_standard", "markov_trace_long_term")
missing_objects <- required_objects[!vapply(required_objects, exists, logical(1))]
if (length(missing_objects) > 0L) {
  stop("Load existing inputs first: ", paste(missing_objects, collapse = ", "))
}

required_utilities <- c("stable", "progressive",
                        "decrement_per_consecutive_month_progression")
stopifnot(all(required_utilities %in% names(utilities)),
          all(is.finite(unlist(utilities[required_utilities]))),
          cycle_length_months == 1)

calculate_qalms <- function(trace) {
  stopifnot(all(c("cycle", "stable", "progressive", "death") %in% names(trace)),
            nrow(trace) == 60L,
            all(trace$cycle == seq_len(60L)))
  occupancy <- as.matrix(trace[c("stable", "progressive", "death")])
  stopifnot(all(is.finite(occupancy)), all(occupancy >= 0),
            all(abs(rowSums(occupancy) - 1) < 1e-10))
  
  # 0 decrements in cycles 1-2; 1 in cycle 3; 23 in cycles 25-60.
  decrement_count <- pmin(pmax(trace$cycle - 2L, 0L), 23L)
  progressive_utility <- utilities$progressive - decrement_count *
    utilities$decrement_per_consecutive_month_progression
  
  stable_qalms <- trace$stable * utilities$stable * cycle_length_months
  progressive_qalms <- trace$progressive * progressive_utility * cycle_length_months
  death_qalms <- trace$death * 0  # Dead-state utility is zero.
  cycle_qalms <- stable_qalms + progressive_qalms + death_qalms
  
  data.frame(
    cycle = trace$cycle,
    stable = trace$stable,
    progressive = trace$progressive,
    death = trace$death,
    decrement_count = decrement_count,
    stable_utility = utilities$stable,
    progressive_utility = progressive_utility,
    stable_qalms = stable_qalms,
    progressive_qalms = progressive_qalms,
    death_qalms = death_qalms,
    cycle_qalms = cycle_qalms,
    cumulative_qalms = cumsum(cycle_qalms)
  )
}

qalms_standard <- calculate_qalms(markov_trace_standard)
qalms_long_term <- calculate_qalms(markov_trace_long_term)

total_qalms_standard <- sum(qalms_standard$cycle_qalms)
total_qalms_long_term <- sum(qalms_long_term$cycle_qalms)

qalms_by_cycle <- data.frame(
  cycle = qalms_standard$cycle,
  progressive_utility = qalms_standard$progressive_utility,
  standard_qalms = qalms_standard$cycle_qalms,
  long_term_qalms = qalms_long_term$cycle_qalms,
  standard_cumulative_qalms = qalms_standard$cumulative_qalms,
  long_term_cumulative_qalms = qalms_long_term$cumulative_qalms
)

total_qalms <- data.frame(
  treatment = c("Standard TMZ", "Long-term TMZ"),
  total_qalms = c(total_qalms_standard, total_qalms_long_term)
)

# Display all 60 per-cycle results and the treatment totals.
print(qalms_by_cycle, row.names = FALSE, max = nrow(qalms_by_cycle) * ncol(qalms_by_cycle))
print(total_qalms, row.names = FALSE)

total_qalys_standard <- total_qalms_standard / 12
total_qalys_long_term <- total_qalms_long_term / 12

# Reuse existing cost inputs and Markov traces; do not redefine input data.
# Monthly state costs are weighted by the occupancy in the same trace row.
# Cycle 1 is the initial distribution but incurs a full cycle of costs as
# requested. No discounting or half-cycle correction is applied.

required_objects <- c("costs", "markov_trace_standard", "markov_trace_long_term")
missing_objects <- required_objects[!vapply(required_objects, exists, logical(1))]
if (length(missing_objects) > 0L) {
  stop("Load existing inputs first: ", paste(missing_objects, collapse = ", "))
}

calculate_cycle_costs <- function(trace, treatment_costs) {
  required_costs <- c("stable_months_1_3", "stable_months_4_6",
                      "stable_subsequent_months", "progressive", "death")
  stopifnot(all(required_costs %in% names(treatment_costs)),
            all(is.finite(unlist(treatment_costs[required_costs]))),
            all(unlist(treatment_costs[required_costs]) >= 0),
            all(c("cycle", "stable", "progressive", "death") %in% names(trace)),
            nrow(trace) == 60L,
            all(trace$cycle == seq_len(60L)))
  occupancy <- as.matrix(trace[c("stable", "progressive", "death")])
  stopifnot(all(is.finite(occupancy)), all(occupancy >= 0),
            all(abs(rowSums(occupancy) - 1) < 1e-10))
  
  stable_unit_cost <- ifelse(
    trace$cycle <= 4L,
    treatment_costs$stable_months_1_3,
    ifelse(trace$cycle <= 7L,
           treatment_costs$stable_months_4_6,
           treatment_costs$stable_subsequent_months)
  )
  
  stable_cost <- trace$stable * stable_unit_cost
  progressive_cost <- trace$progressive * treatment_costs$progressive
  death_cost <- trace$death * treatment_costs$death
  cycle_cost <- stable_cost + progressive_cost + death_cost
  
  data.frame(
    cycle = trace$cycle,
    month = trace$cycle - 1L,  # Month 0 denotes the initial distribution.
    stable = trace$stable,
    progressive = trace$progressive,
    death = trace$death,
    stable_unit_cost = stable_unit_cost,
    progressive_unit_cost = treatment_costs$progressive,
    death_unit_cost = treatment_costs$death,
    stable_cost = stable_cost,
    progressive_cost = progressive_cost,
    death_cost = death_cost,
    cycle_cost = cycle_cost,
    cumulative_cost = cumsum(cycle_cost)
  )
}

costs_by_cycle_standard <- calculate_cycle_costs(
  markov_trace_standard, costs$standard_tmz
)
costs_by_cycle_long_term <- calculate_cycle_costs(
  markov_trace_long_term, costs$long_term_tmz
)

total_cost_standard <- sum(costs_by_cycle_standard$cycle_cost)
total_cost_long_term <- sum(costs_by_cycle_long_term$cycle_cost)

costs_by_cycle <- data.frame(
  cycle = costs_by_cycle_standard$cycle,
  month = costs_by_cycle_standard$month,
  standard_cycle_cost = costs_by_cycle_standard$cycle_cost,
  long_term_cycle_cost = costs_by_cycle_long_term$cycle_cost,
  standard_cumulative_cost = costs_by_cycle_standard$cumulative_cost,
  long_term_cumulative_cost = costs_by_cycle_long_term$cumulative_cost
)

total_costs <- data.frame(
  treatment = c("Standard TMZ", "Long-term TMZ"),
  total_cost = c(total_cost_standard, total_cost_long_term)
)

# Display all 60 cycle costs and both totals; retain full precision in objects.
print(costs_by_cycle, row.names = FALSE,
      max = nrow(costs_by_cycle) * ncol(costs_by_cycle))
print(total_costs, row.names = FALSE)

# Incremental comparison: Long-term TMZ minus Standard TMZ.
# Run after the existing cost and QALM calculation scripts.
# Currency units are those used in the original cost inputs.

required_objects <- c("total_cost_standard", "total_cost_long_term",
                      "total_qalms_standard", "total_qalms_long_term")
missing_objects <- required_objects[!vapply(required_objects, exists, logical(1))]
if (length(missing_objects) > 0L) {
  stop("Calculate existing totals first: ", paste(missing_objects, collapse = ", "))
}
for (object_name in required_objects) {
  value <- get(object_name)
  stopifnot(is.numeric(value), length(value) == 1L, is.finite(value))
}

# Convert existing QALM totals to QALYs without rounding intermediate values.
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

# Preserve full precision above; display ICERs without decimal places.
icer_results <- data.frame(
  comparison = "Long-term TMZ vs Standard TMZ",
  measure = c("Cost per QALM gained", "Cost per QALY gained"),
  ICER = formatC(c(icer_per_qalm, icer_per_qaly),
                 format = "f", digits = 0, big.mark = ",")
)
print(icer_results, row.names = FALSE, right = FALSE)