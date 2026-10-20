# Health state utilities
utilities <- list(
  stable = 0.743,
  progressive = 0.731,
  progression_decrement_per_consecutive_month = 0.02
)

# Monthly health state costs
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

states <- c("stable", "progressive", "death")

# Standalone script: Standard TMZ, 60 monthly cycles.
# Implements the specified transformation: 1 - exp(log(y) / t).
# These transformed values are not validated conditional Markov transitions.
# In particular, OS - PFS is progressive-state occupancy, not survival
# conditional on entering progression. No transition matrix is inferred here.

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

# Silent input validation.
stopifnot(
  length(OS_standard) == n_cycles,
  length(PFS_standard) == n_cycles,
  all(is.finite(OS_standard)),
  all(is.finite(PFS_standard)),
  all(OS_standard > 0 & OS_standard <= 1),
  all(PFS_standard > 0 & PFS_standard <= 1),
  all(OS_standard > PFS_standard),
  all(diff(OS_standard) <= 0),
  all(diff(PFS_standard) <= 0)
)

# R's log() is the natural logarithm. All operations are elementwise.
transition_probs_standard <- data.frame(
  cycle = t,
  stable_to_death = 1 - exp(log(OS_standard) / t),
  stable_to_progressive = 1 - exp(log(PFS_standard) / t),
  progressive_to_death = 1 - exp(log(OS_standard - PFS_standard) / t)
)

# Results remain stored in transition_probs_standard; nothing is printed.

# Calculate the probabilities separately
stable_to_stable <- 1 -
  transition_probs_standard$stable_to_death -
  transition_probs_standard$stable_to_progressive

progressive_to_progressive <- 1 -
  transition_probs_standard$progressive_to_death

# Append to the existing data frame
transition_probs_standard$stable_to_stable <- stable_to_stable
transition_probs_standard$progressive_to_progressive <- progressive_to_progressive

# Standalone script: Long-term TMZ, 60 monthly cycles.
# Implements the requested formula as 1 - exp(log(y) / t).
# These transformed values are not validated conditional Markov transitions.
# OS - PFS is progressive-state occupancy, not survival conditional on
# entering progression. No transition matrix is inferred here.

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

# Silent input validation.
stopifnot(
  length(OS_long_term) == n_cycles,
  length(PFS_long_term) == n_cycles,
  all(is.finite(OS_long_term)),
  all(is.finite(PFS_long_term)),
  all(OS_long_term > 0 & OS_long_term <= 1),
  all(PFS_long_term > 0 & PFS_long_term <= 1),
  all(OS_long_term > PFS_long_term),
  all(diff(OS_long_term) <= 0),
  all(diff(PFS_long_term) <= 0)
)

# R's log() is the natural logarithm; calculations are elementwise.
transition_probs_long_term <- data.frame(
  cycle = t,
  stable_to_death = 1 - exp(log(OS_long_term) / t),
  stable_to_progressive = 1 - exp(log(PFS_long_term) / t),
  progressive_to_death = 1 - exp(log(OS_long_term - PFS_long_term) / t)
)

# Results remain stored in transition_probs_long_term; nothing is printed.

# Calculate the probabilities separately
stable_to_stable <- 1 -
  transition_probs_long_term$stable_to_death -
  transition_probs_long_term$stable_to_progressive

progressive_to_progressive <- 1 -
  transition_probs_long_term$progressive_to_death

# Append to the existing data frame
transition_probs_long_term$stable_to_stable <- stable_to_stable
transition_probs_long_term$progressive_to_progressive <- progressive_to_progressive

# Reuse transition_probs_standard and transition_probs_long_term in memory.
# Run the previously supplied transition-probability scripts first if needed.
# This script does not modify those objects or recalculate their survival inputs.
# Assumption: everyone starts stable; progression cannot return to stable;
# death is absorbing. The earlier transition-estimation caveat still applies.

if (!exists("transition_probs_standard") ||
    !exists("transition_probs_long_term")) {
  stop("Load transition_probs_standard and transition_probs_long_term before running this script.")
}

n_cycles <- 60L
initial_distribution <- c(stable = 1, progressive = 0, death = 0)

# Row 1 is initial occupancy. Rows 2, 3, 4, 5, ... use probability
# rows 1, 1, 2, 3, ... respectively. Row 60 uses probability row 58.
transition_cycle_used <- c(NA_integer_, pmax(1L, (2L:n_cycles) - 2L))

calculate_markov_trace <- function(transition_probs, initial_distribution,
                                   transition_cycle_used) {
  required <- c("stable_to_death", "stable_to_progressive",
                "progressive_to_death")
  if (!all(required %in% names(transition_probs))) {
    stop("The transition-probability object is missing required columns.")
  }
  
  # Work on a local copy. Derive complements only if they are not yet appended.
  probs <- as.data.frame(transition_probs)
  if (!"stable_to_stable" %in% names(probs)) {
    probs$stable_to_stable <- 1 - probs$stable_to_death -
      probs$stable_to_progressive
  }
  if (!"progressive_to_progressive" %in% names(probs)) {
    probs$progressive_to_progressive <- 1 - probs$progressive_to_death
  }
  
  n <- length(transition_cycle_used)
  if (nrow(probs) < max(transition_cycle_used, na.rm = TRUE)) {
    stop("Insufficient transition-probability rows.")
  }
  if ("cycle" %in% names(probs)) {
    stopifnot(all(probs$cycle == seq_len(nrow(probs))))
  }
  stopifnot(
    identical(names(initial_distribution), c("stable", "progressive", "death")),
    all(is.finite(initial_distribution)),
    all(initial_distribution >= 0),
    abs(sum(initial_distribution) - 1) < 1e-10
  )
  
  occupancy <- matrix(0, nrow = n, ncol = 3L,
                      dimnames = list(NULL, names(initial_distribution)))
  occupancy[1L, ] <- initial_distribution
  
  for (cycle in 2L:n) {
    p <- probs[transition_cycle_used[cycle], ]
    # Rows are origin states; columns are destination states.
    transition_matrix <- matrix(c(
      p$stable_to_stable, p$stable_to_progressive, p$stable_to_death,
      0, p$progressive_to_progressive, p$progressive_to_death,
      0, 0, 1
    ), nrow = 3L, byrow = TRUE,
    dimnames = list(names(initial_distribution), names(initial_distribution)))
    
    stopifnot(
      all(is.finite(transition_matrix)),
      all(transition_matrix >= 0 & transition_matrix <= 1),
      all(abs(rowSums(transition_matrix) - 1) < 1e-10)
    )
    occupancy[cycle, ] <- as.vector(occupancy[cycle - 1L, ] %*%
                                      transition_matrix)
  }
  
  stopifnot(
    nrow(occupancy) == 60L,
    all(occupancy >= -1e-10 & occupancy <= 1 + 1e-10),
    all(abs(rowSums(occupancy) - 1) < 1e-10),
    all(diff(occupancy[, "death"]) >= -1e-10)
  )
  data.frame(cycle = seq_len(n), occupancy, row.names = NULL)
}

markov_trace_standard <- calculate_markov_trace(
  transition_probs_standard, initial_distribution, transition_cycle_used
)

markov_trace_long_term <- calculate_markov_trace(
  transition_probs_long_term, initial_distribution, transition_cycle_used
)

# Both traces contain exactly 60 rows (cycles 1-60), including the initial row.
# There are 59 transitions; probability rows 59 and 60 are unused.
# Results are stored without printing.

# Uses existing utilities, cycle_length_months, markov_trace_standard,
# and markov_trace_long_term. Does not redefine the input data.
# The requested decrement is interpreted as 0.02 utility units per month,
# not a literal loss of 0.02 QALYs (an annual outcome).
# This is the specified model-cycle schedule, not time-since-progression
# tracking for individual incident cohorts. Death has zero utility.
# All 60 rows contribute one full monthly cycle; no discounting or
# half-cycle correction is applied.

required_objects <- c("utilities", "cycle_length_months",
                      "markov_trace_standard", "markov_trace_long_term")
missing_objects <- required_objects[!vapply(
  required_objects, exists, logical(1), inherits = TRUE
)]
if (length(missing_objects) > 0L) {
  stop("Load the existing inputs first: ", paste(missing_objects, collapse = ", "))
}

stopifnot(
  cycle_length_months == 1,
  all(c("stable", "progressive", "progression_decrement_per_consecutive_month") %in%
        names(utilities))
)

calculate_qalms <- function(trace, utilities, cycle_length_months) {
  stopifnot(
    nrow(trace) == 60L,
    all(c("cycle", "stable", "progressive", "death") %in% names(trace)),
    identical(as.integer(trace$cycle), seq_len(60L))
  )
  occupancy <- as.matrix(trace[, c("stable", "progressive", "death")])
  stopifnot(
    all(is.finite(occupancy)),
    all(occupancy >= -1e-10 & occupancy <= 1 + 1e-10),
    all(abs(rowSums(occupancy) - 1) < 1e-10)
  )
  
  # Zero decrements in cycles 1-2; 1 through 23 in cycles 3-25;
  # 23 decrements thereafter, retaining the cycle-25 utility.
  decrement_count <- pmin(pmax(trace$cycle - 2L, 0L), 23L)
  progressive_utility <- utilities$progressive -
    utilities$progression_decrement_per_consecutive_month * decrement_count
  
  stable_qalms <- trace$stable * utilities$stable * cycle_length_months
  progressive_qalms <- trace$progressive * progressive_utility * cycle_length_months
  death_qalms <- trace$death * 0 * cycle_length_months
  cycle_qalms <- stable_qalms + progressive_qalms + death_qalms
  
  data.frame(
    cycle = trace$cycle,
    stable = trace$stable,
    progressive = trace$progressive,
    death = trace$death,
    stable_utility = utilities$stable,
    progressive_utility = progressive_utility,
    decrement_count = decrement_count,
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

total_qalms <- data.frame(
  treatment = c("Standard TMZ", "Long-term TMZ"),
  total_qalms = c(total_qalms_standard, total_qalms_long_term)
)

# Display every cycle for both treatments, followed by total QALMs.
print(qalms_standard, row.names = FALSE, max = nrow(qalms_standard) * ncol(qalms_standard))
print(qalms_long_term, row.names = FALSE, max = nrow(qalms_long_term) * ncol(qalms_long_term))
print(total_qalms, row.names = FALSE)

total_qalys_standard <- total_qalms_standard / 12
total_qalys_long_term <- total_qalms_long_term / 12


# Reuses the existing costs, markov_trace_standard and markov_trace_long_term.
# Cycle 1 is the initial distribution; cycle 2 is month 1.
# Per the requested convention, cycle 1 also incurs a full cycle of costs.
# Stable cost bands: cycles 1-4, 5-7, and 8-60.
# Costs are undiscounted, with no half-cycle correction.

required_objects <- c("costs", "markov_trace_standard", "markov_trace_long_term")
missing_objects <- required_objects[!vapply(
  required_objects, exists, logical(1), inherits = TRUE
)]
if (length(missing_objects) > 0L) {
  stop("Load the existing inputs first: ", paste(missing_objects, collapse = ", "))
}

calculate_cycle_costs <- function(trace, treatment_costs) {
  stopifnot(
    nrow(trace) == 60L,
    all(c("cycle", "stable", "progressive", "death") %in% names(trace)),
    identical(as.integer(trace$cycle), seq_len(60L)),
    all(c("stable_months_1_3", "stable_months_4_6",
          "stable_subsequent_months", "progressive", "death") %in%
          names(treatment_costs))
  )
  occupancy <- as.matrix(trace[, c("stable", "progressive", "death")])
  stopifnot(
    all(is.finite(occupancy)),
    all(occupancy >= -1e-10 & occupancy <= 1 + 1e-10),
    all(abs(rowSums(occupancy) - 1) < 1e-10)
  )
  
  stable_unit_cost <- ifelse(
    trace$cycle <= 4L, treatment_costs$stable_months_1_3,
    ifelse(trace$cycle <= 7L, treatment_costs$stable_months_4_6,
           treatment_costs$stable_subsequent_months)
  )
  
  stable_cost <- trace$stable * stable_unit_cost
  progressive_cost <- trace$progressive * treatment_costs$progressive
  death_cost <- trace$death * treatment_costs$death
  cycle_cost <- stable_cost + progressive_cost + death_cost
  
  data.frame(
    cycle = trace$cycle,
    month = trace$cycle - 1L,
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

cycle_costs_standard <- calculate_cycle_costs(markov_trace_standard, costs$standard_tmz)
cycle_costs_long_term <- calculate_cycle_costs(markov_trace_long_term, costs$long_term_tmz)

total_cost_standard <- sum(cycle_costs_standard$cycle_cost)
total_cost_long_term <- sum(cycle_costs_long_term$cycle_cost)

total_costs <- data.frame(
  treatment = c("Standard TMZ", "Long-term TMZ"),
  total_cost = c(total_cost_standard, total_cost_long_term)
)

# Print all 60 cycles and totals; retain full precision in the stored objects.
print(cycle_costs_standard, row.names = FALSE,
      max = nrow(cycle_costs_standard) * ncol(cycle_costs_standard))
print(cycle_costs_long_term, row.names = FALSE,
      max = nrow(cycle_costs_long_term) * ncol(cycle_costs_long_term))
print(total_costs, row.names = FALSE)

# Incremental comparison: Long-term TMZ minus Standard TMZ.
# Reuses full-precision totals from the previous cost and QALM scripts.

required_objects <- c("total_cost_standard", "total_cost_long_term",
                      "total_qalms_standard", "total_qalms_long_term")
missing_objects <- required_objects[!vapply(
  required_objects, exists, logical(1), inherits = TRUE
)]
if (length(missing_objects) > 0L) {
  stop("Load the existing totals first: ", paste(missing_objects, collapse = ", "))
}
for (object_name in required_objects) {
  value <- get(object_name)
  stopifnot(is.numeric(value), length(value) == 1L, is.finite(value))
}

incremental_cost <- total_cost_long_term - total_cost_standard
incremental_qalms <- total_qalms_long_term - total_qalms_standard
incremental_qalys <- incremental_qalms / 12

if (incremental_qalms == 0) {
  stop("ICERs are undefined because incremental health benefit is zero.")
}

icer_per_qalm <- incremental_cost / incremental_qalms
icer_per_qaly <- incremental_cost / incremental_qalys

# Store full precision; format only the displayed results without decimals.
icer_results <- data.frame(
  comparison = "Long-term TMZ versus Standard TMZ",
  outcome = c("Cost per QALM gained", "Cost per QALY gained"),
  ICER = c(icer_per_qalm, icer_per_qaly)
)
icer_display <- icer_results
icer_display$ICER <- formatC(icer_results$ICER, format = "f", digits = 0)
print(icer_display, row.names = FALSE)