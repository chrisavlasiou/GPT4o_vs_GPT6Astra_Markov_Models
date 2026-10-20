# Step 1: Store supplied inputs. Sourcing this script prints no data.
# Transition probabilities correspond to the one-month model cycle.
# Currency and further modelling assumptions have not been specified.
tmz_inputs <- list(
  states = c("stable", "progressive", "death"),
  transitions = list(
    standard_tmz = c(stable_to_progressive = 0.0956,
                     stable_to_death = 0.0464,
                     progressive_to_death = 0.0861),
    long_term_tmz = c(stable_to_progressive = 0.0894,
                      stable_to_death = 0.0397,
                      progressive_to_death = 0.0689)
  ),
  utilities = list(
    stable = 0.743,
    progressive = 0.731,
    decrement_per_consecutive_month_of_progression = 0.02
  ),
  costs = list(
    standard_tmz = list(
      stable = c(months_1_to_3 = 1944, months_4_to_6 = 2453,
                 subsequent_months = 350),
      progressive = 2720,
      death = 0
    ),
    long_term_tmz = list(
      stable = c(months_1_to_3 = 1944, months_4_to_6 = 2453,
                 subsequent_months = 2453),
      progressive = 2720,
      death = 0
    )
  ),
  design = list(cycle_length_months = 1, time_horizon_months = 60)
)

# Step 2: Three-state Markov traces for Standard and Long-term TMZ.
# Standalone base-R script; no packages required.
# Assumptions:
# - All patients are stable immediately before cycle 1.
# - Transition probabilities apply per one-month cycle and are constant.
# - No recovery from progression; death is absorbing.
# - Each row is END-of-cycle occupancy (proportions, not patient counts).
# Exactly 60 rows per treatment: cycles 1 through 60, with no cycle-0 row.

states <- c("stable", "progressive", "death")
cycle_length_months <- 1L
time_horizon_months <- 60L
n_cycles <- as.integer(time_horizon_months / cycle_length_months)
initial_distribution <- c(stable = 1, progressive = 0, death = 0)

transition_probabilities <- list(
  standard_tmz = c(stable_to_progressive = 0.0956,
                   stable_to_death = 0.0464,
                   progressive_to_death = 0.0861),
  long_term_tmz = c(stable_to_progressive = 0.0894,
                    stable_to_death = 0.0397,
                    progressive_to_death = 0.0689)
)

make_transition_matrix <- function(probabilities) {
  p_sp <- unname(probabilities["stable_to_progressive"])
  p_sd <- unname(probabilities["stable_to_death"])
  p_pd <- unname(probabilities["progressive_to_death"])
  # Rows = origin states; columns = destination states.
  transition_matrix <- matrix(
    c(1 - p_sp - p_sd, p_sp, p_sd,
      0, 1 - p_pd, p_pd,
      0, 0, 1),
    nrow = 3L, byrow = TRUE,
    dimnames = list(from = states, to = states)
  )
  stopifnot(
    all(is.finite(transition_matrix)),
    all(transition_matrix >= 0 & transition_matrix <= 1),
    all(abs(rowSums(transition_matrix) - 1) < 1e-12)
  )
  transition_matrix
}

calculate_trace <- function(transition_matrix) {
  occupancy <- matrix(NA_real_, nrow = n_cycles, ncol = length(states),
                      dimnames = list(NULL, states))
  current_distribution <- initial_distribution
  for (cycle in seq_len(n_cycles)) {
    current_distribution <- as.numeric(current_distribution %*% transition_matrix)
    occupancy[cycle, ] <- current_distribution
  }
  trace <- data.frame(cycle = seq_len(n_cycles), occupancy,
                      row.names = NULL, check.names = FALSE)
  stopifnot(
    nrow(trace) == 60L,
    identical(trace$cycle, seq_len(60L)),
    all(is.finite(occupancy)),
    all(occupancy >= -1e-12 & occupancy <= 1 + 1e-12),
    all(abs(rowSums(occupancy) - 1) < 1e-12),
    all(diff(trace$death) >= -1e-12)
  )
  trace
}

transition_matrices <- lapply(transition_probabilities, make_transition_matrix)
markov_traces <- lapply(transition_matrices, calculate_trace)
trace_standard_tmz <- markov_traces$standard_tmz
trace_long_term_tmz <- markov_traces$long_term_tmz

# Results are retained in the objects above without printing.
# Optional inspection in R:
# View(trace_standard_tmz)
# View(trace_long_term_tmz)

# Step 3: QALMs using previously defined inputs and Markov traces.
# First run tmz_inputs.R and tmz_markov_trace.R in the same R session.
# No input data are redefined here.
# Uses the requested calendar-cycle schedule, not time since each patient's
# entry into progression. The supplied 0.02 is treated as a utility decrement,
# consistent with the stored inputs; it is not an absolute loss of 0.02 QALYs.
# Death contributes zero QALMs. End-of-cycle rewards, no half-cycle correction
# or discounting, are used to match the previously calculated traces.

if (!exists("tmz_inputs") || !exists("trace_standard_tmz") ||
    !exists("trace_long_term_tmz")) {
  stop("Run tmz_inputs.R and tmz_markov_trace.R before running this script.")
}

calculate_cycle_qalms <- function(trace, treatment) {
  stopifnot(
    nrow(trace) == 60L,
    identical(as.integer(trace$cycle), seq_len(60L)),
    all(c("stable", "progressive", "death") %in% names(trace)),
    all(abs(rowSums(trace[c("stable", "progressive", "death")]) - 1) < 1e-12)
  )
  
  # Cycles 1-2: no decrement; cycles 3-25: 1-23 decrements;
  # cycles 26-60: retain all 23 decrements.
  decrement_count <- pmin(pmax(trace$cycle - 2L, 0L), 23L)
  progressive_utility <- tmz_inputs$utilities$progressive -
    decrement_count *
    tmz_inputs$utilities$decrement_per_consecutive_month_of_progression
  
  stable_qalms <- trace$stable * tmz_inputs$utilities$stable *
    tmz_inputs$design$cycle_length_months
  progressive_qalms <- trace$progressive * progressive_utility *
    tmz_inputs$design$cycle_length_months
  death_qalms <- numeric(nrow(trace))
  cycle_qalms <- stable_qalms + progressive_qalms + death_qalms
  
  result <- data.frame(
    treatment = treatment,
    cycle = trace$cycle,
    stable = trace$stable,
    progressive = trace$progressive,
    death = trace$death,
    stable_utility = tmz_inputs$utilities$stable,
    progressive_utility = progressive_utility,
    decrement_count = decrement_count,
    stable_qalms = stable_qalms,
    progressive_qalms = progressive_qalms,
    death_qalms = death_qalms,
    cycle_qalms = cycle_qalms,
    cumulative_qalms = cumsum(cycle_qalms)
  )
  
  stopifnot(
    all(decrement_count[1:2] == 0L),
    identical(as.integer(decrement_count[3:25]), seq_len(23L)),
    all(decrement_count[26:60] == 23L),
    all(progressive_utility[26:60] == progressive_utility[25])
  )
  result
}

qalms_standard_tmz <- calculate_cycle_qalms(
  trace_standard_tmz, "Standard TMZ therapy"
)
qalms_long_term_tmz <- calculate_cycle_qalms(
  trace_long_term_tmz, "Long-term TMZ therapy"
)

# Each treatment table has exactly 60 rows, labelled cycles 1-60.
qalms_by_cycle <- rbind(qalms_standard_tmz, qalms_long_term_tmz)
rownames(qalms_by_cycle) <- NULL

total_qalms <- data.frame(
  treatment = c("Standard TMZ therapy", "Long-term TMZ therapy"),
  total_qalms = c(sum(qalms_standard_tmz$cycle_qalms),
                  sum(qalms_long_term_tmz$cycle_qalms))
)

# Print every cycle and the totals; retain unrounded values in the objects.
print(qalms_standard_tmz, row.names = FALSE, digits = 8, max = 10000)
print(qalms_long_term_tmz, row.names = FALSE, digits = 8, max = 10000)
print(total_qalms, row.names = FALSE, digits = 10)

total_qalys <- data.frame(
  treatment = total_qalms$treatment,
  total_qalys = total_qalms$total_qalms / 12
)
print(total_qalys, row.names = FALSE)

# Step 4: Costs for exactly 60 cycles per treatment.
# Prerequisites: run tmz_inputs.R and tmz_markov_trace.R in this R session.
# Reuses stored costs, initial distribution, and transition matrices.
# COST timing requested here: cycle 1 = initial state (month 0),
# cycle 2 = month 1, ..., cycle 60 = month 59.
# This differs from the earlier end-of-cycle QALM/QALY timing.
# Existing traces and QALM/QALY results are not overwritten.
# Costs are undiscounted; no half-cycle correction is applied.

if (!exists("tmz_inputs") || !exists("transition_matrices") ||
    !exists("initial_distribution")) {
  stop("Run tmz_inputs.R and tmz_markov_trace.R before this script.")
}

calculate_cycle_costs <- function(transition_matrix, cost_inputs, treatment) {
  cycles <- seq_len(60L)
  occupancy <- matrix(NA_real_, nrow = length(cycles), ncol = 3L,
                      dimnames = list(NULL, c("stable", "progressive", "death")))
  occupancy[1L, ] <- initial_distribution[colnames(occupancy)]
  for (cycle in cycles[-1L]) {
    occupancy[cycle, ] <- occupancy[cycle - 1L, ] %*% transition_matrix
  }
  
  # Months 1-3 costs also apply to cycle 1 (initial distribution).
  stable_unit_cost <- ifelse(
    cycles <= 4L, unname(cost_inputs$stable["months_1_to_3"]),
    ifelse(cycles <= 7L, unname(cost_inputs$stable["months_4_to_6"]),
           unname(cost_inputs$stable["subsequent_months"]))
  )
  stable_cost <- occupancy[, "stable"] * stable_unit_cost
  progressive_cost <- occupancy[, "progressive"] * cost_inputs$progressive
  death_cost <- occupancy[, "death"] * cost_inputs$death
  cycle_cost <- stable_cost + progressive_cost + death_cost
  
  result <- data.frame(
    treatment = treatment,
    cycle = cycles,
    month = cycles - 1L,
    stable = occupancy[, "stable"],
    progressive = occupancy[, "progressive"],
    death = occupancy[, "death"],
    stable_unit_cost = stable_unit_cost,
    progressive_unit_cost = cost_inputs$progressive,
    death_unit_cost = cost_inputs$death,
    stable_cost = stable_cost,
    progressive_cost = progressive_cost,
    death_cost = death_cost,
    cycle_cost = cycle_cost,
    cumulative_cost = cumsum(cycle_cost)
  )
  
  stopifnot(
    nrow(result) == 60L,
    identical(result$cycle, seq_len(60L)),
    all(abs(rowSums(occupancy) - 1) < 1e-12),
    all(is.finite(occupancy)), all(occupancy >= -1e-12),
    all(is.finite(cycle_cost)),
    sum(cycles <= 4L) == 4L,
    sum(cycles >= 5L & cycles <= 7L) == 3L,
    sum(cycles >= 8L) == 53L
  )
  result
}

costs_standard_tmz <- calculate_cycle_costs(
  transition_matrices$standard_tmz, tmz_inputs$costs$standard_tmz,
  "Standard TMZ therapy"
)
costs_long_term_tmz <- calculate_cycle_costs(
  transition_matrices$long_term_tmz, tmz_inputs$costs$long_term_tmz,
  "Long-term TMZ therapy"
)
costs_by_cycle <- rbind(costs_standard_tmz, costs_long_term_tmz)
rownames(costs_by_cycle) <- NULL

total_costs <- data.frame(
  treatment = c("Standard TMZ therapy", "Long-term TMZ therapy"),
  total_cost = c(sum(costs_standard_tmz$cycle_cost),
                 sum(costs_long_term_tmz$cycle_cost))
)

# Print all 60 cycles per treatment and totals; objects retain full precision.
print(costs_standard_tmz, row.names = FALSE, digits = 10, max = 10000)
print(costs_long_term_tmz, row.names = FALSE, digits = 10, max = 10000)
print(total_costs, row.names = FALSE, digits = 12)

# Step 5: Incremental cost-effectiveness ratios.
# Run tmz_inputs.R, tmz_markov_trace.R, tmz_qalms.R and tmz_costs.R first.
# Comparison: Long-term TMZ minus Standard TMZ.
# Reuses existing unrounded totals; no input data are redefined.
# Note: existing costs include initial occupancy at cycle 1, whereas existing
# QALMs use end-of-cycle occupancy. These ICERs retain that timing difference.
# Align cost and benefit timing before using results in an HTA submission.

if (!exists("total_costs") || !exists("total_qalms")) {
  stop("Run the preceding cost and QALM scripts before this script.")
}

get_total <- function(results, treatment_name, value_column) {
  index <- which(results$treatment == treatment_name)
  if (length(index) != 1L) stop("Expected exactly one total per treatment.")
  value <- results[[value_column]][index]
  if (length(value) != 1L || !is.finite(value)) stop("Invalid treatment total.")
  value
}

incremental_cost <-
  get_total(total_costs, "Long-term TMZ therapy", "total_cost") -
  get_total(total_costs, "Standard TMZ therapy", "total_cost")
incremental_qalms <-
  get_total(total_qalms, "Long-term TMZ therapy", "total_qalms") -
  get_total(total_qalms, "Standard TMZ therapy", "total_qalms")
incremental_qalys <- incremental_qalms / 12

if (incremental_qalms == 0) {
  stop("ICER is undefined because incremental effectiveness is zero.")
}

# Retain full precision for calculations.
icer_per_qalm <- incremental_cost / incremental_qalms
icer_per_qaly <- incremental_cost / incremental_qalys

icer_results <- data.frame(
  comparison = "Long-term TMZ versus Standard TMZ",
  outcome = c("Cost per QALM gained", "Cost per QALY gained"),
  incremental_cost = incremental_cost,
  incremental_effect = c(incremental_qalms, incremental_qalys),
  icer = c(icer_per_qalm, icer_per_qaly)
)

# Round only the displayed ICERs, with no decimal places.
icer_display <- data.frame(
  outcome = icer_results$outcome,
  ICER = formatC(icer_results$icer, format = "f", digits = 0, big.mark = ",")
)
print(icer_display, row.names = FALSE, right = FALSE)