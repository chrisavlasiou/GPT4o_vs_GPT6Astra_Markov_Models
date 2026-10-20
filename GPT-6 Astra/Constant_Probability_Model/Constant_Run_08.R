# Step 1: Store supplied inputs only. Sourcing this script prints no output.
# Currency is unspecified. No additional modelling assumptions are imposed.

tmz_inputs <- list(
  transition_probabilities = list(
    standard_tmz = c(
      stable_to_progressive = 0.0956,
      stable_to_death = 0.0464,
      progressive_to_death = 0.0861
    ),
    long_term_tmz = c(
      stable_to_progressive = 0.0894,
      stable_to_death = 0.0397,
      progressive_to_death = 0.0689
    )
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
  design = list(
    cycle_length_months = 1,
    time_horizon_months = 60
  )
)

# Step 1: Store supplied inputs only. Sourcing this script prints no output.
# Currency is unspecified. No additional modelling assumptions are imposed.

tmz_inputs <- list(
  transition_probabilities = list(
    standard_tmz = c(
      stable_to_progressive = 0.0956,
      stable_to_death = 0.0464,
      progressive_to_death = 0.0861
    ),
    long_term_tmz = c(
      stable_to_progressive = 0.0894,
      stable_to_death = 0.0397,
      progressive_to_death = 0.0689
    )
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
  design = list(
    cycle_length_months = 1,
    time_horizon_months = 60
  )
)

# Step 2: Monthly cohort Markov traces, exactly cycles 1 through 60.
# Assumptions: supplied probabilities apply to each one-month cycle;
# all patients are stable immediately before cycle 1; no recovery from
# progression; death is absorbing. Rows are END-of-cycle proportions.
# The initial distribution is not included as an extra trace row.

states <- c("stable", "progressive", "death")
n_cycles <- 60L
initial_distribution <- c(stable = 1, progressive = 0, death = 0)
stopifnot(tmz_inputs$design$time_horizon_months /
            tmz_inputs$design$cycle_length_months == n_cycles)

make_transition_matrix <- function(probabilities) {
  sp <- unname(probabilities["stable_to_progressive"])
  sd <- unname(probabilities["stable_to_death"])
  pd <- unname(probabilities["progressive_to_death"])
  # Rows = current state; columns = next state.
  transition_matrix <- matrix(
    c(1 - sp - sd, sp, sd,
      0, 1 - pd, pd,
      0, 0, 1),
    nrow = 3L, byrow = TRUE,
    dimnames = list(from = states, to = states)
  )
  stopifnot(all(is.finite(transition_matrix)),
            all(transition_matrix >= 0), all(transition_matrix <= 1),
            all(abs(rowSums(transition_matrix) - 1) < 1e-12))
  transition_matrix
}

calculate_trace <- function(transition_matrix) {
  occupancy <- matrix(NA_real_, nrow = n_cycles, ncol = length(states),
                      dimnames = list(NULL, states))
  current_distribution <- initial_distribution[states]
  for (cycle in seq_len(n_cycles)) {
    current_distribution <- as.numeric(current_distribution %*%
                                         transition_matrix)
    occupancy[cycle, ] <- current_distribution
  }
  trace <- data.frame(cycle = seq_len(n_cycles), occupancy,
                      row.names = NULL, check.names = FALSE)
  stopifnot(nrow(trace) == 60L,
            identical(trace$cycle, 1:60),
            all(is.finite(occupancy)), all(occupancy >= 0),
            all(occupancy <= 1 + 1e-12),
            all(abs(rowSums(occupancy) - 1) < 1e-12),
            all(diff(trace$death) >= -1e-12))
  trace
}

transition_matrices <- lapply(tmz_inputs$transition_probabilities,
                              make_transition_matrix)
markov_traces <- lapply(transition_matrices, calculate_trace)
trace_standard_tmz <- markov_traces$standard_tmz
trace_long_term_tmz <- markov_traces$long_term_tmz

# Each trace has 60 rows and columns: cycle, stable, progressive, death.
# Objects are stored without automatically printing inputs or results.
# To view the results interactively, run:
# print(trace_standard_tmz, row.names = FALSE)
# print(trace_long_term_tmz, row.names = FALSE)

# Step 3: Calculate per-cycle and total QALMs for both treatments.
# First run/source tmz_markov_traces.R in the same R session.
# This script uses the existing inputs and traces; it does not redefine them.
#
# Interpretation: 0.02 is a decrement in the utility weight, consistent with
# the stored input. It is not an annual QALY loss converted to monthly units.
# Apply the explicitly requested MODEL-CYCLE schedule to all progressive
# occupancy: no decrement in cycles 1-2, 23 decrements in cycles 3-25,
# then hold the cycle-25 utility through cycle 60. This is not individual
# time-since-progression tracking for patients entering progression later.
# Death utility is assumed to be zero. Use the existing end-of-cycle traces,
# with no discounting or half-cycle correction at this step.

if (!exists("tmz_inputs") || !exists("trace_standard_tmz") ||
    !exists("trace_long_term_tmz")) {
  stop("Run tmz_markov_traces.R first to load the inputs and both traces.")
}

calculate_qalms <- function(trace) {
  required_columns <- c("cycle", "stable", "progressive", "death")
  stopifnot(all(required_columns %in% names(trace)),
            nrow(trace) == 60L,
            all(trace$cycle == seq_len(60L)),
            tmz_inputs$design$cycle_length_months == 1)
  occupancy <- as.matrix(trace[, c("stable", "progressive", "death")])
  stopifnot(all(is.finite(occupancy)), all(occupancy >= 0),
            all(abs(rowSums(occupancy) - 1) < 1e-12))
  
  # 0, 0, 1, 2, ..., 23, 23, ..., 23: exactly 23 reductions.
  decrement_count <- pmin(pmax(trace$cycle - 2L, 0L), 23L)
  progressive_utility <- tmz_inputs$utilities$progressive -
    decrement_count *
    tmz_inputs$utilities$decrement_per_consecutive_month_of_progression
  
  results <- trace
  results$progressive_decrement_count <- decrement_count
  results$utility_stable <- tmz_inputs$utilities$stable
  results$utility_progressive <- progressive_utility
  results$utility_death <- 0
  
  # Proportion x utility x months gives QALMs per initial cohort member.
  results$qalm_stable <- results$stable * results$utility_stable *
    tmz_inputs$design$cycle_length_months
  results$qalm_progressive <- results$progressive *
    results$utility_progressive * tmz_inputs$design$cycle_length_months
  results$qalm_death <- results$death * results$utility_death *
    tmz_inputs$design$cycle_length_months
  results$qalm_total <- results$qalm_stable + results$qalm_progressive +
    results$qalm_death
  results$cumulative_qalms <- cumsum(results$qalm_total)
  
  # Verify schedule boundaries and that utility never resets after cycle 25.
  stopifnot(all(decrement_count[1:2] == 0),
            decrement_count[3] == 1, decrement_count[25] == 23,
            all(decrement_count[25:60] == 23),
            sum(diff(decrement_count) == 1) == 23,
            all(progressive_utility[26:60] == progressive_utility[25]))
  results
}

qalms_standard_tmz <- calculate_qalms(trace_standard_tmz)
qalms_long_term_tmz <- calculate_qalms(trace_long_term_tmz)

total_qalms <- data.frame(
  treatment = c("Standard TMZ therapy", "Long-term TMZ therapy"),
  total_qalms = c(sum(qalms_standard_tmz$qalm_total),
                  sum(qalms_long_term_tmz$qalm_total))
)

# Print all 60 cycles for each treatment, then the totals.
# Print precision affects display only, not the underlying calculations.
cat("\nStandard TMZ therapy: cycle results\n")
print(qalms_standard_tmz, row.names = FALSE, digits = 8,
      max = nrow(qalms_standard_tmz) * ncol(qalms_standard_tmz))
cat("\nLong-term TMZ therapy: cycle results\n")
print(qalms_long_term_tmz, row.names = FALSE, digits = 8,
      max = nrow(qalms_long_term_tmz) * ncol(qalms_long_term_tmz))
cat("\nTotal QALMs per initial cohort member over 60 cycles\n")
print(total_qalms, row.names = FALSE, digits = 10)

total_qalms$total_qalys <- total_qalms$total_qalms / 12
print(total_qalms, row.names = FALSE)

# Step 4: Costs for exactly 60 cycles under the requested cost convention.
# Run tmz_markov_traces.R first. Existing input data are reused.
# Cycle 1 = initial distribution (month 0); cycles 2-60 = months 1-59.
# The earlier end-of-cycle traces are shifted for this cost calculation only.
# Earlier QALM/QALY results used a different timing convention and are not
# recalculated here. Align conventions before combining costs and QALYs.
# Costs are per initial cohort member, in the unspecified input currency.
# No discounting or half-cycle correction is applied.

if (!exists("tmz_inputs") || !exists("trace_standard_tmz") ||
    !exists("trace_long_term_tmz") || !exists("initial_distribution")) {
  stop("Run tmz_markov_traces.R first to load inputs, traces and initial distribution.")
}

calculate_cycle_costs <- function(end_of_cycle_trace, treatment_costs) {
  state_names <- c("stable", "progressive", "death")
  stopifnot(nrow(end_of_cycle_trace) == 60L,
            all(end_of_cycle_trace$cycle == seq_len(60L)),
            all(state_names %in% names(end_of_cycle_trace)),
            all(state_names %in% names(initial_distribution)))
  
  # Include the initial distribution, then months 1-59: exactly 60 rows.
  occupancy <- rbind(
    as.numeric(initial_distribution[state_names]),
    as.matrix(end_of_cycle_trace[seq_len(59L), state_names])
  )
  colnames(occupancy) <- state_names
  stopifnot(all(is.finite(occupancy)), all(occupancy >= 0),
            all(abs(rowSums(occupancy) - 1) < 1e-12))
  
  results <- data.frame(cycle = seq_len(60L), month = 0:59,
                        occupancy, row.names = NULL)
  results$cost_band <- ifelse(results$cycle <= 4L, "months_1_to_3",
                              ifelse(results$cycle <= 7L, "months_4_to_6",
                                     "subsequent_months"))
  results$unit_cost_stable <- unname(
    treatment_costs$stable[results$cost_band]
  )
  results$unit_cost_progressive <- treatment_costs$progressive
  results$unit_cost_death <- treatment_costs$death
  
  results$cost_stable <- results$stable * results$unit_cost_stable
  results$cost_progressive <- results$progressive *
    results$unit_cost_progressive
  results$cost_death <- results$death * results$unit_cost_death
  results$cost_total <- results$cost_stable + results$cost_progressive +
    results$cost_death
  results$cumulative_cost <- cumsum(results$cost_total)
  
  stopifnot(nrow(results) == 60L,
            sum(results$cost_band == "months_1_to_3") == 4L,
            sum(results$cost_band == "months_4_to_6") == 3L,
            sum(results$cost_band == "subsequent_months") == 53L,
            all(is.finite(results$cost_total)))
  results
}

costs_standard_tmz <- calculate_cycle_costs(
  trace_standard_tmz, tmz_inputs$costs$standard_tmz
)
costs_long_term_tmz <- calculate_cycle_costs(
  trace_long_term_tmz, tmz_inputs$costs$long_term_tmz
)

total_costs <- data.frame(
  treatment = c("Standard TMZ therapy", "Long-term TMZ therapy"),
  total_cost = c(sum(costs_standard_tmz$cost_total),
                 sum(costs_long_term_tmz$cost_total))
)

# Print all cycle results and totals without rounding stored values.
cat("\nStandard TMZ therapy: cycle costs\n")
print(costs_standard_tmz, row.names = FALSE, digits = 10,
      max = nrow(costs_standard_tmz) * ncol(costs_standard_tmz))
cat("\nLong-term TMZ therapy: cycle costs\n")
print(costs_long_term_tmz, row.names = FALSE, digits = 10,
      max = nrow(costs_long_term_tmz) * ncol(costs_long_term_tmz))
cat("\nTotal costs per initial cohort member\n")
print(total_costs, row.names = FALSE, digits = 12)

# Step 5: ICERs for long-term TMZ versus standard TMZ.
# Run tmz_markov_traces.R, tmz_qalms.R and tmz_costs.R first.
# Reuses existing totals; no input data are redefined.
# IMPORTANT: These ICERs are provisional. Existing QALMs use months 1-60,
# while existing costs use the initial distribution plus months 1-59.
# Align these conventions before using the results for an HTA submission.

if (!exists("total_costs") || !exists("total_qalms")) {
  stop("Run the QALM and cost scripts first to create total_qalms and total_costs.")
}

get_total <- function(results, treatment_name, value_column) {
  stopifnot(all(c("treatment", value_column) %in% names(results)))
  index <- which(results$treatment == treatment_name)
  if (length(index) != 1L) stop("Expected exactly one row per treatment.")
  value <- results[[value_column]][index]
  stopifnot(is.numeric(value), length(value) == 1L, is.finite(value))
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
  stop("ICERs are undefined because incremental effectiveness is zero.")
}

# Preserve full precision in calculations; round only the displayed values.
icer_results <- data.frame(
  outcome = c("QALM", "QALY"),
  incremental_cost = rep(incremental_cost, 2L),
  incremental_effect = c(incremental_qalms, incremental_qalys),
  icer = c(incremental_cost / incremental_qalms,
           incremental_cost / incremental_qalys)
)

icer_display <- data.frame(
  comparison = rep("Long-term TMZ versus Standard TMZ", 2L),
  measure = c("Cost per QALM gained", "Cost per QALY gained"),
  ICER = formatC(icer_results$icer, format = "f", digits = 0, big.mark = ",")
)
cat("\nProvisional ICERs using the existing totals (timing conventions differ)\n")
print(icer_display, row.names = FALSE, right = FALSE)