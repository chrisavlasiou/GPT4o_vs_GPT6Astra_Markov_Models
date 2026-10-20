# Step 1: Store model inputs without printing
inputs <- list(
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
    standard_tmz = c(
      stable_months_1_to_3 = 1944,
      stable_months_4_to_6 = 2453,
      stable_subsequent_months = 350,
      progressive = 2720,
      death = 0
    ),
    long_term_tmz = c(
      stable_months_1_to_3 = 1944,
      stable_months_4_to_6 = 2453,
      stable_subsequent_months = 2453,
      progressive = 2720,
      death = 0
    )
  ),
  
  design = list(
    cycle_length_months = 1L,
    time_horizon_months = 60L
  )
)

# Three-state cohort Markov model: exactly 60 monthly transitions.
# Assumptions: all patients are stable before cycle 1; no recovery from
# progression; death is absorbing; supplied probabilities are monthly.
# Each trace row is END-of-cycle occupancy. The initial distribution is
# stored separately and is not an extra trace row.
# Base R only. Inputs and results are assigned silently.

transition_probabilities <- list(
  standard_tmz = c(stable_to_progressive = 0.0956,
                   stable_to_death = 0.0464,
                   progressive_to_death = 0.0861),
  long_term_tmz = c(stable_to_progressive = 0.0894,
                    stable_to_death = 0.0397,
                    progressive_to_death = 0.0689)
)
cycle_length_months <- 1L
time_horizon_months <- 60L
n_cycles <- as.integer(time_horizon_months / cycle_length_months)
states <- c("stable", "progressive", "death")
initial_distribution <- c(stable = 1, progressive = 0, death = 0)

# Rows are origin states; columns are destination states.
make_transition_matrix <- function(probabilities) {
  sp <- unname(probabilities["stable_to_progressive"])
  sd <- unname(probabilities["stable_to_death"])
  pd <- unname(probabilities["progressive_to_death"])
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
  current_distribution <- initial_distribution
  for (cycle in seq_len(n_cycles)) {
    current_distribution <- drop(current_distribution %*% transition_matrix)
    occupancy[cycle, ] <- current_distribution
  }
  trace <- data.frame(cycle = seq_len(n_cycles), occupancy,
                      row.names = NULL)
  stopifnot(nrow(trace) == 60L,
            identical(trace$cycle, seq_len(60L)),
            all(is.finite(occupancy)),
            all(occupancy >= 0), all(occupancy <= 1),
            all(abs(rowSums(occupancy) - 1) < 1e-12),
            all(diff(trace$death) >= -1e-12))
  trace
}

transition_matrices <- lapply(transition_probabilities, make_transition_matrix)
markov_traces <- lapply(transition_matrices, calculate_trace)
trace_standard_tmz <- markov_traces$standard_tmz
trace_long_term_tmz <- markov_traces$long_term_tmz

# Each result has exactly 60 rows and columns: cycle, stable, progressive, death.
# Utilities and costs are not needed to calculate state occupancy.
# To inspect results interactively, run:
# View(trace_standard_tmz)
# View(trace_long_term_tmz)

# Calculate cycle-level and total QALMs using existing inputs and traces.
# Run the original inputs assignment and the Markov trace script first.
# No input data or transition probabilities are redefined here.
# Uses end-of-cycle occupancy, with no discounting or half-cycle correction.
# The requested decrement is interpreted as 0.02 utility units per monthly
# step, not an annual QALY loss converted to monthly units.
# This is a model-cycle schedule, not a progression-duration cohort model:
# all progressive patients in a given cycle receive the same utility.
# Death contributes zero QALMs.

stopifnot(exists("inputs"), exists("trace_standard_tmz"),
          exists("trace_long_term_tmz"))

calculate_qalms <- function(trace) {
  stopifnot(nrow(trace) == 60L,
            all(trace$cycle == seq_len(60L)),
            all(c("stable", "progressive", "death") %in% names(trace)))
  occupancy <- as.matrix(trace[, c("stable", "progressive", "death")])
  stopifnot(all(is.finite(occupancy)), all(occupancy >= 0),
            all(abs(rowSums(occupancy) - 1) < 1e-12))
  
  # Cycles 1-2: zero decrements; cycles 3-25: 1 through 23 decrements;
  # cycles 26-60: retain all 23 decrements.
  decrement_count <- pmin(pmax(trace$cycle - 2L, 0L), 23L)
  progressive_utility <- inputs$utilities$progressive -
    decrement_count *
    inputs$utilities$decrement_per_consecutive_month_of_progression
  
  results <- data.frame(
    cycle = trace$cycle,
    stable = trace$stable,
    progressive = trace$progressive,
    death = trace$death,
    stable_utility = inputs$utilities$stable,
    progressive_utility = progressive_utility,
    death_utility = 0
  )
  results$stable_qalms <- results$stable * results$stable_utility *
    inputs$design$cycle_length_months
  results$progressive_qalms <- results$progressive *
    results$progressive_utility * inputs$design$cycle_length_months
  results$death_qalms <- results$death * results$death_utility *
    inputs$design$cycle_length_months
  results$cycle_qalms <- results$stable_qalms +
    results$progressive_qalms + results$death_qalms
  results$cumulative_qalms <- cumsum(results$cycle_qalms)
  
  stopifnot(decrement_count[2L] == 0L,
            decrement_count[3L] == 1L,
            decrement_count[25L] == 23L,
            all(progressive_utility[26:60] == progressive_utility[25L]))
  results
}

qalms_standard_tmz <- calculate_qalms(trace_standard_tmz)
qalms_long_term_tmz <- calculate_qalms(trace_long_term_tmz)

total_qalms <- data.frame(
  treatment = c("Standard TMZ therapy", "Long-term TMZ therapy"),
  total_qalms = c(sum(qalms_standard_tmz$cycle_qalms),
                  sum(qalms_long_term_tmz$cycle_qalms))
)

# Display all 60 cycles for each treatment, followed by totals.
print(qalms_standard_tmz, row.names = FALSE, max = 10000L)
print(qalms_long_term_tmz, row.names = FALSE, max = 10000L)
print(total_qalms, row.names = FALSE)

total_qalys <- data.frame(
  treatment = total_qalms$treatment,
  total_qalys = total_qalms$total_qalms / 12
)

print(total_qalys, row.names = FALSE)

# Cycle-level and total costs, using previously stored inputs.
# Prerequisites: run the original inputs assignment and tmz_markov_trace.R.
# No input data are redefined.
# Timing specified for this calculation:
#   Cycle 1 = initial distribution; cycle 2 = end of month 1;
#   cycle 60 = end of month 59. Exactly 60 charged rows.
# Cost bands: cycles 1-4, cycles 5-7, and cycles 8-60.
# No discounting or half-cycle correction is applied.
# Earlier QALY traces used 60 end-of-cycle rows. These cost-specific traces
# use the revised initial-row convention and do not overwrite those objects.
# Align conventions before combining these costs with QALYs in an ICER.

stopifnot(exists("inputs"), exists("transition_matrices"),
          exists("initial_distribution"))

calculate_cycle_costs <- function(transition_matrix, cost_inputs) {
  state_names <- c("stable", "progressive", "death")
  occupancy <- matrix(NA_real_, nrow = 60L, ncol = 3L,
                      dimnames = list(NULL, state_names))
  occupancy[1L, ] <- initial_distribution[state_names]
  for (cycle in 2:60) {
    occupancy[cycle, ] <- occupancy[cycle - 1L, ] %*%
      transition_matrix[state_names, state_names]
  }
  
  cycle <- seq_len(60L)
  stable_unit_cost <- ifelse(
    cycle <= 4L, unname(cost_inputs["stable_months_1_to_3"]),
    ifelse(cycle <= 7L, unname(cost_inputs["stable_months_4_to_6"]),
           unname(cost_inputs["stable_subsequent_months"]))
  )
  
  results <- data.frame(
    cycle = cycle,
    month = cycle - 1L,
    occupancy,
    stable_unit_cost = stable_unit_cost,
    progressive_unit_cost = unname(cost_inputs["progressive"]),
    death_unit_cost = unname(cost_inputs["death"]),
    row.names = NULL
  )
  # Costs are supplied per monthly cycle; weight by cohort proportions.
  # This deliberately charges the initial distribution in cycle 1.
  results$stable_cost <- results$stable * results$stable_unit_cost
  results$progressive_cost <- results$progressive *
    results$progressive_unit_cost
  results$death_cost <- results$death * results$death_unit_cost
  results$cycle_cost <- results$stable_cost + results$progressive_cost +
    results$death_cost
  results$cumulative_cost <- cumsum(results$cycle_cost)
  
  stopifnot(nrow(results) == 60L,
            identical(results$cycle, seq_len(60L)),
            all(is.finite(occupancy)), all(occupancy >= 0),
            all(abs(rowSums(occupancy) - 1) < 1e-12),
            all(is.finite(results$cycle_cost)),
            sum(cycle <= 4L) == 4L,
            sum(cycle >= 5L & cycle <= 7L) == 3L,
            sum(cycle >= 8L) == 53L)
  results
}

costs_standard_tmz <- calculate_cycle_costs(
  transition_matrices$standard_tmz, inputs$costs$standard_tmz
)
costs_long_term_tmz <- calculate_cycle_costs(
  transition_matrices$long_term_tmz, inputs$costs$long_term_tmz
)

total_costs <- data.frame(
  treatment = c("Standard TMZ therapy", "Long-term TMZ therapy"),
  total_cost = c(sum(costs_standard_tmz$cycle_cost),
                 sum(costs_long_term_tmz$cycle_cost))
)

# Display every cycle for each treatment and the total costs per patient.
print(costs_standard_tmz, row.names = FALSE, max = 10000L)
print(costs_long_term_tmz, row.names = FALSE, max = 10000L)
print(total_costs, row.names = FALSE)

# ICERs: Long-term TMZ therapy versus Standard TMZ therapy.
# Run the earlier QALM and cost scripts first; no inputs are redefined.
# IMPORTANT: existing costs include the initial distribution plus 59
# transitions, while existing QALMs use 60 end-of-cycle distributions.
# These ICERs use those totals as requested. Align timing conventions
# before using the results in an HTA submission.

stopifnot(exists("total_costs"), exists("total_qalms"))
comparators <- c("Standard TMZ therapy", "Long-term TMZ therapy")
stopifnot(!anyDuplicated(total_costs$treatment),
          !anyDuplicated(total_qalms$treatment),
          all(comparators %in% total_costs$treatment),
          all(comparators %in% total_qalms$treatment))

cost <- total_costs$total_cost[match(comparators, total_costs$treatment)]
qalm <- total_qalms$total_qalms[match(comparators, total_qalms$treatment)]
stopifnot(all(is.finite(cost)), all(is.finite(qalm)))

incremental_cost <- cost[2L] - cost[1L]
incremental_qalms <- qalm[2L] - qalm[1L]
incremental_qalys <- incremental_qalms / 12

if (incremental_qalms == 0) {
  stop("ICER is undefined because incremental effectiveness is zero.")
}

# Preserve full precision for the calculation; round only the display.
icer_results <- data.frame(
  outcome = c("Cost per QALM gained", "Cost per QALY gained"),
  incremental_cost = incremental_cost,
  incremental_effect = c(incremental_qalms, incremental_qalys),
  icer = c(incremental_cost / incremental_qalms,
           incremental_cost / incremental_qalys)
)

icer_display <- data.frame(
  outcome = icer_results$outcome,
  ICER = formatC(icer_results$icer, format = "f", digits = 0, big.mark = ",")
)
cat("Long-term TMZ therapy versus Standard TMZ therapy\n")
print(icer_display, row.names = FALSE, right = FALSE)