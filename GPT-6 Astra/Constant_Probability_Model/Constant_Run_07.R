# Store model inputs without printing
inputs <- list(
  transitions = list(
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
      stable_months_1_3 = 1944,
      stable_months_4_6 = 2453,
      stable_subsequent_months = 350,
      progressive = 2720,
      death = 0
    ),
    long_term_tmz = c(
      stable_months_1_3 = 1944,
      stable_months_4_6 = 2453,
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

# Three-state TMZ cohort Markov model; base R only.
# Assumptions: supplied probabilities apply to each one-month cycle;
# all patients start stable; no recovery; death is absorbing.
# Each reported row is the END of that cycle: row 1 is after one
# transition and row 60 is after 60 transitions. No cycle 0 is included.

# Store inputs silently.
inputs <- list(
  transitions = list(
    standard_tmz = c(stable_to_progressive = 0.0956,
                     stable_to_death = 0.0464,
                     progressive_to_death = 0.0861),
    long_term_tmz = c(stable_to_progressive = 0.0894,
                      stable_to_death = 0.0397,
                      progressive_to_death = 0.0689)
  ),
  utilities = list(stable = 0.743, progressive = 0.731,
                   decrement_per_consecutive_month_of_progression = 0.02),
  costs = list(
    standard_tmz = c(stable_months_1_3 = 1944, stable_months_4_6 = 2453,
                     stable_subsequent_months = 350, progressive = 2720, death = 0),
    long_term_tmz = c(stable_months_1_3 = 1944, stable_months_4_6 = 2453,
                      stable_subsequent_months = 2453, progressive = 2720, death = 0)
  ),
  design = list(cycle_length_months = 1L, time_horizon_months = 60L)
)

states <- c("stable", "progressive", "death")
initial_distribution <- c(stable = 1, progressive = 0, death = 0)
n_cycles <- as.integer(inputs$design$time_horizon_months /
                         inputs$design$cycle_length_months)
stopifnot(n_cycles == 60L)

# Rows represent the current state; columns represent the next state.
make_transition_matrix <- function(p) {
  sp <- unname(p["stable_to_progressive"])
  sd <- unname(p["stable_to_death"])
  pd <- unname(p["progressive_to_death"])
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
    current_distribution <- as.numeric(current_distribution %*%
                                         transition_matrix)
    occupancy[cycle, ] <- current_distribution
  }
  trace <- data.frame(cycle = seq_len(n_cycles), occupancy,
                      row.names = NULL)
  stopifnot(nrow(trace) == 60L,
            identical(trace$cycle, seq_len(60L)),
            all(is.finite(occupancy)),
            all(occupancy >= 0), all(occupancy <= 1),
            all(abs(rowSums(occupancy) - 1) < 1e-10),
            all(diff(trace$death) >= -1e-12))
  trace
}

transition_matrices <- lapply(inputs$transitions, make_transition_matrix)
markov_traces <- lapply(transition_matrices, calculate_trace)
trace_standard_tmz <- markov_traces$standard_tmz
trace_long_term_tmz <- markov_traces$long_term_tmz

# Inputs and results remain stored without automatic printing.
# To display results interactively, run:
# print(trace_standard_tmz, row.names = FALSE)
# print(trace_long_term_tmz, row.names = FALSE)
# Costs and utilities are retained for later modelling steps and do not
# affect these occupancy traces.

# Run after tmz_markov_trace.R in the same R session.
# Uses existing inputs, trace_standard_tmz and trace_long_term_tmz.
# No input data are redefined.
#
# Apply the explicitly requested calendar-cycle utility schedule to everyone
# occupying the progressive state in that cycle. This is not a patient-level
# time-since-progression schedule, which would require tracking entry cohorts.
# The supplied 0.02 is interpreted as a utility decrement, not an absolute
# QALY loss. Utility times one month gives quality-adjusted life-months (QALMs).
# Death contributes zero QALMs. No discounting or half-cycle correction is used.

stopifnot(exists("inputs"), exists("trace_standard_tmz"),
          exists("trace_long_term_tmz"),
          inputs$design$cycle_length_months == 1)

calculate_qalms <- function(trace) {
  stopifnot(nrow(trace) == 60L,
            identical(as.integer(trace$cycle), seq_len(60L)),
            all(c("stable", "progressive", "death") %in% names(trace)))
  occupancy <- as.matrix(trace[c("stable", "progressive", "death")])
  stopifnot(all(is.finite(occupancy)), all(occupancy >= 0),
            all(abs(rowSums(occupancy) - 1) < 1e-10))
  
  # Cycles 1-2: zero decrements; cycles 3-25: 1-23 decrements;
  # cycles 26-60: retain the 23 decrements reached at cycle 25.
  decrement_count <- pmin(pmax(trace$cycle - 2L, 0L), 23L)
  progressive_utility <- inputs$utilities$progressive -
    decrement_count *
    inputs$utilities$decrement_per_consecutive_month_of_progression
  
  result <- trace
  result$progressive_decrement_count <- decrement_count
  result$stable_utility <- inputs$utilities$stable
  result$progressive_utility <- progressive_utility
  result$death_utility <- 0
  result$stable_qalms <- result$stable * result$stable_utility *
    inputs$design$cycle_length_months
  result$progressive_qalms <- result$progressive *
    result$progressive_utility * inputs$design$cycle_length_months
  result$death_qalms <- result$death * result$death_utility *
    inputs$design$cycle_length_months
  result$cycle_qalms <- result$stable_qalms + result$progressive_qalms +
    result$death_qalms
  result$cumulative_qalms <- cumsum(result$cycle_qalms)
  
  stopifnot(decrement_count[2] == 0L, decrement_count[3] == 1L,
            decrement_count[25] == 23L,
            all(decrement_count[26:60] == 23L),
            all(progressive_utility[26:60] == progressive_utility[25]))
  result
}

qalms_standard_tmz <- calculate_qalms(trace_standard_tmz)
qalms_long_term_tmz <- calculate_qalms(trace_long_term_tmz)

total_qalms <- data.frame(
  treatment = c("Standard TMZ therapy", "Long-term TMZ therapy"),
  total_qalms = c(sum(qalms_standard_tmz$cycle_qalms),
                  sum(qalms_long_term_tmz$cycle_qalms))
)

# Print all 60 cycle results for each treatment, followed by totals.
# Underlying results retain full numerical precision.
cat("\nStandard TMZ therapy: QALMs per patient\n")
print(qalms_standard_tmz, row.names = FALSE, digits = 8, max = 10000)
cat("\nLong-term TMZ therapy: QALMs per patient\n")
print(qalms_long_term_tmz, row.names = FALSE, digits = 8, max = 10000)
cat("\nTotal QALMs per patient over cycles 1-60\n")
print(total_qalms, row.names = FALSE, digits = 10)

total_qalys <- data.frame(
  treatment = total_qalms$treatment,
  total_qalys = total_qalms$total_qalms / 12
)

print(total_qalys, row.names = FALSE)

# Run after tmz_markov_trace.R in the same R session.
# Reuses existing inputs and traces without redefining the input data.
# Cost timing requested here differs from the earlier QALM calculation:
# cycle 1 = initial distribution; cycles 2-60 = months 1-59.
# Stable cost bands: cycles 1-4, 5-7, and 8-60.
# Costs are undiscounted, per patient, in the supplied currency units.
# Progressive and death costs are state-occupancy costs per cycle.

stopifnot(exists("inputs"), exists("initial_distribution"),
          exists("trace_standard_tmz"), exists("trace_long_term_tmz"),
          inputs$design$cycle_length_months == 1)

calculate_costs <- function(end_of_cycle_trace, unit_costs) {
  state_names <- c("stable", "progressive", "death")
  stopifnot(nrow(end_of_cycle_trace) == 60L,
            identical(as.integer(end_of_cycle_trace$cycle), seq_len(60L)),
            all(state_names %in% names(end_of_cycle_trace)))
  
  # Prepend the initial distribution and retain only transitions 1-59.
  # This gives exactly 60 rows, with no extra cycle included.
  occupancy <- rbind(
    initial_distribution[state_names],
    as.matrix(end_of_cycle_trace[seq_len(59L), state_names])
  )
  result <- data.frame(cycle = seq_len(60L), month = 0:59,
                       occupancy, row.names = NULL)
  result$stable_unit_cost <- ifelse(result$cycle <= 4L,
                                    unname(unit_costs["stable_months_1_3"]),
                                    ifelse(result$cycle <= 7L,
                                           unname(unit_costs["stable_months_4_6"]),
                                           unname(unit_costs["stable_subsequent_months"])))
  result$progressive_unit_cost <- unname(unit_costs["progressive"])
  result$death_unit_cost <- unname(unit_costs["death"])
  
  result$stable_cost <- result$stable * result$stable_unit_cost
  result$progressive_cost <- result$progressive *
    result$progressive_unit_cost
  result$death_cost <- result$death * result$death_unit_cost
  result$cycle_cost <- result$stable_cost + result$progressive_cost +
    result$death_cost
  result$cumulative_cost <- cumsum(result$cycle_cost)
  
  stopifnot(nrow(result) == 60L,
            all(is.finite(occupancy)), all(occupancy >= 0),
            all(abs(rowSums(occupancy) - 1) < 1e-10),
            sum(result$cycle <= 4L) == 4L,
            sum(result$cycle >= 5L & result$cycle <= 7L) == 3L,
            sum(result$cycle >= 8L) == 53L,
            all(is.finite(result$cycle_cost)))
  result
}

costs_standard_tmz <- calculate_costs(
  trace_standard_tmz, inputs$costs$standard_tmz)
costs_long_term_tmz <- calculate_costs(
  trace_long_term_tmz, inputs$costs$long_term_tmz)

total_costs <- data.frame(
  treatment = c("Standard TMZ therapy", "Long-term TMZ therapy"),
  total_cost = c(sum(costs_standard_tmz$cycle_cost),
                 sum(costs_long_term_tmz$cycle_cost))
)

# Print all cycle results and totals; calculations remain unrounded.
cat("\nStandard TMZ therapy: costs per patient\n")
print(costs_standard_tmz, row.names = FALSE, digits = 10, max = 10000)
cat("\nLong-term TMZ therapy: costs per patient\n")
print(costs_long_term_tmz, row.names = FALSE, digits = 10, max = 10000)
cat("\nTotal costs per patient over cycles 1-60\n")
print(total_costs, row.names = FALSE, digits = 12)

# Run after the existing QALM and cost scripts in the same R session.
# Uses stored totals without redefining input data.
# Comparator: Standard TMZ therapy. Intervention: Long-term TMZ therapy.
# IMPORTANT: existing costs use initial occupancy plus 59 transitions;
# existing QALMs use 60 end-of-cycle occupancies. These ICERs use those
# totals as requested; align timing before using results for an HTA submission.

stopifnot(exists("total_costs"), exists("total_qalms"))
comparison_order <- c("Standard TMZ therapy", "Long-term TMZ therapy")
cost_index <- match(comparison_order, total_costs$treatment)
qalm_index <- match(comparison_order, total_qalms$treatment)
stopifnot(!anyNA(cost_index), !anyNA(qalm_index),
          !anyDuplicated(total_costs$treatment),
          !anyDuplicated(total_qalms$treatment))

cost_values <- total_costs$total_cost[cost_index]
qalm_values <- total_qalms$total_qalms[qalm_index]
qaly_values <- qalm_values / 12
stopifnot(all(is.finite(c(cost_values, qalm_values, qaly_values))))

incremental_cost <- cost_values[2] - cost_values[1]
incremental_qalms <- qalm_values[2] - qalm_values[1]
incremental_qalys <- qaly_values[2] - qaly_values[1]
if (incremental_qalms == 0 || incremental_qalys == 0) {
  stop("ICER is undefined because incremental health benefit is zero.")
}

# Retain full precision for all calculations.
icer_results <- data.frame(
  outcome = c("QALM", "QALY"),
  incremental_cost = rep(incremental_cost, 2),
  incremental_effect = c(incremental_qalms, incremental_qalys),
  icer = c(incremental_cost / incremental_qalms,
           incremental_cost / incremental_qalys)
)

# Round only for display; show no decimal places.
icer_display <- data.frame(
  measure = c("Cost per additional QALM", "Cost per additional QALY"),
  ICER = formatC(icer_results$icer, format = "f", digits = 0,
                 big.mark = ",")
)
cat("\nLong-term TMZ versus Standard TMZ\n")
print(icer_display, row.names = FALSE, right = FALSE)
cat("\nTiming note: costs and health outcomes use different occupancy timing;\n",
    "these ICERs retain the previously calculated totals.\n", sep = "")