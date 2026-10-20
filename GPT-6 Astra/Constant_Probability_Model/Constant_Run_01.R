# Step 1: Store supplied inputs. Sourcing this script produces no output.
# No transition matrices, model assumptions, or outcomes are calculated here.
tmz_inputs <- list(
  transition_probabilities = data.frame(
    treatment = c("Standard TMZ therapy", "Long-term TMZ therapy"),
    stable_to_progressive = c(0.0956, 0.0894),
    stable_to_death = c(0.0464, 0.0397),
    progressive_to_death = c(0.0861, 0.0689),
    stringsAsFactors = FALSE
  ),
  utilities = list(
    stable = 0.743,
    progressive = 0.731,
    decrement_per_consecutive_month_of_progression = 0.02
  ),
  costs = data.frame(
    treatment = c("Standard TMZ therapy", "Long-term TMZ therapy"),
    stable_months_1_to_3 = c(1944, 1944),
    stable_months_4_to_6 = c(2453, 2453),
    stable_subsequent_months = c(350, 2453),
    progressive = c(2720, 2720),
    death = c(0, 0),
    stringsAsFactors = FALSE
  ),
  design = list(
    cycle_length_months = 1,
    time_horizon_months = 60
  )
)

# Three-state TMZ Markov model: exactly 60 monthly transitions.
# Standalone base-R script; inputs are stored without printing.
# Assumptions: all patients initially stable; no return from progression;
# death is absorbing; supplied transition probabilities apply each month.
# Each trace row is END-of-cycle occupancy. Initial occupancy is held
# separately, so neither cycle 0 nor cycle 61 appears in either trace.

transition_inputs <- data.frame(
  treatment = c("Standard TMZ therapy", "Long-term TMZ therapy"),
  stable_to_progressive = c(0.0956, 0.0894),
  stable_to_death = c(0.0464, 0.0397),
  progressive_to_death = c(0.0861, 0.0689),
  stringsAsFactors = FALSE
)
cycle_length_months <- 1L
time_horizon_months <- 60L
n_cycles <- as.integer(time_horizon_months / cycle_length_months)
states <- c("stable", "progressive", "death")
initial_distribution <- c(stable = 1, progressive = 0, death = 0)

# Rows denote origin states; columns denote destination states.
make_transition_matrix <- function(stable_to_progressive, stable_to_death,
                                   progressive_to_death) {
  P <- matrix(c(
    1 - stable_to_progressive - stable_to_death,
    stable_to_progressive, stable_to_death,
    0, 1 - progressive_to_death, progressive_to_death,
    0, 0, 1
  ), nrow = 3L, byrow = TRUE, dimnames = list(states, states))
  stopifnot(all(is.finite(P)), all(P >= 0), all(P <= 1),
            all(abs(rowSums(P) - 1) < 1e-12))
  P
}

calculate_trace <- function(P) {
  occupancy <- matrix(NA_real_, nrow = n_cycles, ncol = length(states),
                      dimnames = list(NULL, states))
  current <- initial_distribution
  for (cycle in seq_len(n_cycles)) {
    current <- as.numeric(current %*% P)
    occupancy[cycle, ] <- current
  }
  trace <- data.frame(cycle = seq_len(n_cycles), occupancy,
                      row.names = NULL)
  stopifnot(nrow(trace) == 60L, identical(trace$cycle, 1:60),
            all(is.finite(occupancy)), all(occupancy >= 0),
            all(occupancy <= 1),
            all(abs(rowSums(occupancy) - 1) < 1e-12),
            all(diff(trace$death) >= -1e-12))
  trace
}

transition_matrices <- setNames(
  lapply(seq_len(nrow(transition_inputs)), function(i) {
    p <- transition_inputs[i, ]
    make_transition_matrix(p$stable_to_progressive, p$stable_to_death,
                           p$progressive_to_death)
  }), transition_inputs$treatment
)
markov_traces <- lapply(transition_matrices, calculate_trace)
trace_standard_tmz <- markov_traces[["Standard TMZ therapy"]]
trace_long_term_tmz <- markov_traces[["Long-term TMZ therapy"]]

# Verify the first transition and an independent closed-form result at month 60.
for (i in seq_along(markov_traces)) {
  P <- transition_matrices[[i]]
  tr <- markov_traces[[i]]
  a <- P["stable", "stable"]
  b <- P["progressive", "progressive"]
  expected_stable <- a^n_cycles
  expected_progressive <- P["stable", "progressive"] *
    (b^n_cycles - a^n_cycles) / (b - a)
  stopifnot(
    max(abs(as.numeric(tr[1L, states]) - P["stable", ])) < 1e-12,
    abs(tr$stable[n_cycles] - expected_stable) < 1e-12,
    abs(tr$progressive[n_cycles] - expected_progressive) < 1e-12
  )
}

# Results are proportions. Each object below has exactly 60 rows.
# Uncomment to display or export; input data are never printed.
# print(trace_standard_tmz, row.names = FALSE)
# print(trace_long_term_tmz, row.names = FALSE)
# write.csv(trace_standard_tmz, "trace_standard_tmz.csv", row.names = FALSE)
# write.csv(trace_long_term_tmz, "trace_long_term_tmz.csv", row.names = FALSE)

# Step 3: Calculate undiscounted QALMs using previously defined objects.
# Run tmz_inputs.R and tmz_markov_trace.R first in the same R session.
# This script does not redefine input data or recalculate Markov traces.
# Uses existing end-of-cycle occupancy for cycles 1 through 60.
# Death utility is assumed to be zero.
# The requested decrement is interpreted as 0.02 utility units per month.
# This is the specified CALENDAR-cycle schedule for all progressive patients,
# not an individual time-since-progression calculation for later entrants.
# QALMs = monthly state proportions * utilities * cycle length in months.

stopifnot(
  exists("tmz_inputs"),
  exists("trace_standard_tmz"),
  exists("trace_long_term_tmz")
)

calculate_qalms <- function(trace, inputs) {
  state_columns <- c("stable", "progressive", "death")
  stopifnot(
    all(c("cycle", state_columns) %in% names(trace)),
    nrow(trace) == 60L,
    all(trace$cycle == seq_len(60L)),
    inputs$design$cycle_length_months == 1,
    inputs$design$time_horizon_months == 60
  )
  occupancy <- as.matrix(trace[, state_columns])
  stopifnot(all(is.finite(occupancy)), all(occupancy >= 0),
            all(occupancy <= 1),
            all(abs(rowSums(occupancy) - 1) < 1e-12))
  
  # 0 decrements at cycles 1-2; 1 at cycle 3; 23 at cycles 25-60.
  decrement_count <- pmin(pmax(trace$cycle - 2L, 0L), 23L)
  progressive_utility <- inputs$utilities$progressive -
    inputs$utilities$decrement_per_consecutive_month_of_progression *
    decrement_count
  
  stable_qalms <- trace$stable * inputs$utilities$stable *
    inputs$design$cycle_length_months
  progressive_qalms <- trace$progressive * progressive_utility *
    inputs$design$cycle_length_months
  death_qalms <- trace$death * 0
  cycle_qalms <- stable_qalms + progressive_qalms + death_qalms
  
  result <- data.frame(
    cycle = trace$cycle,
    stable = trace$stable,
    progressive = trace$progressive,
    death = trace$death,
    progressive_decrement_count = decrement_count,
    stable_utility = inputs$utilities$stable,
    progressive_utility = progressive_utility,
    death_utility = 0,
    stable_qalms = stable_qalms,
    progressive_qalms = progressive_qalms,
    death_qalms = death_qalms,
    cycle_qalms = cycle_qalms,
    cumulative_qalms = cumsum(cycle_qalms)
  )
  
  # Check decrement timing and the permanent plateau after cycle 25.
  stopifnot(
    all(decrement_count[1:2] == 0L),
    all(decrement_count[3:25] == seq_len(23L)),
    all(decrement_count[26:60] == 23L),
    all(progressive_utility[26:60] == progressive_utility[25L])
  )
  result
}

qalms_standard_tmz <- calculate_qalms(trace_standard_tmz, tmz_inputs)
qalms_long_term_tmz <- calculate_qalms(trace_long_term_tmz, tmz_inputs)

total_qalms <- data.frame(
  treatment = c("Standard TMZ therapy", "Long-term TMZ therapy"),
  total_qalms = c(sum(qalms_standard_tmz$cycle_qalms),
                  sum(qalms_long_term_tmz$cycle_qalms))
)

# Display all 60 cycles for each treatment and the two totals.
cat("\nStandard TMZ therapy: QALMs by cycle\n")
print(qalms_standard_tmz, row.names = FALSE, max = 10000L)
cat("\nLong-term TMZ therapy: QALMs by cycle\n")
print(qalms_long_term_tmz, row.names = FALSE, max = 10000L)
cat("\nTotal QALMs over cycles 1-60\n")
print(total_qalms, row.names = FALSE)

total_qalys <- data.frame(
  treatment = total_qalms$treatment,
  total_qalys = total_qalms$total_qalms / 12
)
print(total_qalys, row.names = FALSE)

# Calculate undiscounted costs for exactly 60 cycles using existing inputs.
# Run tmz_inputs.R and tmz_markov_trace.R first in the same R session.
# No input data are redefined.
# COST timing requested here differs from the earlier end-of-month traces:
# cycle 1 = initial distribution; cycles 2-60 = months 1-59.
# The existing traces are left unchanged. Their month-60 row is not used.
# Cost bands: cycles 1-4, cycles 5-7, and cycles 8-60.

stopifnot(exists("tmz_inputs"), exists("initial_distribution"),
          exists("trace_standard_tmz"), exists("trace_long_term_tmz"))

calculate_cycle_costs <- function(trace, treatment, inputs, initial) {
  states <- c("stable", "progressive", "death")
  stopifnot(nrow(trace) == 60L, all(trace$cycle == seq_len(60L)),
            all(states %in% names(trace)), all(states %in% names(initial)),
            inputs$design$cycle_length_months == 1,
            inputs$design$time_horizon_months == 60)
  cost_row <- inputs$costs[inputs$costs$treatment == treatment, , drop = FALSE]
  stopifnot(nrow(cost_row) == 1L)
  
  # Align occupancy with the user's initial-state cycle numbering.
  occupancy <- rbind(
    matrix(initial[states], nrow = 1L, dimnames = list(NULL, states)),
    as.matrix(trace[seq_len(59L), states])
  )
  cycle <- seq_len(60L)
  stopifnot(nrow(occupancy) == 60L, all(is.finite(occupancy)),
            all(occupancy >= 0), all(occupancy <= 1),
            all(abs(rowSums(occupancy) - 1) < 1e-12))
  
  stable_unit_cost <- ifelse(
    cycle <= 4L, cost_row$stable_months_1_to_3,
    ifelse(cycle <= 7L, cost_row$stable_months_4_to_6,
           cost_row$stable_subsequent_months)
  )
  stable_cost <- occupancy[, "stable"] * stable_unit_cost
  progressive_cost <- occupancy[, "progressive"] * cost_row$progressive
  death_cost <- occupancy[, "death"] * cost_row$death
  cycle_cost <- stable_cost + progressive_cost + death_cost
  
  result <- data.frame(
    cycle = cycle,
    month = cycle - 1L,
    stable = occupancy[, "stable"],
    progressive = occupancy[, "progressive"],
    death = occupancy[, "death"],
    stable_unit_cost = stable_unit_cost,
    progressive_unit_cost = cost_row$progressive,
    death_unit_cost = cost_row$death,
    stable_cost = stable_cost,
    progressive_cost = progressive_cost,
    death_cost = death_cost,
    cycle_cost = cycle_cost,
    cumulative_cost = cumsum(cycle_cost),
    row.names = NULL
  )
  stopifnot(
    sum(cycle <= 4L) == 4L,
    sum(cycle >= 5L & cycle <= 7L) == 3L,
    sum(cycle >= 8L) == 53L,
    all(result$stable_unit_cost[1:4] == cost_row$stable_months_1_to_3),
    all(result$stable_unit_cost[5:7] == cost_row$stable_months_4_to_6),
    all(result$stable_unit_cost[8:60] == cost_row$stable_subsequent_months)
  )
  result
}

costs_standard_tmz <- calculate_cycle_costs(
  trace_standard_tmz, "Standard TMZ therapy", tmz_inputs, initial_distribution
)
costs_long_term_tmz <- calculate_cycle_costs(
  trace_long_term_tmz, "Long-term TMZ therapy", tmz_inputs, initial_distribution
)
total_costs <- data.frame(
  treatment = c("Standard TMZ therapy", "Long-term TMZ therapy"),
  total_cost = c(sum(costs_standard_tmz$cycle_cost),
                 sum(costs_long_term_tmz$cycle_cost))
)

# All monetary results are expected costs per patient in the supplied currency.
# Preserve full precision for calculations; round only when reporting.
cat("\nStandard TMZ therapy: costs by cycle\n")
print(costs_standard_tmz, row.names = FALSE, max = 10000L)
cat("\nLong-term TMZ therapy: costs by cycle\n")
print(costs_long_term_tmz, row.names = FALSE, max = 10000L)
cat("\nTotal costs over cycles 1-60\n")
print(total_costs, row.names = FALSE)

# Incremental cost-effectiveness: Long-term TMZ versus Standard TMZ.
# Run the previous input, trace, QALM, and cost scripts first.
# Reuses total_costs and total_qalms without redefining input data.
# Timing caveat: existing costs use initial occupancy plus months 1-59,
# whereas existing QALMs use months 1-60. These ICERs preserve those results.

stopifnot(exists("total_costs"), exists("total_qalms"))

get_total <- function(results, treatment_name, value_column) {
  stopifnot(all(c("treatment", value_column) %in% names(results)))
  selected <- results[results$treatment == treatment_name, , drop = FALSE]
  stopifnot(nrow(selected) == 1L)
  value <- selected[[value_column]]
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

icer_per_qalm <- incremental_cost / incremental_qalms
icer_per_qaly <- incremental_cost / incremental_qalys

# Keep full precision in numeric results; round only the displayed ICERs.
icer_results <- data.frame(
  comparison = "Long-term TMZ versus Standard TMZ",
  outcome = c("QALM", "QALY"),
  incremental_cost = incremental_cost,
  incremental_effect = c(incremental_qalms, incremental_qalys),
  icer = c(icer_per_qalm, icer_per_qaly)
)

icer_display <- data.frame(
  outcome = c("Cost per additional QALM", "Cost per additional QALY"),
  ICER = sprintf("%.0f", icer_results$icer)
)
cat("\nLong-term TMZ versus Standard TMZ\n")
print(icer_display, row.names = FALSE, right = FALSE)