# Step 1: Store supplied inputs only; sourcing this script prints no data.
# Currency, price year, and subsequent modelling conventions are unspecified.

tmz_inputs <- list(
  states = c("stable", "progressive", "death"),
  transitions = data.frame(
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

# Step 1: Store supplied inputs only; sourcing this script prints no data.
# Currency, price year, and subsequent modelling conventions are unspecified.

tmz_inputs <- list(
  states = c("stable", "progressive", "death"),
  transitions = data.frame(
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

# Step 2: Markov traces, with exactly 60 rows labelled cycles 1 to 60.
# Convention: rows describe occupancy at the START of each monthly cycle.
# Cycle 1 is the initial cohort (100% stable); cycles 2:60 follow 59 updates.
# No cycle 0 or cycle 61 is included.
# Assumptions: supplied probabilities apply per month; death is absorbing;
# progression cannot reverse. Traces are cohort proportions, not counts.

n_cycles <- as.integer(tmz_inputs$design$time_horizon_months /
                         tmz_inputs$design$cycle_length_months)
stopifnot(n_cycles == 60L)
initial_distribution <- c(stable = 1, progressive = 0, death = 0)

make_transition_matrix <- function(p) {
  # Rows are source states; columns are destination states.
  transition_matrix <- matrix(
    c(1 - p$stable_to_progressive - p$stable_to_death,
      p$stable_to_progressive, p$stable_to_death,
      0, 1 - p$progressive_to_death, p$progressive_to_death,
      0, 0, 1),
    nrow = 3, byrow = TRUE,
    dimnames = list(from = tmz_inputs$states, to = tmz_inputs$states)
  )
  stopifnot(all(is.finite(transition_matrix)),
            all(transition_matrix >= 0), all(transition_matrix <= 1),
            all(abs(rowSums(transition_matrix) - 1) < 1e-12))
  transition_matrix
}

calculate_trace <- function(transition_matrix) {
  occupancy <- matrix(0, nrow = n_cycles, ncol = 3,
                      dimnames = list(NULL, tmz_inputs$states))
  occupancy[1, ] <- initial_distribution
  for (cycle in seq.int(2L, n_cycles)) {
    occupancy[cycle, ] <- occupancy[cycle - 1L, ] %*% transition_matrix
  }
  trace <- data.frame(cycle = seq_len(n_cycles), occupancy,
                      row.names = NULL)
  stopifnot(nrow(trace) == 60L,
            identical(trace$cycle, 1:60),
            all(abs(rowSums(occupancy) - 1) < 1e-12),
            all(occupancy >= 0), all(occupancy <= 1),
            all(diff(trace$death) >= -1e-12))
  trace
}

transition_matrices <- setNames(
  lapply(seq_len(nrow(tmz_inputs$transitions)), function(i) {
    make_transition_matrix(tmz_inputs$transitions[i, ])
  }),
  tmz_inputs$transitions$treatment
)
markov_traces <- lapply(transition_matrices, calculate_trace)
trace_standard_tmz <- markov_traces[["Standard TMZ therapy"]]
trace_long_term_tmz <- markov_traces[["Long-term TMZ therapy"]]

# Results remain stored without printing. To inspect, run e.g.:
# View(trace_standard_tmz)
# View(trace_long_term_tmz)

# Step 3: Run after tmz_model.R, with tmz_inputs and markov_traces in memory.
# This script does not redefine the input data or recalculate the traces.
# Cycle 1 is the initial cohort. Exactly 60 start-of-cycle rows are used.
# The requested decrement is interpreted as 0.02 utility units per cycle,
# not an absolute loss of 0.02 QALYs (which would require different units).
# This is a calendar-cycle schedule, not individual time since progression.
# Death has utility zero. No discounting or half-cycle correction is applied.

stopifnot(exists("tmz_inputs"), exists("markov_traces"),
          length(markov_traces) == 2L,
          tmz_inputs$design$cycle_length_months == 1)

calculate_qalms <- function(trace) {
  stopifnot(nrow(trace) == 60L, identical(trace$cycle, 1:60))
  
  # 0 decrements in cycles 1:2; 1:23 in cycles 3:25; 23 thereafter.
  n_decrements <- pmin(pmax(trace$cycle - 2L, 0L), 23L)
  progressive_utility <- tmz_inputs$utilities$progressive -
    tmz_inputs$utilities$decrement_per_consecutive_month_of_progression *
    n_decrements
  
  cycle_results <- data.frame(
    trace,
    n_decrements = n_decrements,
    stable_utility = tmz_inputs$utilities$stable,
    progressive_utility = progressive_utility,
    death_utility = 0
  )
  cycle_results$stable_qalms <- cycle_results$stable *
    cycle_results$stable_utility * tmz_inputs$design$cycle_length_months
  cycle_results$progressive_qalms <- cycle_results$progressive *
    cycle_results$progressive_utility * tmz_inputs$design$cycle_length_months
  cycle_results$death_qalms <- cycle_results$death *
    cycle_results$death_utility * tmz_inputs$design$cycle_length_months
  cycle_results$qalm <- with(cycle_results,
                             stable_qalms + progressive_qalms + death_qalms)
  cycle_results$cumulative_qalms <- cumsum(cycle_results$qalm)
  
  # Check the decrement endpoints and the fixed utility through cycle 60.
  stopifnot(sum(diff(n_decrements) > 0) == 23L,
            abs(progressive_utility[3] - 0.711) < 1e-12,
            abs(progressive_utility[25] - 0.271) < 1e-12,
            all(abs(progressive_utility[25:60] - 0.271) < 1e-12))
  cycle_results
}

qalm_results <- lapply(markov_traces, calculate_qalms)
qalms_standard_tmz <- qalm_results[["Standard TMZ therapy"]]
qalms_long_term_tmz <- qalm_results[["Long-term TMZ therapy"]]

total_qalms <- data.frame(
  treatment = names(qalm_results),
  total_qalms = vapply(qalm_results, function(x) sum(x$qalm), numeric(1)),
  row.names = NULL
)

# Display all 60 cycles for each treatment, followed by the totals.
for (treatment in names(qalm_results)) {
  cat("\n", treatment, "\n", sep = "")
  print(qalm_results[[treatment]], row.names = FALSE, max = 10000)
}
print(total_qalms, row.names = FALSE)

total_qalys <- data.frame(
  treatment = total_qalms$treatment,
  total_qalys = total_qalms$total_qalms / 12
)
print(total_qalys, row.names = FALSE)

# Step 4: Run after tmz_model.R, with tmz_inputs and markov_traces in memory.
# Reuses existing data and the 60-row start-of-cycle traces.
# Cycle 1 is the initial distribution; cycle 2 is month 1.
# As requested, cycle 1 incurs a full cycle of costs.
# Stable cost bands: cycles 1:4, 5:7, and 8:60.
# Costs are per initial patient, in the supplied (unspecified) currency.
# No discounting or half-cycle correction is applied.

stopifnot(exists("tmz_inputs"), exists("markov_traces"),
          length(markov_traces) == 2L)

calculate_cycle_costs <- function(treatment) {
  trace <- markov_traces[[treatment]]
  unit_costs <- tmz_inputs$costs[
    tmz_inputs$costs$treatment == treatment, , drop = FALSE
  ]
  stopifnot(nrow(unit_costs) == 1L, nrow(trace) == 60L,
            identical(trace$cycle, 1:60))
  
  stable_unit_cost <- ifelse(
    trace$cycle <= 4L, unit_costs$stable_months_1_to_3,
    ifelse(trace$cycle <= 7L, unit_costs$stable_months_4_to_6,
           unit_costs$stable_subsequent_months)
  )
  
  results <- data.frame(
    trace,
    month = trace$cycle - 1L,
    stable_unit_cost = stable_unit_cost,
    progressive_unit_cost = unit_costs$progressive,
    death_unit_cost = unit_costs$death
  )
  results$stable_cost <- results$stable * results$stable_unit_cost
  results$progressive_cost <- results$progressive *
    results$progressive_unit_cost
  results$death_cost <- results$death * results$death_unit_cost
  results$cycle_cost <- with(results,
                             stable_cost + progressive_cost + death_cost)
  results$cumulative_cost <- cumsum(results$cycle_cost)
  
  stopifnot(sum(trace$cycle <= 4L) == 4L,
            sum(trace$cycle >= 5L & trace$cycle <= 7L) == 3L,
            sum(trace$cycle >= 8L) == 53L,
            all(is.finite(results$cycle_cost)))
  results
}

cost_results <- setNames(
  lapply(names(markov_traces), calculate_cycle_costs),
  names(markov_traces)
)
costs_standard_tmz <- cost_results[["Standard TMZ therapy"]]
costs_long_term_tmz <- cost_results[["Long-term TMZ therapy"]]

total_costs <- data.frame(
  treatment = names(cost_results),
  total_cost = vapply(cost_results, function(x) sum(x$cycle_cost), numeric(1)),
  row.names = NULL
)

# Display every cycle for both treatments, then the total costs.
# Calculations retain full precision; formatting affects display only.
for (treatment in names(cost_results)) {
  cat("\n", treatment, "\n", sep = "")
  print(cost_results[[treatment]], row.names = FALSE,
        digits = 10, max = 10000)
}
print(total_costs, row.names = FALSE, digits = 10)

# Step 5: Run after tmz_model.R, tmz_qalms.R, and tmz_costs.R.
# Uses existing totals; no input data are redefined.
# Comparison: Long-term TMZ therapy minus Standard TMZ therapy.

stopifnot(exists("total_costs"), exists("total_qalms"))
comparator <- "Standard TMZ therapy"
intervention <- "Long-term TMZ therapy"

get_total <- function(results, treatment_name, value_column) {
  index <- which(results$treatment == treatment_name)
  stopifnot(length(index) == 1L)
  value <- results[[value_column]][index]
  stopifnot(length(value) == 1L, is.finite(value))
  value
}

incremental_cost <-
  get_total(total_costs, intervention, "total_cost") -
  get_total(total_costs, comparator, "total_cost")
incremental_qalms <-
  get_total(total_qalms, intervention, "total_qalms") -
  get_total(total_qalms, comparator, "total_qalms")
incremental_qalys <- incremental_qalms / 12

if (incremental_qalms == 0) {
  stop("ICER is undefined because incremental health benefit is zero.")
}

# Retain full precision for calculations; round only the displayed results.
icer_per_qalm <- incremental_cost / incremental_qalms
icer_per_qaly <- incremental_cost / incremental_qalys
icer_results <- data.frame(
  outcome = c("Cost per QALM gained", "Cost per QALY gained"),
  ICER = c(icer_per_qalm, icer_per_qaly)
)
icer_display <- icer_results
icer_display$ICER <- formatC(icer_results$ICER, format = "f", digits = 0,
                             big.mark = ",")
cat("Long-term TMZ therapy versus Standard TMZ therapy\n")
print(icer_display, row.names = FALSE, right = FALSE)