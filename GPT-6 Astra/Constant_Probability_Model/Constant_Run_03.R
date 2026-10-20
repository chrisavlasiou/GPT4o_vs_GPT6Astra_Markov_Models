# Step 1: Store supplied inputs silently; no model calculations yet.
# Costs are recorded as supplied; currency and price year are unspecified.

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
    cycle_length_months = 1L,
    time_horizon_months = 60L
  )
)

# Step 1: Store supplied inputs silently; no model calculations yet.
# Costs are recorded as supplied; currency and price year are unspecified.

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
    cycle_length_months = 1L,
    time_horizon_months = 60L
  )
)

# Step 2: Calculate end-of-cycle occupancy for exactly cycles 1 through 60.
# Assumptions: all patients start stable immediately before cycle 1;
# monthly probabilities are constant; no return from progression to stable;
# death is absorbing. Initial occupancy is not included as a trace row.
# Utilities and costs do not affect these state occupancy calculations.

n_cycles <- as.integer(tmz_inputs$design$time_horizon_months /
                         tmz_inputs$design$cycle_length_months)
stopifnot(n_cycles == 60L)
initial_state <- c(stable = 1, progressive = 0, death = 0)

make_transition_matrix <- function(p) {
  transition_matrix <- matrix(
    c(1 - p$stable_to_progressive - p$stable_to_death,
      p$stable_to_progressive, p$stable_to_death,
      0, 1 - p$progressive_to_death, p$progressive_to_death,
      0, 0, 1),
    nrow = 3L, byrow = TRUE,
    dimnames = list(from = tmz_inputs$states, to = tmz_inputs$states)
  )
  stopifnot(all(is.finite(transition_matrix)),
            all(transition_matrix >= 0), all(transition_matrix <= 1),
            all(abs(rowSums(transition_matrix) - 1) < 1e-12))
  transition_matrix
}

calculate_trace <- function(transition_matrix) {
  occupancy <- matrix(NA_real_, nrow = n_cycles, ncol = 3L,
                      dimnames = list(NULL, tmz_inputs$states))
  current_state <- initial_state
  for (cycle in seq_len(n_cycles)) {
    current_state <- as.numeric(current_state %*% transition_matrix)
    occupancy[cycle, ] <- current_state
  }
  trace <- data.frame(cycle = seq_len(n_cycles), occupancy,
                      row.names = NULL, check.names = FALSE)
  stopifnot(nrow(trace) == 60L, identical(trace$cycle, 1:60),
            all(is.finite(occupancy)), all(occupancy >= -1e-12),
            all(occupancy <= 1 + 1e-12),
            all(abs(rowSums(occupancy) - 1) < 1e-12),
            all(diff(trace$death) >= -1e-12))
  trace
}

transition_matrices <- setNames(
  lapply(seq_len(nrow(tmz_inputs$transitions)), function(i) {
    make_transition_matrix(tmz_inputs$transitions[i, , drop = FALSE])
  }),
  tmz_inputs$transitions$treatment
)
markov_traces <- lapply(transition_matrices, calculate_trace)
standard_tmz_trace <- markov_traces[["Standard TMZ therapy"]]
long_term_tmz_trace <- markov_traces[["Long-term TMZ therapy"]]

# Each output contains 60 rows of cohort proportions, with no cycle 0 or 61.
# Inputs and results are stored silently. To inspect results interactively:
# View(standard_tmz_trace)
# View(long_term_tmz_trace)

# Step 3: Calculate QALMs using existing inputs and Markov traces.
# Run tmz_markov_trace.R first in the same R session.
# This script does not redefine the input data or recalculate the traces.
# The requested decrement is interpreted as 0.02 utility units per month,
# rather than a literal subtraction of 0.02 annual QALYs.
# The requested schedule uses model cycle, not individual time in progression:
# all progressive patients in a given cycle receive the same utility.
# Death utility is assumed to be zero. No discounting or half-cycle correction.

if (!exists("tmz_inputs") || !exists("standard_tmz_trace") ||
    !exists("long_term_tmz_trace")) {
  stop("Run tmz_markov_trace.R first to load inputs and the two traces.")
}

calculate_cycle_qalms <- function(trace) {
  stopifnot(nrow(trace) == 60L, identical(as.integer(trace$cycle), 1:60),
            tmz_inputs$design$cycle_length_months == 1)
  state_proportions <- as.matrix(trace[c("stable", "progressive", "death")])
  stopifnot(all(is.finite(state_proportions)),
            all(state_proportions >= -1e-12),
            all(abs(rowSums(state_proportions) - 1) < 1e-12))
  
  # 0 decrements at cycles 1-2; 1 at cycle 3; 23 at cycle 25 onward.
  decrement_count <- pmin(pmax(trace$cycle - 2L, 0L), 23L)
  progressive_utility <- tmz_inputs$utilities$progressive -
    tmz_inputs$utilities$decrement_per_consecutive_month_of_progression *
    decrement_count
  
  result <- trace
  result$progressive_decrement_count <- decrement_count
  result$stable_utility <- tmz_inputs$utilities$stable
  result$progressive_utility <- progressive_utility
  result$death_utility <- 0
  result$stable_qalms <- result$stable * result$stable_utility *
    tmz_inputs$design$cycle_length_months
  result$progressive_qalms <- result$progressive * result$progressive_utility *
    tmz_inputs$design$cycle_length_months
  result$death_qalms <- result$death * result$death_utility *
    tmz_inputs$design$cycle_length_months
  result$cycle_qalms <- result$stable_qalms + result$progressive_qalms +
    result$death_qalms
  result$cumulative_qalms <- cumsum(result$cycle_qalms)
  
  stopifnot(decrement_count[2] == 0, decrement_count[3] == 1,
            decrement_count[25] == 23,
            all(progressive_utility[25:60] == progressive_utility[25]))
  result
}

standard_tmz_qalms <- calculate_cycle_qalms(standard_tmz_trace)
long_term_tmz_qalms <- calculate_cycle_qalms(long_term_tmz_trace)

total_qalms <- data.frame(
  treatment = names(transition_matrices),
  total_qalms = c(sum(standard_tmz_qalms$cycle_qalms),
                  sum(long_term_tmz_qalms$cycle_qalms)),
  row.names = NULL
)

# Display all 60 cycles for each treatment and their undiscounted totals.
print(standard_tmz_qalms, row.names = FALSE, max = nrow(standard_tmz_qalms) *
        ncol(standard_tmz_qalms))
print(long_term_tmz_qalms, row.names = FALSE, max = nrow(long_term_tmz_qalms) *
        ncol(long_term_tmz_qalms))
print(total_qalms, row.names = FALSE)

total_qalms$total_qalys <- total_qalms$total_qalms / 12
print(total_qalms[c("treatment", "total_qalys")], row.names = FALSE)

# Step 4: Calculate undiscounted costs using existing inputs and traces.
# Run tmz_markov_trace.R first. No input data are redefined here.
# Cost cycle 1 = initial distribution (month 0).
# Cost cycles 2:60 = existing end-of-month trace rows 1:59.
# Exactly 60 cost cycles are included; month 60 occupancy is not included.
# This differs from the earlier QALM calculation's end-of-month 1:60 timing.
# Costs use the supplied currency units; no half-cycle correction is applied.

if (!exists("tmz_inputs") || !exists("initial_state") ||
    !exists("standard_tmz_trace") || !exists("long_term_tmz_trace")) {
  stop("Run tmz_markov_trace.R first to load inputs and traces.")
}

calculate_cycle_costs <- function(trace, treatment) {
  stopifnot(nrow(trace) == 60L, identical(as.integer(trace$cycle), 1:60))
  unit_costs <- tmz_inputs$costs[
    tmz_inputs$costs$treatment == treatment, , drop = FALSE]
  stopifnot(nrow(unit_costs) == 1L)
  states <- tmz_inputs$states
  occupancy <- rbind(initial_state[states],
                     as.matrix(trace[seq_len(59L), states, drop = FALSE]))
  rownames(occupancy) <- NULL
  stopifnot(nrow(occupancy) == 60L, all(is.finite(occupancy)),
            all(occupancy >= -1e-12),
            all(abs(rowSums(occupancy) - 1) < 1e-12))
  result <- data.frame(cycle = seq_len(60L), month = 0:59,
                       occupancy, row.names = NULL)
  result$stable_unit_cost <- ifelse(
    result$cycle <= 4L, unit_costs$stable_months_1_to_3,
    ifelse(result$cycle <= 7L, unit_costs$stable_months_4_to_6,
           unit_costs$stable_subsequent_months))
  result$progressive_unit_cost <- unit_costs$progressive
  result$death_unit_cost <- unit_costs$death
  result$stable_cost <- result$stable * result$stable_unit_cost
  result$progressive_cost <- result$progressive * result$progressive_unit_cost
  result$death_cost <- result$death * result$death_unit_cost
  result$cycle_cost <- result$stable_cost + result$progressive_cost +
    result$death_cost
  result$cumulative_cost <- cumsum(result$cycle_cost)
  stopifnot(sum(result$cycle <= 4L) == 4L,
            sum(result$cycle >= 5L & result$cycle <= 7L) == 3L,
            sum(result$cycle >= 8L) == 53L)
  result
}

standard_tmz_costs <- calculate_cycle_costs(
  standard_tmz_trace, "Standard TMZ therapy")
long_term_tmz_costs <- calculate_cycle_costs(
  long_term_tmz_trace, "Long-term TMZ therapy")
total_costs <- data.frame(
  treatment = c("Standard TMZ therapy", "Long-term TMZ therapy"),
  total_cost = c(sum(standard_tmz_costs$cycle_cost),
                 sum(long_term_tmz_costs$cycle_cost))
)

# Show every cost cycle, then totals. Retain full precision when summing.
print(standard_tmz_costs, row.names = FALSE,
      max = nrow(standard_tmz_costs) * ncol(standard_tmz_costs))
print(long_term_tmz_costs, row.names = FALSE,
      max = nrow(long_term_tmz_costs) * ncol(long_term_tmz_costs))
print(total_costs, row.names = FALSE)

# Step 5: Incremental cost-effectiveness of Long-term versus Standard TMZ.
# Run tmz_qalms.R and tmz_costs.R first, after tmz_markov_trace.R.
# Reuse existing totals without redefining inputs or rounding calculations.
# Timing note: existing costs cover initial occupancy plus months 1:59;
# existing QALMs cover end-of-month occupancy for months 1:60.
# These ICERs reproduce those totals; timing should be aligned before HTA use.

if (!exists("total_costs") || !exists("total_qalms")) {
  stop("Run tmz_qalms.R and tmz_costs.R first to calculate the totals.")
}

extract_total <- function(table, treatment_name, value_column) {
  value <- table[table$treatment == treatment_name, value_column]
  if (length(value) != 1L || !is.finite(value)) {
    stop(paste("Expected one finite", value_column, "for", treatment_name))
  }
  value
}

incremental_cost <-
  extract_total(total_costs, "Long-term TMZ therapy", "total_cost") -
  extract_total(total_costs, "Standard TMZ therapy", "total_cost")
incremental_qalms <-
  extract_total(total_qalms, "Long-term TMZ therapy", "total_qalms") -
  extract_total(total_qalms, "Standard TMZ therapy", "total_qalms")
incremental_qalys <- incremental_qalms / 12

if (incremental_qalms == 0) {
  stop("ICER is undefined because incremental health benefit is zero.")
}

# Full-precision numeric results remain available for subsequent analysis.
icer_results <- data.frame(
  outcome = c("QALM", "QALY"),
  incremental_cost = incremental_cost,
  incremental_effect = c(incremental_qalms, incremental_qalys),
  icer = c(incremental_cost / incremental_qalms,
           incremental_cost / incremental_qalys)
)

# Format only the displayed ICERs: whole numbers, no decimal places.
icer_display <- data.frame(
  comparison = "Long-term TMZ versus Standard TMZ",
  measure = c("Cost per QALM gained", "Cost per QALY gained"),
  ICER = formatC(icer_results$icer, format = "f", digits = 0, big.mark = ",")
)
print(icer_display, row.names = FALSE, right = FALSE)