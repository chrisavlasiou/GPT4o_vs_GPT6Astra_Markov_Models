# Step 1: Store supplied inputs; sourcing this file prints no data.
# No transition matrices, utility-duration rules, or outcomes calculated yet.
tmz_inputs <- list(
  states = c("stable", "progressive", "death"),
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

# Step 1: Store supplied inputs; sourcing this file prints no data.
# Inputs are retained without printing their values.
tmz_inputs <- list(
  states = c("stable", "progressive", "death"),
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

# Step 2: Calculate end-of-cycle Markov traces for exactly 60 monthly cycles.
# Assumptions: all patients start stable before cycle 1; transition
# probabilities apply per monthly cycle and remain constant over time;
# no return from progressive to stable; death is absorbing.
# The initial distribution is kept separately, never as a trace row.
n_cycles <- as.integer(tmz_inputs$design$time_horizon_months /
                         tmz_inputs$design$cycle_length_months)
stopifnot(n_cycles == 60L)
initial_distribution <- c(stable = 1, progressive = 0, death = 0)

make_transition_matrix <- function(p) {
  P <- matrix(
    c(1 - p$stable_to_progressive - p$stable_to_death,
      p$stable_to_progressive, p$stable_to_death,
      0, 1 - p$progressive_to_death, p$progressive_to_death,
      0, 0, 1),
    nrow = 3L, byrow = TRUE,
    dimnames = list(from = tmz_inputs$states, to = tmz_inputs$states)
  )
  stopifnot(all(is.finite(P)), all(P >= 0), all(P <= 1),
            all(abs(rowSums(P) - 1) < 1e-12))
  P
}

calculate_trace <- function(P, initial, n_cycles) {
  occupancy <- matrix(NA_real_, nrow = n_cycles, ncol = length(initial),
                      dimnames = list(NULL, colnames(P)))
  current <- initial[colnames(P)]
  for (cycle in seq_len(n_cycles)) {
    current <- as.numeric(current %*% P)
    occupancy[cycle, ] <- current
  }
  trace <- data.frame(cycle = seq_len(n_cycles), occupancy,
                      row.names = NULL)
  stopifnot(nrow(trace) == 60L, identical(trace$cycle, seq_len(60L)),
            all(is.finite(occupancy)), all(occupancy >= 0),
            all(occupancy <= 1 + 1e-12),
            all(abs(rowSums(occupancy) - 1) < 1e-12))
  trace
}

transition_matrices <- setNames(
  lapply(seq_len(nrow(tmz_inputs$transition_probabilities)), function(i) {
    make_transition_matrix(tmz_inputs$transition_probabilities[i, ])
  }),
  tmz_inputs$transition_probabilities$treatment
)
markov_traces <- lapply(transition_matrices, calculate_trace,
                        initial = initial_distribution, n_cycles = n_cycles)
trace_standard_tmz <- markov_traces[["Standard TMZ therapy"]]
trace_long_term_tmz <- markov_traces[["Long-term TMZ therapy"]]

# Each trace contains exactly 60 rows and columns:
# cycle, stable, progressive, death. Occupancies are cohort proportions.
# Cycle 1 follows one transition; cycle 60 follows 60 transitions.
# Objects are stored silently. To display a trace interactively, use:
# print(trace_standard_tmz)
# print(trace_long_term_tmz)

# Step 3: QALMs using the existing inputs and end-of-cycle traces.
# Run after tmz_model.R in the same R session. No input data are redefined.
# Interpretation: the stored decrement is 0.02 utility points per month,
# not an absolute loss of 0.02 QALYs (which would equal 0.24 QALMs).
# Apply the explicitly requested calendar-cycle schedule to everyone in
# progression; this does not track each patient's time since progression.
# Death utility is assumed zero. No discounting or half-cycle correction.

if (!exists("tmz_inputs") || !exists("trace_standard_tmz") ||
    !exists("trace_long_term_tmz")) {
  stop("Run tmz_model.R first to load the inputs and calculate both traces.")
}

calculate_cycle_qalms <- function(trace, inputs) {
  stopifnot(nrow(trace) == 60L, all(trace$cycle == seq_len(60L)),
            inputs$design$cycle_length_months == 1)
  
  # Cycles 1-2: zero decrements; cycles 3-25: 1 through 23 decrements;
  # cycles 26-60: keep all 23 decrements permanently.
  decrement_count <- pmin(pmax(trace$cycle - 2L, 0L), 23L)
  progressive_utility <- inputs$utilities$progressive -
    inputs$utilities$decrement_per_consecutive_month_of_progression *
    decrement_count
  
  results <- trace
  results$progressive_utility <- progressive_utility
  results$qalm_stable <- trace$stable * inputs$utilities$stable *
    inputs$design$cycle_length_months
  results$qalm_progressive <- trace$progressive * progressive_utility *
    inputs$design$cycle_length_months
  results$qalm_death <- trace$death * 0
  results$qalm_total <- results$qalm_stable + results$qalm_progressive +
    results$qalm_death
  results$cumulative_qalms <- cumsum(results$qalm_total)
  
  # Guard the requested start, stop and permanent utility plateau.
  stopifnot(
    all(decrement_count[1:2] == 0L),
    all(decrement_count[3:25] == seq_len(23L)),
    all(decrement_count[26:60] == 23L),
    all(progressive_utility[26:60] == progressive_utility[25]),
    all(is.finite(results$qalm_total))
  )
  results
}

qalms_standard_tmz <- calculate_cycle_qalms(trace_standard_tmz, tmz_inputs)
qalms_long_term_tmz <- calculate_cycle_qalms(trace_long_term_tmz, tmz_inputs)

total_qalms <- data.frame(
  treatment = c("Standard TMZ therapy", "Long-term TMZ therapy"),
  total_qalms = c(sum(qalms_standard_tmz$qalm_total),
                  sum(qalms_long_term_tmz$qalm_total))
)

# Print all 60 cycles for each treatment, followed by the two totals.
cat("\nStandard TMZ therapy: QALMs per cycle\n")
print(qalms_standard_tmz, row.names = FALSE, max = 10000L)
cat("\nLong-term TMZ therapy: QALMs per cycle\n")
print(qalms_long_term_tmz, row.names = FALSE, max = 10000L)
cat("\nTotal QALMs over cycles 1-60\n")
print(total_qalms, row.names = FALSE)

total_qalms$total_qalys <- total_qalms$total_qalms / 12
print(total_qalms, row.names = FALSE)

# Step 4: Costs for exactly 60 cycles, using existing input data.
# Run after tmz_model.R. No input data are redefined.
# Revised cost timing: cycle 1 = initial distribution; cycle 2 = month 1;
# cycle 60 = month 59. Thus the cost trace uses 59 transitions.
# Earlier end-of-cycle traces/QALYs are not overwritten by this script.
# Costs are undiscounted, without half-cycle correction, per initial patient.

if (!exists("tmz_inputs") || !exists("transition_matrices") ||
    !exists("initial_distribution")) {
  stop("Run tmz_model.R first to load inputs and transition matrices.")
}

calculate_cycle_costs <- function(treatment) {
  P <- transition_matrices[[treatment]]
  unit_costs <- tmz_inputs$costs[
    tmz_inputs$costs$treatment == treatment, , drop = FALSE
  ]
  stopifnot(!is.null(P), nrow(unit_costs) == 1L,
            tmz_inputs$design$cycle_length_months == 1)
  cycles <- seq_len(60L)
  occupancy <- matrix(NA_real_, nrow = 60L, ncol = ncol(P),
                      dimnames = list(NULL, colnames(P)))
  occupancy[1L, ] <- initial_distribution[colnames(P)]
  for (cycle in 2:60) {
    occupancy[cycle, ] <- occupancy[cycle - 1L, ] %*% P
  }
  
  stable_unit_cost <- ifelse(
    cycles <= 4L, unit_costs$stable_months_1_to_3,
    ifelse(cycles <= 7L, unit_costs$stable_months_4_to_6,
           unit_costs$stable_subsequent_months)
  )
  results <- data.frame(cycle = cycles, month = cycles - 1L, occupancy,
                        stable_unit_cost = stable_unit_cost,
                        progressive_unit_cost = unit_costs$progressive,
                        death_unit_cost = unit_costs$death,
                        row.names = NULL)
  results$cost_stable <- results$stable * results$stable_unit_cost
  results$cost_progressive <- results$progressive *
    results$progressive_unit_cost
  results$cost_death <- results$death * results$death_unit_cost
  results$cost_total <- results$cost_stable + results$cost_progressive +
    results$cost_death
  results$cumulative_cost <- cumsum(results$cost_total)
  
  stopifnot(
    nrow(results) == 60L,
    all(abs(rowSums(occupancy) - 1) < 1e-12),
    all(occupancy >= 0), all(is.finite(results$cost_total)),
    all(stable_unit_cost[1:4] == unit_costs$stable_months_1_to_3),
    all(stable_unit_cost[5:7] == unit_costs$stable_months_4_to_6),
    all(stable_unit_cost[8:60] == unit_costs$stable_subsequent_months)
  )
  results
}

costs_standard_tmz <- calculate_cycle_costs("Standard TMZ therapy")
costs_long_term_tmz <- calculate_cycle_costs("Long-term TMZ therapy")
total_costs <- data.frame(
  treatment = c("Standard TMZ therapy", "Long-term TMZ therapy"),
  total_cost = c(sum(costs_standard_tmz$cost_total),
                 sum(costs_long_term_tmz$cost_total))
)

cat("\nStandard TMZ therapy: costs per cycle\n")
print(costs_standard_tmz, row.names = FALSE, max = 10000L)
cat("\nLong-term TMZ therapy: costs per cycle\n")
print(costs_long_term_tmz, row.names = FALSE, max = 10000L)
cat("\nTotal costs over cycles 1-60\n")
print(total_costs, row.names = FALSE, digits = 10)

# Step 5: ICERs for Long-term TMZ versus Standard TMZ.
# Run after tmz_model.R, tmz_qalms.R and tmz_costs.R.
# Reuses existing totals; no input data are redefined.
# IMPORTANT: existing costs use initial occupancy at cycle 1, whereas
# existing QALMs use end-of-cycle occupancy at cycle 1. These ICERs retain
# that timing difference; align timing before using them in an HTA submission.

if (!exists("total_costs") || !exists("total_qalms")) {
  stop("Run tmz_qalms.R and tmz_costs.R first to calculate the totals.")
}

comparison <- merge(total_costs[c("treatment", "total_cost")],
                    total_qalms[c("treatment", "total_qalms")],
                    by = "treatment", all = TRUE)
comparison <- comparison[match(c("Standard TMZ therapy", "Long-term TMZ therapy"),
                               comparison$treatment), ]
stopifnot(nrow(comparison) == 2L, !anyNA(comparison),
          all(is.finite(comparison$total_cost)),
          all(is.finite(comparison$total_qalms)))
comparison$total_qalys <- comparison$total_qalms / 12

incremental_cost <- comparison$total_cost[2] - comparison$total_cost[1]
incremental_qalms <- comparison$total_qalms[2] - comparison$total_qalms[1]
incremental_qalys <- comparison$total_qalys[2] - comparison$total_qalys[1]
if (incremental_qalms == 0 || incremental_qalys == 0) {
  stop("ICER is undefined because incremental effectiveness is zero.")
}

# Keep full precision in calculations; round only the displayed ICERs.
icer_per_qalm <- incremental_cost / incremental_qalms
icer_per_qaly <- incremental_cost / incremental_qalys
icer_results <- data.frame(
  outcome = c("QALM", "QALY"),
  incremental_cost = incremental_cost,
  incremental_effect = c(incremental_qalms, incremental_qalys),
  ICER = c(icer_per_qalm, icer_per_qaly)
)
icer_display <- data.frame(
  measure = c("Cost per additional QALM", "Cost per additional QALY"),
  ICER = formatC(icer_results$ICER, format = "f", digits = 0, big.mark = ",")
)
cat("\nLong-term TMZ therapy versus Standard TMZ therapy\n")
print(icer_display, row.names = FALSE, right = FALSE)