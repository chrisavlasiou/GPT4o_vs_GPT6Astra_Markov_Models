# Step 1: Store supplied inputs. Sourcing this script produces no output.
# Values are retained as supplied; modelling conventions are deferred.
tmz_inputs <- list(
  transition_probabilities = list(
    standard_tmz = c(stable_to_progressive = 0.0956,
                     stable_to_death = 0.0464,
                     progressive_to_death = 0.0861),
    long_term_tmz = c(stable_to_progressive = 0.0894,
                      stable_to_death = 0.0397,
                      progressive_to_death = 0.0689)
  ),
  utilities = list(
    health_state = c(stable = 0.743, progressive = 0.731),
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

# Step 1: Store supplied inputs. Sourcing this script produces no output.
# Values are retained as supplied; modelling conventions are deferred.
tmz_inputs <- list(
  transition_probabilities = list(
    standard_tmz = c(stable_to_progressive = 0.0956,
                     stable_to_death = 0.0464,
                     progressive_to_death = 0.0861),
    long_term_tmz = c(stable_to_progressive = 0.0894,
                      stable_to_death = 0.0397,
                      progressive_to_death = 0.0689)
  ),
  utilities = list(
    health_state = c(stable = 0.743, progressive = 0.731),
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

# Step 2: Monthly Markov traces (base R; no packages required).
# Assumptions: all patients start stable; probabilities are monthly and constant;
# progression is irreversible; death is absorbing.
# Cycle k reports END-of-month occupancy after k transitions.
# The initial cohort is held separately, so no cycle-0 row is included.

states <- c("stable", "progressive", "death")
initial_cohort <- c(stable = 1, progressive = 0, death = 0)
n_cycles <- as.integer(tmz_inputs$design$time_horizon_months /
                         tmz_inputs$design$cycle_length_months)
stopifnot(n_cycles == 60L)

make_transition_matrix <- function(p) {
  sp <- unname(p["stable_to_progressive"])
  sd <- unname(p["stable_to_death"])
  pd <- unname(p["progressive_to_death"])
  # Rows are origin states; columns are destination states.
  transition <- matrix(
    c(1 - sp - sd, sp, sd,
      0, 1 - pd, pd,
      0, 0, 1),
    nrow = 3L, byrow = TRUE,
    dimnames = list(from = states, to = states)
  )
  stopifnot(all(is.finite(transition)),
            all(transition >= 0), all(transition <= 1),
            all(abs(rowSums(transition) - 1) < 1e-12))
  transition
}

calculate_trace <- function(transition) {
  occupancy <- matrix(NA_real_, nrow = n_cycles, ncol = length(states),
                      dimnames = list(NULL, states))
  cohort <- initial_cohort
  for (cycle in seq_len(n_cycles)) {
    cohort <- drop(cohort %*% transition)
    occupancy[cycle, ] <- cohort
  }
  trace <- data.frame(cycle = seq_len(n_cycles), occupancy,
                      row.names = NULL)
  stopifnot(nrow(trace) == 60L,
            identical(trace$cycle, seq_len(60L)),
            all(is.finite(occupancy)),
            all(occupancy >= -1e-12), all(occupancy <= 1 + 1e-12),
            all(abs(rowSums(occupancy) - 1) < 1e-12),
            all(diff(trace$death) >= -1e-12))
  trace
}

transition_matrices <- lapply(tmz_inputs$transition_probabilities,
                              make_transition_matrix)
markov_traces <- lapply(transition_matrices, calculate_trace)

# Each data frame has exactly 60 rows and four columns:
# cycle, stable, progressive, death. Occupancies are cohort proportions.
trace_standard_tmz <- markov_traces$standard_tmz
trace_long_term_tmz <- markov_traces$long_term_tmz

# Optional display (inputs remain unprinted):
# print(trace_standard_tmz, row.names = FALSE)
# print(trace_long_term_tmz, row.names = FALSE)

# Step 3: Calculate cycle-specific and total QALMs using existing objects.
# Run tmz_markov_trace.R first in the same R session.
# Input data and Markov traces are not redefined here.
# Convention: apply the requested decrement by MODEL CYCLE, not by
# individual duration since progression. Interpret 0.02 as a utility
# decrement per specified cycle; death utility is assumed to be zero.
# Use the existing end-of-cycle occupancy, without half-cycle correction
# or discounting. One monthly cycle contributes occupancy * utility QALMs.

if (!exists("tmz_inputs") || !exists("markov_traces")) {
  stop("Run tmz_markov_trace.R before sourcing this script.")
}
stopifnot(tmz_inputs$design$cycle_length_months == 1,
          tmz_inputs$design$time_horizon_months == 60,
          all(c("standard_tmz", "long_term_tmz") %in% names(markov_traces)))

calculate_qalms <- function(trace) {
  stopifnot(nrow(trace) == 60L,
            all(trace$cycle == seq_len(60L)),
            all(c("stable", "progressive", "death") %in% names(trace)))
  
  # Cycles 1-2: zero decrements; cycles 3-25: 1-23 decrements;
  # cycles 26-60: hold at 23 decrements (no reset or further decrease).
  decrement_count <- pmin(pmax(trace$cycle - 2L, 0L), 23L)
  stable_utility <- unname(tmz_inputs$utilities$health_state["stable"])
  progressive_utility <-
    unname(tmz_inputs$utilities$health_state["progressive"]) -
    tmz_inputs$utilities$decrement_per_consecutive_month_of_progression *
    decrement_count
  death_utility <- 0
  
  cycle_months <- tmz_inputs$design$cycle_length_months
  stable_qalms <- trace$stable * stable_utility * cycle_months
  progressive_qalms <- trace$progressive * progressive_utility * cycle_months
  death_qalms <- trace$death * death_utility * cycle_months
  cycle_qalms <- stable_qalms + progressive_qalms + death_qalms
  
  result <- data.frame(
    cycle = trace$cycle,
    stable = trace$stable,
    progressive = trace$progressive,
    death = trace$death,
    stable_utility = stable_utility,
    progressive_utility = progressive_utility,
    death_utility = death_utility,
    stable_qalms = stable_qalms,
    progressive_qalms = progressive_qalms,
    death_qalms = death_qalms,
    cycle_qalms = cycle_qalms,
    cumulative_qalms = cumsum(cycle_qalms)
  )
  stopifnot(all(decrement_count[1:2] == 0L),
            all(decrement_count[3:25] == seq_len(23L)),
            all(decrement_count[26:60] == 23L),
            all(result$progressive_utility[26:60] ==
                  result$progressive_utility[25]),
            all(is.finite(result$cycle_qalms)))
  result
}

qalm_results <- lapply(markov_traces, calculate_qalms)
qalm_totals <- data.frame(
  treatment = names(qalm_results),
  total_qalms = vapply(qalm_results, function(x) sum(x$cycle_qalms), numeric(1)),
  row.names = NULL
)

# Display all 60 cycles for each treatment, followed by total QALMs.
# Rounding affects display only; calculations retain full precision.
for (treatment in names(qalm_results)) {
  cat("\nTreatment:", treatment, "\n")
  print(qalm_results[[treatment]], row.names = FALSE, digits = 8,
        max = nrow(qalm_results[[treatment]]) * ncol(qalm_results[[treatment]]))
}
cat("\nTotal QALMs over cycles 1-60:\n")
print(qalm_totals, row.names = FALSE, digits = 10)

qalm_totals$total_qalys <- qalm_totals$total_qalms / 12
print(qalm_totals[c("treatment", "total_qalys")], row.names = FALSE)

# Step 4: Costs using existing inputs and Markov traces.
# Run tmz_markov_trace.R first. No input data are redefined here.
# COST timing: cycle 1 = initial distribution; cycles 2-60 = months 1-59.
# The prior Markov traces contain months 1-60 at the end of each month.
# Prepend the existing initial cohort and use only months 1-59 for costs.
# This is a different timing convention from the earlier QALM calculation.
# Costs are undiscounted, with no half-cycle correction, per initial patient.

if (!exists("tmz_inputs") || !exists("markov_traces") ||
    !exists("initial_cohort")) {
  stop("Run tmz_markov_trace.R before sourcing this script.")
}
stopifnot(tmz_inputs$design$cycle_length_months == 1,
          tmz_inputs$design$time_horizon_months == 60,
          all(c("standard_tmz", "long_term_tmz") %in% names(markov_traces)))

calculate_costs <- function(treatment) {
  trace <- markov_traces[[treatment]]
  costs <- tmz_inputs$costs[[treatment]]
  state_names <- c("stable", "progressive", "death")
  stopifnot(nrow(trace) == 60L,
            all(trace$cycle == seq_len(60L)),
            all(state_names %in% names(initial_cohort)))
  
  occupancy <- rbind(
    initial_cohort[state_names],
    as.matrix(trace[seq_len(59L), state_names])
  )
  cycle <- seq_len(60L)
  stable_cost_band <- ifelse(cycle <= 4L, "months_1_to_3",
                             ifelse(cycle <= 7L, "months_4_to_6",
                                    "subsequent_months"))
  stable_unit_cost <- unname(costs$stable[stable_cost_band])
  progressive_unit_cost <- costs$progressive
  death_unit_cost <- costs$death
  
  stable_cost <- occupancy[, "stable"] * stable_unit_cost
  progressive_cost <- occupancy[, "progressive"] * progressive_unit_cost
  death_cost <- occupancy[, "death"] * death_unit_cost
  cycle_cost <- stable_cost + progressive_cost + death_cost
  
  result <- data.frame(
    cycle = cycle,
    month = cycle - 1L,
    stable = occupancy[, "stable"],
    progressive = occupancy[, "progressive"],
    death = occupancy[, "death"],
    stable_unit_cost = stable_unit_cost,
    progressive_unit_cost = progressive_unit_cost,
    death_unit_cost = death_unit_cost,
    stable_cost = stable_cost,
    progressive_cost = progressive_cost,
    death_cost = death_cost,
    cycle_cost = cycle_cost,
    cumulative_cost = cumsum(cycle_cost),
    row.names = NULL
  )
  stopifnot(nrow(result) == 60L,
            all(abs(rowSums(occupancy) - 1) < 1e-12),
            all(is.finite(cycle_cost)), all(cycle_cost >= 0),
            sum(stable_cost_band == "months_1_to_3") == 4L,
            sum(stable_cost_band == "months_4_to_6") == 3L,
            sum(stable_cost_band == "subsequent_months") == 53L)
  result
}

cost_results <- setNames(lapply(names(markov_traces), calculate_costs),
                         names(markov_traces))
cost_totals <- data.frame(
  treatment = names(cost_results),
  total_cost = vapply(cost_results, function(x) sum(x$cycle_cost), numeric(1)),
  row.names = NULL
)

# Display each of the 60 cycles for both treatments; round only for display.
for (treatment in names(cost_results)) {
  cat("\nTreatment:", treatment, "\n")
  print(cost_results[[treatment]], row.names = FALSE, digits = 10,
        max = nrow(cost_results[[treatment]]) * ncol(cost_results[[treatment]]))
}
cat("\nTotal costs over cycles 1-60:\n")
print(cost_totals, row.names = FALSE, digits = 12)

# Step 5: ICERs for long-term TMZ versus standard TMZ.
# Run tmz_markov_trace.R, tmz_qalms.R and tmz_costs.R first.
# Reuse full-precision totals; do not redefine input data.
# NOTE: existing costs use initial occupancy plus months 1-59, whereas
# existing QALMs use end-of-month occupancy for months 1-60. These ICERs
# retain those conventions; align timing before a final HTA analysis.

if (!exists("cost_totals") || !exists("qalm_totals")) {
  stop("Run the QALM and cost scripts before sourcing this script.")
}

get_total <- function(data, treatment_name, value_column) {
  index <- which(data$treatment == treatment_name)
  if (length(index) != 1L) stop("Expected exactly one total per treatment.")
  value <- data[[value_column]][index]
  if (length(value) != 1L || !is.finite(value)) stop("Invalid total.")
  value
}

incremental_cost <-
  get_total(cost_totals, "long_term_tmz", "total_cost") -
  get_total(cost_totals, "standard_tmz", "total_cost")
incremental_qalms <-
  get_total(qalm_totals, "long_term_tmz", "total_qalms") -
  get_total(qalm_totals, "standard_tmz", "total_qalms")
incremental_qalys <- incremental_qalms / 12

if (incremental_qalms == 0) {
  stop("ICER is undefined because incremental effectiveness is zero.")
}

icer_results <- data.frame(
  outcome = c("QALM", "QALY"),
  incremental_cost = incremental_cost,
  incremental_effect = c(incremental_qalms, incremental_qalys),
  icer = c(incremental_cost / incremental_qalms,
           incremental_cost / incremental_qalys)
)

# Retain full precision in icer_results; display no decimal places.
icer_display <- data.frame(
  comparison = "Long-term TMZ versus Standard TMZ",
  measure = c("Cost per QALM gained", "Cost per QALY gained"),
  ICER = sprintf("%.0f", icer_results$icer)
)
print(icer_display, row.names = FALSE, right = FALSE)