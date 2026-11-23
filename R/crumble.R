#' Flexible and general mediation analysis
#'
#' General estimator for common mediation causal effects, including recanting
#' twins, natural effects, organic effects, and randomized interventional
#' effects. Interventions are specified using modified treatment policies.
#' Nuisance parameters are estimated using the 'super learner' algorithm and
#' 'Riesz learning'. Supports binary, categorical, and continuous exposures.
#'
#' @param data [\code{data.frame}]\cr
#'  A \code{data.frame} in wide format containing all necessary variables
#'  for the estimation problem.
#' @param trt [\code{character}]\cr
#'  A vector containing the column names of treatment variables.
#' @param outcome [\code{character(1)}]\cr
#'  The column name of the outcome variable.
#' @param mediators [\code{character}]\cr
#'	A vector containing the column names of the mediator variables.
#' @param moc [\code{character}]\cr
#'  An optional vector containing the column names of the mediator-outcome confounders.
#' @param covar [\code{character} or \code{list}]\cr
#'  Baseline covariate column names. A character vector uses the same covariates
#'  throughout. A named list can specify separate vectors for treatment and
#'  mediation density ratios (\code{trt}), the outcome observation model
#'  (\code{cens}), and the sequential outcome regressions (\code{outcome}).
#'  Omitted or \code{NULL} entries use no baseline covariates for that model.
#'  For randomized effects, the union of \code{trt} and \code{outcome}
#'  covariates is used to construct the permuted mediator-outcome confounders.
#'  For example, \code{list(trt = "w1", cens = "w2", outcome = c("w1", "w3"))}
#'  specifies three covariate sets. Each set must retain the variables needed
#'  to identify the corresponding nuisance function.
#' @param obs [\code{character(1)}]\cr
#'  An optional column name (with values coded as 0 or 1) for whether or not the \code{outcome} is observed.
#'  Must be provided if there is missingness in the outcome! Default is \code{NULL}.
#'  A logistic regression of \code{obs} on treatment, mediators, mediator-outcome
#'  confounders, and the \code{cens} covariates supplies inverse probability
#'  weights for the final outcome residual.
#' @param id [\code{character(1)}]\cr
#'  An optional column name containing cluster level identifiers.
#' @param d0 [\code{closure}]\cr
#'  A two argument function that specifies how treatment variables should be shifted.
#'  See examples for how to specify shift functions for continuous, binary, and categorical exposures.
#' @param d1 [\code{closure}]\cr
#'  A two argument function that specifies how treatment variables should be shifted.
#'  See examples for how to specify shift functions for continuous, binary, and categorical exposures.
#' @param effect [\code{character(1)}]\cr
#'  The type of effect to estimate. Options are \code{"RT"} for recanting twins,
#'  \code{"N"} for natural effects, \code{"RI"} for randomized interventional effects,
#'  and \code{"O"} for organic effects.
#'  If \code{"RT"} or \code{"RI"} is selected, \code{moc} must be provided.
#'  If \code{"N"} or \code{"O"} is selected, \code{moc} must be \code{NULL}.
#' @param weights [\code{numeric}]\cr
#'  A optional vector of survey weights.
#' @param learners [\code{character}]\cr
#'  A vector of \code{mlr3superlearner} algorithms
#'  for estimation of the outcome regressions. Default is \code{"glm"}, a main effects GLM.
#' @param nn_module [\code{function}]\cr A function that returns a neural network module.
#' @param control [\code{crumble_control}]\cr
#'  Control parameters for the estimation procedure. Use \code{crumble_control()} to set these values.
#'
#' @return A \code{crumble} object containing the following components:
#' \item{estimates}{A list of parameter estimates.}
#' \item{outcome_reg}{Predictions from the outcome regressions.}
#' \item{alpha_n}{A list of density ratio estimates.}
#' \item{alpha_r}{A list of density ratio estimates.}
#' \item{fits}{A list of the fitted values from the outcome regressions.}
#' \item{call}{The matched call.}
#' \item{effect}{The estimated effect type.}
#'
#' @rawRd \encoding{UTF-8}
#'
#' @references
#' Liu, R., Williams, N. T., Rudolph, K. E., and Díaz, I. (2025).
#' General targeted machine learning for modern causal mediation analysis.
#' \doi{10.48550/arXiv.2408.14620}.
#'
#' Vo, T.-T., Williams, N., Liu, R., Rudolph, K. E., and Díaz, I. (2026).
#' Recanting twins: Addressing intermediate confounding in mediation analysis.
#' \doi{10.1002/sim.70432}.
#'
#' @importFrom checkmate assert_data_frame assert_function assert_numeric
#'
#' @export
#'
#' @example inst/examples/examples.R
crumble <- function(data,
										trt,
										outcome,
										mediators,
										moc = NULL,
										covar,
										obs = NULL,
										id = NULL,
										d0 = NULL,
										d1 = NULL,
										effect = c("RT", "N", "RI", "O"),
										weights = rep(1, nrow(data)),
										learners = "glm",
										nn_module = sequential_module(),
										control = crumble_control()) {

	# Perform initial checks
	assert_data_frame(data)
	covar <- covariate_sets(covar)
	all_covar <- unique(unlist(covar, use.names = FALSE))
	variables <- c(trt, outcome, mediators, moc, all_covar, obs, id)
	checkmate::assert_subset(variables, names(data))
	assert_not_missing(data, trt, all_covar, mediators, moc, obs)
	assert_function(d0, nargs = 2, null.ok = TRUE)
	assert_function(d1, nargs = 2, null.ok = TRUE)
	assert_function(nn_module)
	if (!is.null(obs)) {
		if (!all(data[[obs]] %in% c(0, 1))) {
			stop("The 'obs' column must contain only 0 and 1.", call. = FALSE)
		}
		data[[outcome]][data[[obs]] == 0] <- NA
	}
	assert_binary_0_1(data, outcome)
	assert_effect_type(moc, match.arg(effect))
	assert_numeric(weights, len = nrow(data), finite = TRUE, any.missing = FALSE)

	params <- switch(
		match.arg(effect),
		N = natural,
		O = organic,
		RT = recanting_twin,
		RI = randomized
	)

	# Create crumble_data object
	cd <- crumble_data(
		data = data,
		vars = crumble_vars(
			A = trt,
			Y = outcome,
			M = mediators,
			Z = moc %??% NA_character_,
			W = covar$outcome,
			W_trt = covar$trt,
			W_cens = covar$cens,
			C = obs %??% NA_character_,
			id = id %??% NA_character_
		),
		weights = weights,
		d0 = d0,
		d1 = d1
	)

	# Create permuted Z
	cd <- add_zp(cd, moc, control)

	# Create folds for cross fitting
	folds <- make_folds(cd@data, control$crossfit_folds, cd@vars@id, cd@vars@Y)
	obs_probability <- estimate_obs_probability(cd, folds)

	# Estimate \theta nuisance parameters
	thetas <- estimate_theta(cd, thetas, folds, params, learners, control)

	# Estimate density ratios, alpha natural
	alpha_ns <- estimate_phi_n_alpha(cd, folds, params, nn_module, control)
	if (!is.null(alpha_ns)) alpha_ns$alpha3 <- alpha_ns$alpha3 / obs_probability
	eif_ns <- calc_eifs(cd, alpha_ns, thetas, eif_n)

  # Estimate density ratios, alpha randomized
	alpha_rs <- estimate_phi_r_alpha(cd, folds, params, nn_module, control)
	if (!is.null(alpha_rs)) alpha_rs$alpha4 <- alpha_rs$alpha4 / obs_probability
	eif_rs <- calc_eifs(cd, alpha_rs, thetas, eif_r)

	# Estimates ---------------------------------------------------------------

	out <- list(
		estimates = switch(
			match.arg(effect),
			N = calc_estimates_natural(eif_ns, weights),
			O = calc_estimates_organic(eif_ns, weights),
			RT = calc_estimates_rt(eif_ns, eif_rs, weights),
			RI = calc_estimates_ri(eif_rs, weights)
		),
		outcome_reg = thetas,
		alpha_n = alpha_ns,
		alpha_r = alpha_rs,
		fits = list(
			theta_n = thetas$theta_n$weights,
			theta_r = thetas$theta_r$weights
		),
		call = match.call(),
		effect = match.arg(effect)
	)

	class(out) <- "crumble"
	out
}
