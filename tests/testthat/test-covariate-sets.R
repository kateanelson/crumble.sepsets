test_that("the outcome set is shared when the exposure set is omitted", {
	expect_identical(covariate_sets(list(outcome = "w")), covariate_sets("w"))
})

test_that("the treatment set changes only the exposure Riesz stage's inputs", {
	skip_if_not(torch::torch_is_installed())
	set.seed(42)
	n <- 80L
	d <- data.frame(
		a = rbinom(n, 1, 0.5), m = rnorm(n), z = rnorm(n),
		w1 = rnorm(n), w2 = rnorm(n), w3 = rnorm(n)
	)
	d$obs <- rbinom(n, 1, plogis(d$w2))
	d$y <- with(d, a + m + z + w1 + rnorm(n))
	d$y[d$obs == 0] <- NA
	control <- crumble_control(crossfit_folds = 1L, epochs = 3L, batch_size = n)

	module <- sequential_module()
	fit <- function(covar) {
		set.seed(123)
		torch::torch_manual_seed(123)
		input_sizes <- integer()
		out <- crumble(
			d, "a", "y", "m", moc = "z", obs = "obs",
			covar = covar,
			effect = "RT", control = control,
			nn_module = function(d_in) {
				input_sizes <<- c(input_sizes, d_in)
				module(d_in)
			}
		)
		list(input_sizes = input_sizes, outcome_reg = out$outcome_reg)
	}

	shared <- fit(c("w1", "w2"))
	separate <- fit(list(trt = "w3", outcome = c("w1", "w2")))
	expect_equal(separate$input_sizes, c(
		rep(c(2, 4, 6), length(recanting_twin$natural)),
		rep(c(2, 4, 4, 6), length(recanting_twin$randomized))
	))
	expect_equal(shared$input_sizes, c(
		rep(c(3, 4, 6), length(recanting_twin$natural)),
		rep(c(3, 4, 4, 6), length(recanting_twin$randomized))
	))
	expect_equal(
		separate$outcome_reg,
		shared$outcome_reg
	)
})
