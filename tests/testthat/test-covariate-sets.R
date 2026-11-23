test_that("different covariate sets feed the density ratios and regressions", {
	skip_if_not(torch::torch_is_installed())
	set.seed(42)
	n <- 80L
	d <- data.frame(
		a = rbinom(n, 1, 0.5), m = rnorm(n), z = rnorm(n),
		w1 = rnorm(n), w2 = rnorm(n), w3 = rnorm(n), w4 = rnorm(n)
	)
	d$obs <- rbinom(n, 1, plogis(d$w3)) == 1
	d$y <- with(d, a + m + z + w4 + rnorm(n))
	d$y[!d$obs] <- 999
	control <- crumble_control(crossfit_folds = 1L, epochs = 3L, batch_size = n)

	input_sizes <- integer()
	module <- sequential_module()
	fit <- function(w_cens) {
		set.seed(123)
		torch::torch_manual_seed(123)
		crumble(
			d, "a", "y", "m", moc = "z", obs = "obs",
			covar = list(trt = c("w1", "w2"), cens = w_cens, outcome = "w4"),
			effect = "RT", control = control,
			nn_module = function(d_in) {
				input_sizes <<- c(input_sizes, d_in)
				module(d_in)
			}
		)
	}

	separate <- fit("w3")
	expect_equal(sort(unique(input_sizes)), 3:5)
	without_cens_covar <- fit(NULL)

	outcome_fit <- glm(y ~ a + m + z + w4, data = d, subset = obs)
	expect_equal(
		unname(separate$outcome_reg$theta_n$natural$fit3_natural[, 1]),
		unname(predict(outcome_fit, newdata = d))
	)
	p_with <- predict(glm(obs ~ a + m + z + w3, data = d, family = binomial()), type = "response")
	p_without <- predict(glm(obs ~ a + m + z, data = d, family = binomial()), type = "response")
	expect_equal(separate$alpha_n$alpha1, without_cens_covar$alpha_n$alpha1)
	expect_equal(separate$alpha_n$alpha3, without_cens_covar$alpha_n$alpha3 * (p_without / p_with))
	expect_equal(separate$alpha_r$alpha4, without_cens_covar$alpha_r$alpha4 * (p_without / p_with))
})
