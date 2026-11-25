test_that("detects missing variables", {
	foo <- data.frame(a = 1, w = 1, m = 1, z = NA, y = 1)
	expect_error(
		crumble(foo, "a", "y", "m", "z", "w"),
		"Assertion on 'data' failed: Missing data found in treatment/covariate/mediator/observed nodes."
	)
})

test_that("checks columns in every covariate set", {
	d <- data.frame(a = 1, w = NA_real_, m = 1, y = 1)
	expect_error(
		crumble(d, "a", "y", "m", covar = list(trt = "absent"), effect = "N"),
		"absent"
	)
	expect_error(
		crumble(d, "a", "y", "m", covar = list(outcome = "w"), effect = "N"),
		"Missing data found"
	)
	expect_error(
		crumble(d, "a", "y", "m", covar = list(other = "w"), effect = "N"),
		"other"
	)
})
