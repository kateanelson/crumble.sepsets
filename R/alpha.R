estimate_obs_probability <- function(cd, folds) {
	observed <- censored(cd@data, cd@vars@C)
	if (all(observed)) {
		return(rep(1, nrow(cd@data)))
	}

	vars <- cd@vars
	features <- na.omit(c(vars@A, vars@M, vars@Z, vars@W_cens, vars@C))
	data <- cd@data[, features, drop = FALSE]
	data[[vars@C]] <- as.integer(observed)
	probability <- rep(1, nrow(data))

	for (fold in folds) {
		train <- fold$training_set
		valid <- fold$validation_set
		if (!any(observed[train])) {
			stop("Each training fold must contain at least one observed outcome.", call. = FALSE)
		}
		if (all(observed[train])) next

		fit <- mlr3superlearner::mlr3superlearner(
			data = data[train, , drop = FALSE],
			target = vars@C,
			library = "glm",
			outcome_type = "binomial",
			folds = 1L
		)
		probability[valid] <- predict(fit, data[valid, , drop = FALSE])
	}

	pmax(probability, .Machine$double.eps)
}

Alpha <- function(train, valid, vars, architecture, .f, weights = NULL, control) {
	model <- nn_sequential_riesz_representer(
		train = train,
		vars = vars,
		architecture = architecture,
		.f = .f,
		weights = weights,
		batch_size = control$batch_size,
		learning_rate = control$learning_rate,
		epochs = control$epochs,
		device = control$device
	)

	list(
		train = as.numeric(
			model(
				as_torch(
					one_hot_encode(train$data, vars),
					device = control$device
				)
			)
		),
		valid = as.numeric(
			model(
				as_torch(
					one_hot_encode(valid$data, vars),
					device = control$device
				)
			)
		)
	)
}
