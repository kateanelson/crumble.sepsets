# enable usage of <S7_object>@name in package code
#' @rawNamespace if (getRversion() < "4.3.0") importFrom("S7", "@")
#' @importFrom stats na.omit weighted.mean var qnorm dist model.matrix predict setNames
NULL

crumble_vars <- S7::new_class("crumble_vars",
  properties = list(
    A = S7::class_character,
    Y = S7::class_character,
    M = S7::class_character,
    Z = S7::class_character,
    W = S7::class_character,
    W_trt = S7::class_character,
    W_cens = S7::class_character,
    C = S7::new_property(class = S7::class_character, default = NA_character_),
    id = S7::new_property(class = S7::class_character, default = NA_character_)
  ),
  validator = function(self) {
     if (length(self@Y) != 1) {
      "self@outcome must be length 1"
    } else if (length(self@C) != 1) {
    	"self@cens must be length 1"
    } else if (length(self@id) != 1) {
    	"self@id must be length 1"
    }
  }
)

covariate_sets <- function(covar) {
  models <- c("trt", "cens", "outcome")
  if (!is.list(covar)) {
    covar <- setNames(rep(list(covar), length(models)), models)
  }

  checkmate::assert_list(covar, names = "unique")
  checkmate::assert_subset(names(covar), models, .var.name = "names(covar)")
  setNames(lapply(models, function(model) {
    vars <- covar[[model]]
    checkmate::assert_character(vars, any.missing = FALSE, null.ok = TRUE,
                                .var.name = paste0("covar$", model))
    unique(as.character(vars))
  }), models)
}
