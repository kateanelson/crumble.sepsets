#' @importFrom generics tidy
#' @export
generics::tidy

#' Tidy a(n) crumble object
#'
#' @param x A \code{crumble} object produced by a call to
#'   \code{\link[=crumble]{crumble.sepsets::crumble()}}.
#' @param ... Unused, included for generic consistency only.
#'
#' @return A tidy [tibble::tibble()] summarizing information about the model.
#'
#' @example inst/examples/examples.R
#'
#' @importFrom purrr list_rbind map
#'
#' @export
tidy.crumble <- function(x, ...) {
	out <- list_rbind(map(x$estimates, ife::tidy), names_to = "estimand")
	class(out) <- c("tbl_df", "tbl", "data.frame")
	out
}
