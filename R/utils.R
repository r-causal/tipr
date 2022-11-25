#' Pipe operator
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom purrr %>%
#' @usage lhs \%>\% rhs
NULL

stop_cli <- function(message, ..., .envir = parent.frame()) {
  cli::cli_abort(
    message,
    ...,
    .envir = .envir
  )
}

warning_cli <- function(message, ..., .envir = parent.frame()) {
  cli::cli_warn(
    message,
    ...,
    .envir = .envir
  )
}

message_cli <- function(message, ..., .envir = parent.frame()) {
  cli::cli_inform(
    message,
    ...,
    .envir = .envir
  )
}

bullets <- function(..., code = TRUE) {
  x <- c(...)
  if (code) x <- glue::glue("`{x}`")
  names(x) <- rep("*", length(x))

  x
}

`%||%` <- function(x, y) {
  if (is.null(x)) {
    y
  } else x
}

check_arguments <- function(what, ...) {
  arg_quos <- rlang::enquos(...)
  arg_names <- purrr::map_chr(arg_quos, rlang::quo_text)
  args <- list(...)

  if (not_enough(args)) {
    stop_cli(c(
      "x" = "`{what}` requires at least {count_required_args(args)} of the \\
      following arguments specifed:",
      bullets(arg_names)
    ))
  }

  invisible(TRUE)
}

count_required_args <- function(.args) {
  length(.args) - 1
}

count_non_null_args <- function(.args) {
  sum(purrr::map_lgl(.args, purrr::negate(is.null)))
}

not_enough <- function(.args) {
  count_non_null_args(.args) < count_required_args(.args)
}

