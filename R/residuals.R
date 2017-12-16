#' A residuals from model to data.frame
#'
#' @inheritParams mod_matrix
#' @param model Model of class \code{mod}.
#' @param var The name of residual column.
#'
#' @return A data.frame where the residuals has been added in a column.
#' @export
#'
#' @examples
#' mod <- mtcars %>% mod_lm(mpg~cyl)
#' mod_add_residuals(mtcars, mod)
mod_add_residuals <- function(data, model, var = "resid") UseMethod("mod_add_residuals")

#' @export
mod_add_residuals.default <- function(data, model, var = "resid") {
  obj <- deparse(substitute(data))
  glue_abort("`{obj}` is not a data.frame")
}

#' @export
mod_add_residuals.data.frame <- function(data, model, var = "resid") {
  if (!any(class(model) == "mod")) {
    glue_abort("econr does not support models of class {class(model)}")
  }

  if (var %in% names(data)) {
    obj <- deparse(substitute(data))
    glue_abort("`{var}` already exists in `{obj}`. Please provide a unique column name.")
  }

  data[[var]] <- model$residuals
  data
}
