#' A residuals from model to data.frame
#'
#' @inheritParams model_matrix
#' @param model Model of class \code{mod}.
#' @param var The name of residual column.
#'
#' @return A data.frame where the residuals has been added in a column.
#' @export
#'
#' @examples
#' mod <- mtcars %>% model_lm(mpg~cyl)
#' model_add_residuals(mtcars, mod)
model_add_residuals <- function(data, model, var = "resid") UseMethod("model_add_residuals")

#' @export
model_add_residuals.default <- function(data, model, var = "resid") {
  obj <- deparse(substitute(data))
  glue_abort("`{obj}` is not a data.frame")
}

#' @export
model_add_residuals.data.frame <- function(data, model, var = "resid") {
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
