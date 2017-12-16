#' Design matrix
#'
#' \code{mod_matrix} returns the design matrix associated to the model formula.
#'
#' @param data An object of type data.frame
#' @param formula An object of type formula
#'
#' @return A tibble of the design matrix.
#' @export
#'
#' @examples mod_matrix(mtcars, mpg~cyl)
mod_matrix <- function(data, formula) UseMethod("mod_matrix")


#' @export
mod_matrix.default <- function(data, formula) {
  obj <- deparse(substitute(data))
  glue_abort("`{obj}` is not a data.frame")
}

#' @export
mod_matrix.data.frame <- function(data, formula) {
  if (!is_formula(formula)) {
    obj <- deparse(substitute(formula))
    glue_abort("`{obj}` is not a formula")
  }

  res <- mostattributes(stats::model.matrix(formula, data = data), NULL)

  tibble::as_tibble(res)
}


#' Response vector
#'
#' \code{mod_response} returns the response vector associated to the model formula.
#'
#' @inheritParams mod_matrix
#'
#' @return A vector containing the model response.
#' @export
#'
#' @examples mod_response(mtcars, mpg~cyl)
mod_response <- function(data, formula) UseMethod("mod_response")

#' @export
mod_response.default <- function(data, formula) {
  obj <- deparse(substitute(data))
  glue_abort("`{obj}` is not a data.frame")
}

#' @export
mod_response.data.frame <- function(data, formula) {
  if (!is_formula(formula)) {
    obj <- deparse(substitute(formula))
    glue_abort("`{obj}` is not a formula")
  }

  res <- stats::model.response(stats::model.frame(formula, data = data))

  mostattributes(res, NULL)
}
