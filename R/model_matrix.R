#' Design matrix
#'
#' \code{model_matrix} returns the design matrix associated to the model formula.
#'
#' @param data An object of type data.frame
#' @param formula An object of type formula
#'
#' @return A tibble of the design matrix.
#' @export
#'
#' @examples model_matrix(mtcars, mpg~cyl)
model_matrix <- function(data, formula) UseMethod("model_matrix")


#' @export
model_matrix.default <- function(data, formula) {
  obj <- deparse(substitute(data))
  glue_abort("`{obj}` is not a data.frame")
}

#' @export
model_matrix.data.frame <- function(data, formula) {
  if (!is_formula(formula)) {
    obj <- deparse(substitute(formula))
    glue_abort("`{obj}` is not a formula")
  }

  res <- mostattributes(stats::model.matrix(formula, data = data), NULL)

  tibble::as_tibble(res)
}


#' Response vector
#'
#' \code{model_response} returns the response vector associated to the model formula.
#'
#' @inheritParams model_matrix
#'
#' @return A vector containing the model response.
#' @export
#'
#' @examples model_response(mtcars, mpg~cyl)
model_response <- function(data, formula) UseMethod("model_response")

#' @export
model_response.default <- function(data, formula) {
  obj <- deparse(substitute(data))
  glue_abort("`{obj}` is not a data.frame")
}

#' @export
model_response.data.frame <- function(data, formula) {
  if (!is_formula(formula)) {
    obj <- deparse(substitute(formula))
    glue_abort("`{obj}` is not a formula")
  }

  mostattributes(stats::model.response(stats::model.frame(formula, data = data)), NULL)
}


