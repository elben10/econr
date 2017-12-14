mostattributes <- `mostattributes<-`

glue_warning <- function(..., call = FALSE) {
  warn(glue::glue(..., .envir = parent.frame()), call = call)
}

glue_abort <- function(..., call = FALSE) {
  abort(glue::glue(..., .envir = parent.frame()), call = call)
}
