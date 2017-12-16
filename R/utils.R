mostattributes <- `mostattributes<-`

glue_warning <- function(..., call = FALSE) {
  warn(glue::glue(..., .envir = parent.frame()), call = call)
}

glue_abort <- function(..., call = FALSE) {
  abort(glue::glue(..., .envir = parent.frame()), call = call)
}

# From skimr (https://github.com/ropenscilabs/skimr/blob/8fb50a48db71efd4af481a8e9cfa066a5765810e/R/stats.R)

inline_hist <- function(x, width) {
  # Handle empty vectors
  if (length(x) < 1) return(structure(" ", class = "spark"))

  # Addresses a known bug in cut()
  if (all(x == 0)) x <- x + 1
  hist_dt <- table(cut(x, width))
  hist_dt <- hist_dt / max(hist_dt)
  structure(pillar:::spark_bar(hist_dt), class = c("spark", "character"))
}
