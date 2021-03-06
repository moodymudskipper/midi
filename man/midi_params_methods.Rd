% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/07_print.R
\name{format.midi_params}
\alias{format.midi_params}
\alias{vec_ptype_abbr.midi_params}
\title{#' @method print midi_track_list
#' @export
print.midi_track_list <- function(x, n= NULL){
  opt <- options(tibble.print_max = n)
  on.exit(options(opt))
  NextMethod()
}
Methods for displaying midi parameters in midi object output}
\usage{
\method{format}{midi_params}(x, ...)

vec_ptype_abbr.midi_params(x)
}
\arguments{
\item{x}{object}

\item{...}{additional parameters passed to other methods}
}
\description{
#' @method print midi_track_list
#' @export
print.midi_track_list <- function(x, n= NULL){
  opt <- options(tibble.print_max = n)
  on.exit(options(opt))
  NextMethod()
}
Methods for displaying midi parameters in midi object output
}
