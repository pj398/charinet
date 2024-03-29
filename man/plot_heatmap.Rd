% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/heatmap.R
\name{plot_heatmap}
\alias{plot_heatmap}
\title{Plot heatmap using ggplot2}
\usage{
plot_heatmap(input_heatmap, cutoff = 0, title = "")
}
\arguments{
\item{input_heatmap}{The character-by-chunk heatmap matrix produced by the
\code{activity_heatmap} function.}

\item{cutoff}{A numeric value to be used to filter the visualisation to only
include those characters who speak more than \code{cutoff} number of lines.}

\item{title}{An optional character string which can be used to add a title to
the visualisation.}
}
\value{
A ggplot2 plot.
}
\description{
Create a quick heatmap visualisation of character activity by
  story chunks using ggplot2.
}
\examples{
tfa <- movienetdata::starwars_tfa
activity_heatmap(tfa$event_list,
              char_names = tfa$node_list$char_name,
              n_chunks = 4) \%>\%
  plot_heatmap(cutoff = 3)

}
