% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_functions.R
\name{adjust_facet_width_of_plot}
\alias{adjust_facet_width_of_plot}
\title{Converts a ggplot object to grob object, gets the amount of unique x-axis values per facet
by counting the rows from given list elements, and change the relative width of the facet
columns according to the amount.}
\usage{
adjust_facet_width_of_plot(ggplot, data)
}
\arguments{
\item{ggplot}{Facetted ggplot object}

\item{data}{List containing one entry for each facet with the correct amount of x axis values. Must be countable with nrow.}
}
\value{
ggplot object with (according to amount of x-axis values per facet) adjusted widths
}
\description{
Converts a ggplot object to grob object, gets the amount of unique x-axis values per facet
by counting the rows from given list elements, and change the relative width of the facet
columns according to the amount.
}
