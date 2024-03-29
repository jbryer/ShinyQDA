% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sentiment_plot.R
\name{sentiment_plot}
\alias{sentiment_plot}
\title{Plot sentiment for an individual text.}
\usage{
sentiment_plot(
  text,
  lexicon = c("nrc", "bing", "loughran", "afinn"),
  lexicon_dir = "."
)
}
\arguments{
\item{text}{the text to calculate sentiment bar plot for.}

\item{lexicon}{the sentiment dictionary to use.}

\item{lexicon_dir}{directory containing the sentiment dictionayr.}
}
\description{
Plot sentiment for an individual text.
}
