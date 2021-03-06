% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/predict_check_input.R
\name{predict_check_input}
\alias{predict_check_input}
\title{Checking if all inputs are valid for function predict_blrf.
If not valid input exists, terminate program and output error message.}
\usage{
predict_check_input(blrf, confidence, probability, lower, upper)
}
\arguments{
\item{blrf}{blrf object.}

\item{confidence}{logical. If TURE, then output confidence interval.}

\item{probability}{logical. If TRUE, then output will be predict probability for factor
type of blrf. If FALSE, then the output will be predict label for "factor"
type of blrf or predict value for "numeric" type of blrf.}

\item{lower}{numeric. If confidence is TRUE, then define lower bound of ci.}

\item{upper}{numeric. If confidence is TRUE, then define upper bound of ci.}
}
\value{
logic.
}
\description{
Checking if all inputs are valid for function predict_blrf.
If not valid input exists, terminate program and output error message.
}
