% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ACMI_contribution.R
\name{ACMI_contribution}
\alias{ACMI_contribution}
\title{Calculate Average Conditional Mutual Information (ACMI) contribution
of each vocabulary term in a sparse DTM.}
\usage{
ACMI_contribution(joint_dist)
}
\arguments{
\item{joint_dist}{A matrix of class "simple_triplet_matrix" generated by the
contingency_table() function. Matching pairs of metadata combinations must be
sequential rows.}
}
\value{
A vector recording the ACMI contribution of each variable.
}
\description{
Generates a vector of contributions to ACMI for each vocabulary
term in a DTM, based on a contingency table generated by the
contingency_table() function.
}
