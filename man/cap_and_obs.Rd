% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cap_and_obs.R
\name{cap_and_obs}
\alias{cap_and_obs}
\title{cap_and_obs}
\usage{
cap_and_obs(
  samp_wind,
  gbi = gbi,
  pcg = 0.5,
  pmi = 0.9,
  pci = 0.9,
  start_obs = 1,
  end_obs = max(samp_wind),
  interval_obs = 1,
  start_cap = 1,
  end_cap = max(samp_wind),
  interval_cap = 1,
  pre_cap = NULL
)
}
\arguments{
\item{samp_wind}{vector recording which groups correspond to which sampling windows/behavioural timesteps.}

\item{gbi}{the true group-by-individual matrix}

\item{pcg}{probability of observing or capturing a group in a sampling window with observations/captures}

\item{pmi}{the probability of an individual being observed in a sampled group}

\item{pci}{the probability of an individual being captured in a sampled group}

\item{start_obs}{start of sequence of observed sampling windows}

\item{end_obs}{end of sequence of observed sampling windows}

\item{interval_obs}{sampling window interval between start_obs and end_obs}

\item{start_cap}{start of sequence of sampling windows with captures}

\item{end_cap}{end of sequence of sampling windows with captures}

\item{interval_cap}{sampling window interval between start_cap and end_cap}

\item{pre_cap}{vector indicating whether any individuals have been previously captured (and so are observable). Defaults to NULL.}
}
\value{
A list with 4 elements: A) the full GBI but for only captured groups; B) a vector indicating which groups were captured ; C) the full GBI for observed and captured groups combined; D) a vector indicating which groups were observed
}
\description{
Generates observations that can be used to generate CMR data and observed social networks.
}
\details{
Note that observations do not happen in the same sampling windows as captures even if defined to do so. Only previously captured individuals are observable. See notes in comments for future updates. I would also like to allow some observation error in another version of this function.
}
