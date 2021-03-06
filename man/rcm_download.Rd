% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rcm_download.R
\name{rcm_download}
\alias{rcm_download}
\title{download the research cycle matrix}
\usage{
rcm_download(include_archived = F, include_validated = F,
  after_year = "2015", main_columns_only = T, fill.dates = T,
  remove_empty = T, gdrive_links = F, raw = F)
}
\arguments{
\item{include_archived}{logical: should archived entries be included?}

\item{include_validated}{logical: should validated entries be included?}

\item{after_year}{excludes all entries from before the given year (defaults to 2015)}

\item{main_columns_only}{logical include only most important columns?}

\item{fill.dates}{logical: should dates be filled from other dates? e.g. "first planned date" copyed to "latest planned date" where that is missing}

\item{remove_empty}{logical: should empty rows be removed (default TRUE)}

\item{gdrive_links}{logical: should a column with links to the google drive row be added?}

\item{raw}{logical: if TRUE ignores all other parameters and returns the raw download (default FALSE)}
}
\value{
a data frame with the research cycle matrix
}
\description{
download the research cycle matrix
}
