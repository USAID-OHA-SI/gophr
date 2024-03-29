% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/identifypd.R
\name{identifypd}
\alias{identifypd}
\title{Extract Current Reporting Period}
\usage{
identifypd(df, pd_type = "full", pd_prior = FALSE)
}
\arguments{
\item{df}{dataset to use to find latest period}

\item{pd_type}{what is returned? (a) full, eg fy2018q1; (b)year, eg 2018; (c) quarter, eg 1}

\item{pd_prior}{do you want the last period returned (instead of the current); default = FALSE}
}
\description{
`identifypd` uses the information from the raw MSD structure to identify
the current reporting period (fiscal year and/or quarter). This function
can be used to make other inputs in your code more dynamic. Originally,
`identifypd` was developed for use in achafetz/PartnerProgress
}
\examples{
\dontrun{
#return full, current period, eg "fy2018q3"
identifypd(df_mer)
#return the current quarter, eg 3
identifypd(df_mer, "quarter")
#return the prior quarter, eg "fy2018q2"
identifypd(df_mer, pd_prior = TRUE)
  }

}
