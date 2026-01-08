\name{sort_asc}
\alias{sort_asc}
\alias{sort_desc}
\title{Sort helpers for Notion queries}
\usage{
sort_asc(property)
sort_desc(property)
}
\arguments{
\item{property}{Property name.}
}
\value{
A sort payload.
}
\description{
Helpers to build sort payloads.
}
\examples{
list(sort_desc("예약일시"))
}
