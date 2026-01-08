\name{filter_text_contains}
\alias{filter_text_contains}
\alias{filter_text_equals}
\alias{filter_number_equals}
\alias{filter_select_equals}
\alias{filter_multi_select_contains}
\alias{filter_checkbox_equals}
\alias{filter_date_on_or_after}
\alias{filter_date_on_or_before}
\alias{filter_and}
\alias{filter_or}
\title{Filter helpers for Notion queries}
\usage{
filter_text_contains(property, value)
filter_text_equals(property, value)
filter_number_equals(property, value)
filter_select_equals(property, value)
filter_multi_select_contains(property, value)
filter_checkbox_equals(property, value)
filter_date_on_or_after(property, value)
filter_date_on_or_before(property, value)
filter_and(...)
filter_or(...)
}
\arguments{
\item{property}{Property name.}
\item{value}{Filter value.}
\item{...}{Filter payloads.}
}
\value{
A filter list payload.
}
\description{
Convenience helpers for building filter payloads.
}
\examples{
filter_text_contains("제목", "hello")
filter_and(filter_checkbox_equals("완료", TRUE))
}
