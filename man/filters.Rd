\name{filters}
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
\alias{sort_asc}
\alias{sort_desc}
\title{Filter and Sort Helpers}
\usage{
filter_text_contains(property, value)
filter_text_equals(property, value)
filter_number_equals(property, value)
filter_select_equals(property, value)
filter_multi_select_contains(property, value)
filter_checkbox_equals(property, value)
filter_date_on_or_after(property, iso_date_or_datetime)
filter_date_on_or_before(property, iso_date_or_datetime)
filter_and(...)
filter_or(...)
sort_asc(property)
sort_desc(property)
}
\arguments{
\item{property}{Property name.}
\item{value}{Filter value.}
\item{iso_date_or_datetime}{ISO date or datetime string.}
\item{...}{Filters to combine.}
}
\description{Convenience helpers for building filter and sort payloads.}
