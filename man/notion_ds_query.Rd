\name{notion_ds_query}
\alias{notion_ds_query}
\title{Query a Notion Data Source}
\usage{
notion_ds_query(
  data_source_id,
  filter = NULL,
  sorts = NULL,
  page_size = 100,
  start_cursor = NULL,
  token = notion_token(),
  notion_version = "2025-09-03"
)
}
\arguments{
\item{data_source_id}{Notion data source (database) ID.}
\item{filter}{Optional filter payload.}
\item{sorts}{Optional sorts payload.}
\item{page_size}{Page size for pagination.}
\item{start_cursor}{Cursor for pagination.}
\item{token}{Notion token.}
\item{notion_version}{Notion API version.}
}
\value{Parsed JSON list from the Notion API.}
\description{Posts a query to a Notion data source.}
