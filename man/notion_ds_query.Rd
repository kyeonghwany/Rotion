\name{notion_ds_query}
\alias{notion_ds_query}
\alias{notion_ds_query_all}
\title{Query a Notion data source}
\usage{
notion_ds_query(
  data_source_id,
  filter = NULL,
  sorts = NULL,
  page_size = 100,
  start_cursor = NULL,
  token = notion_token(),
  notion_version = .notion_version_default()
)

notion_ds_query_all(
  data_source_id,
  filter = NULL,
  sorts = NULL,
  page_size = 100,
  start_cursor = NULL,
  token = notion_token(),
  notion_version = .notion_version_default()
)
}
\arguments{
\item{data_source_id}{Notion data source (database) id.}
\item{filter}{Optional filter payload.}
\item{sorts}{Optional sorts payload.}
\item{page_size}{Page size (max 100).}
\item{start_cursor}{Optional cursor for pagination.}
\item{token}{Notion API token.}
\item{notion_version}{Notion API version.}
}
\value{
Parsed JSON response as a list, or a list of page objects for \code{notion_ds_query_all}.
}
\description{
Query a Notion data source and optionally return all pages.
}
\examples{
\dontrun{
notion_ds_query("DATA_SOURCE_ID")
notion_ds_query_all("DATA_SOURCE_ID")
}
}
