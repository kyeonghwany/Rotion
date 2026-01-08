\name{notion_ds_tibble}
\alias{notion_ds_tibble}
\title{Query Notion Data Source and Return Tibble}
\usage{
notion_ds_tibble(
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
\value{Tibble of page properties.}
\description{Queries all pages and returns a tibble with core metadata columns.}
\examples{
Sys.setenv(NOTION_TOKEN = "secret_abc123")
\dontrun{
df <- notion_ds_tibble("DATA_SOURCE_ID")

f <- filter_and(
  filter_select_equals("상태", "완납"),
  filter_date_on_or_after("예약일", "2026-01-01")
)
df <- notion_ds_tibble("DATA_SOURCE_ID", filter = f)

s <- list(sort_desc("예약일시"))
df <- notion_ds_tibble("DATA_SOURCE_ID", sorts = s)
}
}
