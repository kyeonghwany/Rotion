\name{notion_ds_tibble}
\alias{notion_ds_tibble}
\title{Query a Notion data source and return a tibble}
\usage{
notion_ds_tibble(
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
A tibble with page fields and property columns.
}
\description{
Query all pages and coerce properties into columns.
}
\examples{
\dontrun{
Sys.setenv(NOTION_TOKEN = "secret")

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
