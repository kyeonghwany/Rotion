\name{notion_token}
\alias{notion_token}
\title{Retrieve Notion API token}
\usage{
notion_token()
}
\value{
A character token string.
}
\description{
Reads the \code{NOTION_TOKEN} environment variable.
}
\examples{
\dontrun{
Sys.setenv(NOTION_TOKEN = "secret")
notion_token()
}
}
