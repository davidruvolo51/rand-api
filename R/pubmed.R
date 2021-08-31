#'
#' @name pubmed
#' @author dcruvolo
#'
#' @description Mini API client for querying publication metadata from Pubmed
#'
#' @section Instructions:
#'
#' The pubmed class contains several methods for interacting with the pubmed API.
#' The primary method `pubmed` automatically handles the requests to Pubmed's
#' Eutils service, which is a two step process of: 1. retrieve UIDs for all
#' matching publications and 2. retrieve publication metadata based on the
#' IDs found in step 1.
#'
#' @section Notes:
#'
#' This client sends requests to the eutils service. If you would like to
#' send requests to other services, you will need to expand the workflow
#' and add additional methods for processing data. I needed a lightweight client
#' and a specific output format for a project I was working on.
#'
#' @examples
#' \dontrun {
#' # find publications with "R Core Team" that were published in 2021
#' p <- pubmed$new()
#' p$pubmed("(\"R Core Team\") AND ((\"2021\"[Date - Publication]))")
#' p
#' }
#'
#' @references
#' - Eutils docs: \url{https://www.ncbi.nlm.nih.gov/books/NBK25501/}
#'
#' @import R6, httr, rjson, tibble
#'
pubmed <- R6::R6Class(
    classname = "pubmed",
    public = list(
        #' @field ids a vector containing pubmed IDs
        ids = NULL,

        #' @field a data object containing publication metadata
        data = NULL,

        #' @field failed array IDs that failed
        failed = NULL,

        #' @field query string containing the last run query
        query = NULL,

        #' @name .__req__send
        #' @description wrapper around GET for standardizing requests
        #' @param url Api endpoint to send request (host and params) to
        #' @return httr response object
        #' @noRd
        .__req__send = function(url) {
            httr::GET(url, httr::add_headers(`Content-Type` = "application/json"))
        },

        #' @name .__req__extract
        #' @description extract content from response
        #' @param response httr response object
        #' @return list object
        #' @noRd
        .__req__extract = function(response) {
            r <- httr::content(response, "text", encoding = "UTF-8")
            rjson::fromJSON(r)
        },

        #' @name .__pubmed__build__data
        #' @description should a request fail, return a blank dataset
        #' @param id string containing a publication ID
        #' @param data extracted object from request (.__req__extract)
        #' @return data.frame
        #' @noRd
        .__pubmed__build__data = function(id, data) {
            pubs <- data.frame(
                uid = as.character(id),
                pubDate = as.Date(data[["result"]][[id]][["sortpubdate"]]),
                fullJournalName = data[["result"]][[id]][["fulljournalname"]],
                volume = data[["result"]][[id]][["volume"]],
                doiUrl = gsub(
                    pattern = "doi: ",
                    replacement = "https://doi.org/",
                    x = data[["result"]][[id]][["elocationid"]]
                ),
                doiLabel = gsub(
                    pattern = "doi: ",
                    replacement = "",
                    x = data[["result"]][[id]][["elocationid"]]
                ),
                title = data[["result"]][[id]][["title"]],
                authors = paste0(
                    sapply(
                        data[["result"]][[id]][["authors"]], function(n) n[["name"]]
                    ),
                    collapse = "; "
                )
            )

            pubs <- pubs[order(pubs$pubDate, decreasing = TRUE), ]
            tibble::as_tibble(pubs)
        },

        #' @name ._pubmed_get_ids
        #' @description using a user supplied string, fetch all relevant pubmedIDs
        #' @param query string containing a search term
        #' @return a character vector containing pubmed IDs
        #' @noRd
        ._pubmed_get_ids = function(query) {
            url <- paste0(
                "https://eutils.ncbi.nlm.nih.gov/entrez/eutils/esearch.fcgi?",
                "db=pubmed",
                "&term=", utils::URLencode(query),
                "&retmode=json"
            )

            resp <- self$.__req__send(url)
            if (resp$status_code != 200) {
                cli::cli_alert_warning(
                    "Failed to retrived data:{.val {resp$status_code}}"
                )
                return(NA)
            }

            ids <- self$.__req__extract(resp)

            cli::cli_alert_success("Found {.val {length(ids)}} pubmed ID{?s}")
            self$ids <- ids[["esearchresult"]][["idlist"]]
        },

        #' @name ._pubmed_get_data
        #' @description fetch publication metadata using IDs
        #' @param delay time (ms) to rest inbetween requests
        #' @noRd
        ._pubmed_get_data = function(delay) {
            cli::cli_alert_info(
                "Fetching pubmed data for {.val {length(self$ids)}} ID{?s}"
            )
            data <- data.frame()
            for (id in self$ids) {
                cli::cli_alert_info("Fetching data for {.val {id}}")
                resp <- self$.__req__send(url = paste0(
                    "https://eutils.ncbi.nlm.nih.gov/entrez/eutils/esummary.fcgi?",
                    "db=pubmed",
                    "&id=", id,
                    "&retmode=json&"
                ))
                if (resp$status_code == 200) {
                    d <- self$.__req__extract(resp)
                    row <- self$.__pubmed__build__data(id, d)
                    if (NROW(data) == 0) {
                        data <- row
                    } else {
                        data <- rbind(data, row)
                    }
                } else {
                    cli::cli_alert_danger("Failed to get data for {.val {id}}")
                    self$failed <- c(self$failed, id)
                }
                Sys.sleep(delay)
            }

            self$data <- data
        },

        #' @name pubmed
        #' @description find pubmed metadata based on a search query
        #' @param query a pubmed search query
        #' @return
        #' @noRd
        pubmed = function(query, delay = 0.4) {
            self$query <- query
            self$._pubmed_get_ids(query)
            self$._pubmed_get_data(delay)
            self$print()
        },

        #' @name print
        #' @noRd
        print = function() {
            list(query = self$query, ids = self$ids, data = self$data)
        }
    )
)
