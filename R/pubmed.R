#'////////////////////////////////////////////////////////////////////////////
#' FILE: pubmed.R
#' AUTHOR: David Ruvolo
#' CREATED: 2021-01-20
#' MODIFIED: 2021-02-05
#' PURPOSE: source publications list from pubmed
#' STATUS: working
#' PACKAGES: httr; rjson; purrr;
#' COMMENTS: NA
#'////////////////////////////////////////////////////////////////////////////

#' @name pubmed
#' @description Mini API client for querying publication metadata from Pubmed
#' @noRd
pubmed <- R6::R6Class(
    classname = "pubmed",
    public = list(
        #' @name queryPublications
        #' @description find pubmed metadata based on a search query
        #' @param query a pubmed search query
        #' @return
        #' @noRd
        queryPublications = function(query) {
            private$getPubmedIds(query)
            private$getPubmedData(delay = 0.5)
            self$print()
        },
        print = function() {
            list(ids = private$ids, data = private$data)
        }
    ),
    private = list(
        #' @field ids a vector containing pubmed IDs
        ids = NA,

        #' @field a data object containing publication metadata
        data = NA,

        #' @name .validateResponse
        #' @description given a set of response codes, validate response object
        #' @param codes an numeric vector containing respnse codes
        #' @return bool
        #' @noRd
        .__validateResponse = function(codes, response) {
            response$status_code %in% codes
        },

        #' @name .__createBlankPubDf
        #' @description should a request fail, return a blank dataset
        #' @param id string containing a publication ID
        #' @param resp httr response object
        #' @blank If TRUE, an empty data.frame object will be returned.
        #'      This argument overrides evertything else. Useful for cases
        #'      where the response fails and you don't want to break the
        #'      loop.
        #' @return data.frame
        #' @noRd
        .__createPubDataFrame = function(id, resp, blank = FALSE) {
            pubs <- data.frame(
                uid = as.character(id),
                pubDate = NA,
                fullJournalName = NA,
                volume = NA,
                doiUrl = NA,
                doiLabel = NA,
                title = NA,
                authors = NA
            )

            if (blank)
                return(pubs)

            # pubs$uid <- as.character(response[["result"]][["uids"]])
            pubs$pubDate <- resp[["result"]][[id]][["sortpubdate"]]
            pubs$fullJournalName <- resp[["result"]][[id]][["fulljournalname"]]
            pubs$volume <- resp[["result"]][[id]][["volume"]]
            pubs$doiUrl <- gsub(
                pattern = "doi: ",
                replacement = "https://doi.org/",
                x = resp[["result"]][[id]][["elocationid"]]
            )
            pubs$doiLabel <- gsub(
                pattern = "doi: ",
                replacement = "",
                x = resp[["result"]][[id]][["elocationid"]]
            )
            pubs$title <- resp[["result"]][[id]][["title"]]
            pubs$authors <- paste0(
                sapply(
                    resp[["result"]][[id]][["authors"]], function(n) n[["name"]]
                ),
                collapse = "; "
            )

            pubs$pubDate <- as.Date(lubridate::ymd(pubs$pubDate))
            pubs <- pubs[order(pubs$pubDate, decreasing = TRUE), ]
            pubs
        },

        #' @name sendGetRequest
        #' @description wrapper around GET for standardizing requests
        #' @param url Api endpoint to send request (host and params) to
        #' @return httr response object
        #' @noRD
        .__sendGetRequest = function(url) {
            httr::GET(url, httr::add_headers(`Content-Type` = "application/json"))
        },

        #' @name extractGetRequest
        #' @description extract content from response
        #' @param response httr response object
        #' @return list object
        #' @noRd
        .__extractGetRequest = function(response) {
            r <- httr::content(response, "text", encoding = "UTF-8")
            rjson::fromJSON(r)
        },

        #' @name getPubmedIds
        #' @description using a user supplied string, fetch all relevant pubmedIDs
        #' @param query string containing a search term
        #' @return a character vector containing pubmed IDs
        #' @noRd
        getPubmedIds = function(query) {
            url <- paste0(
                "https://eutils.ncbi.nlm.nih.gov/entrez/eutils/esearch.fcgi?",
                "db=pubmed",
                "&term=", utils::URLencode(query),
                "&retmode=json"
            )

            resp <- private$.__sendGetRequest(url)
            if (!private$.__validateResponse("200", resp)) {
                cli::cli_alert_warning(
                    "Failed to retrived data:{.val {resp$status_code}}"
                )
                return(NA)
            }

            ids <- private$.__extractGetRequest(resp)
            private$ids <- ids[["esearchresult"]][["idlist"]]
            cli::cli_alert_success("Found {.val {length(ids)}} pubmed ID{?s}")

        },

        #' @name getPubmedData
        #' @description fetch publication metadata using IDs
        #'
        getPubmedData = function(delay = 0.5) {
            cli::cli_alert_info("Fetching pubmed data for {.val {private$ids}} ID{?s}")

            rows <- list()
            for (id in private$ids) {
                url <- paste0(
                    "https://eutils.ncbi.nlm.nih.gov/entrez/eutils/esummary.fcgi?",
                    "db=pubmed",
                    "&id=", id,
                    "&retmode=json&"
                )

                resp <- private$.__sendGetRequest(url)

                if (resp$status_code == 200) {
                    row <- private$.__createPubDataFrame(id, resp)
                    rows[[id]] <- row
                } else {
                    cli::cli_alert_danger("Failed to fetch data for {.val {id}}")
                }


                Sys.sleep(delay)
            }

            private$data <- rows
        }
    )
)


# test
p <- pubmed$new()
p$queryPublications(query = "Ruvolo, David[Author]")
