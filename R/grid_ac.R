#'
#' @name grid
#' @author dcruvolo
#'
#' @description Methods for downloading and processing Grid releases
#'
#' @section Instructions:
#'
#' The `grid` class is designed to be a two step process where 1) users fetch
#' a list of available releases (`listReleases`) and then 2) download the
#' preferred release (`downloadRelease`). By default, the download function
#' retrieves the latest version and saves the data to the current directory.
#'
#' @section Disclaimer:
#'
#' The grid class is not an official method for downloading releases nor is it
#' affiliated with the grid.ac project. I developed this to help maintain a
#' database. All data is stored on Figma, but to my knowledge there isn't a
#' clear method for downloading datasets from Figma in R.
#'
#' @examples
#' \dontrun{
#' # download the latest release
#' g <- grid$new()
#' g$listReleases()
#' g$downloadRelease() # default version is "latest"
#' g$print()
#' saveRDS(g$data, "grid_data.RDS")
#' }
#'
#' @import rvest, purrr, cli, R6, dplyr, wordstonumbers, data.table
#' @noRd
grid <- R6::R6Class(
    classname = "grid.ac",
    public = list(
        #' @field releases object for storing releases metadata
        releases = data.frame(),

        #' @field data returned Grid Data
        data = data.frame(),

        #' @name listReleases
        #' @param showReleaseNotes If TRUE, release notes will be shown
        #' @return a data.frame
        listReleases = function() {
            if (NROW(self$releases) > 0)
                return(
                    self$releases[, names(self$releases) %in% private$printVars]
                )

            # otherwise build
            cli::cli_alert_info("Fetching release information...")
            tryCatch({
                self$releases <- private$._listReleases()
                cli::cli_alert_success("Success!")
                self$releases[, names(self$releases) %in% private$printVars]
            }, error = function(err) {
                cli::cli_alert_danger("Unable to build release data:\n{.val {err}}")
            }, warning = function(warn) {
                cli::cli_alert_danger("Unable to build release data:\n{.val {warn}}")
            })
        },
        #' @name downloadRelease
        #' @description download a release by version number
        #' @param version selected version to download (default = latest)
        #' @return NULL
        #' @noRD
        downloadRelease = function(version = "latest") {
            validOpts <- c("latest", self$releases$releaseVersion)
            if (!version %in% validOpts) {
                cli::cli_alert_warning("version {.val {version}} is invalid")
            }

            if (version %in% validOpts) {
                v <- version
                if (v == "latest")
                    v <- self$releases$releaseVersion[1]
                tryCatch({
                    self$data <- private$._downloadRelease(version = v)
                    cli::cli_alert_success("Sucessfully processed release!")
                }, error = function(err) {
                    cli::cli_alert_danger("Failed to download file:\n{.val {err}}")
                }, warning = function(warn) {
                    cli::cli_alert_warning("Failed to download file:\n{.val {warn}}")
                })
            }
        },

        #' @name print
        print = function() {
            return(
                list(
                    releases = self$releases[
                        , names(self$releases) %in% private$printVars
                    ],
                    data = self$data
                )
            )
        }
    ),
    private = list(
        #' @field host hostname of website
        host = "https://grid.ac",

        #' @field hostDownloadPath path to downloads page
        hostDownloadPath = "/downloads",

        #' @field printVars vector of default columns to print
        printVars = c(
            "id",
            "releaseDate",
            "releaseVersion",
            "totalInstitutions",
            "downloadUrl"
        ),

        #' @name .__extract__html__cards__
        #' @description extract all elements that contain information by release
        #' @param html XML nodeset (rvest output)
        #' @return XML nodeset
        #' @noRd
        .__extract__html__cards__ = function(html) {
            rvest::html_elements(html, "main > article .container > .row.mt-5 .card")
        },

        #' @name .__extract__html__tables__
        #' @description convert html tables to R data.frames
        #' @param html an XML nodeset of card elements (see .extract$cards)
        #' @return data.frame
        #' @noRd
        .__extract__html__tables__ = function(html) {
            d <- purrr::imap_dfr(html, function(el, i) {
                cbind(
                    rvest::html_table(
                        rvest::html_element(
                            el, ".card-body table.table.release"
                        )
                    ),
                    "id" = i
                )
            })

            tbl <- reshape(d, idvar = "id", timevar = "X1", direction = "wide")
            colnames(tbl) <- c("id", "doi", "totalInstitutions", "releaseNotes")
            tbl
        },

        #' @name .extract__html__hlevel__
        #' @description Pull text from heading level
        #' @param html an XML nodeset of card elements (see .__extract__html__cards__)
        #' @param h html heading level (1:6)
        #' @return data.frame
        #' @noRd
        .__extract__html__hlevel__ = function(html, h = 3) {
            stopifnot("Invalid Html Heading Level" = h %in% 1:6)
            purrr::imap_dfr(html, function(x, i) {
                data.frame(
                    id = i,
                    releaseDate = gsub(
                        pattern = "Release",
                        replacement = "",
                        x = rvest::html_text(
                            rvest::html_element(x, paste0(".card-header h", h)),
                            trim = TRUE
                        )
                    ),
                    releaseVersion = gsub(
                        pattern = "(The|public release of GRID|[.])",
                        replacement = "",
                        rvest::html_text(
                            rvest::html_element(x, ".card-body p:nth-child(2)"),
                            trim = TRUE
                        ),
                        perl = TRUE
                    )
                )
            })
        },

        #' @name .__extract__html__href__
        #' @description pull download Urls
        #' @param html an XML nodeset of card elements (see .extract$cards)
        #' @return data.frame
        #' @noRd
        .__extract__html__href__ = function(html) {
            purrr::imap_dfr(html, function(x, i) {
                data.frame(
                    id = i,
                    downloadUrl = rvest::html_attr(
                        x = rvest::html_element(
                            x, ".card-body > p.text-center a.btn.btn-red"
                        ),
                        name = "href"
                    )
                )
            })
        },

        #' @name ._listReleases
        #' @description Pull Html content from host + path
        #' @return data.frame
        ._listReleases = function() {

            # read page and extract card elements
            html <- rvest::read_html(paste0(private$host, private$hostDownloadPath))
            cards <- private$.__extract__html__cards__(html)

            # pull data
            content <- list(
                headings = private$.__extract__html__hlevel__(cards, 3),
                urls = private$.__extract__html__href__(cards),
                tables = private$.__extract__html__tables__(cards)
            )

            # convert word to number
            content$headings$releaseVersion <- purrr::map_chr(
                content$headings$releaseVersion, function(x) {
                    x <- trimws(tolower(x))
                    val <- dplyr::case_when(
                        grepl("first", x) ~ gsub("first", "one", x),
                        grepl("second", x) ~ gsub("second", "two", x),
                        grepl("third", x) ~ gsub("third", "three", x),
                        grepl("fourth", x) ~ gsub("fourth", "four", x),
                        grepl("fifth", x) ~ gsub("fifth", "five", x),
                        grepl("sixth", x) ~ gsub("sixth", "six", x),
                        grepl("seventh", x) ~ gsub("seventh", "seven", x),
                        grepl("eighth", x) ~ gsub("eighth", "eight", x),
                        grepl("ninth", x) ~ gsub("ninth", "nine", x),
                        grepl("hundredth", x) ~ gsub("hundredth", "hundred", x),
                        grepl("thousandth", x) ~ gsub("thousandth", "thousand", x),
                        grepl("(tieth)", x, perl = TRUE) ~ gsub("tieth", "ty", x),
                    )
                    wordstonumbers::words_to_numbers(val)
                }
            )

            # merge
            Reduce(
                f = function(df1, df2) merge(df1, df2, by = "id", all = TRUE),
                x = content
            )
        },

        #' @name ._downloadRelease
        ._downloadRelease = function(version) {
            cli::cli_alert_info("Downloading grid.ac {.val {version}}...")
            url <- self$releases$downloadUrl[
                self$releases$releaseVersion == version
            ]

            # init temp locations
            tmpDir <- tempdir()
            tmpFile <- tempfile(tmpdir = tmpDir, fileext = ".zip")
            download.file(url, tmpFile)

            # unzip
            files <- unzip(tmpFile, list = TRUE)
            unzip(tmpFile, exdir = tmpDir, overwrite = TRUE)

            cli::cli_alert_info("Processing data...")
            # load raw data and process
            insts <- data.table::fread(
                file = file.path(tmpDir, "full_tables/institutes.csv")
            )
            addrs <- data.table::fread(
                file = file.path(tmpDir, "full_tables/addresses.csv")
            )

            # flatten acronyms by grid_id
            acron <- data.table::fread(
                file = file.path(tmpDir, "full_tables/acronyms.csv")
            )[, acronym2 := paste0(acronym, collapse = ";"), by = .(grid_id)][
                !duplicated(grid_id), .(grid_id, acronym = acronym2)
            ]

            # flatten alternative organisation names
            alias <- data.table::fread(
                file = file.path(tmpDir, "full_tables/aliases.csv")
            )[, aliases := paste0(alias, collapse = "; "), by = .(grid_id)][
                !duplicated(grid_id), .(grid_id, aliases)
            ]

            # I think most organisations are given a single type, but
            # flatten data just in case
            types <- data.table::fread(
                file = file.path(tmpDir, "full_tables/types.csv")
            )[, type2 := paste0(type, collapse = "; "), by = grid_id][
                !duplicated(grid_id), .(grid_id, type = type2)
            ]


            # Merge DTs
            gridData <- Reduce(
                function(...) merge(..., all.x = TRUE),
                list(insts, addrs, types, alias, acron)
            )

            unlink(tmpDir)
            unlink(tmpFile)
            gridData
        }
    )
)
