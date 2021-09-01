#'////////////////////////////////////////////////////////////////////////////
#' FILE: dev.R
#' AUTHOR: David Ruvolo
#' CREATED: 2021-09-01
#' MODIFIED: 2021-09-01
#' PURPOSE: workspace management
#' STATUS: in.progress
#' PACKAGES: NA
#' COMMENTS: NA
#'////////////////////////////////////////////////////////////////////////////


renv::status()
renv::snapshot()


#' Configuration and Authentication
#' Occasionally, PAT may need to be reset
credentials::set_github_pat()
