# Kaiaulu - https://github.com/sailuh/kaiaulu
#
# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at https://mozilla.org/MPL/2.0/.


########## OpenHub Functions / Ohloh API Interfacing Functions ##########

#' Download a single page of Organization XML Response File.
#'
#' @description Downloads an XML response file from "organization" collection
#' endpoint. The XML response file will return a single page containing the
#' single organization item. The XML response file is saved to disk using the
#' function \code{\link{openhub_download}} with the file name
#' "organization_orgname_unixtimestamp.xml", where "orgname" is the short,
#' unique, handle for the organization from `html_url_or_name`, and
#' "unixtimestamp" is the integer unix timestamp.
#'
#' @param token Your OpenHub API token.
#' @param save_folder_path A folder path to save the downloaded XML page "as-is".
#' @param html_url_or_name Either the URL for the organization page on OpenHub (e.g. "https://openhub.net/orgs/apache") or the short, unique, handle for the organization (e.g. "apache").
#' @return The single page XML response file that contains a single organization item.
#' @references For organization collection details, see \url{https://github.com/blackducksoftware/ohloh_api/blob/main/reference/organization.md}.
#' @export
openhub_api_organizations <- function(token, save_folder_path, html_url_or_name) {

  org_name <- html_url_or_name

  if (stringi::stri_detect_fixed(html_url_or_name, "https")) {

    org_name <- stringi::stri_extract_last_regex(html_url_or_name, "[^/]+$")

  }

  type <- "organization"

  timestamp <- as.integer(as.POSIXct(Sys.time(), tz = "UTC"))

  http_get_request <- paste0("https://openhub.net/orgs/", org_name, ".xml?api_key=", token)

  api_response <- httr::GET(http_get_request)

  openhub_download(api_response, save_folder_path, timestamp, type, unique_information=org_name)

  invisible(api_response)
}

#' Downloads a single page of Portfolio Projects XML Response File.
#'
#' @description Downloads an XML response file from the "portfolio_projects" collection
#' endpoint. The XML response file will return a single page containing a list
#' of portfolio project items belonging to a specific organization specified
#' by `org_name`. The XML response file is saved to disk using the
#' function \code{\link{openhub_download}} with the file name
#' "portfolio_unixtimestamp_orgname_pagenumber.xml", where "unixtimestamp" is
#' the integer unix timestamp, "orgname" is the short, unique, handle for the
#' organization from `org_name`, and "pagenumber" is the current indexed page
#' from `page`. This function returns a response file that is iterable (by
#' pages), the function \code{\link{openhub_api_iterate_pages}} may iterate
#' and download each page from this endpoint unifying the "unixtimestamp" value
#' for the file name to be consistent across the pages (ensure `iterating` is set
#' appropriately).
#'
#' @param token Your OpenHub API token.
#' @param save_folder_path A folder path to save the downloaded XML page "as-is".
#' @param org_name The short, unique, handle for the organization taken from the organization's URL (e.g. "apache" from "https://openhub.net/orgs/apache").
#' @param iterating If TRUE, downloading will be disabled and it is assumed to be handled by \code{\link{openhub_api_iterate_pages}} to synchronize timestamps, if FALSE, downloading will occur (Default set to FALSE).
#' @param page The page number to index (Default set to 1) (e.g. 1).
#' @return The single page XML response file that contains a list of portfolio project items.
#' @references For portfolio projects collection details, see \url{https://github.com/blackducksoftware/ohloh_api/blob/main/reference/portfolio_projects.md}.
#' @export
openhub_api_portfolio_projects <- function(token, save_folder_path, org_name, iterating=FALSE, page=1) {

  type <- "portfolio"

  timestamp <- as.integer(as.POSIXct(Sys.time(), tz = "UTC"))

  http_get_request <- paste0("https://openhub.net/orgs/", org_name, "/projects.xml?api_key=", token, "&page=", page)

  api_response <- httr::GET(http_get_request)

  if (iterating) {
    result <- list()
    result[["api_response"]] <- api_response
    result[["type"]] <- type
    result[["timestamp"]] <- timestamp

    invisible(result)
  } else {
    openhub_download(api_response, save_folder_path, timestamp, type, unique_information=org_name, page=page)

    invisible(api_response)
  }
}

#' Downloads a single page of Project XML Response File.
#'
#' @description Downloads an XML response file from the "project" collection
#' endpoint. The XML response file will return a single page containing a list
#' of project items that are queried (collection request parameter from the
#' "project" collection endpoint that returns all projects that contain the
#' `project_name` text, akin to a "ctrl+F" search rather than a standard query)
#' from the global list of projects using `project_name` to a specify the
#' desired project. The XML response file is saved to disk using the
#' function \code{\link{openhub_download}} with the file name
#' "project_unixtimestamp_projectname_pagenumber.xml", where "unixtimestamp" is
#' the integer unix timestamp, "projectname" is the sanitized version of the
#' name of the project from `project_name`, and "pagenumber" is the current
#' indexed page from `page`. This function returns a response file that is
#' iterable (by pages), the function \code{\link{openhub_api_iterate_pages}}
#' may iterate and download each page from this endpoint unifying the
#' "unixtimestamp" value for the file name to be consistent across the pages
#' (ensure `iterating` is set appropriately).
#'
#' @param token Your OpenHub API token.
#' @param save_folder_path A folder path to save the downloaded XML page "as-is".
#' @param project_name The unique name of the project (e.g. "Apache Tomcat").
#' @param iterating If TRUE, downloading will be disabled and it is assumed to be handled by \code{\link{openhub_api_iterate_pages}} to synchronize timestamps, if FALSE, downloading will occur (Default set to FALSE).
#' @param page The page number to index (Default set to 1) (e.g. 1).
#' @return The single page XML response file that contains a list of project items.
#' @references For project collection details, see \url{https://github.com/blackducksoftware/ohloh_api/blob/main/reference/project.md}.
#' @export
openhub_api_projects <- function(token, save_folder_path, project_name, iterating=FALSE, page=1) {

  type <- "project"

  timestamp <- as.integer(as.POSIXct(Sys.time(), tz = "UTC"))

  http_get_request <- paste0("https://www.openhub.net/p.xml", "?api_key=", token, "&query=", URLencode(project_name), "&page=", page)

  api_response <- httr::GET(http_get_request)

  if (iterating) {
    result <- list()
    result[["api_response"]] <- api_response
    result[["type"]] <- type
    result[["timestamp"]] <- timestamp

    invisible(result)
  } else {
    openhub_download(api_response, save_folder_path, timestamp, type, unique_information=project_name, page=page)

    invisible(api_response)
  }
}

#' Downloads a single page of Analysis XML Response File.
#'
#' @description Downloads an XML response file from the "analysis" collection
#' endpoint. The XML response file will return a single page containing the
#' single analysis item. The XML response file is saved to disk using the
#' function \code{\link{openhub_download}} with the file name
#' "analysis_projectname_unixtimestamp.xml", where "projectname" is the
#' sanitized version of the name of the project from `project_name`, and
#' "unixtimestamp" is the integer unix timestamp derived from the most recent
#' time this analysis collection was updated for the project ("updated_at"
#' field in the analysis collection).
#'
#' @param token Your OpenHub API token.
#' @param save_folder_path A folder path to save the downloaded XML page "as-is".
#' @param project_id The unique ID of the project (e.g. 3562).
#' @param project_name The unique name of the project (e.g. "Apache Tomcat").
#' @return The single page XML response file that contains a single analysis item.
#' @references For analysis collection details, see \url{https://github.com/blackducksoftware/ohloh_api/blob/main/reference/analysis.md}.
#' @export
openhub_api_analyses <- function(token, save_folder_path, project_id, project_name) {

  analysis_collection_site_start <- "https://www.openhub.net/p/"

  analysis_collection_site_end <- "/analyses/latest.xml"

  type <- "analysis"

  timestamp <- NULL

  http_get_request <- paste0(analysis_collection_site_start, URLencode(project_id), analysis_collection_site_end, "?api_key=", token)

  api_response <- httr::GET(http_get_request)

  xmlDoc <- XML::xmlParse(api_response, validate=F)
  root <- XML::xmlRoot(xmlDoc)
  status <- XML::xmlValue(root[[1]]) # the value of <status>
  returnItems <- root[[2]] # <result>
  if (status == "success") {
    updated_at <- XML::xmlValue(returnItems[[1]][[4]]) # value of <analysis><updated_at>
    timestamp <- as.integer(as.POSIXct(updated_at, format="%Y-%m-%dT%H:%M:%SZ", tz="UTC"))
  } else if (status == "Not Found") {
    warning(paste0("openhub_api_analyses: No current analysis collection found for project name: ", project_name))
  } else {
    warning(paste0("openhub_api_analyses: ", status)) # prints the status warning message
  }

  if (!is.null(timestamp)) {
    openhub_download(api_response, save_folder_path, timestamp, type, unique_information=project_name)
  }

  invisible(api_response)
}

#' Parses Organization XML Responses to Table.
#'
#' @description Parses a list of XML responses `api_responses` containing
#' organization items, extracting relevant tags from the each XML response in
#' `api_responses`. This function returns a parsed version of the XML responses
#' in a table format.
#'
#' @param api_responses A list of XML responses obtained from \code{\link{openhub_api_organizations}} function.
#' @return A parsed version of the XML responses into a table with relevant columns.
#' @export
openhub_parse_organizations <- function(api_responses) {
  parse_response <- function(api_response) {
    xmlDoc <- XML::xmlParse(api_response, validate=F)
    root <- XML::xmlRoot(xmlDoc)
    status <- XML::xmlValue(root[[1]]) # <status>
    returnItems <- root[[2]] # <result>
    parsed_response <- list()
    if (status == "success") {
      parsed_response[["name"]] <- append(parsed_response[["name"]], XML::xmlValue(returnItems[[1]][[1]])) # <result><org><name>
      parsed_response[["html_url"]] <- append(parsed_response[["html_url"]], XML::xmlValue(returnItems[[1]][[3]])) # <result><org><html_url>
      parsed_response[["portfolio_projects"]] <- append(parsed_response[["portfolio_projects"]], XML::xmlValue(returnItems[[1]][[13]][[4]])) # <result><org><infographic_details><portfolio_projects>
    } else {
      stop(paste0("openhub_parse_organizations: ", status)) # prints the status warning message
    }

    parsed_response <- as.data.table(parsed_response)

    return(parsed_response)
  }
  return(rbindlist(lapply(api_responses,parse_response),fill=TRUE))
}

#' Parses Portfolio Projects XML Responses to Table.
#'
#' @description Parses a list of XML responses `api_responses` containing
#' portfolio project items, extracting relevant tags from each XML response in
#' `api_responses`. This function returns a parsed version of the XML responses in a
#' table format.
#'
#' @param api_responses A list of XML responses obtained from \code{\link{openhub_api_portfolio_projects}} function.
#' @return A parsed version of the XML responses into a table with relevant columns.
#' @export
openhub_parse_portfolio_projects <- function(api_responses) {
  parse_response <- function(api_response) {
    xmlDoc <- XML::xmlParse(api_response, validate=F)
    root <- XML::xmlRoot(xmlDoc)
    itemsReturned <- XML::xmlValue(root[[2]]) # <items_returned>
    status <- XML::xmlValue(root[[1]]) # <status>
    returnItems <- root[[5]] # <result>
    parsed_response <- list()
    if (status == "success") {
      for (i in 1:itemsReturned) {
        parsed_response[["name"]] <- append(parsed_response[["name"]], XML::xmlValue(returnItems[[1]][[i]][[1]])) # <result><portfolio_projects><project><name>
        parsed_response[["primary_language"]] <- append(parsed_response[["primary_language"]], XML::xmlValue(returnItems[[1]][[i]][[3]])) # <result><portfolio_projects><project><primary_language>
        parsed_response[["activity"]] <- append(parsed_response[["activity"]], XML::xmlValue(returnItems[[1]][[i]][[2]])) # <result><portfolio_projects><project><activity>
      }
    } else {
      stop(paste0("openhub_parse_portfolio_projects: ", status)) # prints the status warning message
    }

    parsed_response <- as.data.table(parsed_response)

    return(parsed_response)
  }

  return(rbindlist(lapply(api_responses,parse_response),fill=TRUE))
}

#' Parses Project XML Responses to Table.
#'
#' @description Parses a list of XML responses `api_responses` containing
#' project items, extracting relevant tags from each XML response in
#' `api_responses`. This function returns a parsed version of the XML responses
#' in a table format.
#'
#' @param api_responses A list of XML responses obtained from \code{\link{openhub_api_projects}} function.
#' @return A parsed version of the XML responses into two tables with relevant columns.
#' @export
openhub_parse_projects <- function(api_responses) {
  parse_response <- function(api_response) {
    xmlDoc <- XML::xmlParse(api_response, validate=F)
    root <- XML::xmlRoot(xmlDoc)
    itemsReturned <- XML::xmlValue(root[[2]]) # <items_returned>
    status <- XML::xmlValue(root[[1]]) # <status>
    returnItems <- root[[5]] # <result>
    project_data <- list()
    project_links <- list()
    if (status == "success") {
      for (i in 1:itemsReturned) {
        project_data[["name"]] <- append(project_data[["name"]], XML::xmlValue(returnItems[[i]][[2]])) # <result><project><name>
        project_data[["id"]] <- append(project_data[["id"]], XML::xmlValue(returnItems[[i]][[1]])) # <result><project><id>
        project_data[["html_url"]] <- append(project_data[["html_url"]], XML::xmlValue(returnItems[[i]][[4]])) # <result><project><html_url>
        links_tag <- returnItems[[i]][[23]] # <links> tag (sometimes present in a project's api response)
        if (!is.null(links_tag)) {
          links <- XML::xmlChildren(links_tag)
          for (j in seq_along(links)) {
            link <- links[[j]] # j-th <link> tag in <links>
            link_title <- as.character(XML::xmlValue(link[[1]])) # <title> in the specific <link>
            link_url <- as.character(XML::xmlValue(link[[2]])) # <url> in the specific <link>
            link_category <- XML::xmlValue(link[[3]]) # <category> in specific the <link>
            project_links[["name"]] <- append(project_links[["name"]], XML::xmlValue(returnItems[[i]][[2]])) # <result><project><name>
            project_links[["id"]] <- append(project_links[["id"]], XML::xmlValue(returnItems[[i]][[1]])) # <result><project><id>
            project_links[["project_link_title"]] <- append(project_links[["project_link_title"]], link_title)
            project_links[["project_link_category"]] <- append(project_links[["project_link_category"]], link_category)
            project_links[["project_link_url"]] <- append(project_links[["project_link_url"]], link_url)
          }
        }
      }
    } else {
      stop(paste0("openhub_parse_projects: ", status)) # prints the status warning message
    }

    project_data <- as.data.table(project_data)
    project_links <- as.data.table(project_links)

    return(list("project_data" = project_data, "project_links" = project_links))
  }

  parsed_responses <- lapply(api_responses,parse_response)

  project_data <- lapply(parsed_responses, function(x) x[["project_data"]])

  project_links <- lapply(parsed_responses, function(x) x[["project_links"]])

  double_data_tables_list <- list(rbindlist(project_data,fill=TRUE),
                             rbindlist(project_links,fill=TRUE))

  return(double_data_tables_list)
}

#' Parses Analysis XML Responses to Table.
#'
#' @description Parses a list of XML responses `api_responses` containing
#' analysis items, extracting relevant tags from each XML response in
#' `api_responses`. This function returns a parsed version of the XML responses
#' in a table format.
#'
#' @param api_responses A list of XML responses obtained from \code{\link{openhub_api_analyses}} function.
#' @return A parsed version of the XML responses into a table with relevant columns.
#' @export
openhub_parse_analyses <- function(api_responses) {
  parse_response <- function(api_response) {
    xmlDoc <- XML::xmlParse(api_response, validate=F)
    root <- XML::xmlRoot(xmlDoc)
    status <- XML::xmlValue(root[[1]]) # <status>
    returnItems <- root[[2]] # <result>
    parsed_response <- list()
    if (status == "success") {
      parsed_response[["id"]] <- append(parsed_response[["id"]], XML::xmlValue(returnItems[[1]][[3]])) # <result><analysis><id>
      parsed_response[["min_month"]] <- append(parsed_response[["min_month"]], XML::xmlValue(returnItems[[1]][[6]])) # <result><analysis><min_month>
      parsed_response[["twelve_month_contributor_count"]] <- append(parsed_response[["twelve_month_contributor_count"]], XML::xmlValue(returnItems[[1]][[8]])) # <result><analysis><twelve_month_contributor_count>
      parsed_response[["total_contributor_count"]] <- append(parsed_response[["total_contributor_count"]], XML::xmlValue(returnItems[[1]][[9]])) # <result><analysis><total_contributor_count>
      parsed_response[["twelve_month_commit_count"]] <- append(parsed_response[["twelve_month_commit_count"]], XML::xmlValue(returnItems[[1]][[10]])) # <result><analysis><twelve_month_commit_count>
      parsed_response[["total_commit_count"]] <- append(parsed_response[["total_commit_count"]], XML::xmlValue(returnItems[[1]][[11]])) # <result><analysis><total_commit_count>
      parsed_response[["total_code_lines"]] <- append(parsed_response[["total_code_lines"]], XML::xmlValue(returnItems[[1]][[12]])) # <result><analysis><total_code_lines>
      languages <- XML::xmlChildren(returnItems[[1]][[14]]) # <result><analysis><languages> children tags
      code_languages_data_text <- list()
      for (i in seq_along(languages)) {
        language <- languages[[i]]
        code_language_percentage <- paste0(XML::xmlGetAttr(language, "percentage"), "%") # adds a percentage symbol to the end of the percentage value for the code language
        code_language <- stringi::stri_trim_both(stringi::stri_replace_all_fixed(XML::xmlValue(language), "\n", "")) # extracts code language text, then removes spaces and new line characters
        code_languages_data_text[[i]] <- paste(code_language_percentage, code_language)
      }
      code_languages_data_text <- paste(code_languages_data_text, collapse = ", ")
      parsed_response[["code_languages"]] <- append(parsed_response[["code_languages"]], code_languages_data_text)
    } else {
      stop(paste0("openhub_parse_analyses: ", status)) # prints the status warning message
    }

    parsed_response <- as.data.table(parsed_response)

    return(parsed_response)
  }

  return(rbindlist(lapply(api_responses,parse_response),fill=TRUE))
}

#' OpenHub API Response Downloader
#'
#' @description Stores a XML response file `api_response` into a defined folder
#' path `save_folder_path`. This function sanitizes the `unique_information`
#' parameter, if not NULL, (`html_url_or_name` in
#' \code{\link{openhub_api_organizations}}, `org_name` in
#' \code{\link{openhub_api_portfolio_projects}}, `project_name` in
#' \code{\link{openhub_api_projects}}, and `project_name` in
#' \code{\link{openhub_api_analyses}}) for use in the file name
#' (e.g. "Apache (Java_Test)" becomes "apachejavatest").
#'
#' @param save_folder_path A folder path to save the downloaded XML response file "as-is".
#' @param api_response A single XML response file obtained from an openhub_api_* function.
#' @param type The type of XML response file ("organization", "portfolio", "project", or "analysis").
#' @param unique_information Identifying information about the type of XML response (Default set to NULL).
#' @param page The page number of the response file (Default set to NULL).
#' @export
openhub_download <- function(api_response, save_folder_path, timestamp, type, unique_information=NULL, page=NULL) {

  invalid_chars <- "[\\\\/:*?\"<>|()_]"

  if (!is.null(page)) {
    if (!is.null(unique_information)) {
      unique_information <- stringi::stri_replace_all_regex(unique_information, invalid_chars, "")
      unique_information <- stringi::stri_trans_tolower(unique_information)
      unique_information <- stringi::stri_replace_all_fixed(unique_information, " ", "")
      file_name <- stringi::stri_c(type, '_', timestamp, '_', unique_information, '_', page, '.xml')
    } else {
      file_name <- stringi::stri_c(type, '_', timestamp, '_', page, '.xml')
    }
  } else {
    if (!is.null(unique_information)) {
      unique_information <- stringi::stri_replace_all_regex(unique_information, invalid_chars, "")
      unique_information <- stringi::stri_trans_tolower(unique_information)
      unique_information <- stringi::stri_replace_all_fixed(unique_information, " ", "")
      file_name <- stringi::stri_c(type, '_', unique_information, '_', timestamp, '.xml')
    } else {
      file_name <- stringi::stri_c(type, '_', timestamp, '.xml')
    }
  }
  save_file_path <- paste0(save_folder_path, file_name)
  io_make_file(save_file_path, api_response)
}

#' OpenHub Parser Helper
#'
#' @description A helper function for `openhub_parse_*` functions to retrieve
#' XML responses as files from a specified folder path `folder_path`.
#'
#' @param folder_path A folder path to retrieve XML files.
#' @return A list of XML responses as files stored in `folder_path`.
#' @export
openhub_retrieve <- function(folder_path) {
  folder_path <- stri_replace_last_regex(folder_path, "/$", "") # remove trailing slash because full.names is set to TRUE

  api_responses <- list.files(folder_path, full.names = TRUE)

  return(api_responses)
}

#' OpenHub Page Iterator
#'
#' @description Ohloh API endpoints return data in pages, each containing a
#' set number of items. This iterator can be used to iterate over the pages in
#' the collection that corresponds to `openhub_api_function`. `max_pages` can
#' be used to define the maximum number of pages to iterate through, otherwise,
#' the maximum number of pages will be iterated.
#'
#' @param token Your OpenHub API token.
#' @param openhub_api_function A function that downloads a page of a specific XML response file (e.g. \code{\link{openhub_api_portfolio_projects}}).
#' @param save_folder_path A folder path to save the downloaded XML pages "as-is".
#' @param openhub_api_function_parameter Required unique parameter for use in `openhub_api_function` (`org_name` in \code{\link{openhub_api_portfolio_projects}} or `project_name` in \code{\link{openhub_api_projects}}).
#' @param max_pages The maximum number of pages to download, if NULL, maximum number of pages will be used, and if max_pages exceeds the maximum number of pages, it will use the maximum number of pages (Default set to NULL).
#' @export
openhub_api_iterate_pages <- function(token, openhub_api_function, save_folder_path, openhub_api_function_parameter, max_pages=NULL) {
  initial_api_result <- openhub_api_function(token, save_folder_path, openhub_api_function_parameter, iterating=TRUE, page=1)
  initial_api_response <- initial_api_result[["api_response"]]
  type <- initial_api_result[["type"]]
  timestamp <- initial_api_result[["timestamp"]]
  initialXmlDoc <- XML::xmlParse(initial_api_response, validate=F)
  initialRoot <- XML::xmlRoot(initialXmlDoc)
  initialStatus <- XML::xmlValue(initialRoot[[1]]) # <status>
  #print(paste0("Status: ", initialStatus))
  api_responses <- list()

  if (initialStatus == "success") {
    maxPageCount <- 1
    initialItemsReturned <- XML::xmlValue(initialRoot[[2]]) # <items_returned>
    initialItemsAvailable <- XML::xmlValue(initialRoot[[3]]) # <items_available>
    if (!is.na(initialItemsAvailable)) {
      #print(paste0("Items per page / Available Items: ", initialItemsAvailable, " / ", initialItemsReturned))
      maxPageCount <- ceiling(as.numeric(initialItemsAvailable)/as.numeric(initialItemsReturned)) # If the requested XML response file returned successfully, but is only one page with one item, then its maxPageCount is set to 1.
    }
    #print(paste0("maxPageCount is: ", maxPageCount))

    if (!is.nan(maxPageCount)){
      if (!is.null(max_pages)) {
        if (max_pages > maxPageCount) {
          warning(paste0("max_pages is greater than the maximum number of available pages: requesting maximum number of pages possible (", maxPageCount, ")" ))
        } else {
          maxPageCount <- max_pages
        }
      }
      api_responses[[1]] <- initial_api_response
      openhub_download(api_responses[[1]], save_folder_path, timestamp, type, unique_information=openhub_api_function_parameter, page=1)
      if (maxPageCount > 1) {
        for (page in 2:maxPageCount) {
          api_responses[[page]] <- openhub_api_function(token, save_folder_path, openhub_api_function_parameter, iterating=TRUE, page=page)[["api_response"]]
          openhub_download(api_responses[[page]], save_folder_path, timestamp, type, unique_information=openhub_api_function_parameter, page=page)
        }
      }
    } else {
      warning("No items available in the return request.")
    }
  } else {
    stop(paste0("openhub_api_iterate_pages: ", initialStatus)) # prints the status warning message
  }
}

########## Configuration File Parser Functions ##########

#' Returns the parsed configuration file (.yml).
#'
#' @description The input file is expected to be in the .yml format.
#' The function returns a parsed version of the input .yml file, and it will
#' inform the user if the input .yml file path does not exist. The contents
#' of the input .yml file may contain machine-dependent paths that may need to
#' be modified by the user.
#'
#' @param config_path The path of the config file from the kaiaulu directory (e.g. "conf/kaiaulu.yml").
#' @return The parsed config file whose path is specified by `config_path`.
#' @export
parse_config <- function(config_path) {

  conf <- yaml::read_yaml(config_path)

  if (is.null(conf)) {
    warning("Path does not exist.")
  }

  return(conf)
}

##### Git Getter Functions #####

#' Returns the path to the .git of the project repository that is being analyzed.
#'
#' @description This function returns the specific path to the .git of the
#' project repository that is being analyzed specified in the input parameter
#' `config_file`. The input, `config_file` must be a parsed configuration file.
#' The function will inform the user if the .git path of the project repository
#' exists in the parsed configuration file, `config_file`.
#'
#' @param config_file The parsed configuration file obtained from \code{\link{parse_config}}.
#' @return The local git repository path specified in `config_file`.
#' @export
get_git_repo_path <- function(config_file) {

  git_repo_path <- config_file[["version_control"]][["log"]]

  if (is.null(git_repo_path)) {
    warning("Attribute does not exist in the configuration file.")
  }

  return(git_repo_path)
}

#' Returns the list of git branches used for analysis in the current project.
#'
#' @description This function returns a list of the git branches used for
#' analysis in the current project specified in the input parameter
#' `config_file`. The input, `config_file` must be a parsed configuration file.
#' The function will inform the user if the list of branches to be analyzed
#' exists in the parsed configuration file, `config_file`.
#'
#' @param config_file The parsed configuration file obtained from \code{\link{parse_config}}.
#' @return The list of git branches.
#' @export
get_git_branches <- function(config_file) {

  git_branch <- config_file[["version_control"]][["branch"]]

  if (is.null(git_branch)) {
    warning("Attribute does not exist in the configuration file.")
  }

  return(git_branch)
}



##### Mailing List Functions Start #####

#' Returns the list of mailing list mod mbox project keys.
#'
#' @description This function returns the list of mailing list mod mbox project
#' keys, that is specified in the input parameter `config_file`. The input,
#' `config_file` must be a parsed configuration file. The function will inform
#' the user if the project keys exist in the parsed configuration
#' file, `config_file`.
#'
#' @param config_file The parsed configuration file obtained from \code{\link{parse_config}}.
#' @return The list of mod mbox mailing list keys.
#' @export
get_mbox_key_indexes <- function(config_file) {

  mbox_keys <- config_file[["mailing_list"]][["mod_mbox"]]

  if (is.null(mbox_keys)) {
    warning("Attribute does not exist in the configuration file.")
  }

  return(mbox_keys)
}

#' Returns the URL to the archives for mbox for a specific project key.
#'
#' @description This function returns the URL to the archives for a specific
#' project key, `project_key_index`, that is specified in the input parameter
#' `config_file`. The input, `config_file` must be a parsed configuration file.
#' The function will inform the user if the specific URL to the archives for
#' mbox exists in the parsed configuration file, `config_file`.
#'
#' @param config_file The parsed configuration file obtained from \code{\link{parse_config}}.
#' @param project_key_index The name of the index of the project key (e.g. "project_key_1" or "project_key_2").
#' @return The URL of the mbox mailing list archive for project specified by key `project_key_index`.
#' @export
get_mbox_domain <- function(config_file, project_key_index) {

  mbox_url <- config_file[["mailing_list"]][["mod_mbox"]][[project_key_index]][["mailing_list"]]

  if (is.null(mbox_url)) {
    warning("Attribute does not exist in the configuration file.")
  }

  return(mbox_url)
}

#' Returns the local folder path to store mbox data for a specific project key.
#'
#' @description This function returns the local folder path used to store
#' mbox data for a specific project key, `project_key_index`, that is specified
#' in the input parameter `config_file`. The input, `config_file` must be a
#' parsed configuration file. The function will inform the user if the specific
#' local folder path to store mbox data exists in the parsed configuration
#' file, `config_file`.
#'
#' @param config_file The parsed configuration file obtained from \code{\link{parse_config}}.
#' @param project_key_index The name of the index of the project key (e.g. "project_key_1" or "project_key_2").
#' @return The local mbox path for project specified by key `project_key_index`.
#' @export
get_mbox_path <- function(config_file, project_key_index) {

  mbox_path <- config_file[["mailing_list"]][["mod_mbox"]][[project_key_index]][["save_folder_path"]]

  if (is.null(mbox_path)) {
    warning("Attribute does not exist in the configuration file.")
  }

  return(mbox_path)
}

#' Returns the local input file for mbox for a specific project key.
#'
#' @description This function returns the local file used for input for
#' mbox for a specific project key, `project_key_index`, that is specified
#' in the input parameter `config_file`. The input, `config_file` must be a
#' parsed configuration file. The function will inform the user if the specific
#' local input file path for mbox exists in the parsed configuration file,
#' `config_file`.
#'
#' @param config_file The parsed configuration file obtained from \code{\link{parse_config}}.
#' @param project_key_index The name of the index of the project key (e.g. "project_key_1" or "project_key_2").
#' @return The local input file mbox path for project specified by key `project_key_index`.
#' @export
get_mbox_input_file <- function(config_file, project_key_index) {

  mbox_input <- config_file[["mailing_list"]][["mod_mbox"]][[project_key_index]][["mbox_file_path"]]

  if (is.null(mbox_input)) {
    warning("Attribute does not exist in the configuration file.")
  }

  return(mbox_input)
}

#' Returns the URL to the archives for pipermail for a specific project key.
#'
#' @description This function returns the URL to the archives for a specific
#' project key, `project_key_index`, that is specified in the input parameter
#' `config_file`. The input, `config_file` must be a parsed configuration file.
#' The function will inform the user if the specific URL to the archives for
#' pipermail exists in the parsed configuration file, `config_file`.
#'
#' @param config_file The parsed configuration file obtained from \code{\link{parse_config}}.
#' @param project_key_index The name of the index of the project key (e.g. "project_key_1" or "project_key_2").
#' @return The URL of the pipermail mailing list archive for project specified by key `project_key_index`.
#' @export
get_pipermail_domain <- function(config_file, project_key_index) {

  pipermail_url <- config_file[["mailing_list"]][["pipermail"]][[project_key_index]][["mailing_list"]]

  if (is.null(pipermail_url)) {
    warning("Attribute does not exist in the configuration file.")
  }

  return(pipermail_url)
}

#' Returns the local folder path to store pipermail data for a specific project key.
#'
#' @description This function returns the local folder path used to store
#' pipermail data for a specific project key, `project_key_index`, that is specified
#' in the input parameter `config_file`. The input, `config_file` must be a
#' parsed configuration file. The function will inform the user if the specific
#' local folder path to store pipermail data exists in the parsed configuration
#' file, `config_file`.
#'
#' @param config_file The parsed configuration file obtained from \code{\link{parse_config}}.
#' @param project_key_index The name of the index of the project key (e.g. "project_key_1" or "project_key_2").
#' @return The local pipermail path for project specified by key `project_key_index`.
#' @export
get_pipermail_path <- function(config_file, project_key_index) {

  pipermail_path <- config_file[["mailing_list"]][["pipermail"]][[project_key_index]][["save_folder_path"]]

  if (is.null(pipermail_path)) {
    warning("Attribute does not exist in the configuration file.")
  }

  return(pipermail_path)
}

#' Returns the local input file for pipermail for a specific project key.
#'
#' @description This function returns the local file used for input for
#' pipermail for a specific project key, `project_key_index`, that is specified
#' in the input parameter `config_file`. The input, `config_file` must be a
#' parsed configuration file. The function will inform the user if the specific
#' local input file path for pipermail exists in the parsed configuration file,
#' `config_file`.
#'
#' @param config_file The parsed configuration file obtained from \code{\link{parse_config}}.
#' @param project_key_index The name of the index of the project key (e.g. "project_key_1" or "project_key_2").
#' @return The local input file pipermail path for project specified by key `project_key_index`.
#' @export
get_pipermail_input_file <- function(config_file, project_key_index) {

  pipermail_input <- config_file[["mailing_list"]][["pipermail"]][[project_key_index]][["mbox_file_path"]]

  if (is.null(pipermail_input)) {
    warning("Attribute does not exist in the configuration file.")
  }

  return(pipermail_input)
}

##### Mailing List Functions End #####

##### Issue Tracker Functions Start #####

##### Jira Functions #####

#' Returns the list of Jira issue tracker project keys.
#'
#' @description This function returns the list of Jira issue tracker project
#' keys, that is specified in the input parameter `config_file`. The input,
#' `config_file` must be a parsed configuration file. The function will inform
#' the user if the project keys exist in the parsed configuration
#' file, `config_file`.
#'
#' @param config_file The parsed configuration file obtained from \code{\link{parse_config}}.
#' @return The list of Jira issue tracker project keys.
#' @export
get_jira_keys <- function(config_file) {

  jira_key <- config_file[["issue_tracker"]][["jira"]]

  if (is.null(jira_key)) {
    warning("Attribute does not exist in the configuration file.")
  }

  return(jira_key)
}

#' Returns the Jira project domain for a specific project key.
#'
#' @description This function returns the Jira project domain for a specific
#' project key, that is specified in the input parameter `config_file`.
#' The input, `config_file` must be a parsed configuration file. The function
#' will inform the user if the domain exists in the parsed configuration file,
#' `config_file`.
#'
#' @param config_file The parsed configuration file obtained from \code{\link{parse_config}}.
#' @param project_key_index The name of the index of the project key (e.g. "project_key_1" or "project_key_2").
#' @return The Jira domain for project specified by key `project_key_index`.
#' @export
get_jira_domain <- function(config_file, project_key_index) {

  domain <- config_file[["issue_tracker"]][["jira"]][[project_key_index]][["domain"]]

  if (is.null(domain)) {
    warning("Attribute does not exist in the configuration file.")
  }

  return(domain)
}

#' Returns the name of the Jira project key for a specific project key.
#'
#' @description This function returns the Jira project key name for a specific
#' project key, that is specified in the input parameter `config_file`.
#' The input, `config_file` must be a parsed configuration file. The function
#' will inform the user if the project key name exists in the parsed
#' configuration file, `config_file`.
#'
#' @param config_file The parsed configuration file obtained from \code{\link{parse_config}}.
#' @param project_key_index The name of the index of the project key (e.g. "project_key_1" or "project_key_2").
#' @return The Jira project key name for project specified by key `project_key_index`.
#' @export
get_jira_project_key_name <- function(config_file, project_key_index) {

  name <- config_file[["issue_tracker"]][["jira"]][[project_key_index]][["project_key"]]

  if (is.null(name)) {
    warning("Attribute does not exist in the configuration file.")
  }

  return(name)
}

#' Returns the local folder path for Jira issues for a specific project key.
#'
#' @description This function returns the folder path for Jira issues for a
#' specific project key, that is specified in the input parameter `config_file`.
#' The input, `config_file` must be a parsed configuration file. The function
#' will inform the user if the folder path for Jira issues exists in the parsed
#' configuration file, `config_file`.
#'
#' @param config_file The parsed configuration file obtained from \code{\link{parse_config}}.
#' @param project_key_index The name of the index of the project key (e.g. "project_key_1" or "project_key_2").
#' @return The Jira issue folder path for project specified by key `project_key_index`.
#' @export
get_jira_issues_path <- function(config_file, project_key_index) {

  jira_issues_path <- config_file[["issue_tracker"]][["jira"]][[project_key_index]][["issues"]]

  if (is.null(jira_issues_path)) {
    warning("Attribute does not exist in the configuration file.")
  }

  return(jira_issues_path)
}

#' Returns the local folder path for Jira issue comments for a specific
#' project key.
#'
#' @description This function returns the local folder path for Jira issue
#' comments for a specific project key, that is specified in the input
#' parameter `config_file`. The input, `config_file` must be a parsed
#' configuration file. The function will inform the user if the local folder
#' path for the comments exists in the parsed configuration file, `config_file`.
#'
#' @param config_file The parsed configuration file obtained from \code{\link{parse_config}}.
#' @param project_key_index The name of the index of the project key (e.g. "project_key_1" or "project_key_2").
#' @return The folder path for Jira issue comments for project specified by key `project_key_index`.
#' @export
get_jira_issues_comments_path <- function(config_file, project_key_index) {

  jira_issue_comments_path <- config_file[["issue_tracker"]][["jira"]][[project_key_index]][["issue_comments"]]

  if (is.null(jira_issue_comments_path)) {
    warning("Attribute does not exist in the configuration file.")
  }

  return(jira_issue_comments_path)
}

##### Github Functions #####

#' Returns the list of GitHub issue tracker project keys.
#'
#' @description This function returns the list of GitHub issue tracker project
#' keys, that is specified in the input parameter `config_file`. The input,
#' `config_file` must be a parsed configuration file. The function will inform
#' the user if the project keys exist in the parsed configuration
#' file, `config_file`.
#'
#' @param config_file The parsed configuration file obtained from \code{\link{parse_config}}.
#' @return The list of GitHub issue tracker project keys.
#' @export
get_github_keys <- function(config_file) {

  keys <- config_file[["issue_tracker"]][["github"]]

  if (is.null(keys)) {
    warning("Attribute does not exist in the configuration file.")
  }

  return(keys)
}

#' Returns the owner for a GitHub repository for a specific project key.
#'
#' @description This function returns the owner for a GitHub repository for a
#' specific project key, that is specified in the input parameter `config_file`.
#' The input, `config_file` must be a parsed configuration file. The function
#' will inform the user if the owner for the GitHub repository exists in the
#' parsed configuration file, `config_file`.
#'
#' @param config_file The parsed configuration file obtained from \code{\link{parse_config}}.
#' @param project_key_index The name of the index of the project key (e.g. "project_key_1" or "project_key_2").
#' @return The GitHub project owner name for project specified by key `project_key_index`.
#' @export
get_github_owner <- function(config_file, project_key_index) {

  owner <- config_file[["issue_tracker"]][["github"]][[project_key_index]][["owner"]]

  if (is.null(owner)) {
    warning("Attribute does not exist in the configuration file.")
  }

  return(owner)
}

#' Returns the name of the GitHub repository for a specific project key.
#'
#' @description This function returns the name of the GitHub repository for a
#' specific project key, that is specified in the input parameter `config_file`.
#' The input, `config_file` must be a parsed configuration file. The function
#' will inform the user if the name of the GitHub repository exists in the
#' parsed configuration file, `config_file`.
#'
#' @param config_file The parsed configuration file obtained from \code{\link{parse_config}}.
#' @param project_key_index The name of the index of the project key (e.g. "project_key_1" or "project_key_2").
#' @return The name of the GitHub repository for project specified by key `project_key_index`.
#' @export
get_github_repo <- function(config_file, project_key_index) {

  repo <- config_file[["issue_tracker"]][["github"]][[project_key_index]][["repo"]]

  if (is.null(repo)) {
    warning("Attribute does not exist in the configuration file.")
  }

  return(repo)
}

#' Returns the local folder path for GitHub issues for a specific project key.
#'
#' @description This function returns the local folder path for GitHub issues
#' for a specific project key, that is specified in the input parameter
#' `config_file`. The input, `config_file` must be a parsed configuration file.
#' The function will inform the user if the folder path for GitHub issues exists
#' in the parsed configuration file, `config_file`.
#'
#' @param config_file The parsed configuration file obtained from \code{\link{parse_config}}.
#' @param project_key_index The name of the index of the project key (e.g. "project_key_1" or "project_key_2").
#' @return The local folder path for GitHub issues for project specified by key `project_key_index`.
#' @export
get_github_issue_path <- function(config_file, project_key_index) {

  issue_path <- config_file[["issue_tracker"]][["github"]][[project_key_index]][["issue"]]

  if (is.null(issue_path)) {
    warning("Attribute does not exist in the configuration file.")
  }

  return(issue_path)
}

#' Returns the local folder path for GitHub Issue or Pull Request comments for
#' a specific project key.
#'
#' @description This function returns the local folder path for GitHub Issue or
#' Pull Request comments for a specific project key, that is specified in the
#' input parameter `config_file`. The input, `config_file` must be a parsed
#' configuration file. The function will inform the user if the local folder
#' path for the comments exists in the parsed configuration file, `config_file`.
#'
#' @param config_file The parsed configuration file obtained from \code{\link{parse_config}}.
#' @param project_key_index The name of the index of the project key (e.g. "project_key_1" or "project_key_2").
#' @return The local folder path for GitHub Issues or PR comments for project specified by key `project_key_index`.
#' @export
get_github_issue_or_pr_comment_path <- function(config_file, project_key_index) {

  issue_or_pr_comment_path <- config_file[["issue_tracker"]][["github"]][[project_key_index]][["issue_or_pr_comment"]]

  if (is.null(issue_or_pr_comment_path)) {
    warning("Attribute does not exist in the configuration file.")
  }

  return(issue_or_pr_comment_path)
}

#' Returns the local folder path for GitHub Issue Searches for a specific
#' project key.
#'
#' @description This function returns the local folder path for GitHub Issue
#' Searches for a specific project key, that is specified in the input parameter
#' `config_file`. The input, `config_file` must be a parsed configuration file.
#' The function will inform the user if the local folder path for the issue
#' searches exists in the parsed configuration file, `config_file`.
#'
#' @param config_file The parsed configuration file obtained from \code{\link{parse_config}}.
#' @param project_key_index The name of the index of the project key (e.g. "project_key_1" or "project_key_2").
#' @return The local folder path for GitHub issue search for project specified by key `project_key_index`.
#' @export
get_github_issue_search_path <- function(config_file, project_key_index) {

  issue_search_path <- config_file[["issue_tracker"]][["github"]][[project_key_index]][["issue_search"]]

  if (is.null(issue_search_path)) {
    warning("Attribute does not exist in the configuration file.")
  }

  return(issue_search_path)
}

#' Returns the local folder path for GitHub Pull Requests for a specific
#' project key.
#'
#' @description This function returns the local folder path for GitHub Pull
#' Requests for a specific project key, that is specified in the input
#' parameter `config_file`. The input, `config_file` must be a parsed
#' configuration file. The function will inform the user if the local folder
#' path for the pull requests exists in the parsed configuration file,
#' `config_file`.
#'
#' @param config_file The parsed configuration file obtained from \code{\link{parse_config}}.
#' @param project_key_index The name of the index of the project key (e.g. "project_key_1" or "project_key_2").
#' @return The local folder path for GitHub pull requests for project specified by key `project_key_index`.
#' @export
get_github_pull_request_path <- function(config_file, project_key_index) {

  pull_request_path <- config_file[["issue_tracker"]][["github"]][[project_key_index]][["pull_request"]]

  if (is.null(pull_request_path)) {
    warning("Attribute does not exist in the configuration file.")
  }

  return(pull_request_path)
}

#' Returns the local folder path for GitHub issue events for a specific project
#' key.
#'
#' @description This function returns the local folder path for GitHub issue
#' events for a specific project key, that is specified in the input
#' parameter `config_file`. The input, `config_file` must be a parsed
#' configuration file. The function will inform the user if the local folder
#' path for the issue events exists in the parsed configuration file,
#' `config_file`.
#'
#' @param config_file The parsed configuration file obtained from \code{\link{parse_config}}.
#' @param project_key_index The name of the index of the project key (e.g. "project_key_1" or "project_key_2").
#' @return The local folder path for GitHub issue events for project specified by key `project_key_index`.
#' @export
get_github_issue_event_path <- function(config_file, project_key_index) {

  issue_event_path <- config_file[["issue_tracker"]][["github"]][[project_key_index]][["issue_event"]]

  if (is.null(issue_event_path)) {
    warning("Attribute does not exist in the configuration file.")
  }

  return(issue_event_path)
}

#' Returns the local folder path for GitHub commits for a specific project key.
#'
#' @description This function returns the local folder path for GitHub commits
#' for a specific project key, that is specified in the input
#' parameter `config_file`. The input, `config_file` must be a parsed
#' configuration file. The function will inform the user if the local folder
#' path for the commits exists in the parsed configuration file,
#' `config_file`.
#'
#' @param config_file The parsed configuration file obtained from \code{\link{parse_config}}.
#' @param project_key_index The name of the index of the project key (e.g. "project_key_1" or "project_key_2").
#' @return The local folder path for GitHub commits for project specified by key `project_key_index`.
#' @export
get_github_commit_path <- function(config_file, project_key_index) {

  commit_path <- config_file[["issue_tracker"]][["github"]][[project_key_index]][["commit"]]

  if (is.null(commit_path)) {
    warning("Attribute does not exist in the configuration file.")
  }

  return(commit_path)
}

### Bugzilla Functions #####

#' Returns the name of the Bugzilla project key for a specific project key index.
#'
#' @description This function returns the name of the Bugzilla project key for
#' a specific project key, that is specified in the input parameter
#' `config_file`. The input, `config_file` must be a parsed configuration file.
#' The function will inform the user if the name of the Bugzilla project key
#' exists in the parsed configuration file, `config_file`.
#'
#' @param config_file The parsed configuration file obtained from \code{\link{parse_config}}.
#' @param project_key_index The name of the index of the project key (e.g. "project_key_1" or "project_key_2").
#' @return The Bugzilla project key name for project specified by key `project_key_index`.
#' @export
get_bugzilla_project_key <- function(config_file, project_key_index) {

  bugzilla_key <- config_file[["issue_tracker"]][["bugzilla"]][[project_key_index]][["project_key"]]

  if (is.null(bugzilla_key)) {
    warning("Attribute does not exist in the configuration file.")
  }

  return(bugzilla_key)
}

#' Returns the local folder path for Bugzilla issues for a specific project key.
#'
#' @description This function returns the local folder path for Bugzilla issues
#' for a specific project key, that is specified in the input parameter
#' `config_file`. The input, `config_file` must be a parsed configuration file.
#' The function will inform the user if the folder path for Bugzilla issues
#' exists in the parsed configuration file, `config_file`.
#'
#' @param config_file The parsed configuration file obtained from \code{\link{parse_config}}.
#' @param project_key_index The name of the index of the project key (e.g. "project_key_1" or "project_key_2").
#' @return The local folder path for Bugzilla issues for project specified by key `project_key_index`.
#' @export
get_bugzilla_issue_path <- function(config_file, project_key_index) {

  issue_path <- config_file[["issue_tracker"]][["bugzilla"]][[project_key_index]][["issues"]]

  if (is.null(issue_path)) {
    warning("Attribute does not exist in the configuration file.")
  }

  return(issue_path)
}

#' Returns the local folder path for Bugzilla issue comments for a specific project key.
#'
#' @description This function returns the local folder path for Bugzilla issue
#' comments for a specific project key, that is specified in the input parameter
#' `config_file`. The input, `config_file` must be a parsed configuration file.
#' The function will inform the user if the folder path for Bugzilla issue
#' comments exists in the parsed configuration file, `config_file`.
#'
#' @param config_file The parsed configuration file obtained from \code{\link{parse_config}}.
#' @param project_key_index The name of the index of the project key (e.g. "project_key_1" or "project_key_2").
#' @return The local folder path for Bugzilla issue comments for project specified by key `project_key_index`.
#' @export
get_bugzilla_issue_comment_path <- function(config_file, project_key_index) {

  issue_comment_path <- config_file[["issue_tracker"]][["bugzilla"]][[project_key_index]][["issue_comments"]]

  if (is.null(issue_comment_path)) {
    warning("Attribute does not exist in the configuration file.")
  }

  return(issue_comment_path)
}

##### Issue Tracker Functions End #####



##### Vulnerabilities Functions Start #####

#' Returns the local folder path that contains the nvd (National Vulnerability
#' Database) feeds.
#'
#' @description This function returns the local folder path for nvd feeds,
#' that is specified in the input parameter `config_file`. The input,
#' `config_file` must be a parsed configuration file. The function will inform
#' the user if the local folder path for the nvd feeds exists in the parsed
#' configuration file, `config_file`.
#'
#' @param config_file The parsed configuration file obtained from \code{\link{parse_config}}.
#' @return The folder path with nvd feeds.
#' @export
get_nvdfeed_folder_path <- function(config_file) {

  nvdfeed_folder_path <- config_file[["vulnerabilities"]][["nvd_feed"]]

  if (is.null(nvdfeed_folder_path)) {
    warning("Attribute does not exist in the configuration file.")
  }

  return(nvdfeed_folder_path)
}

##### Vulnerabilities Functions End #####



##### Regular Expression Functions Start #####

#' Returns the issue Id regular expression for commit messages.
#'
#' @description This function returns the issue Id regular expression for commit
#' messages, that is specified in the input parameter `config_file`. The input,
#' `config_file` must be a parsed configuration file. The function will inform
#' the user if the issue Id regular expression for commit messages exists in the
#' parsed configuration file, `config_file`.
#'
#' @param config_file The parsed configuration file obtained from \code{\link{parse_config}}.
#' @return The commit message issue Id regular expression.
#' @export
get_issue_id_regex <- function(config_file) {

  issue_id_regex <- config_file[["commit_message_id_regex"]][["issue_id"]]

  if (is.null(issue_id_regex)) {
    warning("Attribute does not exist in the configuration file.")
  }

  return(issue_id_regex)
}

#' Returns the cve (Common Vulnerabilities and Exposures) regular expression
#' for commit messages.
#'
#' @description This function returns the cve regular expression for commit
#' messages, that is specified in the input parameter `config_file`. The input,
#' `config_file` must be a parsed configuration file. The function will inform
#' the user if the cve regular expression for commit messages exists in the
#' parsed configuration file, `config_file`.
#'
#' @param config_file The parsed configuration file obtained from \code{\link{parse_config}}.
#' @return The commit message CVE regular expression.
#' @export
get_cveid_regex <- function(config_file) {

  cveid_regex <- config_file[["commit_message_id_regex"]][["cve_id"]]

  if (is.null(cveid_regex)) {
    warning("Attribute does not exist in the configuration file.")
  }

  return(cveid_regex)
}

##### Regular Expression Functions End #####



##### Filter Functions Start #####

#' Returns the list of file extensions used for filtering files to keep.
#'
#' @description This function returns the list of file extensions that will be
#' used for filtering files specified in the input parameter `config_file`. The
#' input, `config_file` must be a parsed configuration file. The function will
#' inform the user if the list of file extensions exists in the parsed
#' configuration file, `config_file`.
#'
#' @param config_file The parsed configuration file obtained from \code{\link{parse_config}}.
#' @return The list of file extensions to keep.
#' @export
get_file_extensions <- function(config_file) {

  file_extensions <- config_file[["filter"]][["keep_filepaths_ending_with"]]

  if (is.null(file_extensions)) {
    warning("Attribute does not exist in the configuration file.")
  }

  return(file_extensions)
}

#' Returns the list of file extensions used for filtering files to remove.
#'
#' @description This function returns the list of file extensions that will be
#' used for filtering files specified in the input parameter `config_file`. The
#' input, `config_file` must be a parsed configuration file. The function will
#' inform the user if the list of file extensions exists in the parsed
#' configuration file, `config_file`.
#'
#' @param config_file The parsed configuration file obtained from \code{\link{parse_config}}.
#' @return The list of file extensions to remove.
#' @export
get_substring_filepath <- function(config_file) {

  substring_filepath <- config_file[["filter"]][["remove_filepaths_containing"]]

  if (is.null(substring_filepath)) {
    warning("Attribute does not exist in the configuration file.")
  }

  return(substring_filepath)
}

#' Returns the commit size threshold to remove file paths.
#'
#' @description This function returns an integer number that represents the
#' threshold for a commit size to remove file paths specified in the input
#' parameter `config_file`. The input, `config_file` must be a parsed
#' configuration file. The function will inform the user if the commit size
#' threshold exists in the parsed configuration file, `config_file`.
#'
#' @param config_file The parsed configuration file obtained from \code{\link{parse_config}}.
#' @return The commit size to filter out.
#' @export
get_filter_commit_size <- function(config_file) {

  filter_commit_size <- config_file[["filter"]][["remove_filepaths_on_commit_size_greather_than"]]

  if (is.null(filter_commit_size)) {
    warning("Attribute does not exist in the configuration file.")
  }

  return(filter_commit_size)
}

##### Filter Functions End #####



##### Third Party Tools Functions Start #####

#' Returns the specified tool project from a parsed tool configuration file.
#'
#' @description This function returns a path to a specified tool from a
#' specified parsed tool configuration file. The function takes the input
#' `tool_name` and uses it to index a specific tool project in a parsed
#' tool configuration file, `config_file`, where it then returns the specified
#' tool project. The function will inform the user if the specified attribute,
#' `tool_name`, exists in the parsed configuration file, `config_file`.
#'
#' @param tool_name The name of the tool (e.g. "perceval" or "dv8").
#' @param config_file The parsed configuration file obtained from \code{\link{parse_config}}.
#' @return The specified `tool_name` tool project from `config_file`.
#' @export
get_tool_project <- function(tool_name, config_file) {

  tool_path <- config_file[[tool_name]]

  if (is.null(tool_path)) {
    warning("Attribute does not exist.")
  }

  return(tool_path)
}

#' Returns the depends code language for analysis.
#'
#' @description This function returns the specified code language that should
#' be used to parse file-file static dependencies with the depends tool, that
#' is specified in the input parameter `config_file`. The input, `config_file`
#' must be a parsed configuration file. The function will inform the user if
#' the depends code language exists in the parsed configuration file, `config_file`.
#'
#' @param config_file The parsed configuration file obtained from \code{\link{parse_config}}.
#' @return The code language for parsing file-file static dependencies.
#' @export
get_depends_code_language <- function(config_file) {

  language <- config_file[["tool"]][["depends"]][["code_language"]]

  if (is.null(language)) {
    warning("Attribute does not exist in the configuration file.")
  }

  return(language)
}

#' Returns a list of the types of dependencies to keep for analysis.
#'
#' @description This function returns the specified types of dependencies to
#' keep for analysis with the depends tool, that is specified in the input
#' parameter `config_file`. The input, `config_file` must be a parsed
#' configuration file. The function will inform the user if the list of the
#' types of dependencies exists in the parsed configuration file, `config_file`.
#'
#' @param config_file The parsed configuration file obtained from \code{\link{parse_config}}.
#' @return A list of the types of depends dependencies to keep for analysis.
#' @export
get_depends_keep_dependencies_type <- function(config_file) {

  keep_dependencies_type <- config_file[["tool"]][["depends"]][["keep_dependencies_type"]]

  if (is.null(keep_dependencies_type)) {
    warning("Attribute does not exist in the configuration file.")
  }

  return(keep_dependencies_type)
}

#' Returns the path to the folder used to store files for DV8 analysis.
#'
#' @description This function returns the path to the folder that will be
#' used to store various intermediate files for DV8 analysis, that is specified
#' in the input parameter `config_file`. The input, `config_file` must be a
#' parsed configuration file. The function will inform the user if the path
#' path to the folder for intermediate file storage for DV8 analysis exists in
#' the parsed configuration file, `config_file`.
#'
#' @param config_file The parsed configuration file obtained from \code{\link{parse_config}}.
#' @return The DV8 project folder path.
#' @export
get_dv8_folder_path <- function(config_file) {

  project_path <- config_file[["tool"]][["dv8"]][["folder_path"]]

  if (is.null(project_path)) {
    warning("Attribute does not exist in the configuration file.")
  }

  return(project_path)
}

#' Returns the list of architectural flaws thresholds for DV8 analysis.
#'
#' @description This function returns the list of architectural flaws thresholds
#' for DV8 analysis, that is specified in the input parameter `config_file`.
#' The input, `config_file` must be a parsed configuration file. The function
#' will inform the user if the list of architectural flaws thresholds
#' exists in the parsed configuration file, `config_file`.
#'
#' @param config_file The parsed configuration file obtained from \code{\link{parse_config}}.
#' @return The list of DV8 architectural flaws thresholds.
#' @export
get_dv8_flaws_params <- function(config_file) {

  dv8_flaws_params <- config_file[["tool"]][["dv8"]][["architectural_flaws"]]

  if (is.null(dv8_flaws_params)) {
    warning("Attribute does not exist in the configuration file.")
  }

  return(dv8_flaws_params)
}

#' Returns the types to keep to to be considered for analysis.
#'
#' @description This function returns the types of file-file dependencies that
#' should be considered, that are specified in the input parameter
#' `config_file`. The input, `config_file` must be a parsed configuration file.
#' The function will inform the user if the lines type to keep exists in the
#' parsed configuration file, `config_file`.
#'
#' @param config_file The parsed configuration file obtained from \code{\link{parse_config}}.
#' @return The lines type to keep for analysis.
#' @export
get_uctags_line_types <- function(config_file) {

  kinds <- config_file[["tool"]][["uctags"]][["keep_lines_type"]]

  if (is.null(kinds)) {
    warning("Attribute does not exist in the configuration file.")
  }

  return(kinds)
}

#' Returns the file path for the output of the srcML analysis for the project.
#'
#' @description This function returns the file path to be used to store the
#' output of the srcML analysis for the project, that is specified in the
#' input parameter `config_file`. The input, `config_file` must be a parsed
#' configuration file. The function will inform the user if the file path
#' exists in the parsed configuration file, `config_file`.
#'
#' @param config_file The parsed configuration file obtained from \code{\link{parse_config}}.
#' @return The output file path for srcML analysis.
#' @export
get_srcml_filepath <- function(config_file) {

  srcml_filepath <- config_file[["tool"]][["srcml"]][["srcml_path"]]

  if (is.null(srcml_filepath)) {
    warning("Attribute does not exist in the configuration file.")
  }

  return(srcml_filepath)
}

#' Returns the folder path for class pattern4 analysis.
#'
#' @description This function returns the folder path used to store the classes
#' for the pattern4 analysis for the project, that is specified in the input
#' parameter `config_file`. The input, `config_file` must be a parsed
#' configuration file. The function will inform the user if the folder path
#' exists in the parsed configuration file, `config_file`.
#'
#' @param config_file The parsed configuration file obtained from \code{\link{parse_config}}.
#' @return The Pattern4 class folder path.
#' @export
get_pattern4_folder_path <- function(config_file) {

  pattern4_folder_path <- config_file[["tool"]][["pattern4"]][["class_folder_path"]]

  if (is.null(pattern4_folder_path)) {
    warning("Attribute does not exist in the configuration file.")
  }

  return(pattern4_folder_path)
}

#' Returns the folder path for the output of the pattern4 analysis.
#'
#' @description This function returns the folder path that contains the
#' output of the pattern4 analysis for the project, that is specified in the
#' input parameter `config_file`. The input, `config_file` must be a parsed
#' configuration file. The function will inform the user if the folder path
#' exists in the parsed configuration file, `config_file`.
#'
#' @param config_file The parsed configuration file obtained from \code{\link{parse_config}}.
#' @return The Pattern4 output folder path.
#' @export
get_pattern4_filepath <- function(config_file) {

  pattern4_filepath <- config_file[["tool"]][["pattern4"]][["output_filepath"]]

  if (is.null(pattern4_filepath)) {
    warning("Attribute does not exist in the configuration file.")
  }

  return(pattern4_filepath)
}

#' Returns the understand code language for analysis.
#'
#' @description This function returns the specified code language that should
#' be used to parse dependencies with the understand tool, that
#' is specified in the input parameter `config_file`. The input, `config_file`
#' must be a parsed configuration file. The function will inform the user if
#' the understand code language exists in the parsed configuration file, `config_file`.
#'
#' @param config_file The parsed configuration file obtained from \code{\link{parse_config}}.
#' @return The code language for parsing with the understand tool.
#' @export
get_understand_code_language <- function(config_file) {

  language <- config_file[["tool"]][["understand"]][["code_language"]]

  if (is.null(language)) {
    warning("Attribute does not exist in the configuration file.")
  }

  return(language)
}

#' Returns a list of the types of understand dependencies to keep for analysis.
#'
#' @description This function returns the specified types of dependencies to
#' keep for analysis with the understand tool, that is specified in the input
#' parameter `config_file`. The input, `config_file` must be a parsed
#' configuration file. The function will inform the user if the list of the
#' types of understand dependencies exists in the parsed configuration file, `config_file`.
#'
#' @param config_file The parsed configuration file obtained from \code{\link{parse_config}}.
#' @return A list of the types of understand dependencies to keep for analysis.
#' @export
get_understand_keep_dependencies_type <- function(config_file) {

  keep_dependencies_type <- config_file[["tool"]][["understand"]][["keep_dependencies_type"]]

  if (is.null(keep_dependencies_type)) {
    warning("Attribute does not exist in the configuration file.")
  }

  return(keep_dependencies_type)
}

#' Returns the folder path for the input of the understand analysis.
#'
#' @description This function returns the folder path that contains the
#' input of the understand analysis for the project, that is specified in the
#' input parameter `config_file`. The input, `config_file` must be a parsed
#' configuration file. The function will inform the user if the folder path
#' exists in the parsed configuration file, `config_file`.
#'
#' @param config_file The parsed configuration file obtained from \code{\link{parse_config}}.
#' @return The understand project folder path.
#' @export
get_understand_project_path <- function(config_file) {

  understand_project_path <- config_file[["tool"]][["understand"]][["project_path"]]

  if (is.null(understand_project_path)) {
    warning("Attribute does not exist in the configuration file.")
  }

  return(understand_project_path)
}

#' Returns the folder path for the output of the understand analysis.
#'
#' @description This function returns the folder path that contains the
#' output of the understand analysis for the project, that is specified in the
#' input parameter `config_file`. The input, `config_file` must be a parsed
#' configuration file. The function will inform the user if the folder path
#' exists in the parsed configuration file, `config_file`.
#'
#' @param config_file The parsed configuration file obtained from \code{\link{parse_config}}.
#' @return The understand output folder path.
#' @export
get_understand_output_path <- function(config_file) {

  understand_output_path <- config_file[["tool"]][["understand"]][["output_path"]]

  if (is.null(understand_output_path)) {
    warning("Attribute does not exist in the configuration file.")
  }

  return(understand_output_path)
}

##### Third Party Tools Functions End #####



##### Analysis Functions Start #####

#' Returns the list of topics and keywords for analysis.
#'
#' @description This function returns the list of keywords and topics for
#' analysis, that is specified in the input parameter `config_file`. The
#' input, `config_file` must be a parsed configuration file. The function will
#' inform the user if the list of keywords and topics exists in the parsed
#' configuration file, `config_file`.
#'
#' @param config_file The parsed configuration file obtained from \code{\link{parse_config}}.
#' @return The list of keywords and topics for analysis.
#' @export
get_topics <- function(config_file) {

  topics <- config_file[["analysis"]][["topics"]]

  if (is.null(topics)) {
    warning("Attribute does not exist in the configuration file.")
  }

  return(topics)
}

#' Returns the starting commit for a window for analysis.
#'
#' @description This function returns the starting commit for a window of time
#' for analysis (the time stamp is inferred from gitlog), that is specified in
#' the input parameter `config_file`. The input, `config_file` must be a parsed
#' configuration file. The function will inform the user if the start commit
#' exists in the parsed configuration file, `config_file`.
#'
#' @param config_file The parsed configuration file obtained from \code{\link{parse_config}}.
#' @return The start commit for a window for analysis.
#' @export
get_window_start_commit <- function(config_file) {

  start_commit <- config_file[["analysis"]][["window"]][["start_commit"]]

  if (is.null(start_commit)) {
    warning("Attribute does not exist in the configuration file.")
  }

  return(start_commit)
}

#' Returns the ending commit for a window for analysis.
#'
#' @description This function returns the ending commit for a window of time
#' for analysis (the time stamp is inferred from gitlog), that is specified in
#' the input parameter `config_file`. The input, `config_file` must be a parsed
#' configuration file. The function will inform the user if the end commit
#' exists in the parsed configuration file, `config_file`.
#'
#' @param config_file The parsed configuration file obtained from \code{\link{parse_config}}.
#' @return The end commit for a window for analysis.
#' @export
get_window_end_commit <- function(config_file) {

  end_commit <- config_file[["analysis"]][["window"]][["end_commit"]]

  if (is.null(end_commit)) {
    warning("Attribute does not exist in the configuration file.")
  }

  return(end_commit)
}

#' Returns the size of a window for analysis.
#'
#' @description This function returns the size of a window, that is
#' specified in the input parameter `config_file`. The input, `config_file`
#' must be a parsed configuration file. The function will inform the user if
#' the window size exists in the parsed configuration file, `config_file`.
#'
#' @param config_file The parsed configuration file obtained from \code{\link{parse_config}}.
#' @return The size of a window for analysis.
#' @export
get_window_size <- function(config_file) {

  window_size <- config_file[["analysis"]][["window"]][["size_days"]]

  if (is.null(window_size)) {
    warning("Attribute does not exist in the configuration file.")
  }

  return(window_size)
}

#' Returns the list of enumerated commit intervals for analysis.
#'
#' @description This function returns a list of enumerated commit intervals,
#' that is specified in the input parameter `config_file`. The input,
#' `config_file` must be a parsed configuration file. The function will inform
#' the user if the list of enumerated commit intervals exists in the parsed
#' configuration file, `config_file`.
#'
#' @param config_file The parsed configuration file obtained from \code{\link{parse_config}}.
#' @return The list of enumerated commit intervals.
#' @export
get_enumeration_commits <- function(config_file) {

  enumeration_commit <- config_file[["analysis"]][["enumeration"]][["commit"]]

  if (is.null(enumeration_commit)) {
    warning("Attribute does not exist in the configuration file.")
  }

  return(enumeration_commit)
}

##### Analysis Functions End #####
