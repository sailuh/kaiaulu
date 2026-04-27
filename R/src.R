# Kaiaulu - https://github.com/sailuh/kaiaulu
#
# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at https://mozilla.org/MPL/2.0/.

############## Understand Project Builder ##############

#' Build Understand DB
#'
#' Uses Scitools Understand to create a source code project Und Database.
#'
#' @param scitools_path path to the scitools binary `und`
#' @param project_path path to the project source code folder to create the Understand DB.
#' @param language the primary language of the project (language must be supported by Understand)
#' @param output_dir path to output directory (formatted output_path/)
#'
#' @return The created Scitools Understand DB path
#' @references See pg. 352 in https://documentation.scitools.com/pdf/understand.pdf Sept. 2024 Edition
#' @export
#' @family parsers
build_understand_project <- function(scitools_path, project_path, language, output_dir){

  scitools_path <- path.expand(scitools_path)

  # Create variables for command line
  command <- scitools_path
  project_path <- shQuote(project_path) # Quoting the project path
  db_dir <- file.path(output_dir, "Understand.und")
  args <- c("create", "-db", db_dir, "-languages", language)

  # Build the Understand project by parsing through using Understand's und command
  build_output <- system2(command, args)
  args <- c("-db", db_dir, "add", project_path)
  db_output <- system2(command, args)
  analyze_output <- args <- c("analyze", db_dir)
  output <- system2(command, args)

  return(db_dir)

}

#' Extract Understand Dependencies
#'
#' Extract the XML dependency file for either class or file granularity from
#' an understand DB.
#'
#' @param scitools_path path to the scitools binary `und`
#' @param db_filepath path to the scitools DB (see \code{\link{build_understand_project}})
#' @param parse_type Type of dependencies to generate into xml (either "file" or "class")
#' @param output_filepath path to the output XML filepath of dependencies
#'
#' @return The output directory where the db will be created, i.e. output_dir parameter.
#' @references See pg. 352 in https://documentation.scitools.com/pdf/understand.pdf Sept. 2024 Edition
#' @export
#' @family parsers
export_understand_dependencies <- function(scitools_path, db_filepath, parse_type = c("file", "class"), output_filepath){

  scitools_path <- path.expand(scitools_path)

  # Before running, check if parse_type is correct
  parse_type <- match.arg(parse_type)

  # Create the variables used in command lines
  #db_dir <- file.path(understand_dir, "Understand.und")

  #file_name <- paste0(parse_type, "Dependencies.xml")
  #xml_dir <- file.path(db_dir, file_name)

  # Generate the XML file
  # Derived from pg. 352 in https://documentation.scitools.com/pdf/understand.pdf Sept. 2024 Edition
  args <- c("export", "-dependencies", parse_type, "cytoscape", output_filepath, db_filepath)
  output <- system2(scitools_path, args)

  return(output_filepath)

  # Generated XML file is assumed to be in this approximate format (regardless of parse_type) using Understand Build 1202
  # <graph ...>
  #   ... [Irrelevant graph attributes and rdf grandchildren]
  #   <node id="67" label="ObjectMapper id:67">
  #     <att type="string" name="node.shape" value="rect"/>
  #     <att type="string" name="node.fontSize" value="5"/>
  #     <att type="string" name="node.label" value="ObjectMapper"/>
  #     <att type="string" name="longName" value="com.fasterxml.jackson.databind.ObjectMapper"/>
  #     <att type="string" name="kind" value="Unknown Class"/>
  #     <graphics type="RECTANGLE" h="35" w="35" x="0" y="0" fill="#ffffff" width="1" outline="#000000" cy:nodeTransparency="1.0" cy:nodeLabelFont="Default-0-8" cy:borderLineType="solid"/>
  #   </node>
  #   ... [Other nodes sharing the format]
  #   <edge source="2" target="9" label="App(Depends On)CalculatorUI">
  #     <att type="string" name="edge.targetArrowShape" value="ARROW"/>
  #     <att type="string" name="edge.color" value="#0000FF"/>
  #     <att type="string" name="canonicalName" value="App(Depends On)CalculatorUI"/>
  #     <att type="string" name="interaction" value="Depends On"/>
  #     <att type="string" name="dependency kind" value="Call, Create"/>
  #   </edge>
  #   ... [Other edges sharing the format]


}

############## Parsers ##############

#' Parse Scitools Understand Dependencies XML
#'
#' Parses either a file or class scitools understand dependency XML to table.
#'
#' @param dependencies_path path to the exported Understand dependencies file (see \code{\link{export_understand_dependencies}}).
#' @export
#' @family parsers
parse_understand_dependencies <- function(dependencies_path) {

  # Parse the XML file
  xml_data <- xmlParse(dependencies_path)  # Creates pointer to file
  xml_nodes <- xmlRoot(xml_data)  # Finds the head: graph
  xml_nodes <- xmlChildren(xml_nodes)
  # xml_nodes now contains the nodes and edges (which were children of graph) and also graph's atts

  # From child nodes- filter for those with name "node"
  # Create a list by iterating through all the children in xml_nodes
  node_elements <- lapply(xml_nodes, function(child) {
    if (xmlName(child) == "node") {  # We're searching for nodes, not att or edges
      id <- xmlGetAttr(child, "id")  # Extract the id from the node line
      att_nodes <- xmlChildren(child)  # To access the atts of the node
      node_label <- xmlGetAttr(att_nodes[[3]], "value")  # Relevant att is the 3rd line
      long_name <- xmlGetAttr(att_nodes[[4]], "value")  # Relevant att is the 4th line
      return(data.table(node_label = node_label, id = id, long_name = long_name))  # Returns the table containing the filtered node data
    } else {
      return(NULL) # Return NULL for the entry to be filtered out later
    }
  })

  # Remove NULLs and combine the results from the node_elements list
  node_list <- rbindlist(node_elements[!sapply(node_elements, is.null)], use.names = TRUE, fill = TRUE)

  # From child nodes- filter for those with name "edge"
  # Create a list by iterating through all the children in xml_nodes
  edge_elements <- lapply(xml_nodes, function(child) {
    if (xmlName(child) == "edge") {  # We're searching for edges, not att or nodes
      # Extract the id_from and id_to from the edge line
      id_from <- xmlGetAttr(child, "source")
      id_to <- xmlGetAttr(child, "target")
      att_nodes <- xmlChildren(child)  # To access the atts of the edge
      dependency_kind <- xmlGetAttr(att_nodes[[5]], "value")  # Relevant att is the 5th line
      # Error handling for empty and NULL dependency_kind (this is necessary as errors do occur even in the formatted style)
      # Code correctly handles all the edges, however produces error if error handling is not included... so...
      if (!is.null(dependency_kind) && dependency_kind != "") {
        dependency_kind <- unlist(stri_split(dependency_kind, regex = ",\\s*"))  # Separates the string into a vector
        return(data.table(id_from = id_from, id_to = id_to, dependency_kind = dependency_kind)) # Returns the table containing the filtered node data
      } else {
        return(NULL) # Return NULL for the entry to be filtered out later
      }
    } else {
      return(NULL) # Return NULL for the entry to be filtered out later
    }
  })

  # Remove NULLs and combine the results from the edge_elements list
  edge_list <- rbindlist(edge_elements[!sapply(edge_elements, is.null)], use.names = TRUE, fill = TRUE)

  # Merge edges with nodes to get label_from
  edge_list <- merge(edge_list, node_list[, .(id, node_label)], by.x = "id_from", by.y = "id", all.x = TRUE)
  setnames(edge_list, "node_label", "label_from")

  # Merge again to get label_to
  edge_list <- merge(edge_list, node_list[, .(id, node_label)], by.x = "id_to", by.y = "id", all.x = TRUE)
  setnames(edge_list, "node_label", "label_to")

  # Reorder columns to have label_from and label_to on the left
  edge_list <- edge_list[, .(label_from, label_to, id_from, id_to, dependency_kind)]

  # Create a list of the network to return
  graph <- list(node_list = node_list, edge_list = edge_list)
  return(graph)
}

#' Parse dependencies from Depends
#'
#' @param depends_jar_path path to depends jar
#' @param git_repo_path path to git repo (ends in .git)
#' @param output_dir path to output directory (formatted output_path/)
#' @param language the language of the .git repo (accepts cpp, java, ruby, python, pom)
#' @export
#' @family parsers
parse_dependencies <- function(depends_jar_path,git_repo_path,language,output_dir="/tmp/"){
  # Expand paths (e.g. "~/Desktop" => "/Users/someuser/Desktop")
  depends_jar_path <- path.expand(depends_jar_path)
  git_repo_path <- path.expand(git_repo_path)
  # Remove ".git"
  folder_path <- stri_replace_last(git_repo_path,replacement="",regex=".git")
  project_name <- stri_split_regex(folder_path,pattern="/")[[1]]
  project_name <- project_name[length(project_name)-1]

  # Use Depends to parse the code folder.
  system2("java",
          args = c("-jar",depends_jar_path,
                   language,folder_path,
                   project_name,'--dir',
                   output_dir,
                   '--auto-include',
                   '--granularity=file', '--namepattern=/',
                   '--format=json'),
          stdout = FALSE,
          stderr = FALSE)
  # Construct /output_dir/ file path
  output_path <- stri_c(output_dir, project_name,"-file.json")
  # Parsed JSON output.
  depends_parsed <- jsonlite::read_json(output_path)
  # The JSON has two main parts. The first is a vector of all file names.
  file_names <- unlist(depends_parsed[["variables"]])
  # Depends will create full filepaths, but folder_path may be a relative path.
  # We must guarantee our folder_path is also a full path in order to turn all
  # full filepaths generated by Depends into relative paths.
  # "../rawdata/git_repo/helix/" => "/Users/cvp/Desktop/kaiaulu/rawdata/git_repo/helix"
  normalized_folder_path <- paste0(normalizePath(folder_path),"/")
  # /Users/user/git_repos/APR/xml/apr_xml_xmllite.c => "xml/apr_xml_xmllite.c"
  file_names <- stri_replace_first(file_names,replacement="",regex=normalized_folder_path)
  # The second part is the dependencies itself, which refer to the file name indices.
  dependencies <- depends_parsed[["cells"]]
  # The types of dependencies is a list of lists. First we unlist the various types.
  dependencies_types <- rbindlist(lapply(dependencies,
                                         function(x) as.data.table(x$values)),
                                  fill=TRUE)
  # Fixes column types to numeric, and replace NAs by 0s, as an NA means 0 dependencies.
  dependencies_types <- data.table(sapply(dependencies_types,as.numeric))
  dependencies_types[is.na(dependencies_types)] <- 0
  # Then we unlist the src and dest files.
  dependencies_files <- rbindlist(lapply(dependencies,
                                         function(x) as.data.table(x[c("src","dest")])),
                                  fill=TRUE)
  # And finally we combine them
  depends_parsed <- cbind(dependencies_files,dependencies_types)
  # We use the file_names to re-label the files for further analysis
  # Note the +1: The json assumes a file index starts at 0. R index starts 1, hence the + 1.
  depends_parsed$src <- file_names[depends_parsed$src + 1]
  depends_parsed$dest <- file_names[depends_parsed$dest + 1]

  edgelist <- depends_parsed
  data.table::setnames(x = edgelist,
                       old = c("src","dest"),
                       new = c("src_filepath","dest_filepath"))
  nodes <- data.table(filepath=file_names)
  graph <- list(nodes=nodes,edgelist=edgelist)

  return(graph)
}

#' Parse Java Code Refactorings
#'
#' @param rminer_path The path to RMiner binary.
#'  See \url{https://github.com/tsantalis/RefactoringMiner#running-refactoringminer-from-the-command-line}
#' @param git_repo_path path to git repo (ends in .git)
#' @param start_commit the start commit hash
#' @param end_commit the end commit hash
#' @export
#' @references Nikolaos Tsantalis, Matin Mansouri, Laleh Eshkevari,
#' Davood Mazinanian, and Danny Dig, "Accurate and Efficient Refactoring
#' Detection in Commit History," 40th
#' International Conference on Software Engineering (ICSE 2018),
#' Gothenburg, Sweden, May 27 - June 3, 2018.
#' @keywords internal
parse_java_code_refactoring_json <- function(rminer_path,git_repo_path,start_commit,end_commit){
  # Expand paths (e.g. "~/Desktop" => "/Users/someuser/Desktop")
  rminer_path <- path.expand(rminer_path)
  git_repo_path <- path.expand(git_repo_path)
  # Remove ".git"
  git_uri <- stri_replace_last(git_repo_path,replacement="",regex=".git")
  # Use percerval to parse mbox_path. --json line is required to be parsed by jsonlite::fromJSON.
  rminer_output <- system2(rminer_path,
                           args = c('-bc',git_uri,start_commit,end_commit),
                           stdout = TRUE,
                           stderr = FALSE)
  # Parsed JSON output as a data.table.
  rminer_parsed <- jsonlite::parse_json(rminer_output)
  return(rminer_parsed)
}
#' Parse File Line Metrics
#'
#' @param scc_path The path to scc binary.
#'  See \url{https://github.com/boyter/scc}
#' @param git_repo_path path to git repo (ends in .git)
#' @export
parse_line_metrics <- function(scc_path,git_repo_path){
  # Expand paths (e.g. "~/Desktop" => "/Users/someuser/Desktop")
  scc_path <- path.expand(scc_path)
  git_repo_path <- path.expand(git_repo_path)
  # Remove ".git"
  folder_path <- stri_replace_last(git_repo_path,replacement="",regex=".git")
  # Use Depends to parse the code folder.
  stdout <- system2(
    scc_path,
    args = c(folder_path, '--by-file','--format','csv'),
    stdout = TRUE,
    stderr = FALSE
  )
  line_metrics <- fread(stri_c(stdout,collapse = "\n"))
  # /Users/user/git_repos/APR/xml/apr_xml_xmllite.c => "xml/apr_xml_xmllite.c"
  line_metrics$Location <- stri_replace_first(line_metrics$Location,
                                              replacement="",
                                              regex=folder_path)
  return(line_metrics)
}
#' Parse File Line Type
#'
#' @param utags_path The path to utags binary.
#'  See \url{https://github.com/universal-ctags/ctags}
#' @param filepath path to file
#' @param kinds the entity kinds utags should identify per line.
#' @export
#' @keywords internal
parse_line_type_file <- function(utags_path,filepath,kinds){
  # Expand paths (e.g. "~/Desktop" => "/Users/someuser/Desktop")
  utags_path <- path.expand(utags_path)
  filepath <- path.expand(filepath)
  language <- stri_trans_tolower(last(stri_split_regex(filepath,"\\.")[[1]]))
  # Entity Kinds e.g. (function, class, etc) are specified by user.
  file_kinds <- kinds[[language]]
  # Specify fields of uctags output this function will parse and show to user:
  # n = start line
  # e = end line
  # k = entity kind specified as single letter (i.e. 'f','c', etc)
  fields <- c("n","e","k")
  stdout <- system2(
    command = utags_path,
    args = c(
      stri_c("--fields=",stri_c(fields, collapse = "")),
      stri_c("--kinds-", language,"=",stri_c(file_kinds,collapse=""), collapse =""),
      '-f','-',filepath),
    stdout = TRUE,
    stderr = FALSE
  )
  parsed_tags <- data.table(
    stri_match_first_regex(stdout,
                           pattern = '^(\\S+)\\t(\\S+)\\t/\\^(.+)\\$?\\/;\"\\t(\\w)\\tline:(\\d+)\\tend:(\\d+)')
  )
  setnames(parsed_tags,
           c("raw_ctags","entity_name","filepath","line_content","entity_type","line_start","line_end"))

  parsed_tags[]
  return(parsed_tags)
}

#' Parse R File and Function Dependencies
#'
#' @param folder_path The path to an R folder path
#' @export
parse_r_dependencies <- function(folder_path){

  all_filepaths <- list.files(file.path(path.expand(folder_path)),recursive=TRUE,pattern="\\.r|\\.R",full.names=TRUE)
  parsed_r_files <- lapply(all_filepaths,parse_rfile_ast)
  names(parsed_r_files) <- all_filepaths


  parsed_r_files <- lapply(parsed_r_files,function(x)
    x[,filepath:= stri_replace_first(filepath,regex = stri_c(folder_path,"/"),replacement = "")])

  definitions <- lapply(parsed_r_files,parse_r_function_definition)
  definitions <- rbindlist(definitions)
  unique_definitions <- unique(definitions)


  parse_r_network <- function(parsed_r_file,unique_definitions){
    function_edgelist <- parse_r_function_dependencies(parsed_r_file,unique_definitions)
    return(function_edgelist)
  }

  edgelists <- lapply(parsed_r_files,parse_r_network,unique_definitions)

  filter_by_ownership <- function(edgelist,unique_definitions){
    edgelist <- edgelist[src_functions_call_name %in% unique_definitions$src_functions_name &
                           src_functions_caller_name %in% unique_definitions$src_functions_name]
    return(edgelist)
  }

  edgelists <- lapply(edgelists,filter_by_ownership,unique_definitions)
  edgelists <- rbindlist(edgelists)

  names(parsed_r_files) <- sapply(stri_split_regex(all_filepaths,"/"),data.table::last)

  return(edgelists)
}

############## Network Transform ##############

#' Transform Understand Dependencies
#'
#' @description This function subsets a parsed table from parse_understand_dependencies
#'
#' @param parsed Parsed table from \code{\link{parse_understand_dependencies}}
#' @param weight_types The weight types as defined in Depends. Accepts single string and vector input
#' @param weight A string naming a numeric column in the parsed edge list to use as the edge weight. When \code{NULL} (default), edges are weighted by dependency occurrence count.
#' @param weight_agg A function to aggregate the weight column across multiple rows sharing the same \code{from}/\code{to} pair. Defaults to \code{mean}.
#' @export
#' @family edgelists
transform_understand_dependencies_to_network <- function(parsed, weight_types, weight = NULL, weight_agg = mean) {
  dependency_kind <- node_label <- id <- label_from <- id_from <- label_to <- id_to <- NULL # due to NSE notes in R CMD check
  nodes <- parsed[["node_list"]]
  edges <- parsed[["edge_list"]]

  # Disambiguate node labels with their ID since the same label may appear in multiple contexts
  nodes[, node_label := stringi::stri_c(node_label, "|", id)]
  edges[, label_from := stringi::stri_c(label_from, "|", id_from)]
  edges[, label_to   := stringi::stri_c(label_to,   "|", id_to)]

  # Filter to requested dependency types
  if (length(weight_types) > 0) {
    edges <- edges[dependency_kind %in% weight_types]
  }
  if (nrow(edges) == 0) {
    stop("No edges found under weight_types.")
  }

  # Build nodes table
  depend_nodes <- data.table(
    name  = nodes[["node_label"]],
    type  = FALSE
  )
  # Build edgelist — weight = count of this dependency kind per from-to pair, or custom column if provided
  if(is.null(weight)){
    depend_edgelist <- edges[, .(weight = .N), by = .(from = label_from, to = label_to, label = dependency_kind)]
  } else {
    if(!weight %in% colnames(edges)){
      stop("Column '", weight, "' not found in parsed edge list.")
    }
    weight_col <- weight
    depend_edgelist <- edges[, .(weight = weight_agg(.SD[[weight_col]])),
                              by = .(from = label_from, to = label_to, label = dependency_kind),
                              .SDcols = weight_col]
  }
  depend_edgelist[, direction := "directed"]
  graph <- model_unimodal_graph(depend_nodes, depend_edgelist, direction = "directed")
  return(graph)
}

#' Transform parsed dependencies into a network
#'
#' @param depends_parsed A parsed mbox by \code{\link{parse_dependencies}}.
#' @param weight_types The weight types as defined in Depends.
#'
#' @export
#' @family edgelists
transform_dependencies_to_network <- function(depends_parsed,weight_types=NA){
  src <- dest <- weight <- NULL # due to NSE notes in R CMD check
  # Verify input is a valid parse_dependencies output before transforming
  if(!is.list(depends_parsed)){
    stop("depends_parsed must be a list returned by parse_dependencies.")
  }
  if(!all(c("nodes","edgelist") %in% names(depends_parsed))){
    stop("depends_parsed must contain 'nodes' and 'edgelist' elements.")
  }
  if(!is.data.table(depends_parsed[["nodes"]])){
    stop("depends_parsed[['nodes']] must be a data.table.")
  }
  if(!is.data.table(depends_parsed[["edgelist"]])){
    stop("depends_parsed[['edgelist']] must be a data.table.")
  }
  # Can only include types user wants if Depends found them at least once on codebase

  nodes <- depends_parsed[["nodes"]]
  edgelist <- depends_parsed[["edgelist"]]

  # Capture NA flag before intersect() converts NA to character(0)
  use_all_types <- any(is.na(weight_types))
  weight_types <- intersect(names(edgelist)[3:ncol(edgelist)],weight_types)
  # copy() preserves all dependency type columns (Import, Call, Use, etc.)
  dependency_edgelist <- copy(edgelist)
  if(use_all_types){
    dependency_edgelist$weight <- rowSums(edgelist[,3:ncol(edgelist),with=FALSE])
  }else{
    dependency_edgelist$weight <- rowSums(edgelist[,weight_types,with=FALSE])
  }
  # Remove dependencies not chosen by user
  dependency_edgelist <- dependency_edgelist[weight != 0]
  setnames(dependency_edgelist,
           old=c("src_filepath","dest_filepath"),
           new=c("from","to"))
  # Build nodes table in transform before passing to constructor.
  # copy() prevents setnames from mutating the caller's depends_parsed object.
  # Copying from parse output preserves all files including isolated nodes.
  dependency_nodes <- copy(nodes)
  setnames(dependency_nodes, old="filepath", new="name")
  dependency_nodes$type <- FALSE
  dependency_nodes$color <- "#f4dbb5"
  # Constructor only wraps pre-built tables and assigns graph type.
  dependency_edgelist[, direction := "directed"]
  depends_network <- model_unimodal_graph(dependency_nodes, dependency_edgelist, direction = "directed")
  return(depends_network)
}
#' Transform parsed R dependencies into a graph
#' @param r_dependencies_edgelist A parsed R folder by \code{\link{parse_r_dependencies}}.
#' @param dependency_type The type of dependency to be parsed: Function or File
#' @param weight A string naming a numeric column in \code{r_dependencies_edgelist} to use as the edge weight. When \code{NULL} (default), edges are weighted by dependency occurrence count.
#' @param weight_agg A function to aggregate the weight column across multiple rows sharing the same \code{from}/\code{to} pair. Defaults to \code{mean}.
#' @export
transform_r_dependencies_to_network <- function(r_dependencies_edgelist, dependency_type=c("function","file"), weight = NULL, weight_agg = mean){
  src_functions_call_name <- src_functions_caller_name <- src_functions_call_filename <- src_functions_caller_filename <- NULL # due to NSE notes in R CMD check
  mode <- match.arg(dependency_type)
  if(mode == "function"){
    from_col <- "src_functions_call_name"
    to_col   <- "src_functions_caller_name"
    color    <- "#fafad2"
  } else {
    from_col <- "src_functions_call_filename"
    to_col   <- "src_functions_caller_filename"
    color    <- "#f4dbb5"
  }
  nodes    <- data.table(name = unique(c(r_dependencies_edgelist[[from_col]], r_dependencies_edgelist[[to_col]])), type = FALSE, color = color)
  if(is.null(weight)){
    edgelist <- r_dependencies_edgelist[, .(weight = .N), by = c(from_col, to_col)]
  } else {
    if(!weight %in% colnames(r_dependencies_edgelist)){
      stop("Column '", weight, "' not found in r_dependencies_edgelist.")
    }
    weight_col <- weight
    edgelist <- r_dependencies_edgelist[, .(weight = weight_agg(.SD[[weight_col]])),
                                         by = c(from_col, to_col),
                                         .SDcols = weight_col]
  }
  setnames(edgelist, old = c(from_col, to_col), new = c("from", "to"))
  edgelist[, direction := "directed"]
  graph <- model_unimodal_graph(nodes, edgelist, direction = "directed")
  return(graph)
}



############## Syntax Extractor ##############

#' Creates srcML XML
#'
#' Parses src code zip, folder or file and outputs the annotated
#' XML representation of the file.
#'
#' @param srcml_path The path to srcML binary
#' @param src_folder The path to the source code zip, folder or file of analysis
#' @param srcml_filepath The path, filename and extension (.xml) of the output XML file.
#'
#' @return The path where the output xml was saved (i.e. srcml_filepath)
#' @references For details, see \url{https://www.srcml.org/tutorials/creating-srcml.html}.
#' @seealso \code{\link{query_src_text}} to query the output file.
#' @export
#'
annotate_src_text <- function(srcml_path,src_folder,srcml_filepath){
  srcml_path <- path.expand(srcml_path)
  src_folder <- path.expand(src_folder)
  srcml_filepath <- path.expand(srcml_filepath)

  srcml_output <- system2(srcml_path,
                          args = c(src_folder, '--output',srcml_filepath),
                          stdout = FALSE,
                          stderr = FALSE)

  return(srcml_filepath)
}


#' Query srcML XML
#'
#' Queries srcML XML for code units (e.g. function names, declarations, etc.).
#' For a list of code units and languages supported by srcML XML see:
#' \url{https://www.srcml.org/documentation.html}.
#'
#' Note a query, unless explicitly specified to be parsed (e.g. by using the
#' string() scrML function), is a srcML XML itself, which can be also queried.
#'
#' @param srcml_path The path to srcML binary
#' @param xpath_query The XPath query to be performed on the .xml
#' @param srcml_filepath The path to the srcML file to be queried
#' (see \code{\link{annotate_src_text}}).
#'
#' @return The path where the output xml was saved (i.e. srcml_filepath)
#' @references For details, see \url{https://www.srcml.org/tutorials/xpath-query.html}.
#' @export
query_src_text <- function(srcml_path,xpath_query,srcml_filepath){
  srcml_path <- path.expand(srcml_path)
  xpath_query <- path.expand(xpath_query)
  srcml_filepath <- path.expand(srcml_filepath)

  #srcml --xpath "//src:class/src:name" depends.xml

  srcml_output <- system2(srcml_path,
                          args = c('--xpath',paste0('"',xpath_query,'"'),
                                   srcml_filepath),
                          stdout = TRUE,
                          stderr = FALSE)

  return(srcml_output)
}

#' Query srcML Class Names
#'
#' This is a convenience function to parse class names out of a project.
#' \url{https://www.srcml.org/documentation.html}.
#'
#'
#' @param srcml_path The path to srcML binary
#' @param srcml_filepath The path to the srcML file to be queried
#' (see \code{\link{annotate_src_text}}).
#'
#' @return A data.table containing filepath and class name.
#' @references For details, see \url{https://www.srcml.org/documentation.html}.
#' @export
query_src_text_class_names <- function(srcml_path,srcml_filepath){
  srcml_path <- path.expand(srcml_path)
  srcml_filepath <- path.expand(srcml_filepath)

  xpath_query <- "//src:class/src:name"

  srcml_output <- query_src_text(srcml_path,xpath_query,srcml_filepath)

  srcml_output <- XML::xmlTreeParse(srcml_output)
  srcml_root <- XML::xmlRoot(srcml_output)

  # The children of the root node is a list of unit nodes
  srcml_class_names <- XML::xmlChildren(srcml_root)
  # Each unit node is of the form:
  # <unit revision="1.0.0" language="Java" filename="/path/to/file.java" item="2"><name>someClassName</name></unit>


  parse_filepath_and_class_name <- function(unit){
    # The class name is a child node of each node
    class_name <- XML::xmlValue(unit[[1]])
    # The attribute filename contains the filename the class belongs to
    filepath <- XML::xmlGetAttr(unit,"filename")
    return(data.table(filepath=filepath,classname=class_name))
  }
  dt_filepath_classname <- rbindlist(lapply(srcml_class_names,parse_filepath_and_class_name))

  return(dt_filepath_classname)

}

#' Query srcML Namespace
#'
#' This is a convenience function to parse namespace names out of a project.
#' \url{https://www.srcml.org/documentation.html}.
#'
#'
#' @param srcml_path The path to srcML binary
#' @param srcml_filepath The path to the srcML file to be queried
#' (see \code{\link{annotate_src_text}}).
#'
#' @return A data.table containing Namespace.
#' @references For details, see \url{https://www.srcml.org/documentation.html}.
#' @export
query_src_text_namespace <- function(srcml_path,srcml_filepath){
  srcml_path <- path.expand(srcml_path)
  srcml_filepath <- path.expand(srcml_filepath)

  xpath_query <- "//src:package"

  srcml_output <- query_src_text(srcml_path,xpath_query,srcml_filepath)

  srcml_output <- XML::xmlTreeParse(srcml_output)
  srcml_root <- XML::xmlRoot(srcml_output)

  # The children of the root node is a list of unit nodes
  srcml_class_names <- XML::xmlChildren(srcml_root)
  # Each unit node is of the form:
  # <unit revision="1.0.0" language="Java" filename="/Users/lzhan/Desktop/rawdata/git_repo/iotdb/tsfile/src/test/java/org/apache/iotdb/tsfile/read/reader/FakedMultiBatchReader.java" item="1"><package>package <name><name>org</name><operator>.</operator><name>apache</name><operator>.</operator><name>iotdb</name><operator>.</operator><name>tsfile</name><operator>.</operator><name>read</name><operator>.</operator><name>reader</name></name>;</package></unit>


  parse_namespace <- function(unit){
    # The class name is a child node of each node
    class_name <- XML::xmlValue(unit[[1]])
    class_name <- sub("^package", "", class_name)
    class_name <- sub(";$","",class_name)
    # The attribute filename contains the filename the class belongs to
    filepath <- XML::xmlGetAttr(unit,"filename")

    project_name <- 'iotdb'
    # Create a regular expression pattern using project_name
    pattern <- paste0('.*?', project_name, '/')
    # Get relative path
    filepath <- sub(pattern, '', filepath)

    filename <- sub("\\.java$", "", basename(filepath))
    full_path <- paste(class_name, filename, sep=".")
    return(data.table(filepath=filepath, namespace=full_path))
  }
  dt_filepath_classname <- rbindlist(lapply(srcml_class_names,parse_namespace))

  return(dt_filepath_classname)
}

############## GoF Detection ##############


#' Write GoF Patterns
#'
#' Write GoF patterns generated by `pattern4.jar` into a a table.
#' \url{https://www.srcml.org/documentation.html}.
#' Pattern4.jar is available on
#' [Tsantalis' homepage](https://users.encs.concordia.ca/~nikolaos/pattern_detection.html)).
#'
#' @param pattern4_path The path to Tsantalis' pattern4 jar
#' @param class_folder_path The path to a folder one
#' level above subdirectories that contain the class files.
#' @param output_filepath Optional path to store the XML generated by pattern4. If not
#' specified, it will be saved to `/tmp/gof.xml`.
#'
#' @return A data.table containing the parsed gof patterns per class.
#' @references N. Tsantalis, A. Chatzigeorgiou, G. Stephanides, S. T. Halkidis,
#' "Design Pattern Detection Using Similarity Scoring",
#' IEEE Transactions on Software Engineering,
#' vol. 32, no. 11, pp. 896-909, November, 2006.
#' @export
write_gof_patterns <- function(pattern4_path,class_folder_path,output_filepath='/tmp/gof.xml'){

  pattern4_path <- path.expand(pattern4_path)
  class_folder_path <- path.expand(class_folder_path)

  if(!file.exists(pattern4_path)) stop("The specified pattern4_path does not exist!")
  if(!dir.exists(class_folder_path)) stop("The specified class_folder_path does not exist!")

  # java -Xms32m -Xmx512m -jar pattern4.jar -target "C:\foo\myclasses" -output "C:\foo\output.xml"
  gof_pattern_xml_path <- system2("java",
                                  args = c('-Xms64m','-Xmx100000m','-jar',
                                           pattern4_path,
                                           '-target',paste0('"',class_folder_path,'"'),
                                           '-output',paste0('"',output_filepath,'"')),
                                  stdout = TRUE,
                                  stderr = FALSE)
}

#' Parse GoF Patterns
#'
#' Parses GoF patterns generated by \code{\link{write_gof_patterns}} into a a table.
#' \url{https://www.srcml.org/documentation.html}.
#' Pattern4.jar is available on
#' [Tsantalis' homepage](https://users.encs.concordia.ca/~nikolaos/pattern_detection.html)).
#'
#' @param output_filepath Optional path to read the XML generated by \code{\link{write_gof_patterns}}.  If not
#' specified, it will be assumed saved to the temporary folder path `/tmp/gof.xml`.
#'
#' @return A data.table containing the parsed gof patterns per class.
#' @references N. Tsantalis, A. Chatzigeorgiou, G. Stephanides, S. T. Halkidis,
#' "Design Pattern Detection Using Similarity Scoring",
#' IEEE Transactions on Software Engineering,
#' vol. 32, no. 11, pp. 896-909, November, 2006.
#' @export
parse_gof_patterns <- function(output_filepath='/tmp/gof.xml'){
  gof_pattern_xml <- XML::xmlTreeParse(output_filepath)

  # The <system> root node enumerates a fixed number of <pattern> tags.
  gof_root <- XML::xmlRoot(gof_pattern_xml) #class => XML Node
  patterns <- XML::xmlChildren(gof_root) #class => XMLNodeList (lapply safe)

  parse_instance <- function(instance){

    roles <- XML::xmlChildren(instance)
    role_names <- sapply(roles,XML::xmlGetAttr,"name")
    element <- sapply(roles,XML::xmlGetAttr,"element")

    instance <- data.table(instance_id,
                           role_name = role_names,
                           element = element)

    instance_id <<- instance_id + 1

    return(instance)
  }

  parse_pattern <- function(pattern){
    # Each GoF pattern, if occurring on the code, is assigned an instance
    n_instances <- XML::xmlSize(pattern)

    # The XML mentions the pattern name even with no instances detected. We do not
    # include the pattern name if no instances are detected.
    if(n_instances > 0){

      # Note counter bypasses lapply scope <<-
      instance_id <<- 1

      pattern_name <- XML::xmlGetAttr(pattern,"name")

      instances <- XML::xmlChildren(pattern)
      instances_dt <- rbindlist(lapply(instances,parse_instance))
      instances_dt$pattern_name <- pattern_name
      return(instances_dt)
    }else{
      return(data.table())
    }
  }
  patterns_dt <- rbindlist(lapply(patterns,parse_pattern))

  patterns_dt <- patterns_dt[,.(pattern_name,instance_id,role_name,element)]
  return(patterns_dt)
}


#' Subset GoF Classes
#'
#' The \code{\link{write_gof_patterns}} contains not only
#' the participation of a class in a GoF Pattern, but also
#' the participation of methods and variables when applicable.
#' To distinguish a row entry among class, method or variable,
#' we must subset the role names that are associated to classes.
#' This information can be obtained by inspecting the source code
#' of a similar tool to pattern4 by Tsantalis.
#'
#' More specifically, every pattern that pattern4.jar can identify is
#' defined as a PatternDescriptor in DPD4Eclipse/src/gr/uom/java/pattern
#' /PatternGenerator.java (see: https://github.com/tsantalis/DPD4Eclipse).
#'
#' E.g. The PatternDescriptor Decorator has rowNameList.add("Component");
#' rowNameList.add("Decorator"); Therefore, in the XML output by pattern4.jar,
#' it is guaranteed when (pattern_name == "Decorator" & role_name == "Component") or
#' (pattern_name == "Decorator" & role_name == "Decorator").
#'
#' By following this process, a list of role names can be defined to subset the table
#' to only contain classes.
#'
#' Note pattern4 executes in bytecode, hence the classes are identified by their namespace.
#' Refer to \code{\link{query_src_text_namespace}} to obtain a table to map namespace classes
#'  to filepaths.
#' @param gof_patterns A table of parsed GoF Patterns
#' obtained from \code{\link{parse_gof_patterns}}.
#' @export
subset_gof_class <- function(gof_patterns){
  lead_patterns <- c('Creator', 'Abstraction', 'Adapter', 'Singleton', 'Prototype', 'Decorator', 'AbstractClass', 'Composite', 'Subject', 'State', 'Visitor', 'Strategy', 'Observer', 'Command', 'Handler', 'Component', 'Context', 'Implementor', 'ConcreteElement', 'Prototype', 'Client', 'Proxy', 'RealSubject', 'Subject', 'FamilyHead', 'Redirecter')
  return(gof_patterns[role_name %in% lead_patterns])
}



# Various imports
utils::globalVariables(c("."))
#' @importFrom magrittr %>%
#' @importFrom stringi stri_replace_last
#' @importFrom stringi stri_replace_first
#' @importFrom stringi stri_match_all
#' @importFrom stringi stri_match_first_regex
#' @importFrom stringi stri_detect_regex
#' @importFrom stringi stri_c
#' @importFrom stringi stri_split_regex
#' @importFrom stringi stri_trans_tolower
#' @importFrom data.table data.table
#' @importFrom data.table is.data.table
#' @importFrom data.table as.data.table
#' @importFrom data.table .N
#' @importFrom data.table transpose
#' @importFrom data.table :=
#' @importFrom data.table copy
#' @importFrom data.table rbindlist
#' @importFrom data.table setkey
#' @importFrom data.table setkeyv
#' @importFrom data.table setnames
#' @importFrom data.table last
NULL

