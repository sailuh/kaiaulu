# Kaiaulu - https://github.com/sailuh/kaiaulu
#
# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at https://mozilla.org/MPL/2.0/.

#' Triangle Motif
#'
#' Triangle motifs represent two developers who communicate when
#' changing a file. This is seeing as a "positive" motif: We expect
#' developers will communicate when changing the same file. To obtain
#' the project's network for this motif, refer to
#' \code{\link{transform_gitlog_to_bipartite_network}} and
#' \code{\link{transform_reply_to_bipartite_network}}.
#'
#' @family motif
#' @export
#' @references W. Mauerer, M. Joblin, D. A. Tamburri, C. Paradis, R. Kazman and S. Apel,
#' "In Search of Socio-Technical Congruence: A Large-Scale Longitudinal Study,"
#' in IEEE Transactions on Software Engineering, vol. 48, no. 8, pp. 3159-3184,
#' 1 Aug. 2022, doi: 10.1109/TSE.2021.3082074.
motif_factory_triangle <- function(){
  person_color <- "black"
  file_color <- "#f4dbb5"
  motif_triangle <- list()
  motif_triangle[["nodes"]] <- data.table(name=c(1,2,3),
                                          type = c(TRUE,TRUE,FALSE),
                                          color=c(person_color,
                                                  person_color,
                                                  file_color))
  motif_triangle[["edgelist"]] <- data.table(from=c(1,1,2),
                                             to=c(2,3,3))

  return(motif_triangle)
}

#' Anti-Triangle Motif
#'
#' Triangle motifs represent two developers who do NOT communicate when
#' changing a file. This is seeing as a "positive" motif: We expect
#' developers will communicate when changing the same file. To obtain
#' the project's network for this motif, refer to
#' \code{\link{transform_gitlog_to_bipartite_network}} and
#' \code{\link{transform_reply_to_bipartite_network}}.
#'
#' @family motif
#' @export
#' @references W. Mauerer, M. Joblin, D. A. Tamburri, C. Paradis, R. Kazman and S. Apel,
#' "In Search of Socio-Technical Congruence: A Large-Scale Longitudinal Study,"
#' in IEEE Transactions on Software Engineering, vol. 48, no. 8, pp. 3159-3184,
#' 1 Aug. 2022, doi: 10.1109/TSE.2021.3082074.
motif_factory_anti_triangle <- function(){
  person_color <- "black"
  file_color <- "#f4dbb5"
  motif_anti_triangle <- list()
  motif_anti_triangle[["nodes"]] <- data.table(name=c(1,2,3),
                                          type = c(TRUE,TRUE,FALSE),
                                          color=c(person_color,
                                                  person_color,
                                                  file_color))
  motif_anti_triangle[["edgelist"]] <- data.table(from=c(1,2),
                                             to=c(3,3))

  return(motif_anti_triangle)
}

#' Square Motif
#'
#' Square motifs represent two developers who communicate when
#' changing two different files that have a dependency.
#' This is seeing as a "positive" motif: We expect
#' developers will communicate when changing different files which
#' have a dependency. To obtain
#' the project's network for this motif, refer to
#' \code{\link{transform_gitlog_to_bipartite_network}},
#' \code{\link{transform_reply_to_bipartite_network}}, and
#' \code{\link{transform_r_dependencies_to_network}} or
#' \code{\link{transform_dependencies_to_network}}.
#'
#' @family motif
#' @export
#' @references W. Mauerer, M. Joblin, D. A. Tamburri, C. Paradis, R. Kazman and S. Apel,
#' "In Search of Socio-Technical Congruence: A Large-Scale Longitudinal Study,"
#' in IEEE Transactions on Software Engineering, vol. 48, no. 8, pp. 3159-3184,
#' 1 Aug. 2022, doi: 10.1109/TSE.2021.3082074.
motif_factory_square <- function(){
  person_color <- "black"
  file_color <- "#f4dbb5"
  motif_square <- list()
  motif_square[["nodes"]] <- data.table(name=c(1,2,3,4),
                                          type = c(TRUE,TRUE,FALSE,FALSE),
                                          color=c(person_color,
                                                  person_color,
                                                  file_color,
                                                  file_color))
  motif_square[["edgelist"]] <- data.table(from=c(1,1,2,3),
                                             to=c(2,3,4,4))

  return(motif_square)
}

#' Anti-Square Motif
#'
#' Square motifs represent two developers who do NOT communicate when
#' changing two different files that have a dependency.
#' This is seeing as a "positive" motif: We expect
#' developers will communicate when changing different files which
#' have a dependency. To obtain
#' the project's network for this motif, refer to
#' \code{\link{transform_gitlog_to_bipartite_network}},
#' \code{\link{transform_reply_to_bipartite_network}}, and
#' \code{\link{transform_r_dependencies_to_network}} or
#' \code{\link{transform_dependencies_to_network}}.
#'
#' @family motif
#' @export
#' @references W. Mauerer, M. Joblin, D. A. Tamburri, C. Paradis, R. Kazman and S. Apel,
#' "In Search of Socio-Technical Congruence: A Large-Scale Longitudinal Study,"
#' in IEEE Transactions on Software Engineering, vol. 48, no. 8, pp. 3159-3184,
#' 1 Aug. 2022, doi: 10.1109/TSE.2021.3082074.
motif_factory_anti_square <- function(){
  person_color <- "black"
  file_color <- "#f4dbb5"
  motif_anti_square <- list()
  motif_anti_square[["nodes"]] <- data.table(name=c(1,2,3,4),
                                        type = c(TRUE,TRUE,FALSE,FALSE),
                                        color=c(person_color,
                                                person_color,
                                                file_color,
                                                file_color))
  motif_anti_square[["edgelist"]] <- data.table(from=c(1,2,3),
                                           to=c(3,4,4))

  return(motif_anti_square)
}

#' Motif Factory
#'
#' A simple factory function to generate available
#' motifs.
#'
#' @param type The type of motif to construct.
#' @family motif
#' @export
motif_factory <- function(type){
  if(type == "triangle") return(motif_factory_triangle())
  else if(type == "square") return(motif_factory_square())
  if(type == "anti_triangle") return(motif_factory_anti_triangle())
  else if(type == "anti_square") return(motif_factory_anti_square())
  else return(NULL)
}
