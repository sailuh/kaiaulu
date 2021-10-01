# Kaiaulu - https://github.com/sailuh/kaiaulu
#
# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at https://mozilla.org/MPL/2.0/.

#' Organizational Silo Social Smell
#'
#' @description Given the communication and collaboration graphs (mailing lists and VCS)
#' return the collaboration edges, analyzed in groups of two devs, in
#' which one of co-committing devs do not communicate at all. For conceptual
#' details please see references.
#' @param mail.graph A uni-modal network G = (V,E), where V are
#' developers (authors or contributors) and E is a collaboration between developers
#' in the mailing list
#' @param code.graph A uni-modal network G = (V,E), where V are
#' developers (authors or contributors) and E is a collaboration between developers
#' in the git log.
#' @export
#' @references D. A. Tamburri, F. Palomba and R. Kazman,
#' "Exploring Community Smells in Open-Source: An Automated Approach"
#' in IEEE Transactions on Software Engineering, vol. 47, no. 3,
#' pp. 630-652, 1 March 2021, doi: 10.1109/TSE.2019.2901490.
#' @references Simone Magnoni (2016). An approach to measure Community
#' Smells in software development communities. (Doctoral dissertation, Politecnico Milano).
smell_organizational_silo <- function (mail.graph, code.graph) {
  ## discover developers not present in the mailing list graph

  # add a param for is_directed to check only the from to, instead of both ways (?)
  # add weight information to returned vector from git log number of changes

  # To simplify operations, map all names to id to process
  id_to_name <- as.character(unique(c(code.graph[["nodes"]]$name,
                                      mail.graph[["nodes"]]$name)))
  name_to_id <- 1:length(id_to_name)
  names(name_to_id) <- id_to_name

  #code_dev <- V(code.graph)$name
  code_dev <- name_to_id[code.graph[["nodes"]]$name]
  mail_dev <- name_to_id[mail.graph[["nodes"]]$name]

  code.graph[["edgelist"]]$from <- name_to_id[code.graph[["edgelist"]]$from]
  code.graph[["edgelist"]]$to <- name_to_id[code.graph[["edgelist"]]$to]

  # git developers not subscribed to the mailing list
  non.communicative.ids <- setdiff(code_dev, mail_dev)

  silos <- list()
  # for each developer (vert) not in the mailing list:
  for (vert in non.communicative.ids) {
    # ... and for each developer which modified a file with `vert` (git adjacent to it):
    # vert_neighbors <- neighbors(code.graph, V(code.graph)[V(code.graph)$name == vert])$name
    vert_neighbors <- c(code.graph[["edgelist"]][from == vert]$to,
                        code.graph[["edgelist"]][to == vert]$from)
    for (collab in vert_neighbors) {
      ## if both are non-communicative count the collaboration only once to avoid
      ## counting them twice due to the undirected nature of the graph
      if ((collab %in% non.communicative.ids) & (collab < vert)) {
        next()
      }
      ## organisational silo smell detected
      silos[[length(silos) + 1]] <- c(id_to_name[vert], id_to_name[collab])
    }
  }

  return(silos)
}

#' Missing link Social Smell
#'
#' @description Given the communication and collaboration graphs (mailing lists and VCS)
#' return the collaboration edges that do not have a communication counterpart.
#' It is possible to pass precomputed organisational silo community smell. For conceptual
#' details please see references.
#' @param mail.graph A uni-modal network G = (V,E), where V are
#' developers (authors or contributors) and E is a collaboration between developers
#' in the mailing list
#' @param code.graph A uni-modal network G = (V,E), where V are
#' developers (authors or contributors) and E is a collaboration between developers
#' in the git log.
#' @param precomputed.silo Optional. The return of \code{smell_organizational_silo}
#' @export
#' @references D. A. Tamburri, F. Palomba and R. Kazman,
#' "Exploring Community Smells in Open-Source: An Automated Approach"
#' in IEEE Transactions on Software Engineering, vol. 47, no. 3,
#' pp. 630-652, 1 March 2021, doi: 10.1109/TSE.2019.2901490.
#' @references Simone Magnoni (2016). An approach to measure Community
#' Smells in software development communities. (Doctoral dissertation, Politecnico Milano).
smell_missing_links <- function (mail.graph, code.graph, precomputed.silo=NA) {

  # To simplify operations, map all names to id to process
  id_to_name <- as.character(unique(c(code.graph[["nodes"]]$name,
                                      mail.graph[["nodes"]]$name)))
  name_to_id <- 1:length(id_to_name)
  names(name_to_id) <- id_to_name

  #code_dev <- V(code.graph)$name
  code_dev <- name_to_id[code.graph[["nodes"]]$name]
  mail_dev <- name_to_id[mail.graph[["nodes"]]$name]

  code.graph[["edgelist"]]$from <- name_to_id[code.graph[["edgelist"]]$from]
  code.graph[["edgelist"]]$to <- name_to_id[code.graph[["edgelist"]]$to]

  mail.graph[["edgelist"]]$from <- name_to_id[mail.graph[["edgelist"]]$from]
  mail.graph[["edgelist"]]$to <- name_to_id[mail.graph[["edgelist"]]$to]

  missing <- list()
  for (vert in code_dev) {
    if (!(vert %in% mail_dev)) {
      next() # the case of one dev not present in the mailing list is handled later
    }
    # vert_neighbors <- neighbors(code.graph, V(code.graph)[V(code.graph)$name == vert])$name
    vert_neighbors <- c(code.graph[["edgelist"]][from == vert]$to,
                        code.graph[["edgelist"]][to == vert]$from)
    for (coll in vert_neighbors) {
      if (coll > vert) {
        next() # avoid to check twice a graph due to its undirected nature
      }
      if (!(coll %in% mail_dev)) {
        next() # the case of one dev not present in the mailing list is handled later
      }
      ## if a missing communication link is found, it is saved
      # vert_neighbors <- neighbors(mail.graph, V(mail.graph)[V(mail.graph)$name == vert])$name
      vert_neighbors <- c(mail.graph[["edgelist"]][from == vert]$to,
                          mail.graph[["edgelist"]][to == vert]$from)
      if (!(coll %in% vert_neighbors)) {
        #print(stri_c("vert: ",vert," coll: ",coll))
        missing[[length(missing) + 1]] <- c(id_to_name[vert], id_to_name[coll])
      }
    }
  }

  ## if no precoumputed organisational silo we are done
  if (length(precomputed.silo) == 0){
    return(missing)
  }

  ## If organisational silo is not pre-computed, calculate it
  if (is.na(precomputed.silo)){
    precomputed.silo <- smell_organizational_silo(mail.graph, code.graph)
  }
  ## Add the missing links due to developers abstence in the mailing lists
  for (edge in precomputed.silo) {
    missing[[length(missing) + 1]] <- edge
  }

  return(missing)
}


#' Estimates Socio-Technical Congruence
#'
#' @description Socio-technical congruence (st.congruence) is measured as the
#' number of development collaborations that do have a communication counterpart
#' over the total number of collaboration links present in the collaboration
#' Developer Social Network. Development collaborations that do have a
#' communication counterpart are identified analysing one by one the collaboration
#' links that connect different developers present in the collaboration Developer
#' Social Network and check within the communication Developer Social Network if
#' such developers are present and connected through a communication link.
#' Therefore, socio-technical congruence can be computed using Missing Links
#' Community Smell metric as follows:
#'
#' st_congruence = (n_collaborations - n_missing_links)/n_collaborations
#'
#' @param mail.graph A uni-modal network G = (V,E), where V are
#' developers (authors or contributors) and E is a collaboration between developers
#' in the mailing list
#' @param code.graph A uni-modal network G = (V,E), where V are
#' developers (authors or contributors) and E is a collaboration between developers
#' in the git log.
#' @export
#' @references D. A. Tamburri, F. Palomba and R. Kazman,
#' "Exploring Community Smells in Open-Source: An Automated Approach"
#' in IEEE Transactions on Software Engineering, vol. 47, no. 3,
#' pp. 630-652, 1 March 2021, doi: 10.1109/TSE.2019.2901490.
#' @references Simone Magnoni (2016). An approach to measure Community
#' Smells in software development communities. (Doctoral dissertation, Politecnico Milano).
smell_sociotechnical_congruence <- function (mail.graph, code.graph) {
  missing.collaborations <- length(smell_missing_links(mail.graph, code.graph))
  #healthy.collaborations <- length(E(code.graph)) - missing.collaborations
  healthy.collaborations <- nrow(code.graph[["edgelist"]]) - missing.collaborations
  #congruence <- healthy.collaborations / (length(E(code.graph)))
  congruence <- healthy.collaborations / (nrow(code.graph[["edgelist"]]))
  if (is.na(congruence)) {
    congruence <- 1
  }
  return(congruence)
}

#' Radio Silence Social Smell.
#' @description  Given the communication graph and its sub-communities,
#' return the ids of unique boundary spanners towards another
#' sub-community, generation this smell. For conceptual
#' details please see references.
#' @param mail.graph A uni-modal network G = (V,E), where V are
#' developers (authors or contributors) and E is a collaboration between developers
#' in the mailing list
#' @param clusters Clusters obtained via community detection of mail.graph network.
#' @export
#' @references D. A. Tamburri, F. Palomba and R. Kazman,
#' "Exploring Community Smells in Open-Source: An Automated Approach"
#' in IEEE Transactions on Software Engineering, vol. 47, no. 3,
#' pp. 630-652, 1 March 2021, doi: 10.1109/TSE.2019.2901490.
#' @references Simone Magnoni (2016). An approach to measure Community
#' Smells in software development communities. (Doctoral dissertation, Politecnico Milano).
smell_radio_silence <- function (mail.graph, clusters) {

  # To simplify operations, map all names to id to process
  id_to_name <- as.character(unique(mail.graph[["nodes"]]$name))
  name_to_id <- 1:length(id_to_name)
  names(name_to_id) <- id_to_name

  #code_dev <- V(code.graph)$name
  mail_dev <- name_to_id[mail.graph[["nodes"]]$name]

  # Map names to ids within function scope
  mail.graph[["edgelist"]]$from <- name_to_id[mail.graph[["edgelist"]]$from]
  mail.graph[["edgelist"]]$to <- name_to_id[mail.graph[["edgelist"]]$to]
  clusters[["assignment"]]$node_id <- name_to_id[clusters[["assignment"]]$node_id]


  brockers <- c()
  #memships <- membership(clusters)
  clusters_list <- clusters

  # Ensure numeric cluster id are treatred as numeric
  clusters_list[["assignment"]]$cluster_id <- as.numeric(clusters_list[["assignment"]]$cluster_id)
  clusters_list[["info"]]$cluster_id <- as.numeric(clusters_list[["info"]]$cluster_id)

  memships <- clusters_list[["assignment"]]

  clusters <- as.numeric(clusters_list[["info"]]$cluster_id)



  ## consider every communication outside each cluster and if there is just one
  ## communication edge from a sub-community toward another one, we have a
  ## radio silence smell (unique boundary spanner)
  for (clust in 1:length(clusters)) {
    ## If a cluster has only one dev, he is an unique boundary spanner
    # clust_size <- length(V(mail.graph)[memships == clust]$name)
    clust_size <- clusters_list[["info"]]$cluster_size[clust]
    if (clust_size == 1) {
      # clust_name <- V(mail.graph)[memships == clust]$name
      clust_name <- clusters_list[["assignment"]][cluster_id == clust]$node_id
      brockers[length(brockers) + 1] <- id_to_name[clust_name]
      next()
    }
    # broker's next() not triggered, then clust_size > 1
    extra.clust.links <- list()
    # vert_names <- V(mail.graph)[memships == clust]$name
    vert_names <- clusters_list[["assignment"]][cluster_id == clust]$node_id
    for (vert in vert_names) {
      #vert_neighbors <- neighbors(mail.graph, V(mail.graph)[V(mail.graph)$name == vert])
      vert_neighbors <- c(mail.graph[["edgelist"]][from == vert]$to,
                          mail.graph[["edgelist"]][to == vert]$from)
      for (neigh in vert_neighbors) {
        ## Note: neigh is the local graph vertex id, not the developer id
        #neigh_membership <- memships[neigh]
        neigh_membership <- memships[node_id == neigh]$cluster_id[1]
        if (clust != neigh_membership) {
          ## for each outgoing edge, save the cluster developer id and the destination
          ## sub-community id
          extra.clust.links[[length(extra.clust.links) + 1]] <- c(vert, neigh_membership)
        }
      }
    }
    ## for each outgoing edge, substitute destination vertex with its community
    if (length(extra.clust.links) > 0) {
      ## change format to enable comparisons
      extra.clust.links <- matrix(unlist(extra.clust.links), ncol=2, byrow=TRUE)
      for (outClust in unique(extra.clust.links[, 2])) {
        from.dev <- which(extra.clust.links[, 2] == outClust)
        if (length(from.dev) == 1) {
          ## radio silence community smell detected
          brockers[length(brockers) + 1] <- id_to_name[extra.clust.links[from.dev, 1]]
        }
      }
    }
  }

  return(unique(brockers))
}
