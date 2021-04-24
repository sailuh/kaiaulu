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
community_metric_sociotechnical_congruence <- function (mail.graph, code.graph) {
  missing.collaborations <- length(community_smell_missing_links(mail.graph, code.graph))
  healthy.collaborations <- length(E(code.graph)) - missing.collaborations
  congruence <- healthy.collaborations / (length(E(code.graph)))
  if (is.na(congruence)) {
    congruence <- 1
  }
  return(congruence)
}
#' Mean value of Decision Communicability.
#'
#' @description Each collaboration between two developers (A and B) in the
#' software development network is considered as a possible source of
#' architectural and design decision, therefore a developer is considered
#' aware of a decision if he or she is strongly connected to at least one
#' of the two developers whom generated the decision. In-communicability
#' is related to every collaboration within the collaboration DSN and it
#' is based on Tamburri et al.’s formulation:
#'
#' MAI = DEM - DAM
#'
#' DEM = n_collaborators_of_two_developers/n_developers
#'
#' DAM = n_collaborators_of_two_developers_who_communicate_with_them/n_developers
#'
#' Therefore, global in-communicability can be defined as the mean MAI over
#' the entire collaboration network. Communicability is a global indicator
#' which consists n the mean of all local communicability measures, calculated
#' for every collaboration between two developers within the collaboration
#' Developer Social Network in the range in analysis.
#' Communicability was preferred to in-communicability in order to simplify
#' measurement comprehension, because in-communicability tend to be characterised
#' by measurements that tend to zero. Communicability is computed as:
#'
#' (1 - mean(incommunicability))
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
community_metric_mean_communicability <- function (mail.graph, code.graph) {
  ## If no collaborations, communicability is 1
  if (length(E(code.graph)) == 0) {
    return(1)
  }

  ## for each collaboration compute its in-communicability value
  collaborations <- get.edgelist(code.graph)
  mai <- c()
  for (coll in 1:length(E(code.graph))) {
    id.dev1 <- V(code.graph)[V(code.graph)$name == collaborations[coll, 1]]$name
    id.dev2 <- V(code.graph)[V(code.graph)$name == collaborations[coll, 2]]$name
    neigh1 <- neighbors(code.graph, V(code.graph)[V(code.graph)$name == id.dev1])$name
    neigh2 <- neighbors(code.graph, V(code.graph)[V(code.graph)$name == id.dev2])$name
    code.neighbors <- unique(union(neigh1, neigh2))
    dem <- length(code.neighbors) / length(V(code.graph))
    neigh1.mail <- c()
    neigh2.mail <- c()
    if (id.dev1 %in% V(mail.graph)$name) {
      neigh1.mail <- neighbors(mail.graph, V(mail.graph)[V(mail.graph)$name == id.dev1])$name
    }
    if (id.dev2 %in% V(mail.graph)$name) {
      neigh2.mail <- neighbors(mail.graph, V(mail.graph)[V(mail.graph)$name == id.dev2])$name
    }
    mail.neighbors <- intersect(unique(union(neigh1.mail, neigh2.mail)), code.neighbors)
    dam <- length(mail.neighbors) / length(V(code.graph))
    mai[length(mai) + 1] <- dem - dam
  }

  return(1-mean(mai))
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
#' @param precomputed.silo Optional. The return of \code{community_smell_organizational_silo}
#' @export
#' @references D. A. Tamburri, F. Palomba and R. Kazman,
#' "Exploring Community Smells in Open-Source: An Automated Approach"
#' in IEEE Transactions on Software Engineering, vol. 47, no. 3,
#' pp. 630-652, 1 March 2021, doi: 10.1109/TSE.2019.2901490.
#' @references Simone Magnoni (2016). An approach to measure Community
#' Smells in software development communities. (Doctoral dissertation, Politecnico Milano).
community_smell_missing_links <- function (mail.graph, code.graph, precomputed.silo=NA) {
  missing <- list()
  for (vert in V(code.graph)$name) {
    if (!(vert %in% V(mail.graph)$name)) {
      next() # the case of one dev not present in the mailing list is handled later
    }
    for (coll in neighbors(code.graph, V(code.graph)[V(code.graph)$name == vert])$name) {
      if (coll > vert) {
        next() # avoid to check twice a graph due to its undirected nature
      }
      if (!(coll %in% V(mail.graph)$name)) {
        next() # the case of one dev not present in the mailing list is handled later
      }
      ## if a missing communication link is found, it is saved
      if (!(coll %in% neighbors(mail.graph, V(mail.graph)[V(mail.graph)$name == vert])$name)) {
        #print(stri_c("vert: ",vert," coll: ",coll))
        missing[[length(missing) + 1]] <- c(vert, coll)
      }
    }
  }

  ## if no precoumputed organisational silo we are done
  if (length(precomputed.silo) == 0){
    return(missing)
  }

  ## If organisational silo is not pre-computed, calculate it
  if (is.na(precomputed.silo)){
    precomputed.silo <- community_smell_organizational_silo(mail.graph, code.graph)
  }
  ## Add the missing links due to developers abstence in the mailing lists
  for (edge in precomputed.silo) {
    missing[[length(missing) + 1]] <- edge
  }

  return(missing)
}
#' Organisational Silo Social Smell
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
community_smell_organizational_silo <- function (mail.graph, code.graph) {
  ## discover develpers not present in the mailing list graph
  non.communicative.ids <- setdiff(V(code.graph)$name, V(mail.graph)$name)
  silos <- list()
  ## for each non communicative developer, save its collaborations
  for (vert in non.communicative.ids) {
    for (collab in neighbors(code.graph, V(code.graph)[V(code.graph)$name == vert])$name) {
      ## if both are non-communicative count the collaboration only once to avoid
      ## counting them twice due to the undirected nature of the graph
      if ((collab %in% non.communicative.ids) & (collab < vert)) {
        next()
      }
      ## organisational silo smell detected
      silos[[length(silos) + 1]] <- c(vert, collab)
    }
  }

  return(silos)
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
community_smell_radio_silence <- function (mail.graph, clusters) {
  brockers <- c()
  memships <- membership(clusters)
  ## consider every communication outside each cluster and if there is just one
  ## communication edge from a sub-community toward another one, we have a
  ## radio silence smell (unique boundary spanner)
  for (clust in 1:length(clusters)) {
    ## If a cluster has only one dev, he is an unique boundary spanner
    if (length(V(mail.graph)[memships == clust]$name) == 1) {
      brockers[length(brockers) + 1] <- V(mail.graph)[memships == clust]$name
      next()
    }
    extra.clust.links <- list()
    for (vert in V(mail.graph)[memships == clust]$name) {
      for (neigh in neighbors(mail.graph, V(mail.graph)[V(mail.graph)$name == vert])) {
        ## Note: neigh is the local graph vertex id, not the developer id
        if (clust != memships[neigh]) {
          ## for each outgoing edge, save the cluster developer id and the destination
          ## sub-community id
          extra.clust.links[[length(extra.clust.links) + 1]] <- c(vert, memships[neigh])
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
          brockers[length(brockers) + 1] <- extra.clust.links[from.dev, 1]
        }
      }
    }
  }

  return(unique(brockers))
}

#' Potential Black Cloud Community Smell
#' @description  Given the communication graph and its sub-communities
#' return the communication edges potentially causing the community smell.
#' "Potential" because it is an actual smell if iterated over time.For conceptual
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
community_smell_potential_black_cloud <- function (mail.graph, clusters) {
  black.links <- list()
  memships <- membership(clusters)
  ## For every sub-community check how many edges connect it to another
  ## sub-community. If there is just one extra-cluster edge, we have
  ## a possible black cloud

  # For each community do... -cp
  for (clust in 1:length(clusters)) {
    extra.clust.links <- list()
    # This loops over all the people who belongs to cluster id "clust" -cp
    for (vert in V(mail.graph)[memships == clust]$name) {
      # Now we loop over the neighbors of "vert" -cp
      for (neigh in neighbors(mail.graph, V(mail.graph)[V(mail.graph)$name == vert])$name) {
        #if(length(memships[V(mail.graph)[V(mail.graph)$name == neigh]] != clust) > 1)
        #  print(memships[V(mail.graph)[V(mail.graph)$name == neigh]])
        # Checks what the cluster_id of neigh is, and if it is different than the current loop
        # iteration on this community -cp
        if (memships[V(mail.graph)[V(mail.graph)$name == neigh]] != clust) {
          extra.clust.links[[length(extra.clust.links) + 1]] <- c(vert, neigh)
        }
      }
    }
    if (length(extra.clust.links) == 1) {
      # Possible black cloud smell detected
      black.links[[length(black.links) + 1]] <- extra.clust.links[[1]]
    }
  }


  return(black.links)
}
#' Prima-donnas Social Smell
#' @description Given the communication graph, its sub-communities and the collaboration
#' graph, return the sub-community ids behaving as prima-donnas.
#' It is possible to specify the threshold to consider two sub-communities collaborating.
#' It is possible to re-use pre-computed potential black links
#' @param mail.graph A uni-modal network G = (V,E), where V are
#' developers (authors or contributors) and E is a collaboration between developers
#' in the mailing list
#' @param clusters Clusters obtained via community detection of mail.graph network.
#' @param code.graph A uni-modal network G = (V,E), where V are
#' developers (authors or contributors) and E is a collaboration between developers
#' in the git log.
#' @param collaboration Total possible collaborations threshold
#' @param precomputed.black Optional, the return of \code{community_smell_potential_black_cloud}
#' @export
#' @references D. A. Tamburri, F. Palomba and R. Kazman,
#' "Exploring Community Smells in Open-Source: An Automated Approach"
#' in IEEE Transactions on Software Engineering, vol. 47, no. 3,
#' pp. 630-652, 1 March 2021, doi: 10.1109/TSE.2019.2901490.
#' @references Simone Magnoni (2016). An approach to measure Community
#' Smells in software development communities. (Doctoral dissertation, Politecnico Milano).
community_smell_primadonnas <- function (mail.graph, clusters, code.graph, collaboration=0.2,
                                         precomputed.black=NA) {
  primadonnas <- list()
  memships <- membership(clusters)
  comms <- communities(clusters)
  ## For every potential black-cloud, check the collaboration of the involved sub-communities.
  ## if it is greater than the threshold, we have two prima-donnas

  ## if no potential black cloud we are done
  if (length(precomputed.black) == 0){
    return(primadonnas)
  }

  if (is.na(precomputed.black)) {
    precomputed.black <- community_smell_potential_black_cloud(mail.graph, clusters)
  }
  for (black.link in precomputed.black) {
    sub.comm.connections <- 0
    ## retrieve cluster identifier of the two sub-communities
    id.clust1 <- memships[V(mail.graph)[V(mail.graph)$name == black.link[1]]]
    id.clust2 <- memships[V(mail.graph)[V(mail.graph)$name == black.link[2]]]
    ## count inter-collaborations of the two sub-communities
    for (dev.clust1 in V(mail.graph)[memships == id.clust1]$name) {
      if (!(dev.clust1 %in% V(code.graph)$name)) {
        next() # ignore devs present only in the communication graph
      }
      for (dev.clust2 in V(mail.graph)[memships == id.clust2]$name) {
        if (!(dev.clust2 %in% V(code.graph)$name)) {
          next() # ignore devs present only in the communication graph
        }
        if (dev.clust1 %in% neighbors(mail.graph, V(mail.graph)[V(mail.graph)$name == dev.clust2])$name) {
          sub.comm.connections <- sub.comm.connections + 1
        }
      }
      ## If the fraction of present collaborations over the total possible collaborations
      ## (Number of devs of clust1 * Number of devs of clust2) then we have two prima-donnas
      tot.possible.collaborations <- length(comms[[id.clust1]]) * length(comms[[id.clust2]])
      if ((sub.comm.connections / tot.possible.collaborations) > collaboration) {
        ## prima-donnas effect detected
        primadonnas[[length(primadonnas) + 1]] <- c(id.clust1, id.clust2)
      }
    }
  }

  return(primadonnas)
}
