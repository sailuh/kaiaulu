


# Igraph Organizational Silo Social Smell
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

# Igraph Missing link Social Smell
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

# Igraph Radio Silence Social Smell.
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
