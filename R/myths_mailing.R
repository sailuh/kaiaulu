# Mailing-List Reply Motifs
#
# Parse Perceval-emitted mbox JSON, build reply edges, detect radio-silence (broker-loss) events. Supports the congruence lift.
#
# Carried into kaiaulu from icse27theories/lifts/functions.R.
# Style follows kaiaulu's verb_noun snake_case + data.table.

require(data.table)
require(stringi)
require(magrittr)

# Null-coalescing helper used by several functions below. Local to each
# myths_*.R file rather than a global import so each module is
# self-contained when sourced.
`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a


#' Parse All Mbox Files in a Directory via kaiaulu::parse_mbox
#'
#' Wraps Perceval-via-kaiaulu over every \code{*.mbox} file in
#' \code{mbox_dir}. Returns a single data.table with the union of
#' message records.
#'
#' @param perceval_path Path to perceval executable.
#' @param mbox_dir Directory containing one or more .mbox files.
#' @return data.table with kaiaulu's parse_mbox columns
#'   (message_id, reply_to, sender, ...).
#' @export
parse_mbox_dir <- function(perceval_path, mbox_dir) {
  files <- list.files(mbox_dir, pattern = "\\.mbox$",
                      full.names = TRUE)
  if (length(files) == 0) {
    stop("no .mbox files in ", mbox_dir)
  }
  rbindlist(lapply(files, function(f) {
    tryCatch(parse_mbox(perceval_path, f),
             error = function(e) {
               warning(sprintf("parse_mbox failed on %s: %s",
                               basename(f), conditionMessage(e)))
               NULL
             })
  }), fill = TRUE)
}

#' Build Reply Edge List from Mbox Messages
#'
#' Resolves each (child, parent_message_id) reply link to a (child,
#' parent_identity) edge by looking up the parent message's sender.
#'
#' kaiaulu::parse_mbox returns columns:
#'   reply_id, in_reply_to_id, reply_from, ...
#' After identity_match on \code{reply_from}, each row gets
#' identity_id. We pivot on (reply_id → identity_id) to build edges.
#'
#' @param msgs data.table with cols reply_id, in_reply_to_id,
#'   identity_id (post-identity_match).
#' @return data.table with cols src_id, dst_id, weight.
#' @export
build_reply_edges <- function(msgs) {
  mid_to_id <- setNames(msgs$identity_id, msgs$reply_id)
  replies <- msgs[!is.na(in_reply_to_id) & in_reply_to_id != "",
                  .(child_id = identity_id, parent_mid = in_reply_to_id)]
  replies[, parent_id := mid_to_id[parent_mid]]
  replies <- replies[!is.na(parent_id) & parent_id != child_id]
  replies[, `:=`(
    src_id = pmin(child_id, parent_id),
    dst_id = pmax(child_id, parent_id)
  )]
  replies[, .(weight = .N), by = .(src_id, dst_id)]
}

#' Detect Radio-Silence Brokers
#'
#' Ports kaiaulu R/smells.R:207. A broker bridges otherwise-
#' disconnected community pairs: in any cluster, if a vertex is the
#' SOLE outgoing edge to some other cluster, it is a "radio silence"
#' broker — its absence severs the inter-cluster channel.
#'
#' Implementation: build a Louvain partition of the largest connected
#' component, then for each (cluster, vert) compute the count of
#' edges to each external cluster; flag vert as broker if any
#' external-cluster edge count == 1.
#'
#' @param edges data.table of (src_id, dst_id, weight) on identity_ids.
#' @return list with: graph, partition (named vec id→cluster),
#'   brokers (unique ids), incidents (data.table of one row per
#'   (cluster, dev, sole-cluster-bridged-to) tuple), cluster_sizes.
#' @export
detect_radio_silence <- function(edges) {
  g <- igraph::graph_from_data_frame(
    d = edges[, .(src_id, dst_id, weight)],
    directed = FALSE)
  ccs <- igraph::components(g)
  main_ids <- which(ccs$membership == which.max(ccs$csize))
  g_main <- igraph::induced_subgraph(g, main_ids)
  louv <- igraph::cluster_louvain(g_main,
                                  weights = igraph::E(g_main)$weight)
  membership <- igraph::membership(louv)
  ids <- igraph::V(g_main)$name

  brokers   <- character(0)
  incidents <- data.table(dev = character(),
                          cluster = integer(),
                          bridge_to = integer())

  for (cid in unique(membership)) {
    devs <- ids[membership == cid]
    if (length(devs) == 1) {
      brokers <- c(brokers, devs)
      incidents <- rbind(incidents,
                         data.table(dev = devs, cluster = cid,
                                    bridge_to = NA_integer_))
      next
    }
    # outgoing edges per (vert, target-cluster)
    out_counts <- list()
    for (v in devs) {
      nbrs <- igraph::neighbors(g_main, v)
      nbr_ids <- ids[as.integer(nbrs)]
      nbr_cls <- membership[nbr_ids]
      ext <- nbr_cls[nbr_cls != cid]
      if (length(ext) == 0) next
      for (oc in unique(ext)) {
        key <- paste(oc, v, sep = "|")
        out_counts[[key]] <- (out_counts[[key]] %||% 0L) +
                             sum(ext == oc)
      }
    }
    # for each external cluster, find which devs have exactly 1 edge
    by_target <- split(names(out_counts), sapply(strsplit(names(out_counts), "\\|"), `[`, 1))
    for (target_str in names(by_target)) {
      total_links <- sum(unlist(out_counts[by_target[[target_str]]]))
      if (total_links == 1) {
        key <- by_target[[target_str]][1]
        v <- strsplit(key, "\\|")[[1]][2]
        brokers <- c(brokers, v)
        incidents <- rbind(incidents,
                           data.table(dev = v, cluster = cid,
                                      bridge_to = as.integer(target_str)))
      }
    }
  }
  list(
    graph         = g_main,
    partition     = membership,
    brokers       = unique(brokers),
    incidents     = incidents,
    cluster_sizes = sort(as.integer(table(membership)), decreasing = TRUE)
  )
}
