# Union-find helper to replace igraph::components()
# Returns a named integer vector of group membership, keyed by node ID,
# equivalent to igraph::components()$membership.
connected_components <- function(edges) {
  # collect all unique nodes from both columns as characters
  nodes <- as.character(unique(c(edges[[1]], edges[[2]])))

  # initialise parent environment: each node is its own root
  # using an environment gives reliable string-keyed lookup
  parent <- new.env(hash = TRUE, parent = emptyenv(), size = length(nodes))
  for (n in nodes) parent[[n]] <- n

  # find root of a node with path compression
  find <- function(x) {
    while (!identical(parent[[x]], x)) {
      parent[[x]] <- parent[[parent[[x]]]] # path compression
      x <- parent[[x]]
    }
    x
  }

  # union two nodes
  union <- function(x, y) {
    rx <- find(x)
    ry <- find(y)
    if (!identical(rx, ry)) {
      parent[[ry]] <- rx
    }
  }

  # process all edges
  for (i in seq_len(nrow(edges))) {
    union(as.character(edges[[1]][i]), as.character(edges[[2]][i]))
  }

  # resolve final roots and convert to integer group labels
  roots <- vapply(nodes, find, character(1))
  group_ids <- match(roots, unique(roots))
  stats::setNames(group_ids, nodes)
}
