# determine size of a tree
get_size_of_tree = function(tree) {
  lapply(tree, function(nodes) unlist(lapply(nodes, function(node) length(node$subset.idx))))
}

# determine objective values from each node for a specific tree depth or for the entire tree
get_objective_values = function(tree, depth = NULL) {
  if (is.null(depth))
    lapply(tree, function(nodes) unlist(lapply(nodes, function(node) node$objective.value)))
  else 
    unlist(lapply(tree[[depth]], function(node) node$objective.value))
}

# order nodes of specific tree depth according to their objective values
order_nodes_by_objective = function(tree, depth) {
  order(get_objective_values(tree, depth = depth))
}