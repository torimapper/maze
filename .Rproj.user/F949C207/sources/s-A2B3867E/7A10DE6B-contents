#' Create RLmapper
#'
#' @param qvalue Vector of state values
#' @param adjacency Adjacency matrix of states
#' @param num_interval Division number
#'
#' @return Adjacency matrix of set of states
#' @importFrom magrittr %>%
#' @importFrom magrittr %<>%
#' @export
#'
RLmapper <-function(qvalue, adjacency, num_interval = 10){
  splitter <- seq(min(qvalue), max(qvalue), length.out = num_interval + 1)
  level <- purrr::map(1:num_interval, ~ which(qvalue >= splitter[.] & qvalue < splitter[. + 1]))
  level[[num_interval]] <- c(level[[num_interval]], which.max(qvalue))
  node <- level %>%
    purrr::map(~ adjacency[., ., drop = F] %>%
                 igraph::graph.adjacency() %>%
                 igraph::components() %>%
                 igraph::groups()
    )
  node.level <- rep(1:num_interval, sapply(node, length))
  node %<>%
    unlist(recursive = F) %>%
    setter::set_names(paste0("l", node.level, "-", names(.))) %>%
    setter::set_attributes(levels = node.level)

  node.adj <- matrix(0, length(node), length(node))
  for(i in 1:length(node)){
    outlevel <- which(node.level != node.level[i])
    for(j in outlevel) node.adj[i, j] <- adjacency[node[[i]], node[[j]]] %>% as.logical %>% any
  }
  node.adj %<>%
    setter::set_attributes(levels = node.level) %>%
    setter::set_class('RLmapper')
  return(node.adj)
}

#' Plot RLmapper
#'
#' @param x RLmapper object
#' @param ... optional arguments
#'
#' @export
#'
plot.RLmapper <- function(x, ...){
  levels <- attr(x, 'levels')
  col <- levels %>%
    unique() %>%
    length %>%
    grDevices::heat.colors() %>%
    magrittr::extract(levels)
  graph <- igraph::graph.adjacency(x)
  graphics::plot(graph, vertex.color = col)
}
