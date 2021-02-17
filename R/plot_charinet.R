#' Plot the character interaction network using ggraph
#'
#' @description Create a quick visualisation of the aggregated character
#'   interaction network.
#'
#' @param adjacency The adjacency matrix based on a given character interaction
#'   network event list.
#' @param char_names A vector of character names, arranged in order of the
#'   character IDs. If not supplied, names will be inferred from the rownames of
#'   \code{adjacency}.
#' @param degree A vector of numeric values containing information on the
#'   degree/activity of nodes. This will be used to scale the size of the nodes
#'   in the network diagram.
#' @param cutoff A numeric value to be used to filter the visualisation to only
#'   include those characters whose \code{degree} is greater than \code{cutoff}.
#' @param drop_isolates Logical. If TRUE, nodes with no ties to other nodes will
#'   not be included in the graph.
#' @param node_fill A character string providing the colour to be used to fill
#'   the nodes.
#' @param parallel_edges Logical. If TRUE, directed edges will be drawn as two
#'   separate parallel arrows. If FALSE, a single double-sided arrow will be
#'   drawn.
#' @param title An optional character string which can be used to add a title to
#'   the visualisation.
#'
#' @return A ggplot2 plot.
#'
#' @examples
#' tfa <- movienetdata::starwars_01
#' \dontrun{
#' # Plot the network, dropping characters who don't speak more than 3 lines
#' plot_charinet(adjacency = tfa$adjacency,
#'               char_names = tfa$nodelist$char_name,
#'               degree = tfa$nodelist$nlines,
#'               cutoff = 3,
#'               title = "Dialogue interactions in Star Wars: The Force Awakens")}
#'
#' @export
plot_charinet <- function(adjacency = NULL,
                         char_names = NULL,
                         degree = NULL,
                         cutoff = NULL,
                         drop_isolates = TRUE,
                         node_fill = "#eb4034",
                         parallel_edges = FALSE,
                         title = "") {
  check_sugs(c("ggraph", "ggplot2", "graphlayouts", "grid", "igraph"))
  if(is.null(adjacency)) {
    stop("Please provide an adjacency matrix via the 'adjacency' argument")
  }

  # Create the igraph object
  g <- igraph::graph_from_adjacency_matrix(adjacency, weighted = TRUE,
                                           diag = FALSE)
  if(is.null(char_names)) {
    char_names <- rownames(adjacency)
  }
  igraph::V(g)$name <- char_names

  if(is.null(degree)) {
    degree <- rowSums(adjacency)
  }
  igraph::V(g)$size <- scales::rescale(degree, to = c(2, 15))

  if(!is.null(cutoff)) {
    g <- igraph::delete.vertices(g, igraph::V(g)[which(degree < cutoff)])
  }

  if(drop_isolates == TRUE) {
    g <- igraph::delete.vertices(g, igraph::V(g)[which(igraph::degree(g) == 0)])
  }

  g <- graphlayouts::reorder_edges(g, "weight", desc = FALSE)

  if(parallel_edges == TRUE) {
    p <- ggraph::ggraph(g, layout = "stress") +
      ggraph::geom_edge_parallel(ggplot2::aes(edge_colour = .data$weight,
                                              end_cap = ggraph::circle(
                                                .data$node2.size * 1.1,
                                                unit = "pt"
                                                )),
                                 n = 2, arrow = grid::arrow(
                                   angle = 20, length = grid::unit(0.14, "inches"),
                                   ends = "last", type = "closed"))
  } else {
    p <- ggraph::ggraph(g, layout = "stress") +
      ggraph::geom_edge_link(ggplot2::aes(edge_colour = .data$weight,
                                          end_cap = ggraph::circle(
                                            .data$node2.size * 1.1,
                                            unit = "pt")),
                             n = 2, arrow = grid::arrow(
                               angle = 20, length = grid::unit(0.14, "inches"),
                               ends = "last", type = "closed"))
  }
  plot(p + ggraph::geom_node_point(fill = node_fill, size = igraph::V(g)$size,
                                   shape = 21, stroke = 0.5, colour = "black") +
         ggraph::geom_node_text(ggplot2::aes(label = .data$name), size = 4.5,
                                nudge_y = scales::rescale(igraph::V(g)$size,
                                                     to = c(0.06, 0.15))) +
         ggraph::scale_edge_colour_gradient(low = "grey85", high = "grey25",
                                            trans = "sqrt") +
         ggplot2::labs(title = title) +
         ggplot2::scale_x_continuous(expand = c(0.1, 0.1)) +
         ggraph::theme_graph() +
         ggplot2::theme(legend.position = "none",
                        plot.title = ggplot2::element_text(size = 16,
                                                           hjust = 0.5))) %>%
         suppressWarnings()
}
