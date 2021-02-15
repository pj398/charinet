#' Extract an adjacency matrix from an event list
#'
#' @description A helper function for extracting an adjacency matrix based on
#'   the interactions in an event list.
#'
#'   The event list should be structured consistently with the format
#'   specified elsewhere in this package documentation (at least a sender ID
#'   column followed by columns containing binary dummy variables for each
#'   character).
#'
#' @param event_list The event list, containing at least a sender ID column and
#'   columns containing binary dummy variables for each character.
#' @param node_labels An optional character vector containing node names used to
#'   label rows and columns in the adjacency matrix. If not provided, names are
#'   inferred from the recipient dummy column names of the event list.
#' @param from The column containing the sender IDs, followed by dummy
#'   variables for each character.
#' @param check_errors If TRUE, the function checks the event list for common
#'   data input errors. It looks for characters with self-ties, empty rows where
#'   no recipient was inputted, and any values other than 1 and 0 in the
#'   recipient dummy columns. Returns a message reporting the results of the
#'   checks.
#'
#' @return An adjacency matrix aggregated from interactions in an event list.
#'
#' @examples
#' tfa <- movienetdata::starwars_01
#' tfa_adj <- adj_from_events(event_list = tfa$event_list,
#'                            node_labels = tfa$node_list$char_name,
#'                            check_errors = TRUE)
#'
#' @export
adj_from_events <- function(event_list,
                            node_labels = NULL,
                            from = 3,
                            check_errors = FALSE) {
  # Create adjacency matrix from the event list
  nchars <- length(unique(event_list[ , from]))
  adj <- matrix(0, nchars, nchars)
  for (i in 1:nchars) {
    for (j in 1:nchars) {
      adj[i, j] <- length(which(event_list[ , from] == i &
                                 event_list[ , j + (from)] == 1))
    }
  }
  if(length(node_labels) > 0) {
    colnames(adj) <- node_labels
    rownames(adj) <- node_labels
  } else {
    colnames(adj) <- rownames(event_list)[from + 1:ncol(event_list)]
    rownames(adj) <- rownames(event_list)[from + 1:ncol(event_list)]
  }

  if(check_errors == TRUE) {
    self_ties <- vector("numeric", length = nrow(adj))
    # Check diagonal for self-ties
    for (i in 1:nrow(adj)){
      if(adj[i, i] > 0) {
        self_ties[i] <- 1
      } else {
        self_ties[i] <- 0
      }
    }
    if(length(which(self_ties > 0)) > 0) {
      cat("Characters with self-ties: ", which(self_ties > 0))
    } else {
      cat("No characters with self-ties found.")
    }
    cat("\n")
    # Check for empty rows (no recipients indicated)
    if(length(which(rowSums(event_list[ , (from + 1):ncol(event_list)]) == 0)) > 0)
    {
      cat("Empty rows: ",
          which(rowSums(event_list[ , (from + 1):ncol(event_list)]) == 0))
    } else {
      cat("No empty rows found.")
    }
    cat("\n")
    # Check for other data entry errors (cell values not in c(0, 1))
    if(FALSE %in% unique(c(as.matrix(event_list)[ , (from + 1):ncol(event_list)]))
       %in% c("0", "1")) {
      cat("Data entry values: ",
          unique(c(as.matrix(event_list)[ , (from + 1):ncol(event_list)])), "\n")
    }
  }

  return(adj)
}

#' Extract a nodelist from an event list
#'
#' @description A helper function for extracting a simple list of nodes based on
#'   the interactions in an event list. Extracts the character ID numbers,
#'   character names, and the number of lines spoken by each character.
#'
#'   The event list should be structured consistently with the format specified
#'   elsewhere in this package documentation (at least a sender ID column
#'   followed by columns containing binary dummy variables for each character).
#'
#' @param event_list The event list, containing at least a sender ID column and
#'   columns containing binary dummy variables for each character.
#' @param from The column containing the sender IDs, followed by dummy variables
#'   for each character.
#' @param receivers If TRUE, a column \code{linesin} will be added to the nodelist
#'   containing the number of times the node was the recipient of a line of
#'   dialogue. Only makes sense when dummy variables for recipients correspond
#'   to direct interaction data (but not when the dummies correspond to
#'   co-occurence or proximity, as with automatic extraction methods).
#'
#' @return A data frame with rows corrsponding to characters and columns
#'   containing node-level variables.
#'
#' @examples
#' tfa_events <- movienetdata::starwars_01$event_list
#' tfa_nodes <- nodes_from_events(event_list = tfa_events,
#'                                from = 3,
#'                                receivers = TRUE)
#'
#' @export
nodes_from_events <- function(event_list,
                              from = 3,
                              receivers = FALSE) {
  n_chars <- length(unique(event_list[ , from]))
  nodes <- data.frame(
    charID = seq.int(1:n_chars),
    char_name = colnames(event_list)[(from + 1):ncol(event_list)]
  )
  nodes$nlines <- vector("numeric", n_chars)
  for (i in 1:n_chars) {
    nodes$nlines[i] <- length(which(event_list[ , (from)] == i))
  }
  if(receivers == TRUE) {
    nodes$linesin <- colSums(event_list[ , (from + 1):ncol(event_list)])
  }
  return(nodes)
}

# Internal helper function for checking whether the packages listed in suggests
# are installed (and thus whether the plotting functions, which use tidyverse
# packages, can be run by the user). Takes a character vector of package names
# to search for.
check_suggests <- function(suggest) {
  for (sug in suggest) {
    if(!requireNamespace(sug, quietly = TRUE)) {
      stop(paste0("Please install the package '", sug, "' to use this function."))
    }
  }
