#' Read in character interaction network data from standardised input format
#'
#' @description A helper function for quickly reading in data recorded in a
#'   standardised format wherein certain column orders and variable names are
#'   used. For data not meeting this format, read the data in manually.
#'
#'   To work, the event list should be structured consistently with the format
#'   specified elsewhere in this package documentation (at least a sender ID
#'   column followed by columns containing binary dummy variables for each
#'   character). The function will look for a column called "char_name" in the
#'   nodelist to label the adjacency matrix. If it doesn't find one, it will
#'   infer names from event list columns names.
#'
#' @param events_file The event list, containing at least a sender ID column and
#'   columns containing binary dummy variables for each character.
#' @param nodes_file The node list, containing as many rows as there are unique
#'   characters in the events list.
#' @param start_at The column containing the sender IDs, followed by dummy
#'   variables for each character.
#'
#' @return A named list containing the event list, node list and an adjacency
#'   matrix derived from the event list.
qread_film <- function(events_file, nodes_file, start_at = 3) {
  # First, let's read in the event list and the node list
  lines <- read.csv(events_file, sep = ',', stringsAsFactors = FALSE)
  # Now, let's read in the node list and add some attributes we'll want later
  chars <- read.csv(nodes_file, sep = ',', stringsAsFactors = FALSE)
  chars$nlines <- vector("numeric", nrow(chars))
  for (i in 1:nrow(chars)) {
    chars$nlines[i] <- length(which(lines[ , (start_at)] == i))
  }
  chars$linesin <- colSums(lines[ , (start_at + 1):ncol(lines)])
  # This hacky fix makes sure the code runs in the case of non-numeric input:
  if((FALSE %in% (apply(lines, 2, class) %in% "integer")) == FALSE) {
    lines <- as.matrix(lines)
  }

  # Create adjacency matrix from the event list
  adj <- matrix(0, nrow(chars), nrow(chars))
  for (i in 1:nrow(chars)) {
    for (j in 1:nrow(chars)) {
      adj[i,j] <- length(which(lines[ , start_at] == i &
                                 lines[ , j + (start_at)] == 1))
    }
  }
  if(length(chars$char_name) > 0) {
    colnames(adj) <- chars$char_name
    rownames(adj) <- chars$char_name
  } else {
    colnames(adj) <- rownames(lines)[start_at + 1:ncol(lines)]
    rownames(adj) <- rownames(lines)[start_at + 1:ncol(lines)]
  }

  return(list(event_list = lines, node_list = chars, adjacency = adj))
}
