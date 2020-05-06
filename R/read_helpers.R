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
#'
#' @export
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

#' Check for easily identifiable data input errors
#'
#' @description This helper function checks for common data input errors. It
#'   looks for characters with self-ties, empty rows where no recipient was
#'   inputted, and any values other than 1 and 0.
#'
#' @param adjacency The adjacency matrix returned from data read-in.
#' @param event_list The event list returned from data read-in.
#' @param start_at Where to start checking for unexpected values. Important to
#'   note that unlike other functions in the package, start_at should be set to
#'   the first recipient dummy column in the data, which is set by default to 4.
#'
#' @return A message reportinf the results of the check.
#'
#' @export
check_for_errors <- function(adjacency = NULL, event_list = NULL,
                             start_at = 4) {
  if(is.null(adjacency)) {
    stop("Please provide an adjacency matrix via the 'adjacency' argument")
  }
  if(is.null(event_list)) {
    stop("Please provide an event list via the 'event_list' argument")
  }

  self_ties <- vector("numeric", length = nrow(adjacency))
  # Check diagonal for self-ties
  for (i in 1:nrow(adjacency)){
    if(adjacency[i, i] > 0) {
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
  if(length(which(rowSums(event_list[ , start_at:ncol(event_list)]) == 0)) > 0)
  {
    cat("Empty rows: ",
        which(rowSums(event_list[ , start_at:ncol(event_list)]) == 0))
  } else {
    cat("No empty rows found.")
  }
  # Check for other data entry errors (cell values not in c(0, 1))
  if(FALSE %in% unique(c(as.matrix(event_list)[ , start_at:ncol(event_list)]))
     %in% c("0", "1")) {
    cat("\nData entry values: ",
        unique(c(as.matrix(event_list)[ , start_at:ncol(event_list)])))
  }
}
