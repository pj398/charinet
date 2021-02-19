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
#' @param char_names An optional character vector containing node names used to
#'   label rows and columns in the adjacency matrix. If not provided, names are
#'   inferred from the event list.
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
#'                            char_names = tfa$node_list$char_name,
#'                            check_errors = TRUE)
#'
#' @export
adj_from_events <- function(event_list,
                            char_names = NULL,
                            from = 3,
                            check_errors = FALSE) {
  if(is.null(char_names)) {
    char_names <- colnames(event_list)[(from + 1):ncol(event_list)]
  }
  nchars <- length(char_names)
  adj <- matrix(0, nchars, nchars)
  for (i in 1:nchars) {
    for (j in 1:nchars) {
      adj[i, j] <- length(which(event_list[ , from] == i &
                                 event_list[ , j + (from)] == 1))
    }
  }

  colnames(adj) <- char_names
  rownames(adj) <- char_names

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
check_sugs <- function(suggest) {
  for (sug in suggest) {
    if(!requireNamespace(sug, quietly = TRUE)) {
      stop(paste0("Please install the package '", sug, "' to use this function."))
    }
  }
}

#' Convert a dyadic event list to multicast format
#'
#' @description This function converts a time-ordered event list in a dyadic
#'   edge-list style format with a 'from' column and a 'to' column into a
#'   multicast format as used by this package with receiver dummy variables
#'   after the from column. If the dyadic event list contains a column
#'   indicating the time or event ID, this can be specified to collect multiple
#'   simultaneous dyadic events spread across multiple rows into a single row in
#'   the multicast event list.
#'
#' @param event_list A time-ordered dyadic event list.
#' @param from The numerical index of the 'from' column in \code{event_list}.
#' @param to The numerical index of the 'to' column in \code{event_list}.
#' @param time_col The numerical index of the column in \code{event_list}
#'   containing the event or time IDs. This is used to identify where multiple
#'   dyadic events are actually part of the same multicast interaction. If no
#'   such column exists, or all events are truly dyadic and sequential, the
#'   function will simply treat each row as an event.
#' @param char_names An optional character vector of character names, ordered by character
#'   ID, used to label the columns.
#'
#' @return A time-ordered multicast event list as a matrix.
#'
#' @examples
#' tfa <- movienetdata::starwars_01
#' # Convert The Force Awakens event list to dyadic format
#' tfa_dyads <- multicast_to_dyadic(tfa$event_list,
#'                                  from = 3)
#' # And convert it back to multicast format
#' tfa_events <- dyadic_to_multicast(event_list = tfa_dyads,
#'                                   from = 4,
#'                                   to = 5,
#'                                   time_col = 1,
#'                                   char_names = tfa$node_list$char_name)
#'
#' @export
dyadic_to_multicast <- function(event_list,
                                from = 1,
                                to = 2,
                                time_col = NULL,
                                char_names = NULL) {
  nchars <- length(unique(c(event_list[ , from], event_list[ , to])))
  if(is.numeric(event_list[ , from]) & is.numeric(event_list[ , to])) {
    fromID <- event_list[ , from]
    toID <- event_list[ , to]
  } else {
    fromID <- numeric(length = nrow(event_list))
    toID <- numeric(length = nrow(event_list))
    chars <- unique(c(event_list[ , from], event_list[ , to]))
    for(i in 1:nrow(event_list)) {
      fromID[i] <- which(chars == event_list[i, from])
      toID[i] <- which(chars == event_list[i, to])
    }
  }

  if(is.null(time_col)) {
    timeID <- seq.int(from = 1, to = nrow(event_list))
  } else {
    timeID <- event_list[ , time_col]
  }
  multicast <- matrix(0, nrow = length(unique(timeID)), ncol = nchars + 2)
  if(!is.null(char_names)) {
    colnames(multicast) <- c("eventID", "from", char_names)
  }else {
    if(is.numeric(event_list[ , from]) & is.numeric(event_list[ , to])) {
      colnames(multicast) <- c("eventID", "from", paste0("Character ",
                                                         1:nchars))
    } else {
      colnames(multicast) <- c("eventID", "from", chars)
    }
  }

  for (i in 1:nrow(event_list)) {
    event_id <- timeID[i]
    multicast[event_id, 1] <- event_id
    multicast[event_id, 2] <- fromID[i]
    multicast[event_id, (toID[i] + 2)] <- 1
  }
  return(multicast)
}

#' Convert a multicast event list to dyadic format
#'
#' @description Convert a time-ordered multicast event list into a dyadic
#'   from-to format, preserving other event-level variables.
#'
#' @param event_list A multicast event list containing a column of sender IDs
#'   followed immediately by dummy variables for each character.
#' @param from The column containing the sender IDs.
#' @param force_seq Logical. If TRUE, events with multiple recipients will be
#'   forced into a false sequence, and each row in the dyadic event list will
#'   increment the event ID variable. If FALSE, multicast events will be split
#'   into the dyadic components, but the event ID variable will indicate that
#'   these dyadic events are simultaneous.
#'
#' @return A time-ordered dyadic event list (as a matrix).
#'
#' @examples
#' tfa <- movienetdata::starwars_01
#' # Convert The Force Awakens event list to dyadic format
#' tfa_dyads <- multicast_to_dyadic(tfa$event_list,
#'                                  from = 3)
#' # And convert it back to multicast format
#' tfa_events <- dyadic_to_multicast(event_list = tfa_dyads,
#'                                   from = 4,
#'                                   to = 5,
#'                                   time_col = 1,
#'                                   char_names = tfa$node_list$char_name)
#'
#' @export
multicast_to_dyadic <- function(event_list,
                                from = 3,
                                force_seq = FALSE) {
  events_data <- matrix(0,
                        nrow = sum(event_list[ , (from + 1):ncol(event_list)]),
                        ncol = from + 2)
  k <- 0
  for (i in 1:nrow(event_list)) {
    for (j in (from + 1):ncol(event_list)) {
      if(event_list[i, j] == 1) {
        k <- k + 1
        if(force_seq == TRUE) {
          events_data[k, ] <- c(k, event_list[i, 1:from], j - from)
        } else {
          events_data[k, ] <- c(i, event_list[i, 1:from], j - from)
        }
      } else {
        next
      }
    }
  }
  colnames(events_data) <- c("time", dimnames(event_list)[[2]][1:from - 1],
                             "from", "to")
  return(events_data)
}
