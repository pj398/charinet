#' Read in character interaction network data from standardised input format
#'
#' @description A helper function for quickly reading in data recorded in a
#'   standardised format wherein certain column orders and variable names are
#'   used. For data not meeting this format, read the data in manually.
#'
#'   More specifically, in order to work, the event list should be structured
#'   consistently with the format specified elsewhere in this package
#'   documentation (at least a sender ID column followed immediately by columns
#'   containing binary dummy variables for each character).
#'
#' @param events_file The event list, containing at least a sender ID column and
#'   columns containing binary dummy variables for each character.
#' @param nodes_file The node list, containing as many rows as there are unique
#'   characters in the events list. If not supplied, it will be automatically
#'   generated from the event list, with character names being inferred from how
#'   they appear in the event list.
#' @param from The column containing the sender IDs, followed by dummy variables
#'   for each character.
#' @param check_errors If TRUE, the function checks for common data input
#'   errors. It looks for characters with self-ties, empty rows where no
#'   recipient was inputted, and any values other than 1 and 0 in the recipient
#'   dummy columns. Returns a message reporting the results of the checks.
#'
#' @return A named list containing the event list, node list and an adjacency
#'   matrix derived from the event list.
#'
#' @export
qread_film <- function(events_file,
                       nodes_file = NULL,
                       from = 3,
                       check_errors = TRUE) {

  lines <- read.csv(events_file, sep = ',', stringsAsFactors = FALSE)

  if(is.null(nodes_file)) {
    chars <- nodes_from_events(event_list = lines, from = from)
  } else {
    chars <- read.csv(nodes_file, sep = ',', stringsAsFactors = FALSE)
  }

  if(is.numeric(unique(lines[, from]))) {
    chars$nlines <- vector("numeric", nrow(chars))
    for (i in 1:nrow(chars)) {
      chars$nlines[i] <- length(which(lines[ , (from)] == i))
    }
  }
  chars$linesin <- colSums(lines[ , (from + 1):ncol(lines)])
  # This hacky fix makes sure the code runs in the case of non-numeric input:
  if((FALSE %in% (apply(lines, 2, class) %in% "integer")) == FALSE) {
    lines <- as.matrix(lines)
  }

  poss_labels <- c("char_name", "character.name", "char.name", "name", "label")
  if(TRUE %in% (poss_labels %in% names(chars))) {
    character_names <- chars[ , which(
      names(chars) == poss_labels[min(which(poss_labels %in% names(chars)))]
      )]
  } else {
    character_names <- colnames(lines)[(from + 1):ncol(lines)]
  }

  adj <- adj_from_events(event_list = lines,
                         char_names = character_names,
                         check_errors = check_errors,
                         from = from)

  return(list(event_list = lines, node_list = chars, adjacency = adj))
}
