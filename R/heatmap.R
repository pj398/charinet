#' Generate a character by story-chunk matrix for analysing character activity
#' by story chunks.
#'
#' @description  This function calculates the number of interactions initated by
#'   each character within each story chunk. To calculate this, the events in
#'   the event list need to be indexed by a variable indicating the story chunk.
#'   Story chunks could be scenes, chapters, or some other arbitrary division of
#'   the event list into temporal segments such as quartiles or deciles.
#'
#' @param event_list An event list containing the time-ordered interactions. The
#'   function expects a particular format (described in detail elsewhere in the
#'   package documentation) wherein each row is an event, and the event list
#'   contains at least a column indicating the ID of the sending character,
#'   followed by columns containing dummy variables for each character which
#'   take the value 1 if that character is a target of the interaction, and 0
#'   otherwise.
#' @param char_names An optional vector of character names to be used to label
#'   the rows of the matrix returned by the function (must be of equal length to
#'   the number of unique characters in the event list). If not supplied, names
#'   will be inferred from the event list.
#' @param story_chunk_col The position of the column in the input event_list
#'   which indexes the events by story chunk.
#' @param start_at The position of the column in the input event_list which
#'   indexes the event by the sender ID (e.g. for dialogue data, this will be
#'   the speaker ID column).
#'
#' @return A matrix is returned, with each row corresponding to a character and
#'   each column corresponding to a story chunk. The value of cell [i, j] is the
#'   number of interactions sent by character i in chunk j.
#'
#' @examples
#' tfa <- movienetData::starwars_01
#' my_heatmap <- story_heatmap(event_list = tfa$event_list,
#'                             char_names = tfa$node_list$char_name,
#'                             story_chunk_col = 2,
#'                             start_at = 3)
#' # How many times does character 5 speak in scene 6?
#' my_heatmap[5, 6]
#'
#' @export
story_heatmap <- function(event_list,
                          char_names = NULL,
                          story_chunk_col = 2,
                          start_at = 3) {
  heatmat <- matrix(0,
                    nrow = length(unique(event_list[ , start_at])),
                    ncol = length(unique(event_list[ , story_chunk_col])))

  if(is.null(char_names)) {
    char_names <- colnames(event_list)[(start_at + 1):ncol(event_list)]
  }
  rownames(heatmat) <- char_names
  colnames(heatmat) <- paste("chunk", seq.int(1:ncol(heatmat)), sep = "")

  for (c in unique(event_list[ , start_at])) {
    for (scene in unique(event_list[ , story_chunk_col])) {
      heatmat[which(unique(event_list[ , start_at]) == c),
              which(unique(event_list[ , story_chunk_col]) == scene)] <- length(
                which(event_list[ , story_chunk_col] == scene &
                                          event_list[ , start_at] == c))
    }
  }
  return(heatmat)
}
