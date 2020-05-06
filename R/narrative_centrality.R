#' Calculate dynamic centrality of characters
#'
#' @description  This function calculates the dynamic centrality scores of
#'   characters on directed interaction data using the measure defined in Jones,
#'   Quinn and Koskinen (2020).
#'
#' @param event_list An event list containing the time-ordered interactions. The
#'   function expects a particular format wherein the first column contains the
#'   ID of the sender ID (for dialogue data, this is the speaker) and each
#'   column C represents a dummy variable for character C-1 which takes the
#'   value of 1 if character C-1 is a recipient of the interaction (for dialogue
#'   data, this means being spoken to) at time t and 0 otherwise. For example,
#'   column 2 contains the dummy variables for the character whose ID is 1, and
#'   so on.
#' @param chars An optional vector of character names to be used to label the
#'   scores matrix returned by the function (must be of equal length to the
#'   number of unique characters in the event list). If not supplied, names will
#'   be inferred from the event list.
#' @param mode This argument specifies whether to return the speaking scores
#'   ("out"), spoken-to scores ("in"), or both ("both"). In the case of “both”,
#'   a named list is returned containing first the “out” matrix and then the
#'   “in” matrix. For either “out” or “in”, a single matrix is returned. The
#'   default option is "both".
#' @param wp A value to be used for the weighting parameter lambda in the
#'   calculation of scores (see Jones, Quinn and Koskinen 2020). The default
#'   value is 0.01.
#' @param normalised Logical. When \code{normalised = TRUE} (which is the
#'   default value), the returned scores are normalised so that each character's
#'   score at time t is divided by the sum of all characters' scores, such that
#'   the sum of all character scores at time t is always equal to 1. When
#'   \code{normalised = FALSE}, absolute scores are returned.
#' @param start_at The column in the input data (as structured in the way
#'   described above) containing the sender IDs. This ensures the function
#'   always works the same way, even when the input event list contains
#'   different numbers of additional event-level variables (e.g. weight or
#'   type), so long as these columns are positioned prior to the sender ID
#'   column. The default is 1.
#'
#' @return If \code{mode = "out"} or \code{mode = "in"}, an N-by-T matrix is
#'   returned, where N is the total number of characters in the event list and T
#'   is the total number of interactions in the event list. Each cell [n,t] in
#'   this matrix represents the score of character n at time t. If \code{mode =
#'   "both"}, a named list of length 2 is returned containing first the "out"
#'   matrix and second the "in" matrix.
#'
#' @examples
#' tfa <- movienetData::starwars_01
#' tfa_scores <- narrative_centrality(tfa$event_list,
#'                                    chars = tfa[[2]]$char_name,
#'                                    wp = 0.01,
#'                                    start_at = 3)
#'
#' # What is the (normalised) speaking score of character 5 at time t=50?
#' tfa_scores$out_scores[5, 50]
#'
#' @section References: Jones, Pete, Eithne Quinn and Johan Koskinen. 2020.
#'   "Measuring centrality in film narratives using dynamic character
#'   interaction networks." \emph{Social Networks} 63: 21-37. DOI:
#'   \url{https://doi.org/10.1016/j.socnet.2020.03.003}.
#'
#' @export
narrative_centrality <- function(event_list,
                                 chars = NULL,
                                 mode = "both",
                                 wp = 0.01,
                                 normalised = TRUE,
                                 start_at = 1) {
  C_in <- matrix(1, length(chars), nrow(event_list))
  C_out <- matrix(1, length(chars), nrow(event_list))
  C_in_norm <- matrix(1, length(chars), nrow(event_list))
  C_out_norm <- matrix(1, length(chars), nrow(event_list))
  if(is.null(chars)) {
    chars <- rownames(event_list)[start_at + 1:ncol(event_list)]
  }
  rownames(C_in) <- chars
  rownames(C_in_norm) <- chars
  rownames(C_out) <- chars
  rownames(C_out_norm) <- chars
  # Compute the scores
  C_in_t <- matrix(1, length(chars), 1)
  C_out_t <- matrix(1, length(chars), 1)
  for (t in 1:nrow(event_list)) {
    speaker <- event_list[t, start_at]
    receivers <- which(event_list[t, (start_at + 1):dim(event_list)[2]] == 1)
    C_in_t[receivers] <- C_in_t[receivers] + (wp * C_in_t[speaker])
    C_out_t[speaker] <- C_out_t[speaker] +
      ((wp / length(receivers)) * sum(C_out_t[receivers]))
    for (c in 1:length(chars)) {
      C_in[c, t] <- C_in_t[c]
      C_out[c, t] <- C_out_t[c]
      C_in_norm[c, t] <- C_in_t[c] / sum(C_in_t)
      C_out_norm[c, t] <- C_out_t[c] / sum(C_out_t)
    }
  }
  # Return the requested values according to `normalised` and `mode`
  if(normalised == TRUE) {
    if(mode == "both") {
      return(list("out_scores" = C_out_norm, "in_scores" = C_in_norm))
    }
    if(mode == "in") {
      return(C_in_norm)
    }
    if(mode == "out") {
      return(C_out_norm)
    }
  } else {
    if(mode == "both") {
      return(list(C_out, C_in))
    }
    if(mode == "in") {
      return(C_in)
    }
    if(mode == "out") {
      return(C_out)
    }
  }
}
