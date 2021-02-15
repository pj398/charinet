#' Generate a character by story-chunk matrix for analysing character activity
#' by story chunks.
#'
#' @description  This function calculates the number of interactions initated by
#'   each character within each story chunk. Story chunks could be scenes,
#'   chapters, or some other arbitrary division of the event list into temporal
#'   segments such as quartiles or deciles. This produces a
#'   character-by-story-chunk matrix which can be used to generate a heatmap of
#'   character activity over the course of a narrative.
#'
#'   For pre-defined story chunks (e.g. scenes or chapters), the user must
#'   specify (via \code{story_chunk_col}) a column in the event list which
#'   represents a variable indexing the chunk ID of each interaction.
#'   Alternatively, the function can automatically chunk the event list into
#'   equally-sized segments such as quartiles using the \code{n_chunks}
#'   argument.
#'
#' @param event_list An event list containing the time-ordered interactions. The
#'   function expects a particular format (described in detail elsewhere in the
#'   package documentation) wherein each row is an event, and the event list
#'   contains at least a column indicating the ID of the sending character,
#'   followed by columns containing dummy variables for each character which
#'   take the value 1 if that character is a target of the interaction, and 0
#'   otherwise.
#' @param char_names An optional vector of character names to be used to label
#'   the rows of the matrix returned by the function. This is useful when the
#'   sender ID column contains the numeric IDs of characters rather than the
#'   character names. If specified, this vector must be of equal length to the
#'   number of unique characters in the event list, and must be ordered by
#'   character ID number. If not supplied, names will be assigned based on how
#'   character names appear in the event list.
#' @param n_chunks If a numeric value is passed to this argument, the event list
#'   will be "chunked" into that many equally-sized segments. This will override
#'   the \code{story_chunk_col} argument which is for specifying user-defined
#'   story chunk variables.
#' @param story_chunk_col The position of the column in the input
#'   \code{event_list} which indexes the events by story chunk. This argument
#'   will be ignored unless \code{n_chunks = NULL}.
#' @param from The position of the column in the input \code{event_list} which
#'   indexes the event by the sender ID (e.g. for dialogue data, this will be
#'   the speaker ID column).
#'
#' @return A matrix is returned, with each row corresponding to a character and
#'   each column corresponding to a story chunk. The value of cell [i, j] is the
#'   number of interactions sent by character \emph{i} in chunk \emph{j}.
#'
#' @examples
#' tfa <- movienetdata::starwars_01
#' my_heatmap <- story_heatmap(event_list = tfa$event_list,
#'                             char_names = tfa$node_list$char_name,
#'                             story_chunk_col = 2,
#'                             from = 3)
#' # How many times does character 5 speak in scene 6?
#' my_heatmap[5, 6]
#'
#' @export
story_heatmap <- function(event_list,
                          char_names = NULL,
                          n_chunks = NULL,
                          story_chunk_col = NULL,
                          from = 3) {

  n_events <- nrow(event_list)

  if(!is.null(n_chunks)) {

    heatmat <- matrix(0,
                      nrow = length(unique(event_list[ , from])),
                      ncol = n_chunks)

    chunk_id <- vector(mode = "numeric", length = n_events)

    for (i in 1:n_events) {
      for (chunk in 1:n_chunks) {
        if(i <= (n_events / n_chunks) * chunk &
           i > ((n_events / n_chunks) * (chunk - 1))) {
          chunk_id[i] <- chunk
        }
      }
    }

    for (c in unique(event_list[ , from])) {
      for (scene in unique(chunk_id)) {
        heatmat[which(unique(event_list[ , from]) == c),
                which(unique(chunk_id) == scene)] <- length(
                  which(chunk_id == scene &
                          event_list[ , from] == c))
      }
    }
  } else {

    if(is.null(story_chunk_col)) {
      stop("Neither `n_chunks` nor `story_chunk_col` has been specified.")
    }

    heatmat <- matrix(0,
                      nrow = length(unique(event_list[ , from])),
                      ncol = length(unique(event_list[ , story_chunk_col])))

    for (c in unique(event_list[ , from])) {
      for (scene in unique(event_list[ , story_chunk_col])) {
        heatmat[which(unique(event_list[ , from]) %in% c),
                which(unique(event_list[ , story_chunk_col]) == scene)] <- length(
                  which(event_list[ , story_chunk_col] == scene &
                          event_list[ , from] == c))
      }
    }
  }

  if(is.null(char_names)) {
    if(is.numeric(unique(event_list[ , from]))) {
      char_names <- colnames(event_list)[(from + 1):ncol(event_list)]
      rownames(heatmat) <- char_names[unique(event_list[ , from])]
    } else {
      rownames(heatmat) <- unique(event_list[ , from])
    }
  } else {
    if(is.numeric(unique(event_list[ , from]))) {
      rownames(heatmat) <- char_names[unique(event_list[ , from])]
    } else {
      rownames(heatmat) <- unique(event_list[ , from])
      message("A vector of character names was passed to `char_names` but the
            values in the `from` column of `event_list` do not correspond
            to numeric character IDs. As such, the `char_names` vector has been
            ignored and row names have been assigned based on how the characters
            appear in the `from` column of `event_list`.")
      }
    }
  colnames(heatmat) <- paste("chunk", seq.int(1:ncol(heatmat)), sep = "")

  return(heatmat)
}

#' Plot heatmap using ggplot2
#'
#' @description Create a quick heatmap visualisation of character activity by
#'   story chunks using ggplot2.
#'
#' @param input_heatmap The character-by-chunk heatmap matrix produced by the
#'   \code{story_heatmap} function.
#' @param cutoff A numeric value to be used to filter the visualisation to only
#'   include those characters who speak more than \code{cutoff} number of lines.
#'
#' @return A ggplot2 plot.
#'
#' @examples
#' tfa <- movienetdata::starwars_01
#' story_heatmap(tfa$event_list,
#'               char_names = tfa$node_list$char_name,
#'               n_chunks = 4) %>%
#'   plot_heatmap(cutoff = 3)
#'
#' @export
plot_heatmap <- function(input_heatmap, cutoff = 0) {
  charinet::check_sugs(c("dplyr", "ggplot2", "magrittr", "tibble", "tidyr"))

  tidied_hm <- tibble::as_tibble(input_heatmap, rownames = "Character") %>%
    tidyr::pivot_longer(cols = !Character,
                        names_to = "Chunk",
                        names_prefix = "chunk",
                        values_to = "Activity") %>%
    dplyr::mutate(Chunk = readr::parse_integer(.data$Chunk)) %>%
    dplyr::relocate(Chunk, .before = dplyr::everything()) %>%
    dplyr::arrange(Chunk)

  n_chunks <- length(unique(tidied_hm$Chunk))

  tidied_hm %>%
    dplyr::group_by(Character) %>%
    dplyr::mutate(nlines = sum(Activity)) %>%
    dplyr::filter(nlines > cutoff) %>%
    ggplot2::ggplot(ggplot2::aes(x = .data$Chunk,
               y = stats::reorder(.data$Character,
                                  dplyr::desc(.data$Character)))) +
    ggplot2::geom_tile(ggplot2::aes(fill = .data$Activity), colour = "#FFFFFF") +
    ggplot2::scale_fill_gradient(low = "#f7f7fc", high = "#726d9c") +
    ggplot2::scale_x_continuous(name = "Story chunk", expand = c(0.005, 0),
                                n.breaks = n_chunks) +
    ggplot2::scale_y_discrete(name = "Character",
                              limits = rev(levels(.data$Character))) +
    ggplot2::theme_light() +
    ggplot2::theme(legend.position = "none",
                   axis.ticks = ggplot2::element_blank(),
                   axis.text.x = ggplot2::element_text(
                     angle = ifelse(n_chunks > 25, 90, 0)
                     ),
                   panel.border = ggplot2::element_blank(),
                   panel.grid.major.y = ggplot2::element_blank(),
                   panel.grid.major.x = ggplot2::element_blank())
}
