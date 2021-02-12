#' Tag a well-formatted screenplay in PDF format and extract event list
#'
#' @description This function reads in a well-formatted screenplay (via
#'   \code{textreadr::read_pdf}), parses it and tags each line based on a series
#'   of regular expressions. The function attempts to tag each line as either a
#'   scene boundary, scene description, character name, dialogue, dialogue
#'   description, stage direction, or page information. These tags are then used
#'   to generate an event list of character interactions based on these tags. In
#'   particular, the scene boundary tags and character name tags are used to
#'   identify which characters speak within scenes.
#'
#'   In order to work, the screenplay must use consistent patterns of
#'   indentation for different components and follow certain industry standard
#'   formatting conventions for screenplays. It is unlikely to work if the
#'   screenplay is watermarked. See Agarwal et al. (2014) for more discussion of
#'   the limitations of regex-based tagging of screenplays.
#'
#' @param pdf_file File path to the screenplay PDF file.
#' @param window A numeric value specifying the context window within which to
#'   look for 'recipients' of a line of dialogue. For a given line of dialogue
#'   \code{t} spoken by \code{i}, the function will consider another character
#'   \code{j} to be a 'recipient' of \code{i}'s dialogue if \code{j} spoke
#'   within the same scene and within n=\code{window} lines of dialogue of
#'   \code{t}.
#'
#' @return A matrix containing a time-ordered multicast event list. The first
#'   column contains an event index, the second contains a scene index, the
#'   third contains the speaker ID, and the remaining columns contain dummy
#'   variables for each character indicating whether they were the 'recipient'
#'   of the line of dialogue (which is determined by whether they spoke within
#'   the same scene and within n=\code{window} lines of dialogue of the current
#'   line).
#'
#' @section References: Agarwal, Apoorv, Sriramkumar Balasubramanian, Jiehan
#'   Zheng, and Sarthak Dash. ''Parsing Screenplays for Extracting Social
#'   Networks from Movies.'' In \emph{Proceedings of the 3rd Workshop on Computational
#'   Linguistics for Literature (CLfL)}, 50-58. Gothenburg, Sweden: Association
#'   for Computational Linguistics, 2014.
#'
#' @examples
#' \dontrun{ my_pdf <- "path/to/pdf/of/screenplay.pdf"
#' my_event_list <- screenplay_to_events(my_pdf) }
#'
#' @export
screenplay_to_events <- function(pdf_file, window = 5) {
  tag_screenplay <- function(...) {
    lines <- textreadr::read_pdf(pdf_file, trim = FALSE)
    lines$tag <- vector("character", length = length(nrow(lines)))
    # Define the main regular expressions as variables
    scene_regex <- "(-|[[:space:]])*\\<(INT|EXT)[-.:]? "
    stagedir_regex <- "\\<([CO]UT|DISSOLVE|FADE|PAN) (TO|FROM|OUT|IN(TO)?)\\>( ?[A-Z]*)*[[:punct:]]+$"
    char_regex <- "( \\((O\\. ?S\\.?|V\\. ?O\\.?|CONT[[:punct:]]?(INUE)?D)\\)$)+"
    lead_ws_regex <- "^[[:space:]]*[[:punct:]...]* ?\\<"

    # Tag the scene boundaries:
    lines$tag[grep(scene_regex, lines$text, ignore.case = TRUE)] <- "S"

    # Tag the first-pass character names (only on untagged lines)
    for (i in grep(char_regex, lines$text, ignore.case = TRUE)) {
      if (lines$tag[i] == "") {
        lines$tag[i] <- "C"
      }
    }

    # Tag the regex-identifiable stage direction lines
    for (i in grep(stagedir_regex, lines$text, ignore.case = FALSE)) {
      if (lines$tag[i] == "") {
        lines$tag[i] <- "M"
      }
    }
    # Anything entirely within square brackets probably belongs in this group
    lines$tag[grepl("^[[:space:]]*\\[[^]]+\\]?$",
                    lines$text, ignore.case = TRUE) & lines$tag == ""] <- "M"
    lines$tag[grepl("^[[:space:]]*[^[]+\\]$",
                    lines$text, ignore.case = TRUE) & lines$tag == ""] <- "M"

    # Tag remaining untagged character lines based on indentation of 1st pass chars
    C_1 <- grep(char_regex, lines$text, ignore.case = TRUE, value = TRUE)
    C_dents <- unique(attr(regexpr("^[[:space:]]+[[:punct:]]?\\<", C_1),
                           "match.length"))

    for (i in which(attr(regexpr("^[[:space:]]+[[:punct:]]?\\<", lines$text),
                         "match.length") %in% C_dents)) {
      if (lines$tag[i] == "") {
        lines$tag[i] <- "C"
      }
    }

    # Tag remaining scene description lines based on indentation of 1st pass scenes
    S_1 <- grep(scene_regex, lines$text, ignore.case = TRUE, value = TRUE)
    S_dents <- unique(attr(regexpr(lead_ws_regex, S_1), "match.length"))
    # Only tag these if scenes are actually indented!!!
    if(min(S_dents) > 1) {
      for (i in which(
        attr(regexpr(lead_ws_regex, lines$text), "match.length") %in% S_dents)
      ) {
        if (lines$tag[i] == "") {
          lines$tag[i] <- "N"
        }
      }
    }

    # Tag any lines where max(S_dents) < length(whitespace) < min(C_dents) as "D"
    # First, get leading whitespace of all lines
    leading_ws <- attr(regexpr(lead_ws_regex, lines$text),
                       "match.length")

    # Identify possible "D"s based on whitespace length
    maybe_D <- which(leading_ws > max(max(S_dents), 2) &
                       leading_ws < min(C_dents) &
                       lines$tag == "")
    # Only lines after the first "C" can be dialogue
    maybe_D <- maybe_D[maybe_D > min(which(lines$tag == "C"))]
    lines$tag[maybe_D] <- "D"

    # Find those "D" lines that are entirely parenthetical and tag these "d"
    lines$tag[grepl("^[[:space:]]+\\([^//)]+\\)?$",
                    lines$text, ignore.case = TRUE) & lines$tag == "D"] <- "d"
    lines$tag[grepl("^[[:space:]]+[^\\(]+\\)$",
                    lines$text, ignore.case = TRUE) & lines$tag == "D"] <- "d"

    # No leading whitespace and ending in number should be "P" (page info)
    for (i in grep("(^[[:space:]]*.*[0-9]+[[:punct:]]?$)|©",
                   lines$text, ignore.case = FALSE)) {
      if (lines$tag[i] == "") {
        lines$tag[i] <- "P"
      }
    }

    # Everything else should be "U"
    lines$tag[which(lines$tag == "")] <- "U"

    return(as.data.frame(lines))
  }

  to_events <- function(data = lines, ...) {
    disambiguate_names <- function(...) {
      gsub(pattern = " \\(O\\. ?S\\.?\\)| ?\\(CONT(.|INUE)D\\)| \\(TOGETHER\\)| \\(AS [[:alnum:]]+\\)| \\(V\\. ?O\\.?\\)| \\([[:digit:]]+\\)| \\((ON|OFF) SCREEN\\)",
           replacement = "",
           x = gsub("^[[:space:]]+", "", data$text[which(data$tag == "C")]),
           ignore.case = TRUE)
    }

    speakers <- disambiguate_names()
    num_chars <- length(unique(speakers))

    tag_scenes <- function(...) {
      scenes <- vector("numeric", length = nrow(data))
      for (i in 1:(length(which(data$tag == "S")) - 1)) {
        scenes[seq(
          from = which(data$tag == "S")[i],
          to = which(data$tag == "S")[i + 1] - 1
        )] <- i
      }
      scenes[seq(from = max(which(data$tag == "S")),
                 to = nrow(data))] <- length(which(data$tag == "S"))
      return(scenes)
    }

    scenes <- tag_scenes(data)[which(data$tag == "C")]

    get_id <- function(char_name = "") {
      which(unique(speakers) == char_name)
    }

    events <- matrix(0, nrow = length(speakers), ncol = 3 + num_chars)
    colnames(events) <- c("eventID", "sceneID", "speakerID", unique(speakers))

    for (x in 1:length(speakers)) {
      events[x, 1] <- x
      events[x, 2] <- scenes[x]
      events[x, 3] <- get_id(speakers[x])
      for(c in unique(speakers)) {
        if(c %in% speakers[max(x - window,
                               min(which(scenes == scenes[x])),
                               1):
                           min(x + window,
                               max(which(scenes == scenes[x])),
                               length(scenes))]) {
          events[x, 3 + get_id(c)] <- 1
          events[x, 3 + events[x, 3]] <- 0 # No self-ties!
        }
      }
    }

    return(events)
  }

  lines <- tag_screenplay(pdf_file = pdf_file)
  return(to_events(data = lines))
}
