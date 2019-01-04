#' Calculate important sentences using the Luhn method
#'
#' Available package/palette combinations are available in the data.frame
#' \code{\link[paletteer]{palettes_d_names}}. Both `package` and `palette`
#' can be supplied as symbols or strings.
#'
#' @param text A string of length 1 to be summarized.
#' @param n_score_words Number of words to be considered when scoring. Defaults to 4.
#' @param stopwords A string of words to be ignored when scoring sentences.
#' @return A tibble with 2 columns, sentence and score.
#' @export
recap_luhn <- function(text, n_score_words = 4, stopwords = stopwords::stopwords()) {
  word_scores <- tokenizers::tokenize_word_stems(text, stopwords = stopwords) %>%
    table() %>%
    sort(decreasing = TRUE) %>%
    magrittr::extract(seq_len(n_score_words)) %>%
    names()

  sentences <- tokenizers::tokenize_sentences(text, simplify = TRUE)

  sentence_scores <- sentences %>%
    tokenizers::tokenize_word_stems() %>%
    purrr::map_int(~ sum(.x %in% word_scores))

  setence_id <- sort(sentence_scores, index.return = TRUE, decreasing = TRUE)

  tibble::tibble(
    sentence = sentences[setence_id$ix],
    score = setence_id$x
  )
}
