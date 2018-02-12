

ifps <- read_excel("test-ifps.xlsx")

make_request <- function(question, ifp_list = ifps) {
  ifp <- ifp_list %>% filter(`discover question id`==question)
  
  seps <- ifp$`answer name` %>% 
    str_extract_all(., "[-0-9\\.,]+") %>%
    sapply(., paste, collapse = "-") %>%
    str_replace_all(., ",", "")
  
  out <- list(
    payload = list(
      separations = list(
        values = seps,
        original_options = ifp$`answer name`
      ),
      historical_data = list(
        ts = data.frame(NULL)
      ),
      `last-event-date` = NA
    ),
    ifp = list(
      name = unique(ifp$`question name`),
      starts_at = unique(ifp$`question starts`),
      ends_at = unique(ifp$`question ends at`),
      discover_answer_id = ifp$`discover answer id`,
      options = data.frame(name = ifp$`answer name`),
      discover_question_id = unique(ifp$`discover question id`),
      `binary?` = as.logical(length(ifp$`answer name`)==1)
    )
  )
  out
}