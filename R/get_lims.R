#' @export
get_lims <- function(d) {
  if (!inherits(d, "casecounts"))
    stop("Data must be of class 'casecounts'.")

  tmp <- dplyr::bind_rows(lapply(seq_len(nrow(d)), function(i) {
    d$data[[i]]$idx <- i
    d$data[[i]]
  }))

  pdat <- tmp %>%
    dplyr::group_by(source, idx) %>%
    dplyr::mutate(
      new_cases = c(cases[1], diff(cases)),
      new_deaths = c(deaths[1], diff(deaths)),
      new_cases = ifelse(new_cases < 0, 0, new_cases),
      new_deaths = ifelse(new_deaths < 0, 0, new_deaths),
      case_fatality_pct = ifelse(cases == 0, 0, 100 * deaths / cases)
    ) %>%
    dplyr::filter(date >= min(date[cases > 0]))

  nms <- c("cases", "deaths", "new_cases", "new_deaths", "case_fatality_pct")

  lims <- list(daily = list(), weekly = list())

  lims$daily$min <- pdat %>%
    dplyr::ungroup() %>%
    dplyr::select(tidyselect::one_of(nms)) %>%
    dplyr::summarise_all(function(x) max(c(min(x), 1))) %>%
    as.list()

  lims$daily$max <- pdat %>%
    dplyr::ungroup() %>%
    dplyr::select(tidyselect::one_of(nms)) %>%
    dplyr::summarise_all(max) %>%
    as.list()

  wpdat <- pdat %>%
    dplyr::group_by(source, idx) %>%
    dplyr::mutate(
      ind = tail(c(rep(1:(ceiling(n() / 7)), each = 7), 0), n())) %>%
    dplyr::group_by(source, ind, idx) %>%
    dplyr::summarise(
      date = tail(date, 1),
      cases = tail(cases, 1),
      deaths = tail(deaths, 1),
      new_cases = sum(new_cases),
      new_deaths = sum(new_deaths),
      case_fatality_pct = ifelse(cases == 0, 0, 100 * deaths / cases),
      n = n()
    ) %>%
    dplyr::filter(n == 7) %>%
    dplyr::select(-n)

  lims$weekly$min <- wpdat %>%
    dplyr::ungroup() %>%
    dplyr::select(tidyselect::one_of(nms)) %>%
    dplyr::summarise_all(function(x) max(c(min(x), 1))) %>%
    as.list()

  lims$weekly$max <- wpdat %>%
    dplyr::ungroup() %>%
    dplyr::select(tidyselect::one_of(nms)) %>%
    dplyr::summarise_all(max) %>%
    as.list()

  lims
}
