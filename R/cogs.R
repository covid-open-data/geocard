#' @export
get_cogs <- function(x, pop) {
  chk <- function(x)
    if (length(x) == 0 || is.nan(x) || is.infinite(x)) NA else x
  get_new <- function(cur, prev)
    cur - ifelse(is.na(prev), 0, prev)

  ref_source <- levels(x$source)[1]

  each_cog <- lapply(levels(x$source), function(a) {
    id <- tolower(a)
    b <- dplyr::filter(x, source == !!a) %>% dplyr::arrange(date)
    last <- tail(b, 1)
    last2 <- head(tail(b, 2), ifelse(nrow(b) == 1, 0, 1))

    tibble(
      !!paste0("cur_case_", id) := cog(chk(last$cases),
        desc = paste0("Total cases (", a, ")")),
      !!paste0("cur_death_", id) := cog(chk(last$deaths),
        desc = paste0("Total deaths (", a, ")")),
      !!paste0("prev_case_", id) := cog(chk(last2$cases),
        desc = paste0("Prior day cases (", a, ")")),
      !!paste0("prev_death_", id) := cog(chk(last2$deaths),
        desc = paste0("Prior day deaths (", a, ")")),
      !!paste0("new_case_", id) := cog(chk(get_new(last$cases, last2$cases)),
        desc = paste0("New cases (", a, ")")),
      !!paste0("new_death_", id) := cog(chk(get_new(last$deaths, last2$deaths)),
        desc = paste0("New deaths (", a, ")")),
    )
  })
  names(each_cog) <- levels(x$source)


  idx <- which(levels(x$source) == ref_source)
  ref_id <- tolower(ref_source)
  ref_cog <- each_cog[[idx]]
  rcc <- ref_cog[[paste0("cur_case_", ref_id)]]
  rcd <- ref_cog[[paste0("cur_death_", ref_id)]]
  rpc <- ref_cog[[paste0("prev_case_", ref_id)]]
  rpd <- ref_cog[[paste0("prev_death_", ref_id)]]
  rnc <- ref_cog[[paste0("new_case_", ref_id)]]
  rnd <- ref_cog[[paste0("new_death_", ref_id)]]

  diff_cog <- lapply(levels(x$source)[-idx], function(a) {
    id <- tolower(a)
    cc <- each_cog[[a]][[paste0("cur_case_", id)]]
    cd <- each_cog[[a]][[paste0("cur_death_", id)]]
    tibble(
      !!paste0("case_abs_diff_", id) := cog(abs(rcc - cc),
        desc = paste0("Absolute difference between ",
          a, " and ", ref_source, " cases")),
      !!paste0("death_abs_diff_", id) := cog(abs(rcd - cd),
        desc = paste0("Absolute difference between ",
          a, " and ", ref_source, " deaths"))
    )
  })

  n_reps <- x %>%
    filter(cases > 0) %>%
    group_by(source) %>%
    tally() %>%
    pull(n)
  days_since_first_case <- max(n_reps)
  new_entity <- !any(n_reps > 1)

  b <- x %>%
    dplyr::filter(source == ref_source) %>%
    dplyr::arrange(date) %>%
    tail(15)

  wk_stats <- b %>%
    summarise(
      cases = ifelse(dplyr::n() < 15 || (cases[8] - cases[1]) == 0, NA,
        round(100 * ((cases[15] - cases[8]) - (cases[8] - cases[1])) /
          (cases[8] - cases[1]), 1)),
      deaths = ifelse(dplyr::n() < 15 || (deaths[8] - deaths[1]) == 0, NA,
        100 * round(((deaths[15] - deaths[8]) - (deaths[8] - deaths[1])) /
          (deaths[8] - deaths[1]), 1)))

  last3 <- head(tail(b, 3), ifelse(nrow(b) <= 2, 0, 1))
  rnpc <- chk(get_new(rpc, last3$cases))
  rnpd <- chk(get_new(rpd, last3$deaths))

  pct <- function(val)
    round(100 * val, 1)

  extra_cog <- tibble(
    case_increase_pct = cog(ifelse(rpc == 0, NA, pct(rnc / rpc)),
      desc = paste0("% increase in cases (", ref_source, ")"),
      type = "numeric"),
    death_increase_pct = cog(ifelse(rpd == 0, NA, pct(rnd / rpd)),
      desc = paste0("% increase in deaths (", ref_source, ")"),
      type = "numeric"),
    new_case_change_pct = cog(ifelse(rnpc == 0, NA, pct((rnc - rnpc) / rnpc)),
      desc = paste0("% day-to-day change in new cases (", ref_source, ")"),
      type = "numeric"),
    new_death_change_pct = cog(ifelse(rnpd == 0, NA, pct((rnd - rnpd) / rnpd)),
      desc = paste0("% day-to-day change in new deaths (", ref_source, ")"),
      type = "numeric"),
    new_wk_case_change_pct = cog(wk_stats$cases,
      desc = paste0("% week-to-week change in new cases (", ref_source, ")"),
      type = "numeric"),
    new_wk_death_change_pct = cog(wk_stats$deaths,
      desc = paste0("% week-to-week change in new deaths (", ref_source, ")"),
      type = "numeric"),
    case_fatality_pct = cog(chk(pct(rcd / rcc)),
      desc = paste0("% case fatality (", ref_source, ")"),
      type = "numeric"),
    attack_rate = cog(chk(round(rcc / pop * 100000, 1)),
      desc = paste0("Cases / population per 100k (", ref_source, ")"),
      type = "numeric"),
    new_entity = cog(ifelse(new_entity, "yes", "no"),
      desc = "New geographic entity", group = "general"),
    days_since_first_case = cog(days_since_first_case,
      desc = "Days frome case 1", group = "general")
  )

  # unlist(c(each_cog, diff_cog, list(extra_cog)) %>% bind_cols())

  c(each_cog, diff_cog, list(extra_cog)) %>% bind_cols()
}
