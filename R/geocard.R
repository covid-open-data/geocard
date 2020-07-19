#' Create a geo card
#'
#' @param data TODO
#' @param card_name TODO
#' @param cog TODO
#' @param population TODO
#' @param ref_source TODO
#' @param y_domain TODO
#' @param y_log_domain TODO
#' @param case_fatality_max TODO
#' @param max_date TODO
#' @param min_date TODO
#' @param img_url TODO
#' @param feed_url TODO
#' @param twitter_account TODO
#' @param facebook_account TODO
#' @param width TODO
#' @param height TODO
#' @param element_id TODO
#'
#' @import htmlwidgets
#' @importFrom htmltools tags div tagList
#' @importFrom stats IQR
#' @importFrom jsonlite toJSON
#' @importFrom ggthemes tableau_color_pal
#' @importFrom stats quantile
#' @importFrom tidyselect all_of
#'
#' @examples
#' geocard( 
#'   wa_cases, 
#'   card_name = "Washington", 
#'   population = 7549403, 
#'   ref_source = "NYT", 
#'   img_url = "https://raw.githubusercontent.com/hafen/us-locator-maps/master/thumbs/admin1/US/53.png" 
#' )
#' @export
geocard <- function(
  data,
  card_name,
  cog = NULL,
  population = NA,
  ref_source = NULL,
  y_domain = NULL,
  y_log_domain = NULL,
  case_fatality_max = 25,
  max_date = NULL,
  min_date = NULL,
  img_url = NULL,
  feed_url = NULL,
  twitter_account = NULL,
  facebook_account = NULL,
  width = NULL,
  height = NULL,
  element_id = NULL
) {
  chk <- function(x)
    if (length(x) == 0 || is.nan(x) || is.infinite(x)) NA else x
  get_new <- function(cur, prev)
    cur - ifelse(is.na(prev), 0, prev)

  x <- data
  cg <- cog
  pop <- population
  if (is.null(cog)) {
    message("Cognostics not supplied. Calculating...")
    cg <- get_cogs(x, pop)
  }
  ref_id <- tolower(ref_source)
  srcs <- levels(x$source)

  if (is.null(max_date))
    max_date <- max(x$date)
  max_date <- as.Date(max_date)
  if (is.null(min_date))
    min_date <- max_date - (8 * 7)

  cur_date_str <- format(max_date, "%b%d")
  prev_date_str <- format(max_date - 1, "%b%d")

  flag <- ""
  if (!is.null(img_url))
    flag <- tags$img(src = img_url,
      alt = "flag", height = "35")

  # if ("map_url" %in% names(data))
  #   names(data)[names(data) == "map_url"] <- "flag_url"
  # if ("flag_url" %in% names(data)) {
  #   flag <- tags$div(class = "no-flag")
  #   if (!is.na(data$flag_url))
  #     flag <- tags$img(src = data$flag_url,
  #       alt = "flag", height = "35")
  # }

  cur_case_ref <- cg[[paste0("cur_case_", ref_id)]]
  new_case_ref <- cg[[paste0("new_case_", ref_id)]]
  cur_death_ref <- cg[[paste0("cur_death_", ref_id)]]
  new_death_ref <- cg[[paste0("new_death_", ref_id)]]
  prev_case_ref <- cg[[paste0("prev_case_", ref_id)]]
  prev_death_ref <- cg[[paste0("prev_death_", ref_id)]]

  new_entity <- cg$new_entity

  plot_id <- tolower(gsub(" ", "", gsub("[^[:alpha:]]", "_", card_name)))

  new_entity_div <- NULL
  if (length(new_entity) == 1 && new_entity == "yes")
    new_entity_div <- tags$div(class = "new_entity", "new entity")

  get_ref_tags <- function(lst, class) {
    if (length(lst) == 0)
      return(NULL)
    tagList(lapply(lst, function(x) {
      tags$a(
        class = "out-link",
        href = x,
        target = "_blank",
        tags$span(class = class)
      )
    }))
  }

  moh <- NULL
  if ("feed_url" %in% names(data)) {
    moh <- data$feed_url[[1]]
    moh <- get_ref_tags(moh, "icon-feed")
  }

  twitter <- NULL
  if ("twitter_account" %in% names(data)) {
    twitter <- data$twitter_account[[1]]
    twitter <- get_ref_tags(twitter, "icon-twitter")
  }

  facebook <- NULL
  if ("facebook_account" %in% names(data)) {
    facebook <- data$facebook_account[[1]]
    facebook <- get_ref_tags(facebook, "icon-facebook")
  }

  hide_links <- FALSE
  if (is.null(moh) && is.null(twitter) && is.null(facebook))
    hide_links <- TRUE

  tmp <- get_ts_data(data, min_date)
  pdat <- tmp$pdat
  wpdat <- tmp$wpdat

  get_max_outl <- function(x) {
    iqr <- stats::IQR(x, na.rm = TRUE)
    iqr <- ifelse(iqr == 0, 1, iqr)
    cutoff <- stats::quantile(x, 0.95, na.rm = TRUE) + 8 * iqr
    if (length(x) == 0)
      return(NA)
    if (length(x[x < cutoff]) == 0)
      return(max(x, na.rm = TRUE))
    max(x[x < cutoff], na.rm = TRUE)
  }

  svars <- c("cases", "deaths", "new_cases", "new_deaths")

  maxes <- pdat %>%
    dplyr::ungroup() %>%
    dplyr::select(tidyselect::all_of(svars)) %>%
    dplyr::summarise_all(get_max_outl) %>%
    as.list()
  maxes$case_fatality_pct <- case_fatality_max

  wmaxes <- wpdat %>%
    dplyr::ungroup() %>%
    dplyr::select(tidyselect::all_of(svars)) %>%
    dplyr::summarise_all(get_max_outl) %>%
    as.list()
  wmaxes$case_fatality_pct <- case_fatality_max

  # max_dt <- Sys.time()
  # max_dt2 <- format(max_dt, "%Y-%m-%dT%H:%M")
  # max_date <- max(pdat$date)
  # idx <- which(pdat$date == max_date)
  # pdat$date <- as.POSIXct(pdat$date + 1, tz = "UTC") - 1
  # pdat$date[idx] <- max_dt
  # pdat$date <- format(pdat$date, "%Y-%m-%dT%H:%M")

  # wpdat$date <- as.POSIXct(wpdat$date + 1, tz = "UTC") - 1
  # wpdat$date <- format(wpdat$date, "%Y-%m-%dT%H:%M")

  vega_spec <- jsonlite::read_json(
    system.file("spec.json", package = "geocard"),
    simplifyVector = FALSE, simplifyDataFrame = FALSE)

  cols <- ggthemes::tableau_color_pal()(length(srcs))
  names(cols) <- srcs

  # update spec to only show legend and tooltip for series we have data for
  pnms <- unique(as.character(pdat$source))
  ttp <- lapply(pnms, function(nm)
    list(field = nm, type = "quantitative", format = ","))
  vega_spec$layer[[2]]$encoding$tooltip <- c(
    vega_spec$layer[[2]]$encoding$tooltip,
    ttp)
  vega_spec$layer[[1]]$encoding$color$scale$domain <- I(pnms)
  vega_spec$layer[[1]]$encoding$color$scale$range <- I(unname(cols[pnms]))

  if (is.null(y_domain))
    y_domain <- c(0, maxes$cases)

  vega_spec$encoding$x$axis$values <- max_date - 2 - (rep(0:7) * 7)
  # vega_spec$encoding$x$scale$domain <- c(min_date, max_date + 1)
  vega_spec$encoding$x$scale$domain <- c(min_date, max_date) + 1
  vega_spec$layer[[1]]$encoding$y$scale$domain <- y_domain

  other_sources <- setdiff(srcs, ref_source)

  kosovo_text <- ""
  if (grepl("Kosovo", card_name)) {
    card_name <- "Kosovo\u00b9"
    kosovo_text <- "\u00b9In the context of the United Nations Security Council resolution 1244 (1999)"
  }

  na_dash <- function(a)
    ifelse(is.na(a), "-", a)

  fmt <- function(a)
    format(a, big.mark = ",")

  down_icon <- tags$span(class = "icon-arrow-thin-down")
  up_icon <- tags$span(class = "icon-arrow-thin-up")

  get_icon_color <- function(val) {
    if (is.na(val) || val == 0) {
      icon <- tags$span()
      color <- "gray"
    } else if (val < 0) {
      icon <- down_icon
      color <- "green"
    } else {
      icon <- up_icon
      color <- "red"
    }
    list(
      icon = icon,
      color = color
    )
  }

  # Total   New   New    Day   Week   Per 100k
  # May10 May10 May09 Change Change Population

  obj <- tags$div(class = "container",
    new_entity_div,
    tags$div(class = "entity_name", card_name),
    tags$div(class = "flag",
      flag
    ),
    tags$table(class = "data-table",
      tags$tr(class = "data-row data-row-header",
        tags$td(class = "data-cell dc-1"),
        tags$td(class = "data-cell dc-2", "Total",
          tags$br(),
          tags$span(class = "header-date", cur_date_str)),
        tags$td(class = "data-cell dc-3", "New",
          tags$br(),
          tags$span(class = "header-date", prev_date_str)),
        tags$td(class = "data-cell dc-4", "New",
          tags$br(),
          tags$span(class = "header-date", cur_date_str)),
        tags$td(class = "data-cell dc-5", "Day",
          tags$br(),
          tags$span(class = "header-date", "Change")),
        tags$td(class = "data-cell dc-6", "Week",
          tags$br(),
          tags$span(class = "header-date", "Change")),
        tags$td(class = "data-cell dc-7", "Per 100k",
          tags$br(),
          tags$span(class = "header-date", "Population")),
      ),
      lapply(seq_along(srcs), function(ii) {
        src <- srcs[ii]
        lsrc <- tolower(src)

        b <- x %>%
          dplyr::filter(.data$source == src) %>%
          dplyr::arrange(.data$date) %>%
          tail(15)

        wk_stats <- b %>%
          summarise(
            cases = ifelse(dplyr::n() < 15 || (.data$cases[8] - .data$cases[1]) == 0, NA,
              100 * ((.data$cases[15] - .data$cases[8]) - (.data$cases[8] - .data$cases[1])) /
                (.data$cases[8] - .data$cases[1])),
            deaths = ifelse(dplyr::n() < 15 || (.data$deaths[8] - .data$deaths[1]) == 0, NA,
              100 * ((.data$deaths[15] - .data$deaths[8]) - (.data$deaths[8] - .data$deaths[1])) /
                (.data$deaths[8] - .data$deaths[1])))

        cur_cases <- as.numeric(cg[[paste0("cur_case_", lsrc)]])
        cur_deaths <- as.numeric(cg[[paste0("cur_death_", lsrc)]])
        new_cases <- as.numeric(cg[[paste0("new_case_", lsrc)]])
        new_deaths <- as.numeric(cg[[paste0("new_death_", lsrc)]])
        prev_cases <- as.numeric(cg[[paste0("prev_case_", lsrc)]])
        prev_deaths <- as.numeric(cg[[paste0("prev_death_", lsrc)]])

        last3 <- head(tail(b, 3), ifelse(nrow(b) <= 2, 0, 1))
        rnpc <- chk(get_new(prev_cases, last3$cases))
        rnpd <- chk(get_new(prev_deaths, last3$deaths))
        case_change_pct <- ifelse(rnpc == 0, NA,
          100 * (new_cases - rnpc) / rnpc)
        death_change_pct <- ifelse(rnpd == 0, NA,
          100 * (new_deaths - rnpd) / rnpd)

        case_ic <- get_icon_color(case_change_pct)
        death_ic <- get_icon_color(death_change_pct)
        case_wk_ic <- get_icon_color(wk_stats$cases)
        death_wk_ic <- get_icon_color(wk_stats$deaths)

        row_class <- paste0("data-row data-row-data data-row-", src,
          ifelse(ii == 1, "", " hidden"))

        list(
          tags$tr(class = row_class,
            tags$td(class = "data-cell dc-1",
              "Cases", tags$span(class = "icon-aid-kit")),
            tags$td(class = "data-cell dc-2",
              na_dash(fmt(cur_cases))),
            tags$td(class = "data-cell dc-3",
              na_dash(fmt(rnpc))),
            tags$td(class = "data-cell dc-4",
              na_dash(fmt(new_cases))),
            tags$td(class = paste("data-cell dc-5", case_ic$color),
              case_ic$icon,
              ifelse(is.na(case_change_pct), "-",
                sprintf("%d%%", abs(round(case_change_pct, 0))))
            ),
            tags$td(class = paste("data-cell dc-6", case_wk_ic$color),
              case_wk_ic$icon,
              ifelse(is.na(wk_stats$cases), "-",
                sprintf("%d%%", abs(round(wk_stats$cases, 0))))
            ),
            tags$td(class = "data-cell dc-4",
              na_dash(round(100000 * cur_cases / pop, 0)))
          ),
          tags$tr(class = row_class,
            tags$td(class = "data-cell dc-1",
              "Deaths", tags$span(class = "icon-user-x")),
            tags$td(class = "data-cell dc-2",
              na_dash(fmt(cur_deaths))),
            tags$td(class = "data-cell dc-3",
              na_dash(fmt(rnpd))),
            tags$td(class = "data-cell dc-4",
              na_dash(fmt(new_deaths))),
            tags$td(class = paste("data-cell dc-5", death_ic$color),
              death_ic$icon,
              ifelse(is.na(death_change_pct), "-",
                sprintf("%d%%", abs(round(death_change_pct, 0))))
            ),
            tags$td(class = paste("data-cell dc-6", death_wk_ic$color),
              death_wk_ic$icon,
              ifelse(is.na(wk_stats$deaths), "-",
                sprintf("%d%%", abs(round(wk_stats$deaths, 0))))
            ),
            tags$td(class = "data-cell dc-4",
              na_dash(round(100000 * cur_deaths / pop, 0)))
          )
        )
      })
    ),
    tags$div(class = "plot-image", id = paste0("plot-", plot_id),
      style = "height: 250px; width: 500px;"),
    tags$select(
      class = "yvar-selector",
      tags$option(value = "cases", "cases"),
      tags$option(value = "deaths", "deaths"),
      tags$option(value = "new_cases", "new_cases"),
      tags$option(value = "new_deaths", "new_deaths"),
      tags$option(value = "case_fatality_pct", "case_fatality_pct")
    ),
    tags$select(
      class = "agg-selector",
      tags$option(value = "daily", "daily"),
      tags$option(value = "weekly", "weekly")
    ),
    tags$select(
      class = "yax-selector",
      tags$option(value = "linear", "free linear axis"),
      tags$option(value = "log", "fixed log axis")
    ),
    tags$select(
      class = "hdvar-selector",
      lapply(srcs, function(a) tags$option(value = a, a))
    ),
    tags$div(class = paste0("ref-links", ifelse(hide_links, " hidden", "")),
      "Official links: ",
      moh, twitter, facebook
    ),
    tags$div(class = "kosovo-text", kosovo_text)
  )

  # style <- htmlDependency(
  #   "style", "1.0", normalizePath("deps/style"),
  #   stylesheet = "style.css"
  # )
  # icomoon <- htmlDependency(
  #   "icons", "1.1", normalizePath("deps/fonts"),
  #   stylesheet = "fonts.css"
  # )
  # obj2 <- attachDependencies(obj, list(style, icomoon))

  aspect <- 415 / 500

  if (is.character(height) || is.character(width))
    stop("'height' and 'width' must be specified as integers")

  if (is.null(height) && !is.null(width))
    height <- width * aspect

  if (!is.null(height) && is.null(width))
    width <- height / aspect

  # create widget
  htmlwidgets::createWidget(
    name = "geocard",
    list(
      html = as.character(obj),
      spec = vega_spec,
      plot_id = paste0("plot-", plot_id),
      maxes = maxes,
      dat = jsonlite::toJSON(pdat),
      wdat = jsonlite::toJSON(wpdat),
      wmaxes = wmaxes,
      y_log_domain = y_log_domain
    ),
    width = width,
    height = height,
    package = "geocard",
    elementId = element_id,
    sizingPolicy = htmlwidgets::sizingPolicy(
      defaultWidth = 500,
      defaultHeight = 415
    )
  )
}

get_ts_data <- function(data, min_date) {
  cnms <- setdiff(names(data), c("source_url"))

  pdat <- data %>%
    dplyr::select(one_of(cnms)) %>%
    dplyr::group_by(.data$source) %>%
    dplyr::mutate(
      new_cases = c(NA, diff(.data$cases)),
      new_deaths = c(NA, diff(.data$deaths)),
      new_cases = ifelse(.data$new_cases < 0, 0, .data$new_cases),
      new_deaths = ifelse(.data$new_deaths < 0, 0, .data$new_deaths),
      case_fatality_pct = ifelse(.data$cases == 0, 0, 100 * .data$deaths / .data$cases)
    ) %>%
    # dplyr::filter(date >= min(date[cases > 0])) %>%
    dplyr::mutate(date = date + 1)

  wpdat <- pdat %>%
    dplyr::group_by(.data$source) %>%
    # mutate(ind = tail(c(rep(1:(ceiling(n() / 7)), each = 7), 0), n())) %>%
    dplyr::mutate(ind = tail(rep(1:(ceiling(dplyr::n() / 7)), each = 7),
      dplyr::n())) %>%
    dplyr::group_by(.data$source, .data$ind) %>%
    dplyr::summarise(
      date = tail(.data$date, 1),
      cases = tail(.data$cases, 1),
      deaths = tail(.data$deaths, 1),
      new_cases = sum(.data$new_cases),
      new_deaths = sum(.data$new_deaths),
      case_fatality_pct = ifelse(.data$cases == 0, 0, 100 * .data$deaths / .data$cases),
      n = dplyr::n()
    ) %>%
    dplyr::filter(.data$n == 7) %>%
    dplyr::select(-tidyselect::one_of("n"))

  pdat <- dplyr::filter(pdat, date >= min_date)
  wpdat <- dplyr::filter(wpdat, date >= min_date)

  list(pdat = pdat, wpdat = wpdat)
}
