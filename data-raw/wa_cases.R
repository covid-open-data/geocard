sources <- list(
  list(source_id = "JHU", admin_level = 1,
    file = "https://raw.githubusercontent.com/covid-open-data/xform-casecount-us-jhu/master/output/admin1/US.csv"),
  list(source_id = "NYT", admin_level = 1,
    file = "https://raw.githubusercontent.com/covid-open-data/xform-casecount-us-nytimes/master/output/admin1/US.csv"),
  list(source_id = "FACTS", admin_level = 1,
    file = "https://raw.githubusercontent.com/covid-open-data/xform-casecount-us-usafacts/master/output/admin1/US.csv")
)

src <- sources[[1]]
wa_cases <- lapply(sources, function(src) {
  readr::read_csv(src$file) %>%
    dplyr::filter(admin1_code == "53") %>%
    dplyr::select(!dplyr::contains("code")) %>%
    dplyr::mutate(source = src$source_id)
}) %>%
  dplyr::bind_rows()

wa_cases$source <- factor(wa_cases$source)

geocard(wa_cases, "Washington", min_date = min(wa_cases$date), ref_source = "NYT")

usethis::use_data(wa_cases, overwrite = TRUE)
