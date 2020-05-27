test_that("geocard works", {
  geocard( 
    wa_cases, 
    card_name = "Washington", 
    population = 7549403, 
    ref_source = "NYT", 
    img_url = "https://raw.githubusercontent.com/hafen/us-locator-maps/master/thumbs/admin1/US/53.png" 
  )
})
