
get_qpx_flights <-
  function(origin, destination,
           depart_date, return_date, max_price,
           max_connection_duration,
           earliest_time = "06:00", latest_time = "23:00") {

    qpx_key <- get_qpx_key()

    qpx_url <- get_qpx_url(qpx_key)

    flight_query <-
      get_flight_query(
        origin, destination,
        depart_date, return_date, max_price,
        max_connection_duration, earliest_time, latest_time
      )

    res <-
      httr::POST(url = qpx_url, body = flight_query,
                 encode = "json",
                 config = httr::config(httr::content_type_json())
      )

    flights_text <- httr::content(res, as = "text")

    flights_list <-
      jsonlite::fromJSON(flights_text, simplifyVector = FALSE)

    saveRDS(
      flights_list,
      paste0(
        tolower(origin), "_", tolower(destination),
        format(as.Date(depart_date), "%d%m%y"), "_",
        format(as.Date(return_date), "%d%m%y"), "_",
        format(Sys.Date(), "%d%m%y"),
        ".rds"
      )
    )

    # each
    # flights_list[["trips"]][["tripOption"]][[IDX]] contains all data that needs to be parsed into a df
    flights_list
  }
