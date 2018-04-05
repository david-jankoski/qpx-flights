
# return qpx api key stored in .Renviron in workin dir
get_qpx_key <- function(account = NULL) {

  ifelse(
    is.null(account),
    Sys.getenv("QPX_API_KEY"),
    Sys.getenv( paste0(account, "_", "QPX_API_KEY") )
  )

}

# construct the qpx endpoint
get_qpx_url <- function(qpx_key) {

  paste0(
    "https://www.googleapis.com/qpxExpress/v1/trips/search?key=",
    qpx_key,
    "&alt=json"
  )
}

# construct the input flight query
get_flight_query <-
  function(
    origin, destination,
    depart_date, return_date, max_price,
    max_connection_duration, earliest_time, latest_time) {

    slice <- list()

    # departure info
    dep_slice <-
      list(origin = origin, destination = destination,
           date = depart_date, maxStops = 1L,
           maxConnectionDuration = max_connection_duration,
           preferredCabin = "COACH",
           permittedDepartureTime =
             list(earliestTime = earliest_time,
                  latestTime = latest_time)
      )

    slice[[1L]] <- dep_slice

    # if return, add the return info
    if (!is.null(return_date)) {

      ret_slice <-
        list(origin = destination, destination = origin,
             date = return_date, maxStops = 1L,
             maxConnectionDuration = max_connection_duration,
             preferredCabin = "COACH",
             permittedDepartureTime =
               list(earliestTime = earliest_time,
                    latestTime = latest_time)
        )

      slice[[2L]] <- ret_slice
    }

    # qpx price input format is e.g. "EUR100"
    max_price <- paste0("EUR", max_price)

    # construct the flight query
    flight_query <-
      list(
        request =
          list(
            slice = slice,
            maxPrice = max_price,
            passengers =
              list(adultCount = 1,
                   infantInLapCount = 0, infantInSeatCount = 0,
                   childCount = 0, seniorCount = 0
              ),
            solutions = 500,
            refundable = FALSE)
      )


    flight_query
  }
