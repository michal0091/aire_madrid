#' xts2ts
#'
#' @param xts_data xts time series data in daily, monthly, quarterly and yearly frequency
#'
#' @return ts object

xts_ts <- function(xts_data) {
  freq_list <-
    data.table::data.table(
      freq = c('day', 'month', 'quarter', 'year'),
      freq_n = c(365, 12L, 4L, 1L),
      freq_format = c('%Y, %j' ,'%Y, %m', '%Y, %q', '%Y')
    )
  
  d_ferq <- xts::periodicity(xts_data)[["label"]]
  freq_n <- freq_list[freq == d_ferq, freq_n]
  freq_format <- freq_list[freq == d_ferq, freq_format]
  
  # Put NA if missing date
  empty <-
    zoo::zoo(order.by = seq.Date(zoo::index(xts_data)[1], zoo::index(xts_data)[nrow(xts_data)], by = d_ferq))
  no_misssing <- merge(xts_data, empty)
  
  if (d_ferq == 'quarter') {
    start_date <-
      format(zoo::as.yearqtr(xts::periodicity(xts_data)[["start"]]), freq_format)
    
  } else {
    start_date <-
      format(zoo::as.Date(xts::periodicity(xts_data)[["start"]]), freq_format)
  }
  
  stats::ts(zoo::coredata(no_misssing),
            start = as.integer(strsplit(start_date, split = ',')[[1]]),
            frequency = freq_n)
}
