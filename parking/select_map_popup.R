show_selected_on_map <- function(selected_row, data, lat_col, lng_col, name_col = NULL, addr_col = NULL, popup_fn = NULL) {
  if (length(selected_row) && nrow(data) >= selected_row) {
    selected <- data[selected_row, ]
    
    popup <- if (!is.null(popup_fn)) {
      popup_fn(selected)
    } else {
      paste0("<b>", selected[[name_col]], "</b><br>", addr_col, ": ", selected[[addr_col]])
    }
    
    leafletProxy("map") %>%
      clearPopups() %>%
      setView(lng = selected[[lng_col]], lat = selected[[lat_col]], zoom = 16) %>%
      addPopups(
        lng = selected[[lng_col]],
        lat = selected[[lat_col]],
        popup = popup
      )
  }
}

generate_market_popup <- function(row) {
  paste0(
    "<b>", row$시장명, "</b><br>시장유형: ", row$시장유형, "<br>",
    ifelse(!is.na(row$시장개설주기) && trimws(row$시장개설주기) != "",
           paste0("시장개설주기: ", row$시장개설주기, "<br>"), ""),
    ifelse(!is.na(row$점포수) && trimws(row$점포수) != "",
           paste0("점포수: ", row$점포수, "<br>"), ""),
    ifelse(!is.na(row$취급품목) && trimws(row$취급품목) != "",
           paste0("취급품목: ", row$취급품목, "<br>"), "")
  )
}
