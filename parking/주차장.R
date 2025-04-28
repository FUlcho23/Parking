# 필요한 패키지 로드

library(shiny)
library(dplyr)
library(ggplot2)
library(leaflet)
library(DT)
library(geosphere)

# 공통 한글 설정을 변수로 선언
datatable_lang_ko <- list(
  search = "검색:",
  lengthMenu = "페이지당 _MENU_개 보기",
  info = "총 _TOTAL_개 중 _START_ ~ _END_번째",
  infoEmpty = "데이터 없음",
  infoFiltered = "(전체 _MAX_개 중 필터링됨)",
  zeroRecords = "일치하는 데이터가 없습니다",
  paginate = list(
    previous = "이전",
    `next` = "다음"
  )
)

# 외부 데이터 불러오기
tour_data <- read.csv("C:/데이터/tourdate3.csv", stringsAsFactors = FALSE)
festival_data <- read.csv("C:/데이터/전국문화축제표준데이터2.csv", stringsAsFactors = FALSE)
market_data <- read.csv("C:/데이터/market3.csv", header = TRUE)
raw_data <- read.csv("C:/데이터/전국주차장정보표준데이터4.csv", stringsAsFactors = FALSE)

# UI 분리
source("ui.R")  # app_ui 객체 불러옴
# 외부 코드 파일 불러오기
source("select_map_popup.R")
# Server=================================================================
server <- function(input, output, session) {
  
  filtered_data <- reactiveVal()
  tour_filtered <- reactiveVal()
  festival_filtered <- reactiveVal()
  market_filtered <- reactiveVal()
  tour_filtered_data <- reactiveVal()
  festival_filtered_data <- reactiveVal()
  market_filtered_data <- reactiveVal()
  
  #주차장
  observeEvent(input$submit, {
    start_col <- if (input$weekday == "평일")
      "평일운영시작시각"
    else if (input$weekday == "토요일")
      "토요일운영시작시각"
    else
      "공휴일운영시작시각"
    end_col   <- if (input$weekday == "평일")
      "평일운영종료시각"
    else if (input$weekday == "토요일")
      "토요일운영종료시각"
    else
      "공휴일운영종료시각"
    
    data <- raw_data %>%
      mutate(
        주차기본요금_num = suppressWarnings(as.numeric(주차기본요금)),
        유료구분 = ifelse(is.na(주차기본요금_num) |
                        주차기본요금_num == 0, "무료", "유료"),
        장애인전용주차구역보유여부 = ifelse(장애인전용주차구역보유여부 %in% c("Y", "N"), 장애인전용주차구역보유여부, "N"),
        결제방법정리 = case_when(
          grepl("카드", 결제방법) ~ "카드",
          grepl("현금", 결제방법) ~ "현금",
          grepl("무통장|가상계좌", 결제방법) ~ "무통장입금",
          grepl("계좌이체", 결제방법) ~ "계좌이체",
          TRUE ~ "기타"
        )
      )
    
    filtered <- data %>%
      filter(
        grepl(input$region, paste(소재지도로명주소, 소재지지번주소), ignore.case = TRUE),
        grepl(input$weekday, 운영요일),
        .data[[start_col]] <= input$start_hour,
        .data[[end_col]] >= input$end_hour,
        장애인전용주차구역보유여부 %in% input$disabledParking,
        주차장유형 %in% input$parkingType,
        결제방법정리 %in% input$paymentMethods,!is.na(주차기본요금_num),
        주차기본요금_num >= input$feeRange[1],
        주차기본요금_num <= input$feeRange[2]
      )
    
    filtered_data(filtered)
    
    bar_data <- filtered %>%
      mutate(주차장구분 = ifelse(주차장구분 == "", "기타", 주차장구분)) %>%
      group_by(주차장구분, 유료구분) %>%
      summarise(수 = n(), .groups = "drop")
    
    output$barPlot <- renderPlot({
      if (nrow(bar_data) == 0) {
        plot.new()
        text(0.5, 0.5, "조건에 맞는 데이터가 없습니다.", cex = 1.5)
      } else {
        ggplot(bar_data, aes(
          x = 주차장구분,
          y = 수,
          fill = 유료구분
        )) +
          geom_bar(stat = "identity") +
          geom_text(aes(label = 수),
                    position = position_stack(vjust = 0.5),
                    color = "black") +
          theme_minimal()
      }
    })
    
    output$map <- renderLeaflet({
      coords <- filtered %>%
        filter(!is.na(위도), !is.na(경도)) %>%
        mutate(
          위도 = as.numeric(위도),
          경도 = as.numeric(경도),
          id = row_number(),
          start_time = .data[[start_col]],
          end_time = .data[[end_col]]
        )
      
      leaflet(coords) %>%
        addTiles() %>%
        addMarkers(
          ~ 경도,
          ~ 위도,
          popup = ~ paste0(
            "<b>",
            주차장명,
            "</b><br>",
            "주소: ",
            ifelse(소재지도로명주소 != "", 소재지도로명주소, 소재지지번주소),
            "<br>",
            "운영요일: ",
            운영요일,
            "<br>",
            "운영시간: ",
            start_time,
            " ~ ",
            end_time,
            "<br>",
            ifelse(
              !is.na(주차구획수) &
                trimws(주차구획수) != "",
              paste0("주차구획수: ", 주차구획수, "<br>"),
              ""
            ),
            ifelse(
              !is.na(주차기본시간) &
                trimws(주차기본시간) != "",
              paste0("기본시간: ", 주차기본시간, "분<br>"),
              ""
            ),
            ifelse(
              !is.na(주차기본요금) &
                trimws(주차기본요금) != "",
              paste0("기본요금: ", 주차기본요금, "원<br>"),
              ""
            ),
            ifelse(
              !is.na(추가단위시간) &
                trimws(추가단위시간) != "",
              paste0("추가단위시간: ", 추가단위시간, "분<br>"),
              ""
            ),
            ifelse(
              !is.na(추가단위요금) &
                trimws(추가단위요금) != "",
              paste0("추가단위요금: ", 추가단위요금, "원<br>"),
              ""
            ),
            ifelse(
              !is.na(`X1일주차권요금적용시간`) &
                trimws(X1일주차권요금적용시간) != "",
              paste0("1일권 적용시간: ", `X1일주차권요금적용시간`, "<br>"),
              ""
            ),
            ifelse(
              !is.na(`X1일주차권요금`) &
                trimws(X1일주차권요금) != "",
              paste0("1일권 요금: ", `X1일주차권요금`, "원<br>"),
              ""
            ),
            ifelse(
              !is.na(월정기권요금) &
                trimws(월정기권요금) != "",
              paste0("월정기권요금: ", 월정기권요금, "원<br>"),
              ""
            ),
            ifelse(
              !is.na(특기사항) &
                trimws(특기사항) != "",
              paste0("특기사항: ", 특기사항, "<br>"),
              ""
            )
          ),
          layerId = ~ paste0("park_", id)
        )
    })
  })
  
  #지도마커
  observeEvent(input$map_marker_click, {
    filtered <- filtered_data()
    if (is.null(filtered))
      return()
    
    click <- input$map_marker_click
    
    if (is.null(click))
      return()
    if (is.null(click$id) || is.na(click$id))
      return()
    
    # 클릭된 마커가 주차장일 때만 실행
    if (grepl("^park_", click$id)) {
      leafletProxy("map") %>%
        clearGroup("tour") %>%
        clearGroup("festival")%>%
        clearGroup("market")
      
      id_num <- as.numeric(gsub("park_", "", click$id))
      clicked <- filtered %>%
        filter(!is.na(위도), !is.na(경도)) %>%
        slice(id_num)
      
      if (nrow(clicked) == 1) {
        park_lat <- clicked$위도
        park_lon <- clicked$경도
        
        tour_filtered_data <- tour_data %>%
          filter(!is.na(위도), !is.na(경도)) %>%
          mutate(거리 = distHaversine(matrix(c(경도, 위도), ncol = 2), c(park_lon, park_lat))) %>%
          filter(거리 <= 1500)%>%
          mutate(marker_id = paste0("tour_", row_number()))
        tour_filtered(tour_filtered_data)
        
        festival_filtered_data <- festival_data %>%
          filter(!is.na(위도), !is.na(경도)) %>%
          mutate(거리 = distHaversine(matrix(c(경도, 위도), ncol = 2), c(park_lon, park_lat))) %>%
          filter(거리 <= 1500)%>%
          mutate(marker_id = paste0("festival_", row_number()))
        festival_filtered(festival_filtered_data)
        
        market_filtered_data <- market_data %>%
          filter(!is.na(위도), !is.na(경도)) %>%
          mutate(거리 = distHaversine(matrix(c(경도, 위도), ncol = 2), c(park_lon, park_lat)),
                 주차장보유여부 = case_when(
                   주차장보유여부 == "Y" ~ "있음",
                   주차장보유여부 == "N" ~ "없음",
                   TRUE ~ 주차장보유여부
                 ),
                 공중화장실보유여부 = case_when(
                   공중화장실보유여부 == "Y" ~ "있음",
                   공중화장실보유여부 == "N" ~ "없음",
                   TRUE ~ 공중화장실보유여부
                 )
                 
          )%>%
          filter(거리 <= 1500)%>%
          mutate(marker_id = paste0("market_", row_number()))
        market_filtered(market_filtered_data)
        
        leafletProxy("map") %>%
          clearPopups() %>%
          addMarkers(
            data = tour_filtered_data,
            lng = ~ 경도,
            lat = ~ 위도,
            popup = ~ paste0(
              "<b>",관광지명,"</b><br>",
              "주소:",
              소재지지번주소
            ),
            icon = makeIcon(
              iconUrl = "https://raw.githubusercontent.com/pointhi/leaflet-color-markers/master/img/marker-icon-violet.png",
              iconWidth = 25,
              iconHeight = 41
            ),
            group = "tour"
          ) %>%
          addMarkers(
            data = festival_filtered_data,
            lng = ~ 경도,
            lat = ~ 위도,
            popup = ~ paste0(
              "<b>",축제명,"</b><br>",
              "개최장소: ",
              개최장소,
              "<br>",
              ifelse(
                !is.na(축제시작일자) & !is.na(축제종료일자) & 
                  trimws(축제시작일자) != "" & trimws(축제종료일자) != "",
                paste0("기간: ", 축제시작일자, " ~ ", 축제종료일자,"<br>"),
                ""
              )
            ),
            icon = makeIcon(
              iconUrl = "https://raw.githubusercontent.com/pointhi/leaflet-color-markers/master/img/marker-icon-orange.png",
              iconWidth = 25,
              iconHeight = 41
            ),
            group = "festival"
          ) %>%
          addMarkers(
            data = market_filtered_data,
            lng = ~ 경도,
            lat = ~ 위도,
            popup = ~ paste0(
              "<b>", 시장명, "</b><br> ",
              "시장유형:",
              시장유형,
              "<br>",
              ifelse(
                !is.na(시장개설주기) &
                  trimws(시장개설주기) != "",
                paste0("시장개설주기: ", 시장개설주기, "<br>"),
                ""
              ),
              ifelse(
                !is.na(점포수) &
                  trimws(점포수) != "",
                paste0("점포수: ", 점포수, "<br>"),
                ""
              ),
              ifelse(
                !is.na(취급품목) &
                  trimws(취급품목) != "",
                paste0("취급품목: ", 취급품목, "<br>"),
                ""
              )
            ),
            icon = makeIcon(
              iconUrl = "https://raw.githubusercontent.com/pointhi/leaflet-color-markers/master/img/marker-icon-green.png",
              iconWidth = 25,
              iconHeight = 41
            ),
            group = "market"
          )
        
        # 테이블 출력
        output$tourTable <- DT::renderDataTable({
          if (nrow(tour_filtered_data) == 0) {
            datatable(data.frame(메시지 = "주변에 관광지가 없습니다."),
                      options = list(dom = 't'))
          } else {
            datatable(
              tour_filtered_data %>%
                select(관광지명, 소재지지번주소, 공공편익시설정보, 숙박시설정보, 관광지소개, 관리기관전화번호),
              options = list(pageLength = 5, language = datatable_lang_ko),
              rownames = FALSE,
              selection = "single"
            )
          }
        })
        
        output$festivalTable <- DT::renderDataTable({
          if (nrow(festival_filtered_data) == 0) {
            datatable(data.frame(메시지 = "주변에 문화축제가 없습니다."),
                      options = list(dom = 't'))
          } else {
            datatable(
              festival_filtered_data %>%
                select(축제명, 개최장소, 축제시작일자, 축제종료일자, 축제내용, 전화번호, 홈페이지주소),
              options = list(pageLength = 5, language = datatable_lang_ko),
              rownames = FALSE,
              selection = "single"
            )
          }
        })
        
        output$marketTable <- DT::renderDataTable({
          if (nrow(market_filtered_data) == 0) {
            datatable(data.frame(메시지 = "주변에 전통시장이 없습니다."),
                      options = list(dom = 't'))
          } else {
            datatable(
              market_filtered_data %>%
                select(시장명, 시장유형, 시장개설주기, 점포수, 취급품목, 공중화장실보유여부, 주차장보유여부, 전화번호, 홈페이지주소),
              options = list(pageLength = 5, language = datatable_lang_ko),
              rownames = FALSE,
              selection = "single"
            )
          }
        })
      }
    }
  })
  
  #표에서 지도로(따로 나간 파일)
  observe_row_selected <- function(input_id, filtered_data, name_col, addr_col, lat_col = "위도", lng_col = "경도") {
    observeEvent(input[[input_id]], {
      selected_row <- input[[input_id]]
      data <- filtered_data()
      show_selected_on_map(selected_row, data, lat_col, lng_col, name_col, addr_col)
    })
  }
  
  # 사용
  observe_row_selected("tourTable_rows_selected", tour_filtered, "관광지명", "소재지지번주소")
  observe_row_selected("festivalTable_rows_selected", festival_filtered, "축제명", "개최장소")
  
  observeEvent(input$marketTable_rows_selected, {
    show_selected_on_map(
      selected_row = input$marketTable_rows_selected,
      data = market_filtered(),
      lat_col = "위도",
      lng_col = "경도",
      popup_fn = generate_market_popup
    )
  })
}

# 앱 실행
shinyApp(ui = app_ui, server)
