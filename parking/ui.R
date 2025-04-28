
library(shiny)
library(leaflet)
library(DT)

app_ui <- fluidPage(
  tags$head(
    # 🔹 CSS 삽입
    includeCSS("www/style.css"),
    # ✅ JavaScript
    tags$script(src = "script.js")
  ),
  # 🔸 앱 제목
  titlePanel("주차장 정보 플랫폼"),
  
  # 🔸 필터 + 출력 패널
  wellPanel(
    fluidRow(
      column(
        9,
        textInput("region", "지역(구 또는 시군구)을 입력하세요:", value = "동래")
      ),
      column(
        3,
        selectInput(
          "weekday",
          "운영 요일 선택:",
          choices = c("평일", "토요일", "공휴일"),
          selected = "평일"
        )
      )
    ),
    
    fluidRow(
      column(
        6,
        sliderInput(
          "feeRange",
          "주차 기본 요금 (원)",
          min = 0,
          max = 3500,
          value = c(0, 3500),
          width = '100%'
        )
      ),
      column(
        3,
        selectInput("start_hour", "주차 시작:", choices = sprintf("%02d:00", 0:23), selected = "09:00")
      ),
      column(
        3,
        selectInput("end_hour", "주차 종료:", choices = sprintf("%02d:00", 0:23), selected = "18:00")
      )
    ),
    
    fluidRow(
      column(
        6,
        checkboxGroupInput(
          "paymentMethods", "결제 방법:",
          choices = c("카드", "현금", "무통장입금", "계좌이체", "기타"),
          selected = "카드",
          inline = TRUE
        )
      ),
      column(
        3,
        checkboxGroupInput(
          "disabledParking", "장애인 전용 주차구역 여부:",
          choices = c("있음" = "Y", "없음" = "N"),
          selected = "Y",
          inline = TRUE
        )
      ),
      column(
        3,
        checkboxGroupInput(
          "parkingType", "주차장 유형:",
          choices = c("노외", "부설", "노상"),
          selected = "노외",
          inline = TRUE
        )
      )
    ),
    
    actionButton("submit", "검색", icon = icon("search"), 
                 style = "color: white; background-color: #007bff; border: none; padding: 10px 20px; font-size: 16px; border-radius: 5px;")
  ),
  
  # 🔸 결과 패널 (탭으로 분리)
  tabsetPanel(
    tabPanel("막대그래프", plotOutput("barPlot")),
    tabPanel("지도",
             
             # ✅ 범례 영역
             tags$div(
               style = "margin: 10px 0; font-size: 15px;",
               HTML('
                <b> 마커 색상 안내</b><br>
                <span style="color:#3388CC;">■</span> 주차장 &nbsp;&nbsp;
                <span style="color:#9E30CC;">■</span> 관광지 &nbsp;&nbsp;
                <span style="color:#CC8830;">■</span> 문화축제 &nbsp;&nbsp;
                <span style="color:#32AC2C;">■</span> 전통시장
              ')
             ),
             
             leafletOutput("map", height = "400px"),
             tags$div(style = "margin-bottom: 20px"),
             
             tabsetPanel(
               tabPanel(
                 h4("관광지 정보"),
                 DT::dataTableOutput("tourTable")
               ),
               tabPanel(
                 h4("문화축제 정보"),
                 DT::dataTableOutput("festivalTable")  
               ),
               tabPanel(
                 h4("전통시장 정보"),
                 DT::dataTableOutput("marketTable")  
               )
             )
    )
  )
)
