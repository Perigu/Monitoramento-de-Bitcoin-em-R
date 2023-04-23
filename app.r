library(shiny)
library(readr)
library(dplyr)
library(ggplot2)
library(plotly)
library(DT)

dados <- read.csv("D:/Users/Igor Sobreira/Desktop/Work/Codes/R/.vscode/AV3/BTC-USD.csv")

dados <- dados %>%
  select(Date, Close) %>%
  rename(date = Date, price = Close)

colnames(dados) <- c("Data", "Preço")

dados$Data <- as.Date(dados$Data)

ui <- fluidPage(
  titlePanel("Preço do Bitcoin ao longo do último ano"),
  sidebarLayout(
    sidebarPanel(
      dateRangeInput("date_range", label = "Filtrar por data", start = min(dados$Data), end = max(dados$Data)),
      numericInput("price_filter", label = "Filtrar por preço", min = 0, max = max(dados$Preço), step = 10, value = max(dados$Preço)),
      checkboxInput("price_filter_lt", label = "<", value = FALSE),
      checkboxInput("price_filter_gt", label = ">", value = FALSE),
      checkboxInput("price_filter_eq", label = "=", value = FALSE)
    ),
    mainPanel(
      tabsetPanel(
        tabPanel(
          "Gráfico",
          plotlyOutput("btc_plot"),
          br(),
          p("O gráfico acima mostra o preço do Bitcoin em USD ao longo do último ano.")
        ),
        tabPanel(
          "Tabela",
          DTOutput("btc_table"),
          br(),
          p("A tabela acima mostra os dados do preço do Bitcoin em USD ao longo do último ano.")
        )
      )
    )
  )
)

server <- function(input, output) {

  dados_filtrados <- reactive({
    dados %>%
      filter(Data >= input$date_range[1], Data <= input$date_range[2],
             if (input$price_filter_lt) Preço < input$price_filter else TRUE,
             if (input$price_filter_gt) Preço > input$price_filter else TRUE,
             if (input$price_filter_eq) Preço == input$price_filter else TRUE)
  })
  
  output$btc_plot <- renderPlotly({
    p <- ggplot(data = dados_filtrados(), aes(x = Data, y = Preço)) +
      geom_line(size = 0.6, color = "purple") +
      geom_point(color = "#0072B2", fill = "#0072B2", size = 0.6, shape = 21, stroke = 0) +
      labs(x = "Data", y = "Preço (USD)") +
      theme_bw() +
      theme(
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        plot.title = element_text(size = 16, face = "bold")
      )
    
    ggplotly(
      p +
        geom_point(
          aes(
            tooltip = paste(
              "Data: ",
              "<br>Preço: ",
              " USD"
            )
          )
        )
    ) %>%
      layout(autosize = TRUE, margin = list(l = 50, r = 50, t = 80, b = 50))
  })
  
  output$btc_table <- renderDT({
    datatable(
      dados_filtrados(),
      options = list(
        pageLength = 10,
        lengthMenu = c(10, 25, 50),
        dom = "tip",
        autoWidth = TRUE,
        columnDefs = list(list(width = "120px", targets = "_all"))
      ),
      rownames = FALSE
    ) %>%
      formatCurrency("Preço", currency = "USD", interval = 3) %>%
      formatStyle("Preço", backgroundColor = "lightpurple", fontWeight = "bold")
  })
}

shinyApp(ui = ui, server = server)