# Download thư viện 
install.packages('shiny')
install.packages('randomForest')
install.packages('ggplot2')
install.packages('shinythemes')
install.packages('e1071')
install.packages('caret')
install.packages('corrplot')
install.packages('dplyr')
install.packages('plotly')
install.packages('openxlsx')
install.packages('readr')


# Load thư viện
library(shiny)
library(randomForest)
library(ggplot2)
library(shinythemes)
library(e1071)
library(caret)
library(corrplot)
library(dplyr)
library(plotly)
library(openxlsx)
library(readr)

# Tải dữ liệu
boston <- read.csv("D:/app_R/project/MaiQuangMinh11224217/Boston.csv")

boston <- boston[, -1]  # Xóa cột không cần thiết

# Tạo train-test split
set.seed(42)
split <- createDataPartition(boston$medv, p = 0.8, list = FALSE)
train_data <- boston[split, ]
test_data <- boston[-split, ]

# Huấn luyện mô hình
model_rf <- randomForest(medv ~ ., data = train_data)
model_lm <- lm(medv ~ ., data = train_data)
model_svr <- svm(medv ~ ., data = train_data)

# Dự đoán
pred_rf <- predict(model_rf, test_data)
pred_lm <- predict(model_lm, test_data)
pred_svr <- predict(model_svr, test_data)

# Đánh giá mô hình
eval_metrics <- function(true, pred) {
  rmse <- sqrt(mean((true - pred)^2))
  mae <- mean(abs(true - pred))
  r2 <- cor(true, pred)^2
  return(c(RMSE = round(rmse, 2), MAE = round(mae, 2), R2 = round(r2, 2)))
}

model_metrics_df <- data.frame(
  Model = c("Random Forest", "Linear Regression", "SVR"),
  t(sapply(list(pred_rf, pred_lm, pred_svr), function(p) eval_metrics(test_data$medv, p)))
)

# Giao diện người dùng
ui <- fluidPage(
  theme = shinytheme("flatly"),
  tags$head(tags$style(HTML("
    body {background-color: #fff;}
    .navbar {background-color: #ec2027;}
    .tab-content {padding: 20px;}
    h2, h4 {color: #ec2027;}
  "))),
  
  navbarPage("Dự đoán Giá Nhà - Dashboard", id = "tabs",
             
             tabPanel("Dự đoán giá nhà",
                      sidebarLayout(
                        sidebarPanel(
                          fileInput("upload_data", "Hoặc tải lên file CSV để dự đoán hàng loạt", accept = ".csv"),
                          actionButton("download_raw", "Tải dữ liệu gốc (.csv)"),
                          numericInput("crim", "Tỷ lệ tội phạm (crim)", value = 0.1),
                          numericInput("zn", "Tỷ lệ đất nhà ở > 25000 sqft (zn)", value = 0),
                          numericInput("indus", "% diện tích kinh doanh phi bán lẻ (indus)", value = 7),
                          numericInput("chas", "Sông Charles (1 = gần, 0 = không)", value = 0),
                          numericInput("nox", "NOx (nox)", value = 0.5),
                          numericInput("rm", "Số phòng trung bình (rm)", value = 6),
                          numericInput("age", "% nhà xây trước 1940 (age)", value = 60),
                          numericInput("dis", "Khoảng cách đến 5 trung tâm việc làm (dis)", value = 4),
                          numericInput("rad", "Chỉ số tiếp cận đường lớn (rad)", value = 4),
                          numericInput("tax", "Thuế tài sản (tax)", value = 300),
                          numericInput("ptratio", "Tỷ lệ giáo viên/học sinh (ptratio)", value = 18),
                          numericInput("black", "Chỉ số dân da đen (black)", value = 390),
                          numericInput("lstat", "% người thu nhập thấp (lstat)", value = 12),
                          actionButton("predict", "Dự đoán giá nhà"),
                          downloadButton("download_prediction", "Tải kết quả")
                        ),
                        mainPanel(
                          h2("Kết quả dự đoán"),
                          verbatimTextOutput("prediction_output"),
                          tableOutput("batch_prediction")
                        )
                      )
             ),
             
             tabPanel("So sánh mô hình",
                      h2("So sánh hiệu suất mô hình"),
                      p("So sánh ba mô hình dựa trên các chỉ số RMSE, MAE và R-squared."),
                      tableOutput("model_comparison"),
                      plotlyOutput("comparison_plot")
             ),
             
             tabPanel("Phân tích dữ liệu",
                      h2("Phân tích tương quan"),
                      p("Phân tích mối liên hệ giữa các biến trong dữ liệu Boston."),
                      plotOutput("correlation_plot"),
                      h4("Tầm quan trọng của biến (Random Forest)"),
                      plotOutput("importance_plot"),
                      h4("Mối quan hệ giữa số phòng (rm) và giá nhà"),
                      plotOutput("rm_plot"),
                      h4("Mối quan hệ giữa % người thu nhập thấp (lstat) và giá nhà"),
                      plotOutput("lstat_plot")
             )
  )
)

# Server
server <- function(input, output) {
  
  prediction_result <- reactiveVal()
  batch_predictions <- reactiveVal()
  
  observeEvent(input$predict, {
    new_data <- data.frame(
      crim = input$crim,
      zn = input$zn,
      indus = input$indus,
      chas = input$chas,
      nox = input$nox,
      rm = input$rm,
      age = input$age,
      dis = input$dis,
      rad = input$rad,
      tax = input$tax,
      ptratio = input$ptratio,
      black = input$black,
      lstat = input$lstat
    )
    prediction <- predict(model_rf, new_data)
    prediction_result(data.frame(GiaDuDoan = round(prediction, 2)))
    output$prediction_output <- renderPrint({ paste("Giá nhà dự đoán: ", round(prediction, 3), "nghìn USD") })
  })
  
  observeEvent(input$upload_data, {
    req(input$upload_data)
    user_data <- read_csv(input$upload_data$datapath)
    if (!all(colnames(train_data)[-14] %in% colnames(user_data))) {
      showNotification("File CSV phải có đủ các cột giống dữ liệu huấn luyện!", type = "error")
      return()
    }
    predictions <- predict(model_rf, user_data)
    result <- cbind(user_data, GiaDuDoan = round(predictions, 2))
    batch_predictions(result)
    output$batch_prediction <- renderTable({ result })
  })
  
  output$download_prediction <- downloadHandler(
    filename = function() {
      paste("du_doan_gia_nha", Sys.Date(), ".xlsx", sep="")
    },
    content = function(file) {
      if (!is.null(batch_predictions())) {
        write.xlsx(batch_predictions(), file)
      } else {
        write.xlsx(prediction_result(), file)
      }
    }
  )
  
  output$model_comparison <- renderTable({ model_metrics_df })
  
  output$comparison_plot <- renderPlotly({
    df_long <- tidyr::pivot_longer(model_metrics_df, cols = c("RMSE", "MAE", "R2"), names_to = "Metric", values_to = "Value")
    p <- ggplot(df_long, aes(x = Model, y = Value, fill = Metric, label = Value)) +
      geom_bar(stat = "identity", position = position_dodge(width = 0.8)) +
      geom_text(position = position_dodge(width = 0.8), vjust = -0.5, size = 3.5) +
      scale_fill_manual(values = c("#ec2027", "#f39c12", "#27ae60")) +
      theme_minimal() +
      labs(title = "Hiệu suất mô hình", y = "Giá trị", x = "Mô hình")
    ggplotly(p)
  })
  
  output$correlation_plot <- renderPlot({
    corrplot(cor(boston), method = "color", type = "upper", tl.cex = 0.8)
  })
  
  output$importance_plot <- renderPlot({
    varImpPlot(model_rf, main = "Tầm quan trọng của biến")
  })
  
  output$rm_plot <- renderPlot({
    ggplot(boston, aes(x = rm, y = medv)) +
      geom_point(color = "#ec2027") +
      geom_smooth(method = "lm", color = "black") +
      labs(title = "Số phòng trung bình vs Giá nhà", x = "Số phòng", y = "Giá nhà (nghìn USD)")
  })
  
  output$lstat_plot <- renderPlot({
    ggplot(boston, aes(x = lstat, y = medv)) +
      geom_point(color = "#f39c12") +
      geom_smooth(method = "lm", color = "black") +
      labs(title = "% người thu nhập thấp vs Giá nhà", x = "% thu nhập thấp", y = "Giá nhà (nghìn USD)")
  })
  
  output$download_raw <- downloadHandler(
    filename = function() {
      paste("du_lieu_goc_boston", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write_csv(boston, file)
    }
  )
}

# Chạy ứng dụng
shinyApp(ui = ui, server = server)
