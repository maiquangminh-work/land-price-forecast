# Xóa tất cả các biến trước
rm(list=ls()) 

# Cài đặt các gói cần thiết
install.packages("shiny")
install.packages("ggplot2")
install.packages("randomForest")
install.packages("caret")

# Tải thư viện
library(shiny)
library(ggplot2)
library(randomForest)
library(caret)

# Tải dữ liệu
boston <- read.csv("D:/NEU Book and Slide/Kì II Năm Ba/Ứng Dụng AI Trong Kinh Doanh Và Quản Lý/UDAI_Dataset_CLC/Boston.csv")
boston <- boston[, -1]  # Xóa cột không cần thiết

# Tạo mô hình Random Forest
set.seed(42)
rf_model <- randomForest(medv ~ ., data = boston, importance = TRUE)

# Tính toán RMSE, MAE, R2 cho mô hình
predictions <- predict(rf_model, boston)

# Tính RMSE
rmse <- sqrt(mean((predictions - boston$medv)^2))

# Tính MAE
mae <- mean(abs(predictions - boston$medv))

# Tính R2
r2 <- cor(predictions, boston$medv)^2

# Lấy tầm quan trọng của các yếu tố
importance_df <- as.data.frame(importance(rf_model))
importance_df$Feature <- rownames(importance_df)

# Sắp xếp theo tầm quan trọng IncNodePurity
importance_df <- importance_df[order(-importance_df$IncNodePurity), ]

# Vẽ biểu đồ tầm quan trọng IncNodePurity
importance_plot_inc <- ggplot(importance_df, aes(x = reorder(Feature, -IncNodePurity), y = IncNodePurity)) + 
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(IncNodePurity, 2)), hjust = -0.2) +  # Thêm nhãn dữ liệu
  labs(title = "Tầm quan trọng của các yếu tố ảnh hưởng đến giá nhà (IncNodePurity)", 
       x = "Yếu tố", y = "Tầm quan trọng (IncNodePurity)") +
  theme_minimal() +
  coord_flip()

importance_plot_inc

# Sắp xếp theo tầm quan trọng %IncMSE
importance_df_mse <- importance_df[order(-importance_df$`%IncMSE`), ]

# Vẽ biểu đồ tầm quan trọng %IncMSE
importance_plot_mse <- ggplot(importance_df_mse, aes(x = reorder(Feature, -`%IncMSE`), y = `%IncMSE`)) + 
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(`%IncMSE`, 2)), hjust = -0.2) +  # Thêm nhãn dữ liệu
  labs(title = "Tầm quan trọng của các yếu tố ảnh hưởng đến giá nhà (%IncMSE)", 
       x = "Yếu tố", y = "Tầm quan trọng (%IncMSE)") +
  theme_minimal() +
  coord_flip()

importance_plot_mse


# Tính ma trận tương quan giữa các đặc trưng và giá nhà medv
cor_matrix <- cor(boston)

# Tạo heatmap từ ma trận tương quan
heatmap_plot <- pheatmap(cor_matrix, display_numbers = TRUE, main = "Ma trận tương quan giữa các yếu tố và medv")


# Shiny UI
ui <- fluidPage(
  titlePanel("Dự đoán giá nhà với mô hình Random Forest"),
  
  sidebarLayout(
    sidebarPanel(
      h3("Nhập thông tin về ngôi nhà"),
      numericInput("crim", "Tỷ lệ tội phạm:", value = 0.1),
      numericInput("zn", "Tỷ lệ đất cho khu dân cư:", value = 0),
      numericInput("indus", "Tỷ lệ đất công nghiệp:", value = 5),
      numericInput("chas", "Có sông không? (0 = Không, 1 = Có):", value = 0),
      numericInput("nox", "Tỷ lệ khí nitơ oxit:", value = 0.5),
      numericInput("rm", "Số phòng trung bình:", value = 6),
      numericInput("age", "Tuổi ngôi nhà:", value = 50),
      numericInput("dis", "Khoảng cách đến trung tâm thành phố:", value = 4),
      numericInput("rad", "Khoảng cách đến các tuyến đường chính:", value = 1),
      numericInput("tax", "Thuế tài sản:", value = 300),
      numericInput("ptratio", "Tỷ lệ học sinh/giáo viên:", value = 15),
      numericInput("black", "Tỷ lệ người da đen:", value = 380),
      numericInput("lstat", "Tỷ lệ dân số thấp:", value = 5),
      actionButton("predict_btn", "Dự đoán giá nhà")
    ),
    
    mainPanel(
      h3("Kết quả dự đoán giá nhà (medv):"),
      verbatimTextOutput("prediction_result"),
      h3("Các chỉ số đánh giá mô hình:"),
      verbatimTextOutput("model_metrics"),
      h3("Biểu đồ tầm quan trọng của các yếu tố (IncNodePurity):"),
      plotOutput("importance_plot_inc"),
      h3("Biểu đồ tầm quan trọng của các yếu tố (%IncMSE):"),
      plotOutput("importance_plot_mse")
    )
  )
)

# Shiny Server
server <- function(input, output) {
  observeEvent(input$predict_btn, {
    # Lấy các giá trị người dùng nhập
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
    
    # Dự đoán giá nhà từ mô hình Random Forest
    prediction <- predict(rf_model, new_data)
    
    # Hiển thị kết quả dự đoán
    output$prediction_result <- renderText({
      paste("Giá nhà dự đoán (medv):", round(prediction, 2), "nghìn đô la")
    })
    
    # Hiển thị các chỉ số đánh giá mô hình
    output$model_metrics <- renderText({
      paste("RMSE:", round(rmse, 2), "\n",
            "MAE:", round(mae, 2), "\n",
            "R²:", round(r2, 2))
    })
  })
  
  # Hiển thị biểu đồ tầm quan trọng IncNodePurity
  output$importance_plot_inc <- renderPlot({
    print(importance_plot_inc)
  })
  
  # Hiển thị biểu đồ tầm quan trọng %IncMSE
  output$importance_plot_mse <- renderPlot({
    print(importance_plot_mse)
  })
}

# Chạy ứng dụng Shiny
shinyApp(ui = ui, server = server)
