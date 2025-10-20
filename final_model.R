# Xóa tất cả các biến trước
rm(list=ls()) 

# Cài đặt các gói cần thiết

# install.packages("shiny")
# install.packages("ggplot2")
# install.packages("randomForest")
# install.packages("caret")
# install.packages('pheatmap')
# install.packages('e1071')
# install.packages('dplyr') 

# Tải thư viện
library(shiny)
library(ggplot2)
library(randomForest)
library(caret)
library(pheatmap)
library(e1071)
library(dplyr)

# Tải dữ liệu
boston <- read.csv("D:/app_R/project/MaiQuangMinh11224217/Boston.csv")

boston <- boston[, -1]  # Xóa cột không cần thiết

# Tạo mô hình Random Forest với cột mục tiêu là cột "medv"
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

# Tính toán các thống kê mô tả
desc_stats <- boston %>% 
  summarise(
    mean_medv = mean(medv),
    sd_medv = sd(medv),
    min_medv = min(medv),
    max_medv = max(medv),
    q25_medv = quantile(medv, 0.25),
    q50_medv = quantile(medv, 0.5),
    q75_medv = quantile(medv, 0.75),
    skewness_medv = skewness(medv)
  )


#-------------------------------------------------------------------------------
# Lấy tầm quan trọng của các yếu tố
importance_df <- as.data.frame(importance(rf_model))
importance_df$Feature <- rownames(importance_df)

# Sắp xếp theo tầm quan trọng IncNodePurity
importance_df <- importance_df[order(-importance_df$IncNodePurity), ]

# Vẽ biểu đồ tầm quan trọng IncNodePurity
importance_plot_inc <- ggplot(importance_df, aes(x = reorder(Feature, -IncNodePurity),
                                                 y = IncNodePurity)) + 
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(IncNodePurity, 2)), hjust = -0.2) + 
  labs(title = "Tầm quan trọng của các yếu tố ảnh hưởng đến giá nhà (IncNodePurity)", 
       x = "Yếu tố", y = "Tầm quan trọng (IncNodePurity)") +
  theme_minimal() +
  coord_flip()

importance_plot_inc


#-------------------------------------------------------------------------------
# Sắp xếp theo tầm quan trọng %IncMSE
importance_df_mse <- importance_df[order(-importance_df$`%IncMSE`), ]

# Vẽ biểu đồ tầm quan trọng %IncMSE
importance_plot_mse <- ggplot(importance_df_mse, aes(x = reorder(Feature, -`%IncMSE`), y = `%IncMSE`)) + 
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(`%IncMSE`, 2)), hjust = -0.2) + 
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
# Shiny UI mở rộng
ui <- navbarPage("Dự đoán giá nhà với Random Forest",
                 tabPanel("Dự đoán giá nhà",
                          sidebarLayout(
                            sidebarPanel(
                              h3("Nhập thông tin về ngôi nhà"),
                              numericInput("crim", "Tỷ lệ tội phạm:", value = 0.1),
                              numericInput("zn", "Tỷ lệ đất cho khu dân cư:", value = 0),
                              numericInput("indus", "Tỷ lệ đất công nghiệp:", value = 5),
                              numericInput("chas", "Có sông không? (0 = Không, 1 = Có):", value = 0),
                              numericInput("nox", "Tỷ lệ ô nhiễm không khí:", value = 0.5),
                              numericInput("rm", "Số phòng trung bình:", value = 6),
                              numericInput("age", "Tuổi ngôi nhà:", value = 50),
                              numericInput("dis", "Khoảng cách đến trung tâm:", value = 4),
                              numericInput("rad", "Khoảng cách đến đường chính:", value = 1),
                              numericInput("tax", "Thuế tài sản:", value = 300),
                              numericInput("ptratio", "Tỷ lệ học sinh/giáo viên:", value = 15),
                              numericInput("black", "Tỷ lệ người da đen:", value = 380),
                              numericInput("lstat", "Tỷ lệ dân cư tầng lớp thấp:", value = 5),
                              actionButton("predict_btn", "Dự đoán giá nhà")
                            ),
                            
                            mainPanel(
                              h3("Kết quả dự đoán giá nhà:"),
                              verbatimTextOutput("prediction_result"),
                              h3("Đánh giá mô hình Random Forest:"),
                              verbatimTextOutput("model_metrics"),
                              h3("Thống kê mô tả:"),
                              verbatimTextOutput("desc_stats"),
                              h3("Tầm quan trọng (IncNodePurity):"),
                              plotOutput("importance_plot_inc"),
                              h3("Tầm quan trọng (%IncMSE):"),
                              plotOutput("importance_plot_mse"),
                              h3("Ma trận tương quan:"),
                              plotOutput("heatmap_plot")
                            )
                          )
                 ),
                 
                 tabPanel("So sánh mô hình",
                          mainPanel(
                            h3("Bảng so sánh mô hình"),
                            tableOutput("comparison_table"),
                            h3("Biểu đồ so sánh hiệu suất"),
                            plotOutput("comparison_plot")
                          )
                 )
)


# Shiny Server
server <- function(input, output) {
  observeEvent(input$predict_btn, {
    # Lấy các giá trị đã nhập
    new_data <- data.frame(
      crim = input$crim,
      zn = input$zn,
      indus = input$indus,
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
      paste("Giá nhà dự đoán (medv):", round(prediction, 3), "nghìn đô la")
    })
    
    # Hiển thị các chỉ số đánh giá mô hình
    output$model_metrics <- renderText({
      paste("RMSE:", round(rmse, 2), "\n",
            "MAE:", round(mae, 2), "\n",
            "R²:", round(r2, 2))
    })
    
    # Hiển thị thống kê mô tả
    output$desc_stats <- renderText({
      paste("Trung bình giá nhà:", round(desc_stats$mean_medv, 2), "\n",
            "Độ lệch chuẩn giá nhà:", round(desc_stats$sd_medv, 2), "\n",
            "Giá trị min:", round(desc_stats$min_medv, 2), "\n",
            "Giá trị max:", round(desc_stats$max_medv, 2), "\n",
            "Quartile 25%:", round(desc_stats$q25_medv, 2), "\n",
            "Quartile 50%:", round(desc_stats$q50_medv, 2), "\n",
            "Quartile 75%:", round(desc_stats$q75_medv, 2), "\n",
            "Skewness:", round(desc_stats$skewness_medv, 2))
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
  
  # Hiển thị ma trận tương quan
  output$heatmap_plot <- renderPlot({
    print(heatmap_plot)
  })
  
  
  # Chia train/test
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
  
  # Hàm tính chỉ số đánh giá
  eval_metrics <- function(true, pred) {
    rmse <- sqrt(mean((true - pred)^2))
    mae <- mean(abs(true - pred))
    r2 <- cor(true, pred)^2
    return(c(RMSE = round(rmse, 2), MAE = round(mae, 2), R2 = round(r2, 2)))
  }
  
  # Tổng hợp kết quả
  model_metrics_df <- data.frame(
    Model = c("Random Forest", "Linear Regression", "SVR"),
    t(sapply(list(pred_rf, pred_lm, pred_svr), function(p) eval_metrics(test_data$medv, p)))
  )
  
  # Output bảng
  output$comparison_table <- renderTable({
    model_metrics_df
  })
  
  # Output biểu đồ
  output$comparison_plot <- renderPlot({
    library(reshape2)
    metrics_long <- melt(model_metrics_df, id.vars = "Model")
    ggplot(metrics_long, aes(x = Model, y = value, fill = variable)) +
      geom_bar(stat = "identity", position = position_dodge()) +
      labs(title = "So sánh hiệu suất mô hình", x = "Mô hình", y = "Giá trị") +
      theme_minimal()
  })
  
}

# Chạy ứng dụng Shiny
shinyApp(ui = ui, server = server)





