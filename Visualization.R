rm(list = ls())

# Nhập dữ liệu
# Sau đó sẽ trực quan hóa dữ liệu tùy thuộc vào thuộc tính của dataset

# Tải thư viện cần thiết
install.packages('ggplot2')
install.packages('ggthemes')
install.packages('pheatmap')
install.packages('plotly')
install.packages('reshape2')
install.packages('corrplot')
install.packages('GGally')
install.packages('readxl')
install.packages('scales')
install.packages('igraph')
install.packages('maps')
install.packages('dplyr')
install.packages('treemap')
install.packages('fmsb')

# Nhập thư viện
library(ggplot2)  # Trực quan
library(ggthemes)
library(pheatmap) # Heatmap
library(plotly)
library(reshape2) # Chuyển dữ liệu về dạng dài
library(corrplot) # Dùng cho ma trận tương quan
library(GGally)   # Dùng để tạo sự so sánh, sự tương quan
library(readxl)
library(scales)
library(igraph)   # Vẽ biểu đồ mạng lưới
library(maps)     # Vẽ biểu đồ map
library(dplyr)
library(treemap)  # Vẽ treemap
library(fmsb)     # Vẽ radar

# Dataset (Tất cả)
bank_data <- read.csv("D:/app_R/project/MaiQuangMinh11224217/bank-data_xuly.csv") 

wine_data <- read.csv("D:/app_R/project/MaiQuangMinh11224217/winequality-red.csv")

stock_data <- read.csv("D:/app_R/project/MaiQuangMinh11224217/GOOG.csv")

sales_data <- read_excel("D:/app_R/project/MaiQuangMinh11224217/Sales.xlsx")

fashion_data <- read_excel("D:/app_R/project/MaiQuangMinh11224217/Fashion.xlsx")

# BAR CHART
ggplot(bank_data, aes(x = factor(region), fill = factor(region))) +
  geom_bar() +
  labs(title = "Tỷ Lệ Phân Bố Vùng Miền", x = "Vùng", y = "Số Lượng") +
  scale_x_discrete(labels = c("Vùng 0", "Vùng 1", "Vùng 2", "Vùng 3")) +
  scale_fill_manual(values = c("steelblue", "salmon", "red", "yellow")) +
  theme_minimal()


# PIE CHART
married_counts <- table(bank_data$married)

plot_ly(labels = c("Chưa kết hôn", "Đã kết hôn"), values = married_counts, type = 'pie',
        marker = list(colors = c("skyblue", "lightcoral"))) %>%
  layout(title = 'Tỷ Lệ Tình Trạng Hôn Nhân')


# SCATTER PLOT
ggplot(bank_data, aes(x = age, y = income)) +
  geom_point(aes(color = factor(region)), size = 3) + 
  labs(title = "Mối Quan Hệ Giữa Thu Nhập và Tuổi", x = "Tuổi", y = "Thu Nhập") +
  scale_color_manual(values = c("red", "green", "blue", "purple")) +  
  theme_minimal()


# HISTOGRAM
ggplot(bank_data, aes(x = age)) +
  geom_histogram(binwidth = 2, fill = "steelblue", color = "darkblue") +
  labs(title = "Phân Phối Độ Tuổi", x = "Tuổi", y = "Tần Suất") +
  theme_minimal()

# HEATMAP
cor_matrix <- cor(wine_data)

# Vẽ Heatmap
corrplot(cor_matrix, method = "color", 
         col = colorRampPalette(c("blue", "white", "red"))(200), 
         title = "Do thi tuong quan cua cac thuoc tinh chat luong ruou vang", 
         mar = c(0, 0, 1, 0), 
         addCoef.col = "black",  
         number.cex = 0.7)  


# BOX PLOT
# Chuyển dữ liệu thành dạng dài để dễ dàng vẽ biểu đồ Boxplot
data_long <- melt(wine_data)

# Vẽ biểu đồ Boxplot cho các thuộc tính trong dữ liệu
ggplot(data_long, aes(x = variable, y = value)) +
  geom_boxplot(fill = "lightblue", color = "black") +
  labs(title = "Boxplot của các thuộc tính trong rượu vang đỏ", x = "Thuộc tính", y = "Giá trị") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


# LINE CHART
stock_data$Date <- as.Date(stock_data$Date, format="%m/%d/%Y")

# Sắp xếp dữ liệu theo ngày
stock_data <- stock_data[order(stock_data$Date), ]

ggplot(stock_data, aes(x = Date, y = Close, group = 1)) +  # Group = 1 để nhóm dữ liệu
  geom_line(color = "blue") +
  labs(title = "Biểu Đồ Đường Giá Cổ Phiếu Google", x = "Ngày", y = "Giá Đóng Cửa") +
  theme_minimal()

# CANDLESTICK
# Vẽ Candlestick Chart cho giá cổ phiếu Google
set.seed(123)  # Đảm bảo kết quả ngẫu nhiên lặp lại được
sampled_data <- stock_data %>%
  arrange(Date) %>%
  sample_n(30) %>%
  arrange(Date)

fig <- plot_ly(stock_data, x = ~Date, type = "candlestick",
               open = ~Open, close = ~Close,
               high = ~High, low = ~Low,
               increasing = list(line = list(color = 'green')),
               decreasing = list(line = list(color = 'red'))) %>%
  layout(title = "Biểu Đồ Nến Giá Cổ Phiếu Google",
         xaxis = list(type = "category"),
         yaxis = list(title = "Giá Cổ Phiếu"))

fig


# STACK COLUMN
ggplot(sales_data, aes(x = `Customer Segment`, y = Sales, fill = `Product Category`)) +
  geom_bar(stat = "identity") +
  labs(title = "Doanh Thu Theo Phân Khúc Khách Hàng và Loại Sản Phẩm", 
       x = "Phân Khúc Khách Hàng", y = "Doanh Thu") +
  scale_y_continuous(labels = scales::comma) + # Định dạng trục y theo số bình thường
  theme_minimal()

# STACK AREA
sales_data$Order.Date <- as.Date(sales_data$`Order Date`)

start_date <- as.Date("2024-01-01")
end_date <- as.Date("2024-01-31")

filtered_data_stack_area <- subset(sales_data, Order.Date >= start_date & Order.Date <= end_date)

# Vẽ Stacked Area Chart cho doanh thu theo thời gian và loại sản phẩm
ggplot(filtered_data_stack_area, aes(x = Order.Date, y = Sales, fill = `Product Category`)) +
  geom_area() +
  labs(title = "Doanh Thu Theo Thời Gian và Loại Sản Phẩm (01/01/2024 - 31/01/2024)", 
       x = "Ngày", y = "Doanh Thu") +
  theme_minimal() +
  scale_y_continuous(labels = scales::comma)  # Định dạng trục Y theo dạng số bình thường


# AREA CHART
# Lọc dữ liệu trong khoảng thời gian đã chọn
filtered_data_area <- subset(sales_data, Order.Date >= start_date & Order.Date <= end_date)

# Vẽ Biểu đồ Diện Tích (Area Chart) cho doanh thu theo thời gian
ggplot(filtered_data_area, aes(x = Order.Date, y = Sales)) +
  geom_area(fill = "orange", alpha = 0.6) +  # Màu diện tích và độ trong suốt
  labs(title = "Biểu Đồ Diện Tích Doanh Thu (01/01/2024 - 31/01/2024)", 
       x = "Ngày", y = "Doanh Thu") +
  theme_minimal() +
  scale_y_continuous(labels = scales::comma)


# ARC DIAGRAM
# Dữ liệu giả định với nhiều loại mối quan hệ
edges <- data.frame(
  from = c("A", "A", "B", "C", "D", "E", "F", "A", "B", "C"),
  to = c("B", "C", "C", "D", "E", "F", "A", "E", "D", "F"),
  relationship = c("Friend", "Colleague", "Friend", "Manager", "Friend", "Colleague", "Friend", "Colleague", "Manager", "Friend")
)

# Tạo đồ thị từ dữ liệu các mối quan hệ (edges)
g <- graph_from_data_frame(edges, directed = FALSE)

# Màu sắc cho các mối quan hệ
E(g)$color <- ifelse(E(g)$relationship == "Friend", "blue", ifelse(E(g)$relationship == "Colleague", "green", "red"))
E(g)$width <- ifelse(E(g)$relationship == "Friend", 2, ifelse(E(g)$relationship == "Colleague", 3, 4))

# Vẽ Arc Diagram cho mạng xã hội với nhiều mối quan hệ và chú thích
plot(g, layout = layout_in_circle, vertex.size = 40, vertex.label.cex = 1.2, 
     edge.width = E(g)$width, edge.color = E(g)$color, main = "Arc Diagram - Mạng Xã Hội và Công Việc", 
     vertex.label.color = "black", vertex.label.font = 2,  # Định dạng nhãn node
     edge.arrow.size = 0.5, edge.arrow.width = 0.5,       # Định dạng mũi tên nếu có hướng
     mark.groups = TRUE)

legend("topright", legend = c("Friend", "Colleague", "Manager"), 
       fill = c("blue", "green", "red"), title = "Loại Mối Quan Hệ", cex = 0.8)


# MAP
state_sales <- fashion_data %>%
  group_by(State) %>%
  summarise(total_sales = sum(Sales))

# Tải bản đồ Australia
australia_map <- map_data("world") %>% 
  filter(region == "Australia")

# Tạo bảng ánh xạ từ tên đầy đủ của tiểu bang sang mã viết tắt
state_mapping <- data.frame(
  full_name = c("New South Wales", "Victoria", "Queensland", "South Australia", "Western Australia", 
                "Tasmania", "Australian Capital Territory", "Northern Territory"),
  abbr = c("NSW", "VIC", "QLD", "SA", "WA", "TAS", "ACT", "NT")
)

australia_map$subregion <- recode(australia_map$subregion,
                                  "New South Wales" = "NSW",
                                  "Victoria" = "VIC",
                                  "Queensland" = "QLD",
                                  "South Australia" = "SA",
                                  "Western Australia" = "WA",
                                  "Tasmania" = "TAS",
                                  "Australian Capital Territory" = "ACT",
                                  "Northern Territory" = "NT")

australia_map <- australia_map %>%
  left_join(state_sales, by = c("subregion" = "State"))

# Vẽ bản đồ phân bố doanh thu theo tiểu bang
ggplot(australia_map, aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = total_sales), color = "white") +
  scale_fill_gradient(low = "white", high = "blue") +
  theme_void() +
  labs(title = "Doanh Thu Theo Tiểu Bang (Australia)", fill = "Doanh Thu")


# Treemap
treemap(fashion_data,
        index = c("Category", "Manager"),  # Chia theo loại sản phẩm và quản lý
        vSize = "Sales",  # Sử dụng Sales làm kích thước của các ô
        vColor = "Sales",  # Màu sắc biểu thị doanh thu
        draw.labels = TRUE,
        title = "Doanh Thu Theo Loại Sản Phẩm và Quản Lý")

# Bubble chart
ggplot(sales_data, aes(x = `Quantity ordered new`, y = Sales, size = Profit, color = `Product Category`)) +
  geom_point(alpha = 0.6) +  # Sử dụng geom_point để vẽ các điểm bubble
  scale_size_continuous(range = c(3, 5)) +  # Điều chỉnh kích thước các bubble
  labs(title = "Bubble Chart: Doanh Thu, Số Lượng Bán và Lợi Nhuận", 
       x = "Số Lượng Bán", y = "Doanh Thu", size = "Lợi Nhuận", color = "Loại Sản Phẩm") +
  theme_minimal()


# Density plot
# Vẽ Density Plot cho Discount
ggplot(sales_data, aes(x = Discount)) +
  geom_density(fill = "green", alpha = 0.6) +  # Màu sắc và độ trong suốt
  labs(title = "Density Plot của Mức Giảm Giá (Discount)", x = "Mức Giảm Giá", y = "Mật Độ") +
  theme_minimal()  # Sử dụng theme tối giản

# Vẽ Density Plot cho Profit
ggplot(sales_data, aes(x = Profit)) +
  geom_density(fill = "purple", alpha = 0.6) +  # Màu sắc và độ trong suốt
  labs(title = "Density Plot của Lợi Nhuận (Profit)", x = "Lợi Nhuận", y = "Mật Độ") +
  theme_minimal()  # Sử dụng theme tối giản


# Gauge Chart
# Giả sử doanh thu hiện tại là 7500 và mục tiêu là 10000
current_sales <- 7500
max_sales <- 10000

# Vẽ Gauge Chart đẹp hơn
fig <- plot_ly(
  type = "indicator",
  mode = "gauge+number+delta",
  value = current_sales,  # Doanh thu hiện tại
  delta = list(reference = max_sales),  # Mục tiêu doanh thu
  gauge = list(
    axis = list(range = c(0, max_sales), tickwidth = 2, tickcolor = "darkblue"),
    bar = list(color = "green"),
    borderwidth = 1,
    steps = list(
      list(range = c(0, 5000), color = "lightgray"),
      list(range = c(5000, 7500), color = "yellow"),
      list(range = c(7500, max_sales), color = "green")
    )
  ),
  title = list(text = "Doanh Thu Hiện Tại", font = list(size = 16)),
  number = list(suffix = " USD", font = list(size = 20)),
  delta = list(position = "top", font = list(size = 16))
)
fig

# Violin
# Vẽ Violin Chart cho doanh thu (Sales) theo loại sản phẩm (Product Category)
ggplot(sales_data, aes(x = `Product Category`, y = Sales, fill = `Product Category`)) +
  geom_violin(trim = FALSE) +  # Vẽ violin chart, trim = FALSE để giữ nguyên phần mở rộng
  labs(title = "Violin Chart: Doanh Thu Theo Loại Sản Phẩm", 
       x = "Loại Sản Phẩm", y = "Doanh Thu") +
  theme_minimal() +  # Sử dụng theme tối giản
  theme(legend.title = element_blank())  # Loại bỏ tiêu đề trong legend

# Radar
data <- as.data.frame(matrix( sample( 2:20 , 10 , replace=T) , ncol=5))
colnames(data) <- c("Quality", "Price", "Service", "Satisfaction", "Durability")

# Thêm dòng "max" và "min" để xác định giới hạn cho trục
data <- rbind(rep(20, 5) , rep(0, 5) , data)

# Vẽ Radar Chart
radarchart(data,
           axistype = 1,  # Loại trục
           pcol = "blue",  # Màu của đường vẽ
           pfcol = rgb(0.1,0.7,0.1,0.3),  # Màu nền của khu vực
           plwd = 4,  # Độ dày của đường vẽ
           cglcol = "grey",  # Màu của các đường lưới
           cglty = 1,  # Kiểu đường lưới
           axislabcol = "black",  # Màu của các nhãn trục
           caxislabels = seq(0,20,5),  # Các nhãn trục
           title = "Đánh Giá Sản Phẩm"
)

