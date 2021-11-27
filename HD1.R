#Đọc file
my.data <- read.csv("gia_nha.csv")
#Tách những dữ liệu cần thiết
data <- my.data[, c('price', 'floors', 'condition', 'view', 'sqft_above', 'sqft_living', 'sqft_basement')]
#Loại bỏ các giá trị NA
apply(is.na(data),2,which)
data <- data[complete.cases(data),]
apply(is.na(data),2,which)
#Chuyển hoá biến sang dạng logarit tự nhiên
data$price <- ifelse(data$price==0, 0, log(data$price))
data$sqft_above <- ifelse(data$sqft_above==0, 0, log(data$sqft_above))
data$sqft_living <- ifelse(data$sqft_living==0, 0, log(data$sqft_living))
data$sqft_basement <- ifelse(data$sqft_basement==0, 0, log(data$sqft_basement))
#Tính trung bình, trung vị, độ lệch chuẩn, GTNN, GTLN
mean <- apply(data, 2, mean)
median <- apply(data, 2, median)
sd <- apply(data, 2, sd)
min <- apply(data, 2, min)
max <- apply(data, 2, max)
tbl <- data.frame("mean"=mean, "median"=median, "sd"=sd, "min"=min, "max"=max)
#Bảng thống kê số lượng
condition <- data.frame(table(data$condition))
colnames(condition) <- c("condition", "number")
floor <- data.frame(table(data$floor))
colnames(floor) <- c("floor", "number")
view <- data.frame(table(data$view))
colnames(view) <- c("view", "number")
#Vẽ đồ thị phân phối
p <- hist(data$price, col="darkblue", xlab="price", ylab="Frequency", main="Đồ thị phân phối giá nhà") 
#Vẽ boxplot
boxplot(data$price~floors,data = data, col="pink", ylab="price", main="Phân phối của biến price cho từng nhóm phân loại của biến floors", cex.main=1)
boxplot(data$price~view,data = data, col="yellow", ylab="price", main="Phân phối của biến price cho từng nhóm phân loại của biến view", cex.main=1)
boxplot(data$price~condition,data = data, col="green", ylab="price", main="Phân phối của biến price cho từng nhóm phân loại của biến condition", cex.main=1)
#Vẽ biểu đồ phân tán
pairs(data$price~data$sqft_above, col="darkred", labels=c("price", "sqft_above"), main="Ma trận biểu đồ phân tán giữa biến price và biến sqft_above", cex.main=1) 
pairs(data$price~data$sqft_basement, col="darkblue", labels=c("price", "sqft_basement"), main="Ma trận biểu đồ phân tán giữa biến price và biến sqft_basement", cex.main=1) 
pairs(data$price~data$sqft_living, col="darkgreen", labels=c("price", "sqft_living"), main="Ma trận biểu đồ phân tán giữa biến price và biến sqft_living", cex.main=1)
#Xây dựng mô hình hồi quy tuyến tính bội
mo_hinh1 <- lm(price~., data = data)
summary(mo_hinh1)
plot(mo_hinh1)
mo_hinh2 <- lm(price~view + condition + floors + sqft_living + sqft_above, data = data)
summary(mo_hinh2)
plot(mo_hinh2)
#Mo hinh tot nhat
mo_hinh <- lm(price~., data = data)
summary(mo_hinh)
plot(mo_hinh)
#Khoảng tin cậy cho các hệ số hồi quy
confint(mo_hinh)
#Dự báo giá nhà với diện tích ngôi nhà, diện tích khuôn viên nhà, diện tích tầng hầm
#tương ứng bằng với trung bình, số tằng bằng 2, điều kiện kiến trúc ở mức 3, cảnh quan xung quanh ở mức 1
x1 <- data.frame(sqft_above = mean(data$sqft_above),
                 sqft_living = mean(data$sqft_living),
                 sqft_basement = mean(data$sqft_basement),
                 floors = 2, condition = 3, view = 1)
x2 <- data.frame(sqft_above = max(data$sqft_above),
                 sqft_living = max(data$sqft_living),
                 sqft_basement = max(data$sqft_basement),
                 floors = 2, condition = 3, view = 1)
predict(mo_hinh, x1, interval = "confidence")
predict(mo_hinh, x2, interval = "confidence")
