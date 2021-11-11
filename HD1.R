#Đọc file
my.data <- read.csv("gia_nha.csv")

#Tách những dữ liệu cần thiết
data <- my.data[, c('price', 'floors', 'condition', 'view', 'sqft_above', 'sqft_living', 'sqft_basement')]
#Loại bỏ các giá trị NA
N <- apply(is.na(data),2,which)
data <- data[complete.cases(data),]
#Chuyển hoá biến sang dạng logarit tự nhiên
data[, c('price', 'sqft_above', 'sqft_living')] <- log(data[, c('price', 'sqft_above', 'sqft_living')])
#Xuất 10 dòng đầu của data
head(data,10)

#Tính trung bình, trung vị, độ lệch chuẩn, GTNN, GTLN
mean <- apply(data, 2, mean)
median <- apply(data, 2, median)
sd <- apply(data, 2, sd)
min <- apply(data, 2, min)
max <- apply(data, 2, max)
#Vẽ đồ thị phân phối
p <- hist(data$price, col="darkblue", xlab="Price", ylab="Frequency", main="Đồ thị phân phối giá nhà") 
#Vẽ boxplot
boxplot(data$price~floors,data = data, col="pink", ylab="price", main="Phân phối của biến price cho từng nhóm phân loại của biến floors", cex.main=1)
boxplot(data$price~view,data = data, col="yellow", ylab="price", main="Phân phối của biến price cho từng nhóm phân loại của biến view", cex.main=1)
boxplot(data$price~condition,data = data, col="green", ylab="price", main="Phân phối của biến price cho từng nhóm phân loại của biến condition", cex.main=1)
#Vẽ ma trận biểu đồ phân tán
pairs(data$price~data$sqft_above, col="darkred", labels=c("price", "sqft_above"), main="Ma trận biểu đồ phân tán giữa biến price và biến sqft_above", cex.main=1) 
pairs(data$price~data$sqft_basement, col="darkblue", labels=c("price", "sqft_basement"), main="Ma trận biểu đồ phân tán giữa biến price và biến sqft_basement", cex.main=1) 
pairs(data$price~data$sqft_living, col="darkgreen", labels=c("price", "sqft_living"), main="Ma trận biểu đồ phân tán giữa biến price và biến sqft_living", cex.main=1) 

#Xây dựng mô hình hồi quy tuyến tính bội
mo_hinh1 = lm(price ~., data = data)
summary(mo_hinh1)
mo_hinh2 = lm(price ~., data = data)
summary(mo_hinh2)
#Sử dụng anova để kiểm định
anova(mo_hinh1)
anova(mo_hinh2)
anova(mo_hinh1, mo_hinh2)
plot(mo_hinh1)
plot(mo_hinh2)

#Dự báo giá nhà với diện tích ngôi nhà, diện tích khuôn viên nhà, diện tích tầng hầm tương ứng bằng với trung bình, số tằng bằng 2, điều kiện kiến trúc ở mức 3, cảnh quan xung quanh ở mức 1
x1 <- data.frame(sqft_above = mean(data$sqft_above),
                 sqft_living = mean(data$sqft_living),
                 sqft_basement = mean(data$sqft_basement),
                 floors = 2, condition = 3, view = 1)
x2 <- data.frame(sqft_above = max(data$sqft_above),
                 sqft_living = max(data$sqft_living),
                 sqft_basement = max(data$sqft_basement),
                 floors = 2, condition = 3, view = 1)
predict(mo_hinh1, x1, interval = "confidence")