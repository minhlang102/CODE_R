library("readxl")
#Đọc file
my.data <- read_xlsx("Folds5x2_pp.xlsx")
data <- my.data
#Loại bỏ các giá trị NA
N <- apply(is.na(my.data),2,which)
#Tính trung bình, trung vị, độ lệch chuẩn, GTNN, GTLN
mean <- apply(data, 2, mean)
median <- apply(data, 2, median)
sd <- apply(data, 2, sd)
min <- apply(data, 2, min)
max <- apply(data, 2, max)
tbl <- data.frame("mean"=mean, "median"=median, "sd"=sd, "min"=min, "max"=max)
#Vẽ đồ thị phân phối
p <- hist(data$PE, col="darkblue", xlab="PE", ylab="Frequency", main="Đồ thị phân phối của PE") 
#Vẽ boxplot
boxplot(data$AT,data = data, col="pink", ylab="AT", main="Phân phối của biến AT", cex.main=1, horizontal = TRUE)
boxplot(data$AP,data = data, col="yellow", ylab="AP", main="Phân phối của biến AP", cex.main=1, horizontal = TRUE)
boxplot(data$RH,data = data, col="green", ylab="RH", main="Phân phối của biến RH", cex.main=1, horizontal = TRUE)
boxplot(data$V,data = data, col="blue", ylab="V", main="Phân phối của biến V", cex.main=1, horizontal = TRUE)
boxplot(data$PE,data = data, col="red", ylab="PE", main="Phân phối của biến PE", cex.main=1, horizontal = TRUE)
#Vẽ biểu đồ phân tán
pairs(data$AT~data$PE, col="darkred", labels=c("AT","PE"), main="Biểu đồ phân tán giữa biến PE và biến AT", cex.main=1)
pairs(data$AP~data$PE, col="darkblue", labels=c("AP","PE"), main="Biểu đồ phân tán giữa biến PE và biến AP", cex.main=1) 
pairs(data$RH~data$PE, col="darkcyan", labels=c("RH","PE"), main="Biểu đồ phân tán giữa biến PE và biến RH", cex.main=1) 
pairs(data$V~data$PE, col="darkgreen", labels=c("V","PE"), main="Biểu đồ phân tán giữa biến PE và biến V", cex.main=1) 
#Xây dựng mô hình hồi quy tuyến tính bội
mo_hinh <- lm(PE~., data = data)
summary(mo_hinh)
plot(mo_hinh)
#Khoảng tin cậy cho các hệ số hồi quy
confint(mo_hinh)
#Dự báo sản lượng điện với diện tích ngôi nhà, diện tích khuôn viên nhà, diện tích tầng hầm
#tương ứng bằng với trung bình, số tằng bằng 2, điều kiện kiến trúc ở mức 3, cảnh quan xung quanh ở mức 1
x1 <- data.frame(AT = mean(data$AT),
                 AP = mean(data$AP),
                 RH = mean(data$RH),
                 V = mean(data$V))
x2 <- data.frame(AT = max(data$AT),
                 AP = max(data$AP),
                 RH = max(data$RH),
                 V = max(data$V))
predict(mo_hinh, x1, interval = "confidence")
predict(mo_hinh, x2, interval = "confidence")


