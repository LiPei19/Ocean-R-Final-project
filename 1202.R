# 載入所需的套件
library ("dplyr")
library ("tidyr")

# 讀取資料
bird_1 <- read.table ("Ocean-R-Final-project/dwca-trait_454-v1.68/extendedmeasurementorfact.txt", header = TRUE, sep = "\t", fill = TRUE)

bird_2 <- read.table ("Ocean-R-Final-project/dwca-trait_454-v1.68/measurementorfacts.txt", header = TRUE, sep = "\t", fill = TRUE)

bird_3 <- read.table ("Ocean-R-Final-project/dwca-trait_454-v1.68/taxon.txt", header = TRUE, sep = "\t", quote = "", fill = TRUE)


# 日行性相對於夜行性鳥類 是否比較多的羽色二型性?

bird_4 <- merge(bird_2, bird_3, by = "id")

unique(bird_4$measurementType) # 確認有沒有Dimorphism, ActivityTime相關資料

Dimorphism_ActivityTime <- subset(bird_4, grepl("Dimorphism|ActivityTime", measurementType))


