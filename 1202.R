# 載入所需的套件
library ("dplyr")
library ("tidyr")

# 讀取資料
bird_1 <- read.table ("Ocean-R-Final-project/dwca-trait_454-v1.68/extendedmeasurementorfact.txt", header = TRUE, sep = "\t", fill = TRUE)

bird_2 <- read.table ("Ocean-R-Final-project/dwca-trait_454-v1.68/measurementorfacts.txt", header = TRUE, sep = "\t", fill = TRUE)

bird_3 <- read.table ("Ocean-R-Final-project/dwca-trait_454-v1.68/taxon.txt", header = TRUE, sep = "\t", quote = "", fill = TRUE)


# 日行性相對於夜行性鳥類 是否比較多的羽色二型性?

unique(bird_2$measurementType) # 確認有沒有Dimorphism, ActivityTime相關資料

Dimorphism_ActivityTime <- subset(bird_2, grepl("Dimorphism|ActivityTime", measurementType))
# grepl 的使用在這裡是因為需要進行 部分文字匹配，而不是直接篩選一個完整的值。

Dimorphism_ActivityTime_1 <- Dimorphism_ActivityTime %>% separate(col = "measurementType", 
                              sep = "_", 
                              into = c("measurement", "Type"))

Dimorphism <- Dimorphism_ActivityTime_1 %>% filter(measurement=="Dimorphism" & Type=="Plumage")
Dimorphism_1 <- Dimorphism %>% select(-measurement, -Type) %>% rename("Plumage"=measurementValue)

ActivityTime <- Dimorphism_ActivityTime_1 %>% filter(measurement=="ActivityTime" & measurementValue==1)
ActivityTime_1 <- ActivityTime %>% select(id, Type)

bird_dim_act <- merge(Dimorphism_1, ActivityTime_1, by = "id")

chisq.test (bird_dim_act$Type, bird_dim_act$Plumage) # p-value = 0.7822

fisher.test (bird_dim_act$Type, bird_dim_act$Plumage) # p-value = 0.7295
