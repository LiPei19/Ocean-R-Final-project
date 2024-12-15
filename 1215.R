# 載入所需的套件
library(dplyr)
library(tidyr)
library(lme4)
library(ggplot2)
library(ggthemes)

# 讀取資料
bird_1 <- read.table ("Ocean-R-Final-project/dwca-trait_454-v1.68/extendedmeasurementorfact.txt", header = TRUE, sep = "\t", fill = TRUE)

bird_2 <- read.table ("Ocean-R-Final-project/dwca-trait_454-v1.68/measurementorfacts.txt", header = TRUE, sep = "\t", fill = TRUE)

bird_3 <- read.table ("Ocean-R-Final-project/dwca-trait_454-v1.68/taxon.txt", header = TRUE, sep = "\t", quote = "", fill = TRUE)


# 日行性相對於夜行性鳥類，是否比較多的羽色二型性?

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

order <- select(bird_3, id, order)

bird_dim_act_order <- merge(bird_dim_act, order, by = "id")

# logistic

chisq.test (bird_dim_act$Type, bird_dim_act$Plumage) # p-value = 0.7822

logistic <- glm(Plumage~Type, data = bird_dim_act, family = binomial)
summary(logistic)

data_summary <- bird_dim_act %>%
  group_by(Type) %>%
  summarise(Count = n(), Plumage_Proportion = mean(Plumage == 1))
# Diurnal = 0.3167421, Nocturnal = 0.3364486

ggplot(bird_dim_act, aes(x = Type, fill = as.factor(Plumage))) +
  geom_bar(position = "fill", width = 0.6) +
  labs(
    title = "Plumage Dimorphism",
    x = "Activity Time",
    y = "Proportion",
    fill = "Plumage\nDimorphism"
  ) +
  scale_fill_manual(values = c("1" = "skyblue", "0" = "lightcoral"),labels = c("0" = "No", "1" = "Yes"))+ 
  scale_y_continuous(labels = scales::percent_format()) +
  theme_minimal()+
  annotate ("text", x = 2, y = 0.1, label = "n = 107", size = 4, color = "dimgray") +
  annotate ("text", x = 1, y = 0.1, label = "n = 442", size = 4, color = "dimgray")

# GLMM

glmm_model <- glmer(Plumage~Type + (1 | order), 
                    family = binomial, 
                    data = bird_dim_act_order)

qqnorm(residuals(glmm_model))
scatter.smooth(residuals(glmm_model) ~ fitted(glmm_model))
summary(glmm_model)

library(DHARMa)
simulationOutput <- simulateResiduals(fittedModel = glmm_model)
plot(simulationOutput)
