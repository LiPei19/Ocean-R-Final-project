# 載入所需的套件
library(dplyr)

# 讀取資料
bird_1 <- read.table ("D:/碩士資料庫/碩三上課程/Ocean R/final project/bird/dwca-trait_454-v1.68/extendedmeasurementorfact.txt", header = TRUE, sep = "\t", fill = TRUE)

bird_2 <- read.table ("D:/碩士資料庫/碩三上課程/Ocean R/final project/bird/dwca-trait_454-v1.68/measurementorfacts.txt", header = TRUE, sep = "\t", fill = TRUE)

bird_3 <-  read.table ("D:/碩士資料庫/碩三上課程/Ocean R/final project/bird/dwca-trait_454-v1.68/taxon.txt", header = TRUE, sep = "\t", quote = "", fill = TRUE)

# 使用 quote=""： 如果檔案中引號的使用方式不標準，可以嘗試忽略引號，讓 R 自行處理。

# 選非繁殖季同種異種群聚的的資料
flocks_nbd <- bird_2 %>% 
  filter (measurementType %in% c("Flocks_ConspecificsNonBreedingSeason", 
                                 "Flocks_Mixed.speciesNonBreedingSeason"))
co_flocks_nbd <- bird_2 %>% 
  filter (measurementType %in% c("Flocks_ConspecificsNonBreedingSeason"))

mix_nbd <- bird_2 %>% 
  filter (measurementType %in% c("Flocks_Mixed.speciesNonBreedingSeason"))

# 取非Territoriality_NonBreedingSeason鳥類
t_nbs <- bird_2 %>% 
  filter (measurementType == "Territoriality_NonBreedingSeason" & measurementValue == 0)

# 選人為棲息地/非人為棲息地的資料
human_habitat <- bird_2 %>% 
  filter (measurementType %in% 
            c("Habitat_DrylandFarming",
              "Habitat_UrbanParks",
              "Habitat_UrbanRuralArea",
              "Habitat_AquaculturePonds",
              "Habitat_SaltExploitationSites",
              "Habitat_PaddyField") & 
            measurementValue == 1) %>% 
  mutate (habitat = "Anthropogenic habitat")

# 在 filter() 中，== 適合用於匹配單一值，而當有多個值需要篩選時，應使用 %in%。%in% 可以有效地一次性檢查一組值。如果直接使用 ==，只會選c()中的第一個值
nonhuman_habitat <- bird_2 %>% 
  filter (grepl ("Habitat", measurementType) 
          & !(measurementType %in% c("Habitat_DrylandFarming",
                                     "Habitat_UrbanParks",                                 
                                     "Habitat_UrbanRuralArea", 
                                     "Habitat_AquaculturePonds", 
                                     "Habitat_SaltExploitationSites",
                                     "Habitat_PaddyField")) & 
            measurementValue == 1) %>% 
  mutate (habitat = "Natural habitat")

# grepl 的使用在這裡是因為需要進行 部分文字匹配，而不是直接篩選一個完整的值。
# 部分匹配的需求： 如果 measurementType 的欄位值是類似於 "Territoriality_NonBreedingSeason" 或 "Diet_NonBreedingSeason" 等，這些值不完全等於 "NonBreedingSeason"。
# measurementvalue? 1 = yes, 0 = no

# id 只出現在人為或非人為環境，之後用一個新欄位叫人為或非人為環境，再合併taxa

habitat <- rbind (nonhuman_habitat, human_habitat)

combined_data_1 <- flocks_nbd %>%
  left_join (human_habitat, by = "id", relationship = "many-to-many")%>%
  left_join (nonhuman_habitat, by = "id", relationship = "many-to-many")
unique(combined_data_1$measurementType.x)

combined_data_1_filtered <- combined_data_1 %>%
  filter(!is.na(measurementType.y) | !is.na(measurementType)) 

combined_data_2 <- flocks_nbd %>%
  inner_join (human_habitat, by = "id", relationship = "many-to-many")%>%
  inner_join (nonhuman_habitat, by = "id", relationship = "many-to-many")

habitat <- rbind (nonhuman_habitat, human_habitat)

dou_habitat <- habitat %>%
  group_by (id) %>%
  filter (n_distinct(habitat) > 1 )

f_id_habitat <- habitat %>% filter (!id %in% dou_habitat$id)

habitat_count <- rbind (nonhuman_habitat, human_habitat) %>% 
  group_by (id, habitat) %>%
  tally ()

maj_habitat <- 
  rbind (nonhuman_habitat, human_habitat) %>% 
  group_by (id, habitat) %>%
  tally () %>% 
  group_by (id) %>%
  filter (n == max (n) & n != min(n)) %>%
  select (id, majority_habitat = habitat)

co_flock_habitat <- co_flocks_nbd %>%
  inner_join(maj_habitat, by = "id")

sum (f_taxa_co_flock_habitat$majority_habitat == "Natural habitat")
sum (f_taxa_co_flock_habitat$majority_habitat == "Anthropogenic habitat")

taxa_co_flock_habitat <- co_flock_habitat %>% inner_join(bird_3, by = "id")

f_taxa_co_flock_habitat <- taxa_co_flock_habitat %>% 
  filter (id %in% t_nbs$id)

chisq.test (f_taxa_co_flock_habitat$measurementValue, f_taxa_co_flock_habitat$majority_habitat)

# Pearson's Chi-squared test with Yates' continuity correction
# data:  f_taxa_co_flock_habitat$measurementValue and f_taxa_co_flock_habitat$majority_habitat
# X-squared = 5.6319, df = 1, p-value = 0.01764


fisher.test (f_taxa_co_flock_habitat$measurementValue, f_taxa_co_flock_habitat$majority_habitat)

# Fisher's Exact Test for Count Data
# data:  f_taxa_co_flock_habitat$measurementValue and f_taxa_co_flock_habitat$majority_habitat
# p-value = 0.007447
# alternative hypothesis: true odds ratio is not equal to 1
# 95 percent confidence interval:
#  0.01860881 0.74323565
# sample estimates:
# odds ratio 
#  0.1699566 


# %in% 是 R 語言中用於檢查集合中元素是否存在的運算符，通常用於篩選、比對或匹配資料。

# Flocks_ConspecificsNonBreedingSeason, measuremment value = 0 表示此物種不會在非繁殖季有同種群聚性

# 確認是否有相同數目habimeasurementType# 確認是否有相同數目habitat 的id
# n == max(n)：
# 只保留該 id 中 n 最大值的那一列，也就是每個 id 最常出現的 habitat。

# n != min(n)：
# 排除那些 n 等於最小值的列，這一步的目的是為了避免當 n 最大值和最小值相等時，保留這種 id（通常是 Natural Habitat 和 Anthropogenic Habitat 數量相等的情況）。

# select 只保留欄位
  
# tally() 是 dplyr 套件中的一個函數，它的主要作用是計算每個分組的數量，此數量的欄位名稱會以n表示，所以用n == max (n)

# 假設資料中有一列代表鳥類是否屬於都市環境，以及非繁殖季的群聚行為
# 如果沒有該列，需手動新增或合併其他資料
# 在這裡假設 measurementRemarks 是都市/非都市的標籤，measurementValue 表示群聚行為

# 匯總群聚行為數據
summary_data <- non_breeding_data %>% 
  group_by(measurementRemarks, measurementValue) %>% 
  summarise(count = n()) %>% 
  ungroup()

# 進行卡方檢定以檢測是否有顯著差異
contingency_table <- summary_data %>% 
  pivot_wider(names_from = measurementValue, values_from = count, values_fill = 0) %>% 
  select(-measurementRemarks) %>% 
  as.matrix()

chisq_test <- chisq.test(contingency_table)

# 顯示結果
print(chisq_test)

# 如果適用，也可以用一般化線性模型進行分析
# 假設數據是二元的 (有群聚行為:1, 無群聚行為:0)
non_breeding_data$grouping_behavior <- as.numeric(non_breeding_data$measurementValue > 0)

glm_model <- glm(grouping_behavior ~ measurementRemarks, family = binomial, data = non_breeding_data)
summary(glm_model)