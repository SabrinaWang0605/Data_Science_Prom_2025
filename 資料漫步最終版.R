# 載入必要套件
library(tidyverse)      
library(haven)          
library(labelled)       
library(psych)          
library(summarytools)   
library(skimr)          
library(ggplot2)
library(ggpubr)         
library(scales)         
library(RColorBrewer)   
library(corrplot)     
library(ggcorrplot)   
library(klaR)           
library(cluster)        
library(factoextra)     
library(nnet)           
library(MASS)           
library(carData)
library(car)            
library(survival)
library(coin)         
library(rcompanion)     
library(stargazer)      
library(sjPlot)         
library(xtable)         
library(effectsize)
library(pROC)
library(lattice)
library(caret)
library(dplyr)
library(tidyr)
library(gridExtra)
library(ggrepel)
library(sf)
library(tidyverse)
library(patchwork)
library(viridisLite)
library(viridis)


# ==========
# 讀取資料
# ==========

# 資料
d1=read_sav("E:/碩一/資料漫步/data/data.sav")
write.csv(d1,"E:/碩一/資料漫步/data/data.csv",row.names = F)
my_data=read.csv("E:/碩一/資料漫步/data/data.csv",header = T)

# =======================
# 資料清理與變項建構
# =======================
data1 = my_data %>%
  dplyr::select(-c(
    "q3_other", "q4_88_text", "q8_88_text", "q9_88_text", "q10_88_text", 
    "q11_88_text", "q12_8_text", "q13_02_8_text", "q13_03_5_text", 
    "q13_04_8_text", "q13_05_5_text", "q14_01_5_text", "q14_02_5_text", 
    "q14_03_4_text", "q15_02_13_text", "q21a_6_text", "q28_4_text", 
    "q29_5_text"
  ))
data = data1 %>%
  mutate(
    #人口變項 
    性別 = factor(q1, levels = 1:2, labels = c("男", "女")),
    出生年 = q2,
    年齡 = 110 - q2,
    年齡組 = cut(年齡, 
              breaks = c(0, 25, 35, 45, 55, 100),
              labels = c("18-25歲", "26-35歲", "36-45歲", "46-55歲", "56+"),
              right = FALSE),
    # 出生縣市
    出生縣市_raw = q3,
    
    出生縣市 = case_when(
      出生縣市_raw == 1 ~ "臺北市",
      出生縣市_raw == 2 ~ "新北市",
      出生縣市_raw == 3 ~ "基隆市",
      出生縣市_raw == 4 ~ "桃園市",
      出生縣市_raw == 5 ~ "新竹市",
      出生縣市_raw == 6 ~ "新竹縣",
      出生縣市_raw == 7 ~ "苗栗縣",
      出生縣市_raw == 8 ~ "臺中市",
      出生縣市_raw == 9 ~ "彰化縣",
      出生縣市_raw == 10 ~ "南投縣",
      出生縣市_raw == 11 ~ "雲林縣",
      出生縣市_raw == 12 ~ "嘉義市",
      出生縣市_raw == 13 ~ "嘉義縣",
      出生縣市_raw == 14 ~ "臺南市",
      出生縣市_raw == 15 ~ "高雄市",
      出生縣市_raw == 16 ~ "屏東縣",
      出生縣市_raw == 17 ~ "宜蘭縣",
      出生縣市_raw == 18 ~ "花蓮縣",
      出生縣市_raw == 19 ~ "臺東縣",
      出生縣市_raw == 20 ~ "澎湖縣",
      出生縣市_raw == 21 ~ "金門縣",
      出生縣市_raw == 22 ~ "連江縣",
      出生縣市_raw == 24 ~ "其他國家",
      TRUE ~ NA_character_
    ),
    出生縣市 = factor(出生縣市),
    
    # 出生地區分類（六大區域）
    出生地區 = case_when(
      出生縣市 %in% c("臺北市", "新北市", "基隆市", "桃園市", 
                  "新竹市", "新竹縣", "宜蘭縣") ~ "北部",
      出生縣市 %in% c("苗栗縣", "臺中市", "彰化縣", "南投縣", "雲林縣") ~ "中部",
      出生縣市 %in% c("嘉義市", "嘉義縣", "臺南市") ~ "南部",
      出生縣市 %in% c("高雄市", "屏東縣") ~ "高屏",
      出生縣市 %in% c("花蓮縣", "臺東縣") ~ "東部",  
      出生縣市 %in% c("澎湖縣", "金門縣", "連江縣") ~ "離島",
      出生縣市 == "其他國家" ~ "其他國家",
      TRUE ~ NA_character_
    ),
    出生地區 = factor(出生地區, 
                  levels = c("北部", "中部", "南部", "高屏", "東部", 
                             "離島",  "其他國家")),
    # 簡化分類：台灣本島 vs. 境外
    出生地_大分類 = case_when(
      出生地區 %in% c("北部", "中部", "南部", "高屏", "東部") ~ "台灣本島",
      出生地區 == "離島" ~ "台灣離島",
      出生地區 == "其他國家" ~ "其他國家",
      TRUE ~ NA_character_
    ),
    出生地_大分類 = factor(出生地_大分類,
                     levels = c("台灣本島", "台灣離島", "其他國家")),
    #教育程度
    教育程度 = case_when(
      q4 <= 3 ~ "國小以下",
      q4 >= 4 & q4 <= 6 ~ "國高中",
      q4 >= 7 & q4 <= 15 ~ "專科",
      q4 >= 16 & q4 <= 19 ~ "大學",
      q4 >= 20 ~ "研究所",
      TRUE ~ NA_character_
    ),
    教育程度 = factor(教育程度, 
                  levels = c("國小以下", "國高中", "專科", "大學", "研究所"),
                  ordered = TRUE),
    
    # ===== Q22: 被動攻擊曝露（觀察到的不文明言論）=====
    # 檢查並處理缺失值
    q22_01_clean = if_else(
      is.na(q22_01_1) | q22_01_1 < 1 | q22_01_1 > 4, 
      1, 
      as.numeric(q22_01_1)
    ),
    q22_02_clean = if_else(
      is.na(q22_02_1) | q22_02_1 < 1 | q22_02_1 > 4, 
      1, 
      as.numeric(q22_02_1)
    ),
    q22_03_clean = if_else(
      is.na(q22_03_1) | q22_03_1 < 1 | q22_03_1 > 4, 
      1, 
      as.numeric(q22_03_1)
    ),
    q22_04_clean = if_else(
      is.na(q22_04_1) | q22_04_1 < 1 | q22_04_1 > 4, 
      1, 
      as.numeric(q22_04_1)
    ),
    q22_05_clean = if_else(
      is.na(q22_05_1) | q22_05_1 < 1 | q22_05_1 > 4, 
      1, 
      as.numeric(q22_05_1)
    ),
    
    # 五個題項
    Q22_髒話 = q22_01_clean,
    Q22_兇人 = q22_02_clean,
    Q22_罵人 = q22_03_clean,
    Q22_不雅玩笑 = q22_04_clean,
    Q22_諷刺 = q22_05_clean,
    
    # 平均分數（1-4）
    被動攻擊分數 = (Q22_髒話 + Q22_兇人 + Q22_罵人 + Q22_不雅玩笑 + Q22_諷刺) / 5,
    
    # 總分（5-20）
    被動攻擊分數_總分 = Q22_髒話 + Q22_兇人 + Q22_罵人 + Q22_不雅玩笑 + Q22_諷刺,
    
    # 分類
    被動攻擊等級 = case_when(
      被動攻擊分數 < 1.5 ~ "極低",
      被動攻擊分數 < 2.5 ~ "低",
      被動攻擊分數 < 3.5 ~ "中",
      被動攻擊分數 >= 3.5 ~ "高",
      TRUE ~ NA_character_
    ),
    被動攻擊等級 = factor(被動攻擊等級, 
                    levels = c("極低", "低", "中", "高"),
                    ordered = TRUE),
    
    高被動攻擊 = if_else(被動攻擊分數 >= 3, "高", "低"),
    
    # ===== Q23: 主動攻擊（自己使用的不文明言論）=====
    q23_01_clean = if_else(
      is.na(q23_01_1) | q23_01_1 < 1 | q23_01_1 > 4, 
      1, 
      as.numeric(q23_01_1)
    ),
    q23_02_clean = if_else(
      is.na(q23_02_1) | q23_02_1 < 1 | q23_02_1 > 4, 
      1, 
      as.numeric(q23_02_1)
    ),
    q23_03_clean = if_else(
      is.na(q23_03_1) | q23_03_1 < 1 | q23_03_1 > 4, 
      1, 
      as.numeric(q23_03_1)
    ),
    q23_04_clean = if_else(
      is.na(q23_04_1) | q23_04_1 < 1 | q23_04_1 > 4, 
      1, 
      as.numeric(q23_04_1)
    ),
    q23_05_clean = if_else(
      is.na(q23_05_1) | q23_05_1 < 1 | q23_05_1 > 4, 
      1, 
      as.numeric(q23_05_1)
    ),
    
    Q23_髒話 = q23_01_clean,
    Q23_兇人 = q23_02_clean,
    Q23_罵人 = q23_03_clean,
    Q23_不雅玩笑 = q23_04_clean,
    Q23_諷刺 = q23_05_clean,
    
    # 平均分數
    主動攻擊分數 = (Q23_髒話 + Q23_兇人 + Q23_罵人 + Q23_不雅玩笑 + Q23_諷刺) / 5,
    
    # 總分
    主動攻擊分數_總分 = Q23_髒話 + Q23_兇人 + Q23_罵人 + Q23_不雅玩笑 + Q23_諷刺,
    
    # 分類
    主動攻擊等級 = case_when(
      主動攻擊分數 < 1.5 ~ "無",
      主動攻擊分數 < 2.5 ~ "低",
      主動攻擊分數 < 3.5 ~ "中",
      主動攻擊分數 >= 3.5 ~ "高",
      TRUE ~ NA_character_
    ),
    主動攻擊等級 = factor(主動攻擊等級, 
                    levels = c("無", "低", "中", "高"),
                    ordered = TRUE),
    
    高主動攻擊 = if_else(主動攻擊分數 >= 3, "高", "低"),
    
    # ===== Q28: 抵制行為 =====
    被動抵制 = if_else(q28_1 == 1 | q28_2 == 1, 1, 0, missing = 0),
    主動表達 = if_else(q28_3 == 1, 1, 0, missing = 0),
    
    # ===== Q17/Q19: 惡搞行為 =====
    無害惡搞 = if_else(q16 == 1, "有", "無", missing = "無"),
    
    有害惡搞 = if_else(q18 == 1, "有", "無", missing = "無"),
    
    極端行為 = if_else(
      q18 == 1 & (q19_01 == 1 | q19_02 == 1), 
      1, 0, 
      missing = 0
    ),
    
    # ===== 滿意度變項 =====
    生活滿意度 = q38_01_1,
    台灣滿意度 = q38_02_1,
    快樂程度 = q39_1,
    同理心程度 = q31_1,
    
    # ===== 四象限行為分類 =====
    行為類型 = case_when(
      # 未參與：沒有任何行為
      被動抵制 == 0 & 主動表達 == 0 & 主動攻擊分數 < 2 ~ "未參與",
      
      # 被動攻擊：只有被動行為（取消關注/拒看），攻擊言論低
      被動抵制 == 1 & 主動表達 == 0 & 主動攻擊分數 < 2.5 ~ "被動攻擊",
      
      # 主動問責：有主動表達，但攻擊言論低
      主動表達 == 1 & 主動攻擊分數 < 2.5 ~ "主動問責",
      
      # 主動攻擊：有主動表達且攻擊言論高，或攻擊言論非常高
      (主動表達 == 1 & 主動攻擊分數 >= 2.5) | 
        主動攻擊分數 >= 3 ~ "主動攻擊",
      
      TRUE ~ "混合/其他"
    ),
    行為類型 = factor(
      行為類型,
      levels = c("未參與", "被動攻擊", "主動問責", "主動攻擊", "混合/其他")
    )
  )

# 所有資料補0
# 所有資料補0（修正版）
data = data %>%
  mutate(
    # 數值欄位補 0
    across(where(is.numeric), ~replace_na(., 0)),
    # 文字欄位補 "未填"
    across(where(is.character), ~replace_na(., "未填")),
    # 因子欄位補 "未填"（先轉字串再補，最後轉回因子）
    across(where(is.factor), ~factor(replace_na(as.character(.), "未填")))
  )


# ====================
# 圖(一)網路行為四象
# ====================

# 建立四象限示意圖
quadrant_data = data.frame(
  x = c(1, 1, 3, 3),
  y = c(1, 3, 1, 3),
  類型 = c("未參與", "被動攻擊", "主動問責", "主動攻擊"),
  描述 = c(
    "沒有行為\n旁觀者",
    "取消關注/拒看\n靜默抵制",
    "公開指責\n理性批評",
    "言語攻擊\n情緒宣洩"
  )
)

p_quadrant = ggplot(quadrant_data, aes(x = x, y = y)) +
  geom_point(aes(color = 類型), size = 25, alpha = 0.6) +
  geom_text(aes(label = 類型), size = 10, fontface = "bold") +
  geom_text(aes(label = 描述), vjust = 3, size = 5, lineheight = 0.9) +
  geom_hline(yintercept = 2, linetype = "dashed", color = "gray50") +
  geom_vline(xintercept = 2, linetype = "dashed", color = "gray50") +
  scale_color_manual(
    values = c(
      "未參與" = "gray70",
      "被動攻擊" = "#FFA500",
      "主動問責" = "#00BA38",
      "主動攻擊" = "#F8766D"
    )
  ) +
  scale_x_continuous(
    limits = c(0.5, 3.5),
    breaks = c(1, 3),
    labels = c("被動/無", "主動")
  ) +
  scale_y_continuous(
    limits = c(0.5, 3.5),
    breaks = c(1, 3),
    labels = c("低攻擊", "高攻擊")
  ) +
  labs(
    x = "參與程度",
    y = "攻擊性程度"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "none",
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(size = 16, face = "bold"),  # X軸標籤（被動/無、主動）
    axis.text.y = element_text(size = 16, face = "bold"),  # Y軸標籤（低攻擊、高攻擊）
    axis.title.x = element_text(size = 14, face = "bold"), # X軸標題
    axis.title.y = element_text(size = 14, face = "bold")  # Y軸標題
  )

print(p_quadrant)


# ====================================
# 圖(二)樣本台灣人口分布圖
# ====================================
# 計算各縣市樣本數（只計算台灣地區）
sample_dist_taiwan = data %>%
  filter(出生地_大分類 %in% c("台灣本島", "台灣離島")) %>%  # 只保留台灣
  count(出生縣市, name = "樣本數") %>%
  mutate(
    百分比 = 樣本數 / sum(樣本數) * 100,
    標籤 = paste0(樣本數, "\n(", round(百分比, 1), "%)")
  )

cat("=== 台灣地區樣本分布 ===\n")
print(sample_dist_taiwan %>% arrange(desc(樣本數)))

# 境外樣本統計
foreign_count <- data %>%
  filter(出生地_大分類 %in% "其他國家") %>%
  count(出生地_大分類)

cat("\n=== 境外樣本 ===\n")
print(foreign_count)


# 下載與合併地圖
taiwan_map = st_read("https://raw.githubusercontent.com/g0v/twgeojson/master/json/twCounty2010.geo.json")
head(taiwan_map)
# 修復地圖
taiwan_map = st_make_valid(taiwan_map)
taiwan_map = st_transform(taiwan_map, crs=3826)

# 建立縣市名稱
taiwan_map = taiwan_map %>%
  mutate(
    縣市名稱 = if("COUNTYNAME" %in% names(.)) {
      str_replace(COUNTYNAME, "台", "臺")
    } else if("name" %in% names(.)) {
      str_replace(name, "台", "臺")
    } else {
      # 如果都沒有，使用第一個文字欄位
      names(.)[1]
    }
  )
# 計算樣本數
sample_dist = data %>%
  filter(出生地_大分類 %in% c("台灣本島", "台灣離島")) %>%
  count(出生縣市, name = "樣本數") %>%
  mutate(百分比 = 樣本數 / sum(樣本數) * 100)

# 合併資料
map_data = taiwan_map %>%
  left_join(sample_dist, by = c("縣市名稱" = "出生縣市")) %>%
  mutate(樣本數 = replace_na(樣本數, 0))

map_data = map_data %>%
  mutate(
    樣本等級 = cut(
      樣本數,
      breaks = c(-1, 0, 50, 100, 200, 300, Inf),
      labels = c("0人", "1-50人", "51-100人", "101-200人", "201-300人", "300人以上")
    )
  )
# 計算縣市中心點（用於標註）
centroids = st_centroid(map_data)

# 繪製地圖
p_map = ggplot(map_data) +
  geom_sf(aes(fill = 百分比), color = "white", size = 0.5) +
  geom_text_repel(
    data = centroids %>% 
      mutate(
        lon = st_coordinates(.)[,1],
        lat = st_coordinates(.)[,2],
        標籤 = if_else(
          樣本數 > 0,
          paste0(縣市名稱, "\n", sprintf("%.1f", 百分比), "%"),
          縣市名稱
        )
      ),
    aes(x = lon, y = lat, label = 標籤),
    size = 3.5,
    fontface = "bold",
    color = "black",
    bg.color = "white",
    bg.r = 0.15,
    box.padding = 0.5,
    point.padding = 0.3,
    segment.color = "grey50",
    segment.size = 0.3,
    max.overlaps = Inf,
    min.segment.length = 0,
    lineheight = 0.85
  ) +
  
  scale_fill_gradient(
    low = "#EFF3FF", 
    high = "#08519C",
    name = "百分比 (%)",
    na.value = "grey95"
  ) +
  
  labs(
    title = paste0("樣本台灣分布 (N=", sum(sample_dist$樣本數), ")"),
    subtitle = "依出生縣市統計"
  ) +
  
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    legend.position = "right"
  )
print(p_map)

# 完整出生地統計表
birth_summary = data %>%
  count(出生地_大分類, name = "樣本數") %>%
  mutate(百分比 = 樣本數 / sum(樣本數) * 100) %>%
  arrange(desc(樣本數))

cat("\n=== 完整出生地統計 ===\n")
print(birth_summary)

# 2. 台灣地區詳細統計
taiwan_detail = data %>%
  filter(出生地_大分類 %in% c("台灣本島", "台灣離島")) %>%
  count(出生地區, name = "樣本數") %>%
  mutate(百分比 = 樣本數 / sum(樣本數) * 100) %>%
  arrange(desc(樣本數))

cat("\n=== 台灣地區詳細統計 ===\n")
print(taiwan_detail)

# 3. 境外樣本詳細資訊
if(sum(data$出生地_大分類 == "其他國家", na.rm = TRUE) > 0) {
  other_countries_detail = data %>%
    filter(出生地_大分類 == "其他國家")
  
  cat("\n=== 其他國家詳細資訊 ===\n")
  print(other_countries_detail)
}

# ====================
# 圖(三)網路不妥行為
# ====================
# ===== 準備長形數據用於 Likert 圖 =====
df_long <- data %>%
  dplyr::select(
    # 被動攻擊 (觀察到的不文明言論)
    "q22_01_1", "q22_02_1", "q22_03_1", "q22_04_1", "q22_05_1",
    # 主動攻擊 (自己使用的不文明言論)
    "q23_01_1", "q23_02_1", "q23_03_1", "q23_04_1", "q23_05_1"
  ) %>%
  # 轉換為數值
  mutate(across(everything(), ~as.numeric(as.character(.)))) %>%
  # 轉為長形格式
  pivot_longer(
    cols = everything(),
    names_to = "題目代碼",
    values_to = "分數"
  ) %>%
  # 設定 Likert 等級標籤
  mutate(
    分數 = factor(分數, 
                levels = 1:4, 
                labels = c("從來沒有", "很少", "有時", "經常"))
  ) %>%
  # 添加可讀的題目標籤
  mutate(
    題目 = case_when(
      題目代碼 == "q22_01_1" ~ "看到別人\n留言用髒話",
      題目代碼 == "q22_02_1" ~ "看到別人兇\n那些激怒他的人",
      題目代碼 == "q22_03_1" ~ "看到別人\n罵他討厭的人",
      題目代碼 == "q22_04_1" ~ "看到別人\n開不雅玩笑",
      題目代碼 == "q22_05_1" ~ "看到別人\n諷刺他人",
      題目代碼 == "q23_01_1" ~ "留言用髒話",
      題目代碼 == "q23_02_1" ~ "兇那些\n激怒他的人",
      題目代碼 == "q23_03_1" ~ "罵他討厭的人",
      題目代碼 == "q23_04_1" ~ "開不雅玩笑",
      題目代碼 == "q23_05_1" ~ "諷刺他人"
    ),
    # 分類類型
    類型 = if_else(str_detect(題目代碼, "q22"), "被動攻擊\n(觀察他人)", "主動攻擊\n(自己使用)")
  )

# 繪製 Likert 堆疊圖
p1 <- ggplot(df_long, aes(x = factor(題目代碼, levels = c("q22_01_1", "q22_02_1", "q22_03_1", "q22_04_1", "q22_05_1",
                                                      "q23_01_1", "q23_02_1", "q23_03_1", "q23_04_1", "q23_05_1")), fill = 分數)) +
  geom_bar(position = "fill") +
  geom_text(
    stat = "count",
    aes(label = paste0(
      round((after_stat(count)) / tapply(after_stat(count), after_stat(x), sum)[after_stat(x)] * 100, 1), 
      "%"
    )),
    position = position_fill(vjust = 0.5),
    color = "black",
    size = 3,
    fontface = "bold"
  ) +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_brewer(palette = "RdYlBu", direction = -1) +
  labs(
    title = "網路不妥行為",
    x = " ",
    y = " ",
    fill = "頻率選項"
  ) +
  theme_minimal(base_size = 14) +
  coord_flip() +
  theme(
    axis.text.x = element_text(size = 13, face = "bold"),
    axis.text.y = element_text(size = 10, face = "bold"),
    plot.title = element_text(size = 13, face = "bold", hjust = 0.5),
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 10, face = "bold")
  )

print(p1)

# ====================
# 圖(四)查證頻率圖
# ====================
df_long1 <- data %>%
  dplyr::select(starts_with("q24")) %>%
  mutate(across(everything(), ~as.numeric(as.character(.)))) %>%
  pivot_longer(
    cols = everything(),
    names_to = "題目代碼",
    values_to = "分數"
  ) %>%
  mutate(
    分數 = factor(分數, levels = 1:4, labels = c("從來沒有", "很少", "有時", "經常")),
    題目標籤 = case_when(
      題目代碼 == "q24_01_1" ~ "看不同意見",
      題目代碼 == "q24_02_1" ~ "網路查證",
      題目代碼 == "q24_03_1" ~ "傳統媒體查證",
      題目代碼 == "q24_04_1" ~ "因查證改想法",
      題目代碼 == "q24_05_1" ~ "確認新聞來源"
    )
  )

p2 <- ggplot(df_long1, aes(x = factor(題目代碼, levels = c("q24_01_1", "q24_02_1", "q24_03_1", "q24_04_1", "q24_05_1")), 
                           fill = 分數)) +
  geom_bar(position = "fill") +
  geom_text(
    stat = "count",
    aes(label = paste0(
      round((after_stat(count)) / tapply(after_stat(count), after_stat(x), sum)[after_stat(x)] * 100, 1), "%"
    )),
    position = position_fill(vjust = 0.5),
    color = "black",
    size = 3,
    fontface = "bold",
    check_overlap = FALSE
  ) +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(
    values = c(
      "從來沒有" = "#2166AC",
      "很少" = "#92C5DE",
      "有時" = "#F4A582",
      "經常" = "#B2182B"
    )
  ) +
  scale_x_discrete(labels = c("看不同意見", "網路查證", "傳統媒體查證", "因查證改想法", "確認新聞來源")) +
  labs(title = "查證與想法", x = "", y = "", fill = "頻率選項") +
  theme_minimal(base_size = 10) +
  coord_flip() +
  theme(
    axis.text.x = element_text(size = 13, face = "bold"),
    axis.text.y = element_text(size = 11, face = "bold"),
    plot.title = element_text(size = 13, face = "bold", hjust = 0.5),
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 10, face = "bold")
  )
print(p2)

# ===================================
# 圖(五)認為攻擊性言論影響他人頻率圖
# ===================================
df_long2 <- data %>%
  dplyr::select(starts_with("q26")) %>%
  mutate(across(everything(), ~as.numeric(as.character(.)))) %>%
  pivot_longer(
    cols = everything(),
    names_to = "題目代碼",
    values_to = "分數"
  ) %>%
  mutate(
    分數 = factor(分數, levels = 1:4, labels = c("從來沒有", "很少", "有時", "經常")),
    題目標籤 = case_when(
      題目代碼 == "q26_01_1" ~ "影響他人也罵人",
      題目代碼 == "q26_02_1" ~ "讓他人增加罵人",
      題目代碼 == "q26_03_1" ~ "被激怒而罵人"
    )
  )

p3 <- ggplot(df_long2,aes(x = factor(題目代碼, levels = c("q26_01_1", "q26_02_1", "q26_03_1")), fill = 分數)) +
  geom_bar(position = "fill") +
  geom_text(
    stat = "count",
    aes(label = paste0(
      round((after_stat(count)) / tapply(after_stat(count), after_stat(x), sum)[after_stat(x)] * 100, 1), "%"
    )),
    position = position_fill(vjust = 0.5),
    color = "black",
    size = 3,
    fontface = "bold",
    check_overlap = FALSE
  ) +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(
    values = c(
      "從來沒有" = "#2166AC",
      "很少" = "#92C5DE",
      "有時" = "#F4A582",
      "經常" = "#B2182B"
    )
  ) +
  scale_x_discrete(labels = c("影響他人也罵人", "讓他人增加罵人", "被激怒而罵人")) +
  labs(title = "影響他人想法", x = "", y = "", fill = "頻率選項") +
  theme_minimal(base_size = 10) +
  coord_flip() +
  theme(
    axis.text.x = element_text(size = 13, face = "bold"),
    axis.text.y = element_text(size = 11, face = "bold"),
    plot.title = element_text(size = 13, face = "bold", hjust = 0.5),
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 10, face = "bold")
  )
print(p3)

# =================================
# 圖(六)對於用社群抵制名人行為意願
# =================================
df_long3 <- data %>%
  dplyr::select(starts_with("q27")) %>%
  mutate(across(everything(), ~as.numeric(as.character(.)))) %>%
  pivot_longer(
    cols = everything(),
    names_to = "題目代碼",
    values_to = "分數"
  ) %>%
  mutate(
    分數 = factor(分數, levels = 1:5, labels = c("非常不想", "不想", "普通", "想", "非常想")),
    題目標籤 = "想透過社群抵制名人"
  )

p4 <- ggplot(df_long3, aes(x = 題目代碼, fill = 分數)) +
  geom_bar(position = "fill") +
  geom_text(
    stat = "count",
    aes(label = paste0(
      round((after_stat(count)) / tapply(after_stat(count), after_stat(x), sum)[after_stat(x)] * 100, 1), "%"
    )),
    position = position_fill(vjust = 0.5),
    color = "black",
    size = 3,
    fontface = "bold",
    check_overlap = FALSE
  ) +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(
    values = c(
      "非常不想" = "#D73027",
      "不想" = "#FC8D59",
      "普通" = "#FEE090",
      "想" = "#91BFDB",
      "非常想" = "#4575B4"
    )
  ) +
  scale_x_discrete(labels = "想透過社群抵制名人") +
  labs(title = "抵制的想法", x = "", y = "", fill = "頻率選項") +
  theme_minimal(base_size = 10) +
  coord_flip() +
  theme(
    axis.text.x = element_text(size = 13, face = "bold"),
    axis.text.y = element_text(size = 11, face = "bold"),
    plot.title = element_text(size = 13, face = "bold", hjust = 0.5),
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 10, face = "bold")
  )
print(p4)

# ==============================================
# 圖(七) 網路抵制參與者的行為類型與心理因素分布
# ==============================================
# ============= p5: 社群媒體抵制是否有效 (Q30) =============
df_long4 <- data %>%
  dplyr::select(starts_with("q30")) %>%
  mutate(across(everything(), ~as.numeric(as.character(.)))) %>%
  pivot_longer(
    cols = everything(),
    names_to = "題目代碼",
    values_to = "分數"
  ) %>%
  mutate(
    分數 = factor(分數, levels = 1:5, labels = c("完全沒有效", "沒有效", "普通", "有效", "非常有效"))
  ) %>%
  filter(!is.na(分數))

p5 <- ggplot(df_long4, aes(x = 題目代碼, fill = 分數)) +
  geom_bar(position = "fill") +
  geom_text(
    stat = "count",
    aes(label = paste0(
      round((after_stat(count)) / tapply(after_stat(count), after_stat(x), sum)[after_stat(x)] * 100, 1), "%"
    )),
    position = position_fill(vjust = 0.5),
    color = "black",
    size = 3
  ) +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_brewer(palette = "RdYlBu", direction = -1) +
  scale_x_discrete(labels = "社群媒體抵制\n是否有效") +
  labs(title = "透過社群媒體抵制\n是否有效", x = "", y = "", fill = "頻率選項") +
  theme_minimal(base_size = 10) +
  theme(
    plot.title = element_text(size = 13, face = "bold", hjust = 0.5)
  )
print(p5)
# ============= p6: 多常想像被抵制的心情 (Q31) =============
df_long5 <- data %>%
  dplyr::select(starts_with("q31")) %>%
  mutate(across(everything(), ~as.numeric(as.character(.)))) %>%
  pivot_longer(
    cols = everything(),
    names_to = "題目代碼",
    values_to = "分數"
  ) %>%
  mutate(
    分數 = factor(分數, levels = 1:4, labels = c("從來沒有", "很少", "有時", "經常"))
  ) %>%
  filter(!is.na(分數))

p6 <- ggplot(df_long5, aes(x = 題目代碼, fill = 分數)) +
  geom_bar(position = "fill") +
  geom_text(
    stat = "count",
    aes(label = paste0(
      round((after_stat(count)) / tapply(after_stat(count), after_stat(x), sum)[after_stat(x)] * 100, 1), "%"
    )),
    position = position_fill(vjust = 0.5),
    color = "black",
    size = 3
  ) +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_brewer(palette = "RdYlBu", direction = -1) +
  scale_x_discrete(labels = "多常會設身處地\n想像被抵制的心情") +
  labs(title = "多常想像被抵制的心情", x = "", y = "", fill = "頻率選項") +
  theme_minimal(base_size = 10) +
  theme(
    plot.title = element_text(size = 13, face = "bold", hjust = 0.5)
  )
print(p6)
# ============= p7: 社群媒體抵制名人的嚴重程度 (Q32) =============
df_long7 <- data %>%
  dplyr::select(starts_with("q32")) %>%
  mutate(across(everything(), ~as.numeric(as.character(.)))) %>%
  pivot_longer(
    cols = everything(),
    names_to = "題目代碼",
    values_to = "分數"
  ) %>%
  mutate(
    分數 = factor(分數, levels = 1:5, labels = c("非常不嚴重", "不嚴重", "普通", "嚴重", "非常嚴重"))
  ) %>%
  filter(!is.na(分數))

p7 <- ggplot(df_long7, aes(x = 題目代碼, fill = 分數)) +
  geom_bar(position = "fill") +
  geom_text(
    stat = "count",
    aes(label = paste0(
      round((after_stat(count)) / tapply(after_stat(count), after_stat(x), sum)[after_stat(x)] * 100, 1), "%"
    )),
    position = position_fill(vjust = 0.5),
    color = "black",
    size = 3
  ) +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_brewer(palette = "RdYlBu", direction = -1) +
  scale_x_discrete(labels = "覺得社群媒體抵制名人\n對他們的傷害嚴不嚴重") +
  labs(title = "社群媒體抵制名人的嚴重程度", x = "", y = "", fill = "頻率選項") +
  theme_minimal(base_size = 10) +
  theme(
    plot.title = element_text(size = 13, face = "bold", hjust = 0.5)
  )
print(p7)
# ============= p8: 社群媒體抵制的重要程度 (Q33) =============
df_long8 <- data %>%
  dplyr::select(starts_with("q33")) %>%
  mutate(across(everything(), ~as.numeric(as.character(.)))) %>%
  pivot_longer(
    cols = everything(),
    names_to = "題目代碼",
    values_to = "分數"
  ) %>%
  mutate(
    分數 = factor(分數, levels = 1:5, labels = c("非常不重要", "不重要", "普通", "重要", "非常重要"))
  ) %>%
  filter(!is.na(分數))

p8 <- ggplot(df_long8, aes(x = 題目代碼, fill = 分數)) +
  geom_bar(position = "fill") +
  geom_text(
    stat = "count",
    aes(label = paste0(
      round((after_stat(count)) / tapply(after_stat(count), after_stat(x), sum)[after_stat(x)] * 100, 1), "%"
    )),
    position = position_fill(vjust = 0.5),
    color = "black",
    size = 3
  ) +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_brewer(palette = "RdYlBu", direction = -1) +
  scale_x_discrete(labels = "覺得社群媒體抵制\n是否重要") +
  labs(title = "社群媒體抵制的重要程度", x = "", y = "", fill = "頻率選項") +
  theme_minimal(base_size = 10) +
  theme(
    plot.title = element_text(size = 13, face = "bold", hjust = 0.5)
  )
print(p8)
# ============= 組合顯示 p5-p8 =============
grid.arrange(p5, p6, p7, p8, ncol = 2)


# ========================================
# 圖(八) 抵制參與者對行動成效的滿意度評估
# ========================================
# ============= p9: 參加社群媒體抵制的損失 (Q34) =============
df_long9 <- data %>%
  dplyr::select(starts_with("q34")) %>%
  mutate(across(everything(), ~as.numeric(as.character(.)))) %>%
  pivot_longer(
    cols = everything(),
    names_to = "題目代碼",
    values_to = "分數"
  ) %>%
  mutate(
    分數 = factor(分數, levels = 1:5, labels = c("非常少", "有點少", "普通", "有點多", "非常多"))
  ) %>%
  filter(!is.na(分數))

p9 <- ggplot(df_long9, aes(x = 題目代碼, fill = 分數)) +
  geom_bar(position = "fill") +
  geom_text(
    stat = "count",
    aes(label = paste0(
      round((after_stat(count)) / tapply(after_stat(count), after_stat(x), sum)[after_stat(x)] * 100, 1), "%"
    )),
    position = position_fill(vjust = 0.5),
    color = "black",
    size = 3
  ) +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_brewer(palette = "RdYlBu", direction = -1) +
  scale_x_discrete(labels = "參加社群媒體\n抵制損失有多少") +
  labs(title = "參加社群媒體抵制的損失", x = "", y = "", fill = "頻率選項") +
  theme_minimal(base_size = 10) +
  theme(
    plot.title = element_text(size = 13, face = "bold", hjust = 0.5)
  )
print(p9)
# ============= p10: 一起參加社群媒體抵制的人 (Q35) =============
df_long10 <- data %>%
  dplyr::select(starts_with("q35")) %>%
  mutate(across(everything(), ~as.numeric(as.character(.)))) %>%
  pivot_longer(
    cols = everything(),
    names_to = "題目代碼",
    values_to = "分數"
  ) %>%
  mutate(
    分數 = factor(分數, levels = 1:5, labels = c("非常少", "有點少", "普通", "有點多", "非常多"))
  ) %>%
  filter(!is.na(分數))

p10 <- ggplot(df_long10, aes(x = 題目代碼, fill = 分數)) +
  geom_bar(position = "fill") +
  geom_text(
    stat = "count",
    aes(label = paste0(
      round((after_stat(count)) / tapply(after_stat(count), after_stat(x), sum)[after_stat(x)] * 100, 1), "%"
    )),
    position = position_fill(vjust = 0.5),
    color = "black",
    size = 3
  ) +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_brewer(palette = "RdYlBu", direction = -1) +
  scale_x_discrete(labels = "一起參加社群媒體\n抵制的人多不多") +
  labs(title = "一起參加社群媒體抵制", x = "", y = "", fill = "頻率選項") +
  theme_minimal(base_size = 10) +
  theme(
    plot.title = element_text(size = 13, face = "bold", hjust = 0.5)
  )
print(p10)
# ============= p11: 朋友批評不參加的人 (Q36) =============
df_long11 <- data %>%
  dplyr::select(starts_with("q36")) %>%
  mutate(across(everything(), ~as.numeric(as.character(.)))) %>%
  pivot_longer(
    cols = everything(),
    names_to = "題目代碼",
    values_to = "分數"
  ) %>%
  mutate(
    分數 = factor(分數, levels = 1:4, labels = c("從來沒有", "很少", "有時", "經常"))
  ) %>%
  filter(!is.na(分數))

p11 <- ggplot(df_long11, aes(x = 題目代碼, fill = 分數)) +
  geom_bar(position = "fill") +
  geom_text(
    stat = "count",
    aes(label = paste0(
      round((after_stat(count)) / tapply(after_stat(count), after_stat(x), sum)[after_stat(x)] * 100, 1), "%"
    )),
    position = position_fill(vjust = 0.5),
    color = "black",
    size = 3
  ) +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_brewer(palette = "RdYlBu", direction = -1) +
  scale_x_discrete(labels = "朋友多常會批評那些\n不參加的人的頻率") +
  labs(title = "朋友多常會批評不參加的人", x = "", y = "", fill = "頻率選項") +
  theme_minimal(base_size = 10) +
  theme(
    plot.title = element_text(size = 13, face = "bold", hjust = 0.5)
  )
print(p11)
# ============= 組合顯示 p9-p11 =============
grid.arrange(p9, p10, p11, ncol = 2)


# ======================================================================================
# 核心研究變項:被動攻擊分數, 主動攻擊分數,道德正當化指數,生活滿意度, 快樂程度,同理心程度
# ======================================================================================

#主被動攻擊行為
upgrade_data <- data %>%
  mutate(
    被動攻擊等級 = case_when(
      被動攻擊分數 < 2 ~ "低被動",
      被動攻擊分數 < 3 ~ "中被動",
      TRUE ~ "高被動"
    ),
    被動攻擊等級 = factor(被動攻擊等級, levels = c("低被動", "中被動", "高被動")),
    
    主動攻擊等級 = case_when(
      主動攻擊分數 < 1.5 ~ "低攻擊",
      主動攻擊分數 < 2.5 ~ "中攻擊",
      TRUE ~ "高攻擊"
    ),
    主動攻擊等級 = factor(主動攻擊等級, levels = c("低攻擊", "中攻擊", "高攻擊")),
    
    行為類型 = factor(行為類型, levels = c("未參與", "被動攻擊", "主動問責", "主動攻擊", "混合/其他"))
  ) %>%
  count(被動攻擊等級, 主動攻擊等級, 行為類型) %>%
  group_by(被動攻擊等級, 主動攻擊等級) %>%
  mutate(pct = n / sum(n) * 100) %>%
  ungroup()

# ====================================
# 圖(九)道德正當化程度與攻擊行為
# ====================================

# 道德正當化指數
data = data %>%
  mutate(
    道德正當化指數 = (q20_01_1 + q20_02_1 + q25_01_1 + q25_02_1 + q25_03_1 + q25_04_1) / 6,
    道德正當化程度 = case_when(
      道德正當化指數 < 2 ~ "低正當化",
      道德正當化指數 < 3 ~ "中正當化",
      TRUE ~ "高正當化"
    ),
    道德正當化程度 = factor(道德正當化程度, 
                     levels = c("低正當化", "中正當化", "高正當化"))
  )
moral_data = data %>%
  count(道德正當化程度, 行為類型) %>%
  group_by(道德正當化程度) %>%
  mutate(pct = n / sum(n) * 100) %>%
  ungroup()
p2_moral = ggplot(moral_data, 
                  aes(x = 道德正當化程度, y = 行為類型, fill = pct)) +
  geom_tile(color = "white", linewidth = 2) +
  geom_text(aes(label = paste0(round(pct, 1), "%\n(n=", n, ")")), 
            color = "black", size = 5, fontface = "bold", lineheight = 0.9) +
  scale_fill_gradient2(
    low = "#2166AC", 
    mid = "white", 
    high = "#B2182B",
    midpoint = 20, 
    name = "比例 (%)"
  ) +
  labs(
    title = "道德正當化 × 行為類型",
    subtitle = "正當化程度越高，越傾向極端攻擊行為",
    x = "道德正當化程度",
    y = "行為類型"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5, color = "#8B0000", size = 11),
    axis.text.x = element_text(size = 11, face = "bold"),
    axis.text.y = element_text(size = 11, face = "bold"),
    legend.position = "right",
    panel.grid = element_blank(),
    plot.margin = margin(10, 10, 10, 10)
  )

print(p2_moral)

#生活滿意度,快樂程度
emotion_data = data %>%
  mutate(
    生活滿意度_等級 = case_when(
      生活滿意度 <= 2 ~ "不滿意",
      生活滿意度 == 3 ~ "普通",
      TRUE ~ "滿意"
    ),
    生活滿意度_等級 = factor(生活滿意度_等級, 
                      levels = c("不滿意", "普通", "滿意")),
    
    快樂程度_等級 = case_when(
      快樂程度 <= 2 ~ "不快樂",
      快樂程度 == 3 ~ "普通",
      TRUE ~ "快樂"
    ),
    快樂程度_等級 = factor(快樂程度_等級, 
                     levels = c("不快樂", "普通", "快樂")),
    
    主動攻擊_等級 = case_when(
      主動攻擊分數 < 1.5 ~ "低攻擊",
      主動攻擊分數 < 2.5 ~ "中攻擊",
      TRUE ~ "高攻擊"
    ),
    主動攻擊_等級 = factor(主動攻擊_等級, 
                     levels = c("低攻擊", "中攻擊", "高攻擊"))
  ) %>%
  count(生活滿意度_等級, 快樂程度_等級, 主動攻擊_等級) %>%
  group_by(生活滿意度_等級, 快樂程度_等級) %>%
  mutate(pct = n / sum(n) * 100) %>%
  ungroup()

# =========================
#圖(十)同理心程度與行為類型
# =========================

empathy_data = data %>%
  mutate(
    同理心等級 = case_when(
      同理心程度 <= 2 ~ "低同理心",
      同理心程度 == 3 ~ "中同理心",
      同理心程度 == 4 ~ "高同理心",
      TRUE ~ NA_character_
    ),
    同理心等級 = factor(同理心等級, 
                   levels = c("低同理心", "中同理心", "高同理心")),
    
    極端行為 = if_else(q18 == 1 & (q19_01 == 1 | q19_02 == 1), "有極端行為", "無極端行為"),
    極端行為 = factor(極端行為, levels = c("無極端行為", "有極端行為"))
  ) %>%
  filter(!is.na(同理心等級)) %>%
  count(同理心等級, 極端行為, 行為類型) %>%
  group_by(同理心等級) %>%
  mutate(
    同理心等級總數 = sum(n),
    pct = (n / 同理心等級總數) * 100
  ) %>%
  ungroup()


p4_empathy = ggplot(empathy_data, 
                    aes(x = 極端行為, y = 行為類型, fill = pct)) +
  geom_tile(color = "white", linewidth = 1.5) +
  geom_text(aes(label = paste0(round(pct, 1), "%\n(n=", n, ")")), 
            color = "black", size = 3.5, fontface = "bold", lineheight = 0.9) +
  facet_wrap(~同理心等級, ncol = 3) +
  scale_fill_gradientn(
    colors = c(
      "#4A90E2",      
      "white",      
      "skyblue",      
      "gold",
      "pink",
      "#C62828"       
    ),
    name = "比例 (%)",
    limits = c(0, 100),
    breaks = seq(0, 100, by = 20)
  )  +
  labs(
    title = "同理心 × 極端行為 × 行為類型",
    subtitle = "高同理心作為保護因子：降低極端行為參與",
    x = "是否參與極端行為",
    y = "行為類型"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(size = 13, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5, color = "#00008B", size = 10),
    strip.text = element_text(size = 11, face = "bold", color = "white"),
    strip.background = element_rect(fill = "#00008B", color = NA),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
    axis.text.y = element_text(size = 9),
    legend.position = "right",
    panel.border = element_rect(color = "gray80", fill = NA),
    plot.margin = margin(10, 10, 10, 10)
  )

print(p4_empathy)

# ====================================
#圖(十一)Pearson和Spearman 相關矩陣圖
# ====================================

# 準備相關矩陣資料
cor_data = data %>%
  dplyr::select(
    被動攻擊分數, 
    主動攻擊分數,
    道德正當化指數,
    生活滿意度, 
    快樂程度,
    同理心程度,
    年齡
  ) %>%
  na.omit()

library(Hmisc)
# Pearson相關
cor_pearson = rcorr(as.matrix(cor_data), type = "pearson")
r_pearson = cor_pearson$r
p_pearson = cor_pearson$P
cat("Pearson 相關計算完成\n")

# Spearman相關
cor_spearman = rcorr(as.matrix(cor_data), type = "spearman")
r_spearman = cor_spearman$r
p_spearman = cor_spearman$P
cat("Spearman 相關計算完成\n")

#計算差異
diff_matrix = abs(r_pearson - r_spearman)
avg_diff = mean(diff_matrix[upper.tri(diff_matrix)], na.rm = TRUE)

#差異統計
diff_summary = data.frame(
  ranges = c("< 0.01","0.01-0.02","0.02-0.05",">0.05"),
  nums=c(
    sum(diff_matrix[upper.tri(diff_matrix)] < 0.01),
    sum(diff_matrix[upper.tri(diff_matrix)] >= 0.01 &
          diff_matrix[upper.tri(diff_matrix)] < 0.02),
    sum(diff_matrix[upper.tri(diff_matrix)] >= 0.02 &
          diff_matrix[upper.tri(diff_matrix)] < 0.05),
    sum(diff_matrix[upper.tri(diff_matrix)] >= 0.05)
  )
)
print(diff_summary)
# 轉為長格式
compare_long = expand.grid(
  b1 = rownames(r_pearson),
  b2 = colnames(r_pearson),
  stringsAsFactors = FALSE
) %>%
  mutate(
    r_pearson  = mapply(function(x, y) r_pearson[x, y], b1, b2),
    r_spearman = mapply(function(x, y) r_spearman[x, y], b1, b2),
    p_pearson  = mapply(function(x, y) p_pearson[x, y], b1, b2),
    p_spearman = mapply(function(x, y) p_spearman[x, y], b1, b2),
    sig_pearson = case_when(
      is.na(p_pearson) ~ "",
      p_pearson < 0.001 ~ "***",
      p_pearson < 0.01 ~ "**",
      p_pearson < 0.05 ~ "*",
      TRUE ~ ""
    ),
    sig_spearman = case_when(
      is.na(p_spearman) ~ "",
      p_spearman < 0.001 ~ "***",
      p_spearman < 0.01 ~ "**",
      p_spearman < 0.05 ~ "*",
      TRUE ~ ""
    )
  )
compare_long$idx_b1 = match(compare_long$b1, rownames(r_pearson))
compare_long$idx_b2 = match(compare_long$b2, colnames(r_pearson))
compare_long$loc = ifelse(
  compare_long$idx_b1 == compare_long$idx_b2,
  "對角線",
  ifelse(
    compare_long$idx_b1 < compare_long$idx_b2,
    "上三角",
    "下三角"
  )
)

# ✅ 最后用 mutate 添加标签和色彩
compare_long = compare_long %>%
  mutate(
    labels = case_when(
      loc == "對角線" ~ "1.00",
      loc == "上三角" ~ paste0(format(round(r_pearson, 3), nsmall = 3), sig_pearson),
      TRUE ~ paste0(format(round(r_spearman, 3), nsmall = 3), sig_spearman)
    ),
    full = case_when(
      loc == "對角線" ~ 1.0,
      loc == "上三角" ~ r_pearson,
      TRUE ~ r_spearman
    )
  )


table(compare_long$loc)
compare_long %>% group_by(loc) %>% summarise(count = n())

p_compare_main = ggplot(compare_long, 
                        aes(x = b2, y = b1, fill = full)) +
  geom_tile(color = "white", linewidth = 1.5) +
  geom_text(aes(label = labels), 
            color = "black", size = 4.5, fontface = "bold") +
  scale_fill_gradient2(
    low = "#2166AC",    # 藍色（負相關）
    mid = "white",      # 白色（無相關）
    high = "#B2182B",   # 紅色（正相關）
    midpoint = 0,
    limits = c(-1, 1),
    name = "Pearson r /\nSpearman ρ"
  ) +
  scale_y_discrete(limits = rev) +
  labs(
    title = "Pearson vs Spearman 相關對比",
    subtitle = "上三角 = Pearson r | 下三角 = Spearman ρ | *** p<.001, ** p<.01, * p<.05",
    caption = paste0("N = ", nrow(cor_data),
                     " | 平均差異 = ", round(avg_diff, 3))
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5, size = 11, lineheight = 1.2),
    plot.caption = element_text(hjust = 1, size = 10, face = "italic"),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 11, lineheight = 0.9),
    axis.text.y = element_text(size = 11, lineheight = 0.9),
    legend.position = "right",
    legend.title = element_text(size = 10, face = "bold"),
    panel.grid = element_blank(),
    plot.margin = margin(10, 10, 10, 10)
  )

print(p_compare_main)

# 檢查上下三角是否真的不同
verification <- compare_long %>%
  filter(loc != "對角線") %>%
  mutate(
    數值相同 = abs(r_pearson - r_spearman) < 0.0001,
    標籤相同 = labels == lead(labels) | labels == lag(labels)
  ) %>%
  summarise(
    總配對數 = n() / 2,
    數值完全相同 = sum(數值相同, na.rm = TRUE) / 2,
    數值有差異 = 總配對數 - 數值完全相同
  )

# ===============================
# 邏輯斯迴歸（Logistic Regression）
# ===============================
#邏輯斯迴歸：預測高主動攻擊行為

# 準備邏輯迴歸數據
logistic_data = data %>%
  mutate(
    高主動攻擊 = if_else(主動攻擊分數 >= 2.5, 1, 0),
    性別_numeric = if_else(性別 == "男", 1, 0),
    年齡_標準化 = scale(年齡)[,1],
    教育 = 教育程度,
    出生地 = 出生縣市,
    被動攻擊_標準化 = scale(被動攻擊分數)[,1],
    道德正當化_標準化 = scale(道德正當化指數)[,1],
    同理心_標準化 = scale(同理心程度)[,1],
    滿意度_標準化 = scale(生活滿意度)[,1],
    快樂度_標準化 = scale(快樂程度)[,1]
  ) %>%
  dplyr::select(
    高主動攻擊, 年齡_標準化, 性別_numeric, 被動攻擊_標準化,教育,出生地, 
    道德正當化_標準化, 同理心_標準化, 滿意度_標準化,快樂度_標準化
  ) %>%
  na.omit()

cat("樣本數：", nrow(logistic_data), "\n")
cat("高主動攻擊:", sum(logistic_data$高主動攻擊), "人 (", 
    round(sum(logistic_data$高主動攻擊)/nrow(logistic_data)*100, 1), "%)\n\n")

# 2.1 單變項邏輯迴歸
cat("【2.1】單變項邏輯迴歸\n")
univariate_vars = c("年齡_標準化", "性別_numeric", "被動攻擊_標準化","教育", "出生地",
                    "道德正當化_標準化", "同理心_標準化", "滿意度_標準化","快樂度_標準化")

univariate_results = list()
for(var in univariate_vars) {
  formula = as.formula(paste("高主動攻擊 ~", var))
  model = glm(formula, data = logistic_data, family = "binomial")
  univariate_results[[var]] = model
}

# 提取結果
univariate_summary = map_df(univariate_results, function(model) {
  coef_table = summary(model)$coefficients
  tibble(
    變項 = rownames(coef_table)[-1],
    係數 = coef_table[-1, 1],
    標準誤 = coef_table[-1, 2],
    z值 = coef_table[-1, 3],
    p值 = coef_table[-1, 4],
    OR = exp(係數),
    CI_下 = exp(係數 - 1.96 * 標準誤),
    CI_上 = exp(係數 + 1.96 * 標準誤)
  )
}, .id = "模型")

cat("單變項邏輯迴歸結果：\n")
print(univariate_summary %>% dplyr::select(變項, 係數, OR, p值))
print(univariate_summary,n=30)
# 2.2 多變項邏輯迴歸
cat("\n【2.2】多變項邏輯迴歸\n")
model_logistic_full = glm(
  高主動攻擊 ~ 年齡_標準化+性別_numeric+被動攻擊_標準化+
    道德正當化_標準化,
  data = logistic_data,
  family = "binomial"
)
print(summary(model_logistic_full))

# 提取係數
coef_full = summary(model_logistic_full)$coefficients[-1, ]
logistic_results <- tibble(
  變項 = rownames(coef_full),
  係數 = coef_full[, 1],
  標準誤 = coef_full[, 2],
  z值 = coef_full[, 3],
  p值 = coef_full[, 4],
  OR = exp(係數),
  CI_下 = exp(係數 - 1.96 * 標準誤),
  CI_上 = exp(係數 + 1.96 * 標準誤),
  顯著 = case_when(
    p值 < 0.001 ~ "***",
    p值 < 0.01 ~ "**",
    p值 < 0.05 ~ "*",
    TRUE ~ " "
  )
)

cat("\n多變項邏輯迴歸結果：\n")
print(logistic_results %>% dplyr::select(變項, OR, CI_下, CI_上, 顯著))
print(logistic_results)

# 2.4 ROC 曲線與 AUC

cat("\n【2.4】ROC 曲線分析\n")
pred_prob = predict(model_logistic_full, type = "response")
roc_obj = roc(logistic_data$高主動攻擊, pred_prob)
auc_value = auc(roc_obj)

cat("AUC:", round(auc_value, 3), "\n")
cat("解釋：模型正確分類的概率為", round(auc_value*100, 1), "%\n\n")


#【2】邏輯迴歸：預測低主動攻擊行為

# 準備邏輯迴歸數據
logistic_data1 = data %>%
  mutate(
    低主動攻擊 = if_else(主動攻擊分數 < 2.5, 1, 0),
    性別_numeric = if_else(性別 == "男", 1, 0),
    年齡_標準化 = scale(年齡)[,1],
    教育 = 教育程度,
    出生地 = 出生縣市,
    被動攻擊_標準化 = scale(被動攻擊分數)[,1],
    道德正當化_標準化 = scale(道德正當化指數)[,1],
    同理心_標準化 = scale(同理心程度)[,1],
    滿意度_標準化 = scale(生活滿意度)[,1],
    快樂度_標準化 = scale(快樂程度)[,1]
  ) %>%
  dplyr::select(
    低主動攻擊, 年齡_標準化, 性別_numeric, 被動攻擊_標準化,教育,出生地, 
    道德正當化_標準化, 同理心_標準化, 滿意度_標準化,快樂度_標準化
  ) %>%
  na.omit()

cat("樣本數：", nrow(logistic_data1), "\n")
cat("低主動攻擊:", sum(logistic_data1$低主動攻擊), "人 (", 
    round(sum(logistic_data1$低主動攻擊)/nrow(logistic_data1)*100, 1), "%)\n\n")

# 2.1 單變項邏輯迴歸
cat("【2.1】單變項邏輯迴歸\n")
univariate_vars1 = c("年齡_標準化", "性別_numeric", "被動攻擊_標準化","教育", "出生地",
                     "道德正當化_標準化", "同理心_標準化", "滿意度_標準化","快樂度_標準化")

univariate_results1 = list()
for(var in univariate_vars1) {
  formula = as.formula(paste("低主動攻擊 ~", var))
  model = glm(formula, data = logistic_data1, family = "binomial")
  univariate_results1[[var]] = model
}

# 提取結果
univariate_summary1 = map_df(univariate_results1, function(model) {
  coef_table = summary(model)$coefficients
  tibble(
    變項 = rownames(coef_table)[-1],
    係數 = coef_table[-1, 1],
    標準誤 = coef_table[-1, 2],
    z值 = coef_table[-1, 3],
    p值 = coef_table[-1, 4],
    OR = exp(係數),
    CI_下 = exp(係數 - 1.96 * 標準誤),
    CI_上 = exp(係數 + 1.96 * 標準誤)
  )
}, .id = "模型")

cat("單變項邏輯迴歸結果：\n")
print(univariate_summary1 %>% dplyr::select(變項, 係數, OR, p值))
print(univariate_summary1,n=30)
# 2.2 多變項邏輯迴歸
cat("\n【2.2】多變項邏輯迴歸\n")
model_logistic_full1 = glm(
  低主動攻擊 ~ 年齡_標準化+性別_numeric+被動攻擊_標準化+
    道德正當化_標準化,
  data = logistic_data1,
  family = "binomial"
)
print(summary(model_logistic_full1))

# 提取係數
coef_full1 = summary(model_logistic_full1)$coefficients[-1, ]
logistic_results1 <- tibble(
  變項 = rownames(coef_full),
  係數 = coef_full[, 1],
  標準誤 = coef_full[, 2],
  z值 = coef_full[, 3],
  p值 = coef_full[, 4],
  OR = exp(係數),
  CI_下 = exp(係數 - 1.96 * 標準誤),
  CI_上 = exp(係數 + 1.96 * 標準誤),
  顯著 = case_when(
    p值 < 0.001 ~ "***",
    p值 < 0.01 ~ "**",
    p值 < 0.05 ~ "*",
    TRUE ~ " "
  )
)

cat("\n多變項邏輯迴歸結果：\n")
print(logistic_results1 %>% dplyr::select(變項, OR, CI_下, CI_上, 顯著))
print(logistic_results1)

# 2.4 ROC 曲線與 AUC
cat("\n【2.4】ROC 曲線分析\n")
pred_prob1 = predict(model_logistic_full1, type = "response")
roc_obj1 = roc(logistic_data1$低主動攻擊, pred_prob1)
auc_value1 = auc(roc_obj1)

cat("AUC:", round(auc_value1, 3), "\n")
cat("解釋：模型正確分類的概率為", round(auc_value1*100, 1), "%\n\n")

#=======================
#圖(十二)風險因素勝算比
#=======================

# OR 值圖表
# 多變項邏輯迴歸
cat("\n【2.2】多變項邏輯迴歸\n")
model_logistic_full2 = glm(
  高主動攻擊 ~ 年齡_標準化 + 性別_numeric + 被動攻擊_標準化 + 
  道德正當化_標準化 + 同理心_標準化 + 滿意度_標準化 + 快樂度_標準化,
  data = logistic_data,
  family = "binomial"
)
print(summary(model_logistic_full2))

# 提取係數
coef_full2 = summary(model_logistic_full2)$coefficients[-1, ]
logistic_results2 <- tibble(
  變項 = rownames(coef_full2),
  係數 = coef_full2[, 1],
  標準誤 = coef_full2[, 2],
  z值 = coef_full2[, 3],
  p值 = coef_full2[, 4],
  OR = exp(係數),
  CI_下 = exp(係數 - 1.96 * 標準誤),
  CI_上 = exp(係數 + 1.96 * 標準誤),
  顯著 = case_when(
    p值 < 0.001 ~ "***",
    p值 < 0.01 ~ "**",
    p值 < 0.05 ~ "*",
    TRUE ~ " "
  )
)
cat("\n多變項邏輯迴歸結果：\n")
print(logistic_results2 %>% dplyr::select(變項, OR, CI_下, CI_上, 顯著))
print(logistic_results2)

# 2.5 OR 值圖表
p_or = ggplot(logistic_results2, aes(x = OR, y = reorder(變項, OR))) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "red", size = 1) +
  geom_point(color = "black", size = 3) +
  geom_errorbarh(
    aes(xmin = CI_下, xmax = CI_上),
    height = 0.2, size = 1
  ) +
  geom_text(aes(label = paste0(round(OR, 3), " ", 顯著)), 
            hjust = -0.3, size = 5, fontface = "bold",nudge_y = -0.2) +
  scale_x_log10() +
  labs(
    title = "勝算比圖（OR）：主動攻擊的風險因素",
    subtitle = "95% 信心區間 | 紅線 = OR=1（無效應）",
    x = "勝算比（Odds Ratio）",
    y = "變項"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(size = 15, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5, size = 10),
    axis.text = element_text(size = 14, face = "bold")
  )

print(p_or)


