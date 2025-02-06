## Package instalation
pacman::p_load(pacman, dplyr, GGally, ggplot2, ggthemes,
               ggvis, httr, lubridate, plotly, rio, rmarkdown, 
               shiny, stringr, tidyr)
install.packages("QCA")
install.packages("stringr")
install.packages("openxlsx")
install.packages("readxl")
library(datasets, pacman, QCA, stringr, openxlsx, readxl)
head(iris)

## City Socio-economic Data
B <- read_excel("~/Desktop/THSS2/DataSets/City_DataSet/City_Basic.xlsx")
colnames(B) <- c("City","Region",
                 "Kor_Name",
                 "Latitude",
                 "Longitude",
                 "City_Size",
                 "Pop",
                 "GDP",
                 "Edu")

## Capacity Data 
GDP <- read_delim(file = '~/Desktop/THSS2/DataSets/Socio-economic Data/Real_GRDP_Data.csv',
                  col_names = TRUE, delim = ';')
C <- GDP[, c(1,4)]
colnames(C) <- c("Kor_Name", "2019")

## Air Quality Data
AQ1901 <- read_excel("~/Desktop/THSS2/DataSets/Air Quality Data/2019/2019년 01월.xlsx")
AQ1902 <- read_excel("~/Desktop/THSS2/DataSets/Air Quality Data/2019/2019년 02월.xlsx")
AQ1903 <- read_excel("~/Desktop/THSS2/DataSets/Air Quality Data/2019/2019년 03월.xlsx")
AQ1904 <- read_excel("~/Desktop/THSS2/DataSets/Air Quality Data/2019/2019년 04월.xlsx")
AQ1905 <- read_excel("~/Desktop/THSS2/DataSets/Air Quality Data/2019/2019년 05월.xlsx")
AQ1906 <- read_excel("~/Desktop/THSS2/DataSets/Air Quality Data/2019/2019년 06월.xlsx")
AQ1907 <- read_excel("~/Desktop/THSS2/DataSets/Air Quality Data/2019/2019년 07월.xlsx")
AQ1908 <- read_excel("~/Desktop/THSS2/DataSets/Air Quality Data/2019/2019년 08월.xlsx")
AQ1909 <- read_excel("~/Desktop/THSS2/DataSets/Air Quality Data/2019/2019년 09월.xlsx")
AQ1910 <- read_excel("~/Desktop/THSS2/DataSets/Air Quality Data/2019/2019년 10월.xlsx")
AQ1911 <- read_excel("~/Desktop/THSS2/DataSets/Air Quality Data/2019/2019년 11월.xlsx")
AQ1912 <- read_excel("~/Desktop/THSS2/DataSets/Air Quality Data/2019/2019년 12월.xlsx")

# Data Aggregation
RawAQ <- bind_rows(AQ1901, 
                   AQ1902, 
                   AQ1903, 
                   AQ1904, 
                   AQ1905, 
                   AQ1906, 
                   AQ1907, 
                   AQ1908, 
                   AQ1909,
                   AQ1910,
                   AQ1911,
                   AQ1912)

ProsAQ <- RawAQ[RawAQ$망 == "도시대기",c(1:6,9,10)]
colnames(ProsAQ) <- c("Location", 
                      "Type", 
                      "StationCode", 
                      "StationName", 
                      "DateTime", 
                      "SO2", 
                      "NO2", 
                      "PM10"
)

FiltAQ <- ProsAQ %>%
  group_by(StationCode) %>%
  filter(sum(!is.na(PM10)) >= 6570,
         sum(!is.na(NO2)) >= 6570,
         sum(!is.na(SO2)) >= 6570) %>%
  ungroup()  

# Calculating Average Data
avg_AQ <- FiltAQ %>%  
  group_by(StationCode) %>%  
  summarize(
    Location = first(Location),
    StationName = first(StationName),
    avg_SO2 = mean(SO2, na.rm = TRUE),
    avg_NO2 = mean(NO2, na.rm = TRUE), 
    avg_PM10 = mean(PM10, na.rm = TRUE) 
  )

# Renaming Location Values  
AQ2 <- avg_AQ %>%
  separate(Location, into = c("Location", "City"), sep = " ") 

AQ1 <- AQ2 %>%
  mutate(Region = case_when(
    Location == "서울" ~ "서울특별시",
    Location == "부산" ~ "부산광역시",
    Location == "인천" ~ "인천광역시",
    Location == "대구" ~ "대구광역시",
    Location == "대전" ~ "대전광역시",
    Location == "광주" ~ "광주광역시",
    Location == "울산" ~ "울산광역시",
    Location == "세종" ~ "세종특별자치시",
    Location == "경기" ~ "경기도",
    Location == "충남" ~ "충청남도",
    Location == "충북" ~ "충청북도",
    Location == "강원" ~ "강원도",
    Location == "경북" ~ "경상북도",
    Location == "경남" ~ "경상남도",
    Location == "전북" ~ "전라북도",
    Location == "전남" ~ "전라남도",
    TRUE ~ Location
  )) %>%
  select(-Location)

# Only 1 Data Point per City
Spc_Muni <- AQ1 %>%
  filter(Region %in% c("서울특별시",
                       "부산광역시",
                       "인천광역시",
                       "대구광역시",
                       "대전광역시",
                       "광주광역시",
                       "울산광역시",
                       "세종특별자치시")
  )%>%
  group_by(Region) %>%
  slice(which.max(avg_PM10)) %>%
  summarize(
    StationName = first(StationName),
    StationCode = first(StationCode),
    SO2 = first(avg_SO2),
    NO2 = first(avg_NO2),
    PM10 = first(avg_PM10),
  ) %>%
  ungroup() %>%
  rename(City = Region)

Nrm_Muni <- AQ1 %>%
  filter(!Region %in% c("서울특별시",
                        "부산광역시",
                        "인천광역시",
                        "대구광역시",
                        "대전광역시",
                        "광주광역시",
                        "울산광역시",
                        "세종특별자치시")
  )%>%
  group_by(City) %>%
  slice(which.max(avg_PM10)) %>%
  summarize(
    StationName = first(StationName),
    StationCode = first(StationCode),
    SO2 = first(avg_SO2),
    NO2 = first(avg_NO2),
    PM10 = first(avg_PM10),
  ) %>%
  ungroup()  

AQ <- bind_rows(Spc_Muni,
                Nrm_Muni)
colnames(AQ) <- c("Kor_Name",
                  "StationName",
                  "StationCode",
                  "SO2", 
                  "NO2", 
                  "PM10"
)


## ICLEI Membership
M_impt <- read_excel("~/Desktop/THSS2/DataSets/City_DataSet/CityData_member.xlsx")
M <- M_impt[,c(4,5)]
colnames(M) <- c("Kor_Name",
                 "ICLEI"
)


## Bind All Data 
CtyDt <- B %>%
  inner_join(AQ, by = "Kor_Name") %>%
  inner_join(M, by = "Kor_Name")

Dt <- CtyDt %>%
  filter(Pop >= 200000) %>%
  mutate(GDPcapt = (GDP / Pop)*1000000000) %>%
  select(Latitude,
         Longitude,
         City_Size,
         Pop,
         GDPcapt,
         Edu,
         ICLEI,
         PM10,
         NO2,
         SO2)

rownames(Dt) <- CtyDt$City

## Policy Data
Pol_all <- read_excel("~/Desktop/THSS2/DataSets/Policy Data/Policy_Data_2.xlsx")
colnames(Pol_all) <- c("Kor_Name","Policy")

Pol_Key <- read_excel("~/Desktop/THSS2/DataSets/Policy Data/Policy_Keyword.xlsx") %>%
  select(Type,
         Subcatergory,
         Keyword,
         English
  )
colnames(Pol_Key) <- c("Cat", "SubCat", "Keyword", "Trans")

Key_Match <- expand_grid(Pol_all, Pol_Key) %>%
  mutate(Match = str_detect(Policy, Keyword)) %>%
  filter(Match == TRUE) %>%
  select(-Match)

Group_Match <-Key_Match %>%
  group_by(Kor_Name, Policy) %>%
  summarise(
    Keys = paste(Keyword, collapse = ", "),
    Translations = paste(Trans, collapse = ", "),
    Ct_g = paste(Cat, collapse = ", "),
    SbC_g = paste(SubCat, collapse = ", "),
    .groups = "drop"
  )



## Assign Groups to Data for QCA

## Policy Data
P_cl <- Group_Match %>%
  group_by(Kor_Name, Policy) %>%
  summarise(
    Categories = paste(Ct_g, collapse = ", "),
    Unique_Categories = paste(unique(Ct_g), collapse = ", "),
    Adaptation_Count = sum(str_count(Ct_g, "Adaptation")),
    Mitigation_Count = sum(str_count(Ct_g, "Mitigation")),
    Other_Count = sum(str_count(Ct_g, "Other")),
    AirQ_Count = sum(str_count(Ct_g, "AirQuality")),
    # Classification logic
    Category = case_when(
      AirQ_Count > 0 ~ "Air_Quality",
      (Adaptation_Count > 0 | Mitigation_Count > 0) & Other_Count == 0 ~ "Dedicated", 
      (Adaptation_Count > 0 | Mitigation_Count > 0) & Other_Count > 0 ~ "Mixed",      
      Adaptation_Count == 0 & Mitigation_Count == 0 & Other_Count > 0 ~ "Other",
      TRUE ~ "Unclassified"
    ),
    .groups = "drop"
  )

P_score <- P_cl %>%
  group_by(Kor_Name, Category) %>%
  summarise(Count = n(), .groups = "drop") %>%
  pivot_wider(names_from = Category, values_from = Count, values_fill = 0) %>%
  ungroup()

P_fz <- P_score %>%
  mutate(
    Total = Dedicated + Mixed + Other + Air_Quality,
    Dedicated = Dedicated / Total,
    Mixed = Mixed / Total,
    Other = Other / Total,
    Air_Quality = Air_Quality / Total
  )


## QCA 

truth <- truthTable(Dt, outcome = "ICLEI", incl.cut = 0.8)
print(truth)




