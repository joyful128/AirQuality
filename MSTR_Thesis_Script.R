## Package instalation
pacman::p_load(pacman, dplyr, GGally, ggplot2, ggthemes,
               ggvis, httr, lubridate, plotly, rio, rmarkdown, 
               shiny, stringr, tidyr)
install.packages("QCA")
install.packages("stringr")
install.packages("openxlsx")
install.packages("readxl")
install.packages("psych")
library(pacman)
library(QCA)
library(stringr)
library(openxlsx) 
library(readxl)
library(psych)

head(iris)

'''

# Temporary File Creation for Git-Rstudio Connection

file_url <- "https://github.com/joyful128/AirQuality/raw/main/City_DataSet/City_Basic.xlsx"

temp_file <- tempfile(fileext = ".xlsx")

download.file(file_url, destfile = temp_file, mode = "wb")

''' 

## Import Basic City Data

B <- read_excel("~/RProjects/AirQuality/City_DataSet/City_Basic.xlsx")
colnames(B) <- c("City","Region",
                 "Kor_Name",
                 "Lat",
                 "Lon",
                 "Size",
                 "Pop",
                 "GRDP",
                 "Edu"
                 )


## Air Quality Data

AQ1901 <- read_excel("~/RProjects/AirQuality/Air_Quality_2019/Jan_2019.xlsx")
AQ1902 <- read_excel("~/RProjects/AirQuality/Air_Quality_2019/Feb_2019.xlsx")
AQ1903 <- read_excel("~/RProjects/AirQuality/Air_Quality_2019/Mar_2019.xlsx")
AQ1904 <- read_excel("~/RProjects/AirQuality/Air_Quality_2019/Apr_2019.xlsx")
AQ1905 <- read_excel("~/RProjects/AirQuality/Air_Quality_2019/May_2019.xlsx")
AQ1906 <- read_excel("~/RProjects/AirQuality/Air_Quality_2019/Jun_2019.xlsx")
AQ1907 <- read_excel("~/RProjects/AirQuality/Air_Quality_2019/Jul_2019.xlsx")
AQ1908 <- read_excel("~/RProjects/AirQuality/Air_Quality_2019/Aug_2019.xlsx")
AQ1909 <- read_excel("~/RProjects/AirQuality/Air_Quality_2019/Sep_2019.xlsx")
AQ1910 <- read_excel("~/RProjects/AirQuality/Air_Quality_2019/Oct_2019.xlsx")
AQ1911 <- read_excel("~/RProjects/AirQuality/Air_Quality_2019/Nov_2019.xlsx")
AQ1912 <- read_excel("~/RProjects/AirQuality/Air_Quality_2019/Dec_2019.xlsx")


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

ProsAQ <- RawAQ[RawAQ$망 == "도시대기",c(1:5,10)]
colnames(ProsAQ) <- c("Location", 
                      "Type", 
                      "StationCode", 
                      "StationName", 
                      "DateTime", 
                      "PM10"
                      )

FiltAQ <- ProsAQ %>%
  group_by(StationCode) %>%
  filter(sum(!is.na(PM10)) >= 6570) %>%
  ungroup()  

# Calculating Average Data
avg_AQ <- FiltAQ %>%  
  group_by(StationCode) %>%  
  summarize(
    Location = first(Location),
    StationName = first(StationName),
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
    PM10 = first(avg_PM10),
  ) %>%
  ungroup()  

AQ <- bind_rows(Spc_Muni,
                Nrm_Muni)
colnames(AQ) <- c("Kor_Name",
                  "StationName",
                  "StationCode", 
                  "A"
)


## ICLEI Membership & Policy Data

M_impt <- read_excel("~/RProjects/AirQuality/City_DataSet/CityData_member.xlsx")
M <- M_impt[,c(4,5,6)]
colnames(M) <- c("Kor_Name",
                 "M",
                 "Y"
)


## Mayor Political Affiliation

P_impt <- read.csv("~/RProjects/AirQuality/City_DataSet/2018_Election_Results.csv")
Mayor_df <- P_impt[P_impt$선거명 == "시·도지사선거" | P_impt$선거명 == "구·시·군의 장선거", c(3, 5)]
colnames(Mayor_df) <- c("Kor_Name",
                        "Party"
                        )
# Find Matches
match_df <- Mayor_df[Mayor_df$Kor_Name %in% B$Kor_Name, ]

# Value Assignment
P <- match_df %>%
  mutate(P_Name = case_when(
    Party == "더불어민주당" ~ "Liberal",
    Party == "자유한국당" ~ "Conservative",
    Party == "무소속" ~ "Independant",
    Party == "민주평화당" ~ "Liberal",
    TRUE ~ Party
  )) %>%
  select(-Party)

P <- P %>%
  mutate(P = case_when(
    P_Name == "Liberal" ~ 1,
    P_Name == "Conservative" ~ 0,
    P_Name == "Independant" ~ 0.5,
    TRUE ~ NA_real_  
  ))

## Bind All Data 
CtyDt <- B %>%
  inner_join(AQ, by = "Kor_Name") %>%
  inner_join(M, by = "Kor_Name") %>%
  inner_join(P, by = "Kor_Name")

Dt <- CtyDt %>%
  filter(Pop >= 200000) %>%
  mutate(C = (GRDP/Pop)*1000000000) %>%
  select(City,
         C,
         M,
         Y,
         A,
         P
         )

### Yeosu's Mayor ran as independant but later rejoined the liberal party
Dt$P[42] <- 1

'''
write.csv(Dt, "sample_dt.csv", row.names = FALSE)
'''

## Summary Staticstics
describe(Dt)



### QCA 

## Assign Binary 

Dt$A <- as.numeric(Dt$A >= 47)
Dt$C <- as.numeric(Dt$C > 30000000)
Dt$M <- as.numeric(as.logical(Dt$M))
Dt$Y <- as.numeric(as.logical(Dt$Y)) 
Dt$P <- as.numeric(as.logical(Dt$P))

'''
str(Dt)
sapply(Dt, class)
colSums(is.na(Dt))
'''

# Truth Table

truth <- truthTable(Dt, 
                    outcome = "Y", 
                    conditions = c("C","A","M","P"), 
                    incl.cut = 0.8)
print(truth)

# Minimization

complex_sol <- minimize(truth, details = TRUE)
print(complex_sol)