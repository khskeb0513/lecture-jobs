# install.packages('xml2')
library(xml2)

rm(list = ls())
DEBUG = T

# 문제 1

serviceKey = 'MlB2k1cGG%2BUDm0OzU5rF8b8ESs7u1y2mmBmt6E%2BOeHdAg5mO%2F7bx1dJDMPqCA2LgAe2E2H9jaodeKzU8IbFr4g%3D%3D'
numOfRows = 10
pageNo = ''
chrstnType = ''
filterValues = ''
dataType = 'xml'
url_xml = paste0(
  'https://apis.data.go.kr/3740000/suwonEvChrstn/getdatalist?serviceKey=' ,
  serviceKey,
  '&numOfRows=',
  numOfRows,
  '&pageNo=',
  pageNo,
  '&sortKey=',
  chrstnType,
  '&filterKey=',
  chrstnType,
  '&filterValues=',
  filterValues,
  '&type=',
  dataType
)

# urls 링크를 xml로 읽어들이기.
# read xml(): xml 파일 읽어들이기.
raw_xml <- read_xml(url_xml)

# occrrnc0t. dataVall 변수 모두 찾아서 저장하기
# xml_find_all(): xml 파일에서 원하는 '태그'를 찾아 저장.

xml_1 <- xml_find_all(raw_xml, '//chrstnNm')
xml_2 <- xml_find_all(raw_xml, '//latitude')
xml_3 <- xml_find_all(raw_xml, '//longitude')
xml_4 <- xml_find_all(raw_xml, '//charger_status')

# text로 읽어 저장하기
# xml text( :) 텍스트 추출.
chrstnNm <- xml_text(xml_1)
latitude <- xml_text(xml_2)
longitude <- xml_text(xml_3)
charger_status <- xml_text(xml_4)


# 문제2

df = data.frame(
  chrstnNm = chrstnNm,
  latitude = latitude,
  longitude = longitude,
  charger_status = charger_status
)

# 문제3
str(df)
# 문제점) 위도(latitude)와 경도(longitude)를 character형에서 numeric으로 변경하여야.
df$latitude = as.numeric(df$latitude)
df$longitude = as.numeric(df$longitude)
str(df) # df가 고쳐졌는지 확인

# 문제4
df = data.frame()
totalCount = as.numeric(xml_text(xml_find_all(raw_xml, '//totalCount'), trim = T))
totalPage = totalCount %/% 10 - 1
for (pageNo in 0:totalPage) {
  if (sum(ls() == 'serviceKey') == 0) {
    print('NO serviceKey. Arret!')
    break
  }
  if (DEBUG) {
    if (pageNo > 50) {
      break
    }
    print(paste0(c(
      pageNo,
      ' / ',
      totalPage,
      ' [',
      rep('=', times = as.integer(pageNo / totalPage * 20)),
      rep(' ', times = 20 - as.integer(pageNo / totalPage * 20)),
      ']'
    ), collapse = ''))
  }
  numOfRows = 10
  dataType = 'xml'
  url_xml = paste0(
    'https://apis.data.go.kr/3740000/suwonEvChrstn/getdatalist?serviceKey=' ,
    serviceKey,
    '&pageNo=',
    pageNo,
    '&type=',
    dataType,
    '&numOfRows=',
    numOfRows,
    '&sortKey=&filterKey=&filterValues='
  )
  raw_xml <- read_xml(url_xml)
  xml_1 <- xml_find_all(raw_xml, '//chrstnNm')
  xml_2 <- xml_find_all(raw_xml, '//latitude')
  xml_3 <- xml_find_all(raw_xml, '//longitude')
  xml_4 <- xml_find_all(raw_xml, '//charger_status')
  chrstnNm <- xml_text(xml_1)
  latitude <- as.numeric(xml_text(xml_2))
  longitude <- as.numeric(xml_text(xml_3))
  charger_status <- xml_text(xml_4)
  df = rbind(
    df,
    data.frame(
      chrstnNm = chrstnNm,
      latitude = latitude,
      longitude = longitude,
      charger_status = charger_status,
    )
  )
}
str(df)
head(df)

# 문제5
fetch_xml_to_df = function(pageNo) {
  numOfRows = 10
  dataType = 'xml'
  url_xml = paste0(
    'https://apis.data.go.kr/3740000/suwonEvChrstn/getdatalist?serviceKey=' ,
    serviceKey,
    '&pageNo=',
    pageNo,
    '&type=',
    dataType,
    '&numOfRows=',
    numOfRows,
    '&sortKey=&filterKey=&filterValues='
  )
  raw_xml <- read_xml(url_xml)
  xml_1 <- xml_find_all(raw_xml, '//chrstnNm')
  xml_2 <- xml_find_all(raw_xml, '//latitude')
  xml_3 <- xml_find_all(raw_xml, '//longitude')
  xml_4 <- xml_find_all(raw_xml, '//charger_status')
  chrstnNm <- xml_text(xml_1)
  latitude <- as.numeric(xml_text(xml_2))
  longitude <- as.numeric(xml_text(xml_3))
  charger_status <- xml_text(xml_4)
  return(
    data.frame(
      chrstnNm = chrstnNm,
      latitude = latitude,
      longitude = longitude,
      charger_status = charger_status,
    )
  )
}

str(fetch_xml_to_df(0))
head(fetch_xml_to_df(0))

# 문제 6

#install.packages('rjson')
#install.packages('httr')
library(rjson)
library(httr)

fetch_json_to_df = function(pageNo) {
  numOfRows = 10
  dataType = 'json'
  url_json = paste0(
    'https://apis.data.go.kr/3740000/suwonEvChrstn/getdatalist?serviceKey=' ,
    serviceKey,
    '&pageNo=',
    pageNo,
    '&type=',
    dataType,
    '&numOfRows=',
    numOfRows,
    '&sortKey=&filterKey=&filterValues='
  )
  dfQueue = data.frame()
  deserializedItems = fromJSON(content(GET(url_json), 'text'))[['items']]
  for (i in 1:length(deserializedItems)) {
    dfQueue = rbind(
      dfQueue,
      list(
        chrstnNm = deserializedItems[[i]]$chrstnNm,
        latitude = as.numeric(deserializedItems[[i]]$latitude),
        longitude = as.numeric(deserializedItems[[i]]$longitude),
        charger_status = deserializedItems[[i]]$charger_status,
      )
    )
  }
  return(dfQueue)
}

df = data.frame()
for (pageNo in 0:13) {
  df = rbind(df, fetch_json_to_df(pageNo))
}
str(df)
head(df)

# 문제 7

#install.packages('leaflet')
library(leaflet)

my_map = leaflet()
my_map_1 = addTiles(my_map)
my_map_2 = addMarkers(
  my_map_1,
  lng = df$longitude,
  lat = df$latitude,
  popup = df$chrstnNm
)
my_map_2

# 문제 8
# 2022년 8월 한 달 간 기초자치단체 별 음식물쓰레기 종합배출내역 합계

waste_api_key = 'MlB2k1cGG%2BUDm0OzU5rF8b8ESs7u1y2mmBmt6E%2BOeHdAg5mO%2F7bx1dJDMPqCA2LgAe2E2H9jaodeKzU8IbFr4g%3D%3D'

waste_domestic_code_fetch_url = paste0(
  'https://apis.data.go.kr/B552584/RfidFoodWasteServiceNew/getCityList?serviceKey=',
  waste_api_key,
  '&type=xml&page=1&rowNum=10000'
)
raw_waste_domestic_code_xml <-
  read_xml(waste_domestic_code_fetch_url)
xml_1 <- xml_find_all(raw_waste_domestic_code_xml, '//cityCode')
xml_2 <- xml_find_all(raw_waste_domestic_code_xml, '//citySidoName')
xml_3 <- xml_find_all(raw_waste_domestic_code_xml, '//citySggName')
cityCode <- xml_text(xml_1)
citySidoName <- xml_text(xml_2)
citySggName <- xml_text(xml_3)

waste_df = data.frame(cityCode = cityCode,
                      citySidoName = citySidoName,
                      citySggName = citySggName)

waste_quantities = c()
for (i in 1:length(cityCode)) {
  if (DEBUG) {
    print(paste0(c(
      i,
      ' / ',
      length(cityCode),
      ' [',
      rep('=', times = as.integer(i / length(cityCode) * 20)),
      rep(' ', times = 20 - as.integer(i / length(cityCode) * 20)),
      ']'
    ), collapse = ''))
  }
  disYear = '2022'
  disMonth = '08'
  waste_quantity_fetch_url = paste0(
    'https://apis.data.go.kr/B552584/RfidFoodWasteServiceNew/getCityDateList?serviceKey=',
    waste_api_key,
    '&type=xml&disYear=',
    disYear,
    '&disMonth=',
    disMonth,
    '&cityCode=',
    cityCode[i],
    '&page=1&rowNum=31'
  )
  raw_waste_quantity_xml <- read_xml(waste_quantity_fetch_url)
  xml_1 <- xml_find_all(raw_waste_quantity_xml, '//disQuantity')
  waste_quantities = append(waste_quantities, sum(as.numeric(xml_text(xml_1))))
}

waste_df = cbind(waste_df, data.frame(disQuantity = waste_quantities))
str(waste_df)
head(waste_df)

# 문제 9

# 2022/08 기초자치단체 별 배출량 평균
mean(waste_df$disQuantity)
# 2022/08 한 달 간 배출량이 가장 많은 기초자치단체
waste_df[waste_df$disQuantity == max(waste_df$disQuantity),]
# 2022/08 한 달 간 배출량이 가장 적은 기초자치단체, 0을 제외하고 = RFID를 통한 음식물쓰레기 집계를 하지 않는 기초자치단체를 제외
waste_df[waste_df$disQuantity == min(waste_df$disQuantity[waste_df$disQuantity != 0]),]
# 광역자치단체 별 합계 내역 출력
disQuantityBySido = data.frame()
for (sidoName in levels(factor(waste_df$citySidoName))) {
  disQuantityBySido = rbind(disQuantityBySido,
                            list(
                              citySidoName = sidoName,
                              disQuantity = sum(waste_df[waste_df$citySidoName == sidoName, ]$disQuantity)
                            ))
}
disQuantityBySido
# 2022/08 한 달 간 배출량이 가장 적게 집게된 광역자치단체
disQuantityBySido[disQuantityBySido$disQuantity == min(disQuantityBySido$disQuantity),]
