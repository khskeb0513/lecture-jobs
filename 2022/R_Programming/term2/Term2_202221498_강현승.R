# 라이브러리 import
# install.packages('tidyverse')
# install.packages('tidytext')
# install.packages('RcppMeCab')
# install.packages('xml2')
# install.packages('readxl')
# install.packages('httr')
# install.packages('jsonlite')
# install.packages('stopwords')
# install.packages('RSelenium')
# install.packages('wordcloud2')
# install.packages('rvest')
# install.packages('ggplot2')
# install.packages('plotly')
library(tidyverse)
library(tidytext)
library(RcppMeCab)
library(xml2)
library(readxl)
library(httr)
library(jsonlite)
library(stopwords)
library(wordcloud2)
library(rvest)
library(ggplot2)
library(plotly)
# install.packages('devtools')
# library(devtools)
# devtools::install_github('khskeb0513/RSelenium') # Selenium 4.0 Grid 대응
library(RSelenium)

# working dir 설정
if (!str_detect(getwd(), 'term2')) {
  setwd('./term2')
}
# dart api key 변수 설정
crtfc_key = '7f3e88b4460bac062cc3307716cb49f868d13fd6'
# ggplot 띄울 시 맥환경 폰트 호환
theme_set(theme_grey(base_family='NanumGothic'))

IS_REPORT_PRINT = T

# https://opendart.fss.or.kr/api/corpCode.xml?crtfc_key=7f3e88b4460bac062cc3307716cb49f868d13fd6
# 2022-12-08 일자 데이터: 전자공시 실시 기업체 목록
# 백업된 내용
raw_dart_corp_codes = read_xml('https://git.ajou.ac.kr/h5k/ebiz-r-programming/raw/main/term2/CORPCODE.xml')
dart_corp_codes = tibble(corp_name = xml_text(xml_find_all(raw_dart_corp_codes, '//corp_name')),
                         corp_code = xml_text(xml_find_all(raw_dart_corp_codes, '//corp_code')))
# 2022-12-08 11:07:00 기준 삼성전자와 같은 사업보고서를 사용하는 삼성전자우를 제외한 KOSPI 상위 50개 기업 목록
raw_kospi_top_60 = read.csv(
  'https://git.ajou.ac.kr/h5k/ebiz-r-programming/raw/main/term2/kospi_top_60.csv'
)
kospi_top_50 = head(raw_kospi_top_60, 51) %>% filter(!str_detect(종목명, '삼성전자우'))
# 위에서 추출한 KOSPI 상위 50개 기업들의 전자공시 기업코드를 전자공시 실시 기업체 목록에서 가져온다.
dart_top_50_codes = dart_corp_codes %>%
  filter(corp_name %in% kospi_top_50$종목명)
head(dart_top_50_codes, 2)

# 같은 이름을 가진 기업체들의 전자공시 기업코드를 모두 가져왔으므로 이들 중 KOSPI에 상장된 회사만을 선별해야한다. 같은 법인명을 가진 회사들이 존재하기 때문이다. 아래 코드를 통해 전자공시 기업코드를 쿼리로 전달해 기업 목록을 가져온다.
corp_info_list = data.frame()
for (dart_code in dart_top_50_codes$corp_code) {
  corp_info_list = rbind(corp_info_list,
                         data.frame(read_json(
                           paste0(
                             'https://opendart.fss.or.kr/api/company.json?crtfc_key=',
                             crtfc_key,
                             '&corp_code=',
                             dart_code
                           )
                         )))
}
corp_info_list1 = corp_info_list %>% filter(corp_cls == 'Y') # 유가증권만 선별
nrow(corp_info_list1)
corp_info_list1[1, ]

reports = data.frame()

# 위에서 얻은 KOSPI 상위 50개 기업들의 전자공시 기업코드를 바탕으로 지난 10 년 간 기업의 정기보고서 목록을 가져온다.
for (dart_code in corp_info_list1$corp_code) {
  rawjson = read_json(
    paste0(
      'http://opendart.fss.or.kr/api/list.json?crtfc_key=',
      crtfc_key,
      '&corp_code=',
      dart_code,
      '&bgn_de=20130101&pblntf_ty=A&page_count=100&last_reprt_at=Y'
    )
  )$list
  for (v in rawjson) {
    reports = rbind(reports, v)
  }
}
# 위에서 가져온 정기보고서 중 연 초 공시하는 사업보고서만을 얻기 위해 "사업보고서"라는 키워드로 보고서 목록을 필터링하였다.
reports1 = reports %>%
  filter(str_detect(report_nm, '사업보고서'))

# 2022-12-10 기준 크롤링한 공시자료 데이터세트
if (IS_REPORT_PRINT) {
  load(
    url(
      'https://git.ajou.ac.kr/h5k/ebiz-r-programming/raw/main/term2/report_texts'
    )
  )
} else {
  rd = remoteDriver(browserName = 'chrome')
  
  rd$open()
  
  # 확대수준을 최대로 줄여서 한 화면에 모든 요소가 보일 수 있도록 창 조작해야
  rd$navigate(paste0(
    'https://dart.fss.or.kr/dsaf001/main.do?rcpNo=',
    '20220512000853'
  ))
  report_texts = data.frame()
  for (rcept_no in reports1$rcept_no) {
    if (sum(str_detect(rcept_no, report_texts$rcept_no)) > 0) {
      next
    }
    Sys.sleep(4)
    rd$navigate(paste0(
      'http://dart.fss.or.kr/dsaf001/main.do?rcpNo=',
      rcept_no
    ))
    Sys.sleep(1)
    rd$executeScript('closeWinCcommMsg(this)')
    options = rd$findElements(using = 'tag name', 'option')
    rd$executeScript(paste0('changeFamily("', unlist(sapply(options, function(x) {
      x$getElementAttribute('value')
    }))[unlist(sapply(options, function(x) {
      x$getElementAttribute('value')
    })) != 'null'][1], '")'))
    Sys.sleep(1)
    rd$executeScript('closeWinCcommMsg(this)')
    Sys.sleep(1)
    a_tag_ids = read_html(rd$getPageSource()[[1]]) %>%
      html_elements('a') %>%
      sapply(function(x) {
        if (!is.na(html_attr(x, 'aria-level')) &&
            html_attr(x, 'aria-level') == '1') {
          html_attr(x, 'id')
        }
      }) %>%
      unlist
    
    report_text = ''
    for (a_tag_id in a_tag_ids) {
      Sys.sleep(1)
      rd$findElement('xpath', paste0('//*[@id="', a_tag_id, '"]'))$clickElement()
      report_text <-
        paste0(report_text,
               read_html(
                 paste0(
                   'http://dart.fss.or.kr/',
                   read_html(rd$getPageSource()[[1]]) %>%
                     html_elements('iframe') %>% html_attr('src')
                 )
               ) %>%
                 html_text)
    }
    report_texts <-
      rbind(report_texts,
            data.frame(rcept_no = rcept_no, report_text = report_text))
  }
}
report_texts = tibble(report_texts)

# 가져온 공시자료를 어절 단위로 토큰화하여 리스트에 저장
word_tokenized = list()
for (i in 1:nrow(report_texts)) {
  word_tokenized[[report_texts[i, ]$rcept_no]] = report_texts[i,] %>%
    unnest_tokens(word, report_text) %>%
    select(word)
}

# csr 키워드 모음
csr_words = c(
  '경영',
  '사회',
  '사업',
  '성장',
  '환경',
  '노력',
  '가치',
  '글로벌',
  '활동',
  '고객',
  '성과',
  '경쟁',
  '책임',
  '안전',
  '발전',
  '에너지',
  '기술',
  '세계',
  '혁신',
  '변화',
  '경제',
  '창출',
  '경쟁력',
  '미래',
  '기반',
  '협력',
  '지역',
  '확대',
  '구축',
  '국내',
  '문화',
  '제품',
  '개발',
  '추진',
  '동반',
  '실천',
  '확보',
  '개선',
  '목표',
  '역량',
  '친환경',
  '신뢰',
  '운영',
  '최선',
  '공헌',
  '소통',
  '실현',
  '적극',
  '지역',
  '사회',
  '효율',
  '서비스',
  '최고',
  'CSR',
  '관리',
  '품질',
  '협력사',
  '달성',
  '전략',
  '해외',
  '상생',
  '중심',
  '화학',
  '우수',
  '조직',
  '지속',
  '가능성',
  '가스',
  '대응',
  '시스템',
  '수익',
  '지원',
  '도전',
  '투자',
  '계획',
  '공급',
  '기회',
  '도약',
  '윤리',
  '경영',
  '투명',
  '인류',
  '격려',
  '메시지',
  '불확실',
  '생산',
  '세상',
  '시민',
  '정착',
  '준수',
  '집중',
  '건강',
  '매출',
  '부문',
  '비전',
  '인정',
  '주주',
  '지수',
  '체계',
  '핵심',
  '향상',
  '따뜻',
  '경영',
  '환경'
)

# csr 키워드 관련 데이터프레임
# n = csr 키워드를 포함하고 있는 어절 수
# entire_n = 보고서 내 전체 어절 수
# entire_n = 보고서 내 csr 키워드를 포함하고 있는 어절의 비중
#          = n / entire_n
count_csr_word_df = tibble()
for (rcept_no in report_texts$rcept_no) {
  count_csr_word_df = rbind(
    count_csr_word_df,
    tibble(
      rcept_no = rcept_no,
      n = word_tokenized[[rcept_no]] %>%
        filter(str_detect(word, paste(
          csr_words, collapse = '|'
        ))) %>%
        nrow,
      entire_n = word_tokenized[[rcept_no]] %>% nrow,
      n_weight = n / entire_n
    )
  )
}
# 사업보고서 이름에서 보고년도를 추출한다.
reports1$report_year = ''
for (i in 1:nrow(reports1)) {
  reports1[i, 'report_year'] = (2010:2021)[str_detect(reports1[i, ]$report_nm, as.character(2010:2021))]
}
# 자료형 확인
str(reports1)
# 년도를 숫자형으로 변환
reports1$report_year = as.numeric(reports1$report_year)

# [가정 1] 과거보다 지금 기업들의 공시자료에서 CSR 키워드가 더 많이 사용되고 있다.

reports1 = bind_cols(reports1, count_csr_word_df) %>%
  mutate(rcept_no = rcept_no...6)

ggplot(reports1, aes(x = report_year, y = n, color = corp_name, group = corp_name)) + geom_line()

plot = ggplot(reports1, aes(x = report_year, y = n, color = corp_name, group = corp_name)) +
  geom_line() +
  coord_cartesian(ylim = c(0, 10000))
ggplotly(plot)

reports1 %>% filter(str_detect(corp_name, '케이티앤지') & report_year == '2019')
reports1 %>% filter(str_detect(corp_name, '신한지주') & report_year == '2017')
reports1[reports1$rcept_no == '20200320001044', colnames(count_csr_word_df)] = NA
reports1 %>% filter(str_detect(corp_name, '케이티앤지') & report_year == '2019')

reports1$normed_n = 0
for (i in 1:nrow(reports1)) {
  n = reports1[i, ]$n
  ns = reports1[reports1$corp_code == reports1[i, ]$corp_code, ]$n
  reports1[i, 'normed_n'] = (n - min(ns)) / (max(ns) -
                                               min(ns))
}

plot = ggplot(reports1, aes(x = report_year, y = normed_n, color = corp_name, group = corp_name)) +
  geom_line()
ggplotly(plot)

reports1$normed_n_weight = 0
for (i in 1:nrow(reports1)) {
  n_weight = reports1[i,]$n_weight
  n_weights = reports1[reports1$corp_code == reports1[i,]$corp_code,]$n_weight
  reports1[i, 'normed_n_weight'] = (n_weight - min(n_weights)) / (max(n_weights) -
                                                                    min(n_weights))
}

plot = ggplot(reports1, aes(x = report_year, y = normed_n_weight, color = corp_name, group = corp_name)) +
  geom_line()
ggplotly(plot)

# 년도와 CSR 키워드 포함 어절의 보고서 내 사용 빈도 간 상관계수 계산
with(reports1, cor.test(report_year, normed_n))
# 년도와 CSR 키워드 포함 어절의 보고서 내 비중 간 상관계수 계산
with(reports1, cor.test(report_year, normed_n_weight))

# [가정 2]과거보다 지금 기업들이 공시자료에서 CSR 키워드를 더 많이 사용함에 따라 외국인 투자자가 증가하였을 것이다.

# KOSPI 상위 50개 기업들의 증권코드를 가져옴
stock_codes = names(table(reports1$stock_code))

if (IS_REPORT_PRINT) {
  # 2022-12-13 03시 경 크롤링한 주가자료 백업 파일
  load(
    url(
      'https://git.ajou.ac.kr/h5k/ebiz-r-programming/raw/main/term2/filtered_stock_data'
    )
  )
} else {
  for (stock_code in stock_codes) {
    stock_data = tibble()
    for (i in 1:150) {
      html = read_html(GET(
        paste0(
          'https://finance.naver.com/item/frgn.naver?code=',
          stock_code,
          '&page=',
          i
        ),
        add_headers(`User-Agent` = 'Mozilla/5.0 (Windows NT 10.0; Win64; x64)')
      ),
      encoding = 'EUC-KR')
      table = html_table(html, fill = T)[[3]]
      colnames(table) = 1:9
      table = tibble(stock_code = stock_code, table)
      stock_data = rbind(stock_data, table)
    }
  }
  filtered_stock_data = tibble()
  for (each_stock_code in names(table(reports1$stock_code))) {
    print(each_stock_code)
    filtered_by_stock_code = stock_data %>%
      filter(stock_code == each_stock_code)
    colnames(filtered_by_stock_code) = c('stock_code', 'date', 2:9)
    filtered_by_stock_code = filtered_by_stock_code %>%
      filter(date != '날짜' & date != '')
    filtered_stock_data = rbind(filtered_stock_data,
                                filtered_by_stock_code %>%
                                  group_by(date) %>%
                                  slice(1))
  }
  filtered_stock_data = filtered_stock_data %>%
    separate('date', c('year', 'month', 'date'), '\\.')
}
# 각 컬럼 별 자료형을 확인한다.
str(filtered_stock_data)
# 외인 지분 컬럼을 기호를 제거 후 숫자로 변경
filtered_stock_data = filtered_stock_data %>%
  separate('9', c('forign_own_percent', 'p'), '\\%')
filtered_stock_data$forign_own_percent =
  str_replace_all(filtered_stock_data$forign_own_percent, ',', '')
filtered_stock_data$forign_own_percent = as.numeric(filtered_stock_data$forign_own_percent)
filtered_stock_data = filtered_stock_data %>% select(!p)
# 합쳐져 있는 날짜를 년도/월/일 3개의 컬럼으로 분리
filtered_stock_data$full_date = (filtered_stock_data %>%
                                   unite('year', 'month', 'date', col = 'full_date', sep = '') %>%
                                   select(full_date))$full_date

# 데이터의 분포를 확인하기 위해 x축이 `날짜`, y축이 `외국인 투자자 보유율`로된 산점도를 생성하여 시각화를 진행하였다.
filtered_stock_data %>% ggplot(aes(full_date, forign_own_percent, color = year)) + geom_point()
filtered_stock_data %>% filter(forign_own_percent > 10000)
filtered_stock_data = filtered_stock_data %>% filter(forign_own_percent <= 10000)
filtered_stock_data %>% ggplot(aes(full_date, forign_own_percent, color = year)) + geom_point()
filtered_stock_data %>% filter(forign_own_percent > 100)
filtered_stock_data = filtered_stock_data %>% filter(forign_own_percent <= 100)

# 아래 코드로 x축으로 `년도`, y축으로 `csr 키워드 포함 어절 수`, 점의 크기로 `종목별 연간 외국인 투자자 지분율 평균`을 가진 그래프를 생성하여 추세를 확인하고자 한다.

# 종목과 년도별로 외국인 투자자 지분율의 평균을 계산하여 저장해둔다.
summrized_stock_data = filtered_stock_data %>%
  dplyr::group_by(year, stock_code) %>%
  summarise(
    forign_own_percent_mean = mean(forign_own_percent),
    forign_own_percent_median = median(forign_own_percent)
  )
# 보고서 목록 데이터에 위에서 계산한 외국인 투자자 지분율의 평균을 합친다.
reports1$forign_own_percent_mean = 0
for (i in 1:nrow(reports1)) {
  forign_own_percent_mean = (
    summrized_stock_data %>%
      filter(
        year == reports1[i, ]$report_year &
          stock_code == reports1[i, ]$stock_code
      ) %>%
      slice(1)
  )$forign_own_percent_mean
  # 주가 데이터가 없는 경우도 있음
  if (length(forign_own_percent_mean) == 0) {
    reports1[i, 'forign_own_percent_mean'] = NA
  } else {
    reports1[i, 'forign_own_percent_mean'] = forign_own_percent_mean
  }
}

reports1$normed_forign_own_percent_mean = 0
for (i in 1:nrow(reports1)) {
  mean = reports1[i, ]$forign_own_percent_mean
  means = reports1[reports1$corp_code == reports1[i, ]$corp_code, ]$forign_own_percent_mean
  reports1[i, 'normed_forign_own_percent_mean'] =
    (mean - min(means, na.rm = T)) / (max(means, na.rm = T) -
                                        min(means, na.rm = T))
}

plot = reports1 %>%
  ggplot(aes(x = normed_forign_own_percent_mean, y = normed_n)) +
  geom_point(alpha = 2/3)
ggplotly(plot)

# `csr 키워드 포함 어절 수`와 `종목별 연간 외국인 투자자 지분율 평균` 간의 상관계수 계산
with(reports1, cor.test(normed_n, normed_forign_own_percent_mean))
# `csr 키워드 포함 어절 출현빈도`와 `종목별 연간 외국인 투자자 지분율 평균` 간의 상관계수 계산
with(reports1, cor.test(normed_n_weight, normed_forign_own_percent_mean))

# [가정 3] 기업들의 공시자료에서 CSR 키워드를 많이 사용하였을 때 기업가치가 오를 것이다.

# https://github.com/FinanceData/marcap
# 시가총액 데이터셋을 KOSPI 상위 50개 기업의 종목코드으로 필터링하여 백업
# 2010년부터 2022-12-13까지의 데이터셋을 백업
load(url('https://git.ajou.ac.kr/h5k/ebiz-r-programming/raw/main/term2/stock_table'))
stock_table = stock_table %>%
  separate('Date', c('year', 'month', 'date'), '\\-')
summarised_marcap = stock_table %>%
  group_by(Code, year) %>%
  dplyr::summarise(mean_marcap = mean(Marcap))
# 보고서 목록에 시가총액 열 추가
reports1$marcap = 0
for (i in 1:nrow(reports1)) {
  mean_marcap = (
    summarised_marcap %>%
      filter(year == reports1[i,]$report_year &
               Code == reports1[i,]$stock_code) %>%
      slice(1)
  )$mean_marcap
  # 주가 데이터가 없는 경우도 있음
  if (length(mean_marcap) == 0) {
    reports1[i, 'mean_marcap'] = NA
  } else {
    reports1[i, 'mean_marcap'] = mean_marcap
  }
}

reports1$normed_mean_marcap = 0
for (i in 1:nrow(reports1)) {
  mean_marcap = reports1[i, ]$mean_marcap
  mean_marcaps = reports1[reports1$corp_code == reports1[i, ]$corp_code, ]$mean_marcap
  reports1[i, 'normed_mean_marcap'] =
    (mean_marcap - min(mean_marcaps, na.rm = T)) / (max(mean_marcaps, na.rm = T) -
                                                      min(mean_marcaps, na.rm = T))
}

plot = head(reports1, 100) %>% ggplot(aes(
  x = report_year,
  y = normed_mean_marcap,
  color = corp_name,
  group = corp_name
)) + geom_line()
ggplotly(plot)

# 시각화는 x축은  `csr 키워드 포함 어절 출현빈도`, y축은 `종목별 연간 시가총액 평균`으로 그래프를 생성하였고, 두 축의 양 극단은 2020년 코로나 사태로 인해 외부의 영향을 많이 받았을 것을 고려하여 범위를 설정하였다.
plot = reports1 %>% ggplot(aes(
  x = normed_n_weight,
  y = normed_mean_marcap,
  color = corp_name,
  group = corp_name
)) + geom_point() + coord_cartesian(xlim = c(0.1, 0.9), ylim = c(0.1, 0.9))
ggplotly(plot)

# `csr 키워드 포함 어절 비중`와 `종목별 연간 시가총액 평균` 간 상관계수 계산
reports1 %>%
  filter(normed_n_weight >= 0.05 & normed_n_weight <= 0.95 ) %>%
  filter(normed_mean_marcap >= 0.05 & normed_mean_marcap <= 0.95 ) %>% 
  with(cor.test(normed_n_weight, normed_mean_marcap))
# `csr 키워드 포함 어절 출현빈도`와 `종목별 연간 시가총액 평균` 간 상관계수 계산
reports1 %>%
  filter(normed_n >= 0.05 & normed_n <= 0.95 ) %>%
  filter(normed_mean_marcap >= 0.05 & normed_mean_marcap <= 0.95 ) %>% 
  with(cor.test(normed_n, normed_mean_marcap))