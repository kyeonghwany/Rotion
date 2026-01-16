# Rotion

Notion 데이터 소스(데이터베이스)를 조회해 tibble로 반환하는 R
패키지입니다. 필터/정렬 헬퍼와 속성 값 변환을 제공해, Notion API 응답을
R 분석 워크플로에 쉽게 연결할 수 있습니다.

## 특징

- Notion 데이터 소스 쿼리 및 전체 페이지 페이징 지원
- 필터/정렬 빌더 제공(텍스트, 날짜, 선택형 등)
- Notion 속성 값을 R 친화적인 스칼라/리스트로 변환
- 결과를 바로 tibble로 변환

## 설치

현재는 로컬 또는 Git 저장소에서 설치해 사용할 수 있습니다.

``` r
install.packages("devtools")
devtools::install("kyeonghwany/Rotion")
```

## 시작하기

### 인증 설정

Notion 통합 토큰을 환경 변수로 등록합니다.

``` r
Sys.setenv(NOTION_TOKEN = "secret_...")
```

### 기본 조회

``` r
library(Rotion)

# 데이터 소스(데이터베이스) ID로 단일 페이지 쿼리
resp <- notion_ds_query("DATA_SOURCE_ID")

# 전체 페이지를 모두 가져오기
pages <- notion_ds_query_all("DATA_SOURCE_ID")

# 결과를 tibble로 변환
df <- notion_ds_tibble("DATA_SOURCE_ID")
```

## 필터와 정렬

``` r
library(Rotion)

my_filter <- filter_and(
  filter_text_contains("이름", "프로젝트"),
  filter_date_on_or_after("시작일", "2025-01-01"),
  filter_timestamp_on_or_after("last_edited_time", "2025-02-01")
)

my_sorts <- list(
  sort_desc("시작일")
)

df <- notion_ds_tibble(
  "DATA_SOURCE_ID",
  filter = my_filter,
  sorts = my_sorts
)
```

## 속성 값 변환

Notion 속성 타입에 따라 다음과 같이 변환됩니다.

- `title`, `rich_text`: 문자열
- `number`: 숫자
- `select`: 선택 값 문자열
- `multi_select`: 문자열 벡터(리스트 컬럼)
- `date`: ISO 날짜/시간 문자열
- `checkbox`: 논리값
- `url`, `email`, `phone_number`: 문자열
- `created_time`, `last_edited_time`: 문자열
- `formula`: 타입에 따라 문자열/숫자/논리/날짜

`notion_prop_value()`를 통해 개별 속성을 직접 변환할 수도 있습니다.

## 옵션

필요 시 Notion API 기본 URL과 버전을 옵션으로 변경할 수 있습니다.

``` r
options(
  Rotion.base_url = "https://api.notion.com/v1",
  Rotion.notion_version = "2025-09-03"
)
```

## 라이선스

MIT 라이선스를 따릅니다.
