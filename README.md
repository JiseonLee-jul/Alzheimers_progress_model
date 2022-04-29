# [2020] Disease progression model in Alzheimer's disease

### 프로젝트 개요

#### [Notion page 바로가기](https://www.notion.so/2020-Disease-progression-model-in-Alzheimer-s-disease-22bfce3ec70e48388208fa76f2ff87ed)

---
### 분석 내용
#### 1. data_gen.R
- 시뮬레이션 parameter의 조합을 데이터프레임으로 생성
- 시뮬레이션 parameter를 변경하며 데이터 생성 가능한 함수 생성
##### - v2 : parameter type, correlation matric type, missing rate type 변수 추가
##### - v3 : 불필요한 변수 제거하고 간결하게 정리

#### 2. match_pt.R
- cohort 1과 cohort 2의 각 추정값의 bootstrap 신뢰구간 구하기
- cohort 1과 cohort 2가 처음으로 만나는 지점을 찾기
- 위의 과정을 그래프로 그리기

#### 3. excase.R
- 예시 케이스를 함수에 넣어 결과 도출

#### 4. iter.R
- 시뮬레이션을 100번 반복 후 평균 추정 paramter값을 도출
- 추정 paramter값을 이용한 모델 그래프 그리기
