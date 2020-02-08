# Datamining for Business Anaytics - R edition

## 데이터마이닝 교재 내용 솔루션

Galit Shmueli, Peter C. Bruce 등이 쓴 'Datamining for Business Analytics'입니다. 2019년 데이터마이닝 수업을 들으며 작성한 코드와 과제를 업로드 합니다.

<<<<<<< HEAD
![](C:\\Users\\ycg00\\Documents\\datamining-for-business-analytics-solution\\image\\Datamining.png)
=======
![](Datamining.png)
>>>>>>> 04914a671dd704e789bb36cbcb7255b85c101774


## documents

해당 교재를 통해 작성한 과제물을 업로드
 
 ### 예시 3.4
 
![](C:\\Users\\ycg00\\Documents\\datamining-for-business-analytics-solution\\image\\이미지1.png)
> 위 그림을 보면 configuration ~ Retail.Price 가 Configuration의 증가에 맞게 Retail.Price가 증가하는 비례관계임을 알 수 있다. 이 형태를 좀 더 명확하게 보기 위해 회귀선을 부여했다. 회귀선을 부여한 결과, 두 변수의 상관관계를 더 강하게 볼 수 있었다. 노트북의 판매가는 쪼개진 6개의 집단의 구간으로 추정할 수 있다.

![](C:\\Users\\ycg00\\Documents\\datamining-for-business-analytics-solution\\image\\이미지2.png)
> 위 그림은 월별-판매가의 자료 그래프다. 해당 노트북 가격의 누적량을 통해 연초가 연말보다 더 높은 가격을 형성한다고 볼 수 있다. 10월과 12월 사이의 노트북 판매가격은 급격하게 하락한다. 10월과 12월 사이에 추수감사절, 크리스마스 등의 이벤트가 영향을 끼친 것인지 조심스럽게 추측해본다.
>
>요일 데이터의 경우, 1월부터 9월까지는 요일 간 판매가격에 차이가 보이지 않는다. 그러나 10월부터 12월 사이의 데이터에는 유의미한 변화가 보인다. 10월 (금, 토, 일), 11월(월, 화, 수), 12월(수, 목, 금) 판매가가 타 요일에 비해 상대적으로 저렴하다. ‘런던 컴퓨터’가 월마다 해당 요일에 행사를 열었을 거라 생각해본다.

![](C:\\Users\\ycg00\\Documents\\datamining-for-business-analytics-solution\\image\\이미지3.png)
> 분기별 데이터의 경우, 1, 2, 3분기에 비해 4분기 판매량이 급격히 하락했다. 월별 데이터로 파악한 결과, 10, 11, 12월의 매출 하락이 동일한 결과로 나타났다고 본다. 해당 시기의 요일 데이터는 4분기 요일(수,금)이 타 요일에 비해 판매량이 부진한 것을 제외하고는 유의미한 차이가 보이지 않았다.


## R code

해당 교재를 통해 작성한 코드를 업로드


## Dataset

'Datamining for Business Analytics(in R edition)'에서 제공한 데이터셋을 다운로드. 일부 데이터는 직접 다운로드도 해야함