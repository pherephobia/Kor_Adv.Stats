################################################################################
########### Ch.7. Matrix OLS and Non-Parametric Bootstrapping (NPBS) 
########### R codes for Practice
########### Sanghoon Park, Univ. of South Carolina 
################################################################################

# 패키지 불러오기
pacman::p_load(ezpickr, ggplot2, tidyverse)

# QoG 데이터셋 다운로드 및 로드
QOG <- pick(file = 
              "http://www.qogdata.pol.gu.se/data/qog_std_ts_jan20.dta")

# 분석에 사용할 서브셋 만들기
QOG2 <- QOG %>% 
  dplyr::filter(year==2015) %>%  ## 2015년 데이터만 사용
  dplyr::select(ccode, p_polity2, wdi_trade, wdi_gdpcapcon2010) %>% ## 변수 선정
  drop_na() ## 결측치 제외

# 모델 추정: OLS
model1 <- lm(log2(wdi_gdpcapcon2010) ~ ## 종속변수는 로그를 취한 1인당 GDP
               1 + p_polity2 + wdi_trade, data=QOG2) ## 1은 절편을 의미

# 행렬 OLS
# 예측변수 행렬
X <- model.matrix(object = log2(wdi_gdpcapcon2010) ~ 1 +  
                    p_polity2 + wdi_trade, data=QOG2)

# 종속변수 벡터
Y <- log2(QOG2$wdi_gdpcapcon2010)

# 행렬 계산을 통한 계수값 및 분산, 공분산 추정
beta.Hat <- solve(t(X) %*% X) %*% t(X) %*% Y
sig.sq <- sum((Y - X%*%beta.Hat)^2)/(nrow(X)-ncol(X)) ## 분산이죠?
VCV <- sig.sq*chol2inv(chol(t(X)%*%X))
SE <- sqrt(diag(VCV))              

# 행렬 OLS 결과
cbind(beta.Hat, SE) %>% knitr::kable()  

# R lm()을 이용한 OLS 결과
coef(summary(model1)) %>% knitr::kable()

################################################################################
# 비모수 부트스트래핑 (Non-parametric Bootstrapping)
################################################################################

# 비모수 부트스트랩 표본추출 횟수 변화에 따른 결과의 차이를 보겠습니다.
# 먼저 100번 부트스트랩을 할 경우입니다.
holder100 <- matrix(NA, 100) ## 일단 100번 추출한 결과가 들어갈 빈 행렬을 만듭니다.
for(i in 1:100){  ## 함수를 만듭니다. i는 1부터 100까지를 의미합니다.
  ind <- sample(1:nrow(QOG2), ## ind는 전체 관측치(각 행의 번호) 중에서 
                ## QOG2와 동일한 표본규모로 표집됩니다.
                size=nrow(QOG2), replace=T) ## 복원표집된 결과입니다.
  # 이 ind가 헷갈리실 텐데, 뭐냐면 넘버링입니다. 매번 무작위로 넘버링이 
  # 나올거고 이 넘버링에 속하는 관측치들은 개별 부트스트랩 표본을 구성합니다.
  # 그 과정이 i번, 즉 1~100까지 반복되어 총 100개의 부트스트랩 표본이 생깁니다.
  mod <- lm(log2(wdi_gdpcapcon2010) ~ ## 모델은 앞서와 동일하게 적용합니다.
              1 +  p_polity2 + wdi_trade, 
            data=QOG2[ind,]) ## ind에 속하는 행의 관측치만을 가지고 모델분석
  holder100[i,] <- coef(mod)[2] ## 이렇게 새로 부트스트랩 표본으로 모델을 
  # 분석해 나온 계수값만을 아까 만든 깡통행렬에 차곡차곡 하나씩 저장.
  # 민주주의에 대한 계수의 결과만을 보겠습니다.
}
## 1000번 Bootstrapping
holder1000 <- matrix(NA, 1000)
for(i in 1:1000){
  ind <- sample(1:nrow(QOG2), 
                size=nrow(QOG2), replace=T)
  mod <- lm(log2(wdi_gdpcapcon2010) ~ 
              1 +  p_polity2 + wdi_trade,
            data=QOG2[ind,])
  holder1000[i,] <- coef(mod)[2]
}

## 10000번 Bootstrapping
holder10000 <- matrix(NA, 10000)
for(i in 1:10000){
  ind <- sample(1:nrow(QOG2),
                size=nrow(QOG2), replace=T)
  mod <- lm(log2(wdi_gdpcapcon2010) ~ 
              1 +  p_polity2 + wdi_trade,
            data=QOG2[ind,])
  holder10000[i,] <- coef(mod)[2]
}

## 100000번 Bootstrapping
holder100000 <- matrix(NA, 100000)
for(i in 1:100000){
  ind <- sample(1:nrow(QOG2), 
                size=nrow(QOG2), replace=T)
  mod <- lm(log2(wdi_gdpcapcon2010) ~ 
              1 +  p_polity2 + wdi_trade, 
            data=QOG2[ind,])
  holder100000[i,] <- coef(mod)[2]
}

# 각각의 부트스트랩한 계수값 결과를 하나의 데이터로 구축
npbs <- bind_rows(
  holder100 %>%
    as_tibble() %>% mutate(NPBS = "100") %>% 
    rename(Estimates = V1),
  holder1000 %>%
    as_tibble() %>% mutate(NPBS = "1000") %>% 
    rename(Estimates = V1),
  holder10000 %>%
    as_tibble() %>% mutate(NPBS = "10000") %>% 
    rename(Estimates = V1),
  holder100000 %>%           
    as_tibble() %>% mutate(NPBS = "100000") %>% 
    rename(Estimates = V1))

# 각 부트스트랩 횟수별로 평균을 계산. 그리고 비교대상인 OLS의
# 민주주의 계수값 결과도 추가
npbs <- npbs %>% group_by(NPBS) %>% mutate(
  `NPBS Mean` = mean(Estimates, na.rm = T),
  `OLS estimates` = coef(summary(model1))[2,1],
  `OLS se` = coef(summary(model1))[2,2]
) %>% ungroup()

# 네 개 패널로 구성된 부트스트랩 vs. OLS 계수비교 플롯
npbs %>%
  ggplot(aes(x=Estimates, colour=NPBS, fill=NPBS)) + 
  geom_text(aes(label=round(unique(`OLS estimates`), 3), 
                x=0.15, y=13), 
            color="black", hjust=1) + 
  geom_histogram(data = npbs %>% dplyr::filter(NPBS=="100"), 
                 aes(y=..density..), show.legend = F,
                 colour="black", fill="white") + 
  geom_vline(data = npbs %>% dplyr::filter(NPBS=="100"), 
             aes(xintercept = mean(Estimates)), 
             color = "red", size=1, linetype="solid") + 
  geom_text(data = npbs %>% dplyr::filter(NPBS=="100"), 
            aes(label=round(mean(Estimates), 3), x=0.15, y=15), 
            colour="red", hjust=1) + 
  geom_histogram(data = npbs %>% dplyr::filter(NPBS=="1000"), 
                 aes(y=..density..), show.legend = F,
                 colour="black", fill="white") + 
  geom_vline(data = npbs %>% dplyr::filter(NPBS=="1000"), 
             aes(xintercept = mean(Estimates)), 
             color = "blue", size=1, linetype="solid") + 
  geom_text(data = npbs %>% dplyr::filter(NPBS=="1000"), 
            aes(label=round(mean(Estimates), 3), x=0.15, y=15), 
            colour="blue", hjust=1) +  
  geom_histogram(data = npbs %>% dplyr::filter(NPBS=="10000"), 
                 aes(y=..density..), show.legend = F,
                 colour="black", fill="white") + 
  geom_vline(data = npbs %>% dplyr::filter(NPBS=="10000"), 
             aes(xintercept = mean(Estimates)), 
             color = "red", size=1, linetype="solid") + 
  geom_text(data = npbs %>% dplyr::filter(NPBS=="10000"), 
            aes(label=round(mean(Estimates), 3), x=0.15, y=15), 
            colour="red", hjust=1) + 
  geom_histogram(data = npbs %>% dplyr::filter(NPBS=="100000"), 
                 aes(y=..density..), show.legend = F,
                 colour="black", fill="white") + 
  geom_vline(data = npbs %>% dplyr::filter(NPBS=="100000"), 
             aes(xintercept = mean(Estimates)), 
             color = "red", size=1, linetype="solid") + 
  geom_text(data = npbs %>% dplyr::filter(NPBS=="100000"), 
            aes(label=round(mean(Estimates), 3), x=0.15, y=15), 
            colour="red", hjust=1) + 
  geom_density(alpha = .2, show.legend = F) + facet_wrap(~NPBS) + 
  stat_function(fun = dnorm, colour = "black",
                args = c(mean = unique(npbs$`OLS estimates`), 
                         sd = unique(npbs$`OLS se`))) + 
  labs(title = "Coefficient Estimates of the Bootstrapped vs. OLS",
       subtitle = "(Red: means of bootstrapped estimates / Black: OLS estimate)",
       x = "Coefficient of the level of democracy on the size of economy (logged)", 
       y = "Density") +
  theme_bw() + theme(legend.position = "none")

# 표준화 변수를 이용한 부트스트랩과 OLS 의 결과 비교
# 2sd 표준화
QOG2 <- QOG2 %>%
  mutate(
    std.polity = p_polity2 - mean(p_polity2)/2*sd(p_polity2))

# 새롭게 모델링
model.sd <- lm(log2(wdi_gdpcapcon2010) ~ std.polity, data=QOG2)

# 결과
model.sd %>% broom::tidy() %>% 
  mutate_if(is.numeric, round, 3) %>% knitr::kable()

# 비교를 위한 원 변수 모델
model2 <- lm(log2(wdi_gdpcapcon2010) ~ p_polity2, data=QOG2)
model2 %>% broom::tidy() %>% 
  mutate_if(is.numeric, round, 3) %>% knitr::kable()

holder.std <- matrix(NA, 4000)
for(i in 1:4000){
  ind <- sample(1:nrow(QOG2), size=nrow(QOG2), replace=T)
  mod <- lm(log2(wdi_gdpcapcon2010) ~ std.polity, data=QOG2[ind,])
  holder.std[i,] <- coef(mod)[2]
}

# 어떤 방식으로 데이터를 구성했는지 조금 직관적으로 이해할 수 있게
# 단순하고 늘어지는 코드를 써보았습니다.
# 먼저 부트스트랩 결과입니다.
std.data1 <- data.frame(ID="Bootstrapped",
                        Low=quantile(holder.std, 0.025), 
                        M=mean(holder.std),
                        High=quantile(holder.std, 0.975))
# 부트스트랩은 이미 우리가 4000개의 관측치를 가지고 있기 때문에 이 관측치에서
# 왼쪽꼬리 2.5백분위와 우측꼬리 2.5백분위에 해당하는 값이 95% 신뢰구간의
# 기준값이 됩니다.

# OLS 결과입니다.
std.data2 <- data.frame(ID="OLS",
                        Low=coef(summary(model.sd))[2,1] -
                          1.96*coef(summary(model.sd))[2,2],
                        M=coef(summary(model.sd))[2,1],
                        High=coef(summary(model.sd))[2,1] +
                          1.96*coef(summary(model.sd))[2,2])
std.data <- rbind(std.data1, std.data2)

# 플로팅
std.data %>% ggplot(aes(x = ID, color = ID)) +
  geom_pointrange(aes(y = M, ymin = Low, ymax = High)) +
  labs(x="Methods", y="Estimates") + 
  geom_text(aes(label=round(M[ID=="Bootstrapped"], 4), 
                x=0.9, y=M[ID=="Bootstrapped"]), 
            colour="black", hjust=1) + 
  geom_text(aes(label=round(M[ID=="OLS"], 4), 
                x=1.9, y=M[ID=="OLS"]), 
            colour="black", hjust=1) + theme_bw() + 
  theme(legend.position = "none")