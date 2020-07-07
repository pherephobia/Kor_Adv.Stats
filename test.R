pacman::p_load(ezpickr, ggplot2, tidyverse)
QOG <- pick("C:/Users/phere/Dropbox/Scholar/2_Graduates/2020_02_Summer/Hur_data/1. Economic Sanctions/qog_std_ts_jan20.dta")
QOG2 <- QOG %>% 
  dplyr::filter(year==2015) %>% 
  select(ccode, p_polity2, wdi_trade, wdi_gdpcapcon2010) %>% 
  drop_na()

model1 <- lm(log2(wdi_gdpcapcon2010) ~ ## 종속변수는 로그를 취한 1인당 GDP
               1 + p_polity2 + wdi_trade, data=QOG2) ## 1은 절편을 의미

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
  ind <- sample(1:nrow(QOG2), size=nrow(QOG2), replace=T)
  mod <- lm(log2(wdi_gdpcapcon2010) ~ 1 +  p_polity2 + wdi_trade, data=QOG2[ind,])
  holder1000[i,] <- coef(mod)[2]
}

## 10000번 Bootstrapping
holder10000 <- matrix(NA, 10000)
for(i in 1:10000){
  ind <- sample(1:nrow(QOG2), size=nrow(QOG2), replace=T)
  mod <- lm(log2(wdi_gdpcapcon2010) ~ 1 +  p_polity2 + wdi_trade, data=QOG2[ind,])
  holder10000[i,] <- coef(mod)[2]
}

## 100000번 Bootstrapping
holder100000 <- matrix(NA, 100000)
for(i in 1:100000){
  ind <- sample(1:nrow(QOG2), size=nrow(QOG2), replace=T)
  mod <- lm(log2(wdi_gdpcapcon2010) ~ 1 +  p_polity2 + wdi_trade, data=QOG2[ind,])
  holder100000[i,] <- coef(mod)[2]
}
rm(QOG)
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

# 각 부트스트랩 횟수별로 평균을 구해주겠습니다. 그리고 비교대상인 OLS의
# 민주주의 계수값 결과도 추가해주고요.
npbs <- npbs %>% group_by(NPBS) %>% mutate(
  `NPBS Mean` = mean(Estimates, na.rm = T),
  `OLS estimates` = coef(summary(model1))[2,1],
  `OLS se` = coef(summary(model1))[2,2]
) %>% ungroup()


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
