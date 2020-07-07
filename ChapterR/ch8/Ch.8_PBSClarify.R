################################################################################
########### Ch.8. Parametric Bootstrapping: We came, we saw, we CLARIFYed
########### R codes for Practice
########### Sanghoon Park, Univ. of South Carolina 
################################################################################

pacman::p_load(ezpickr, mvtnorm, tidyverse)

QOG <- pick(file = 
              "http://www.qogdata.pol.gu.se/data/qog_std_ts_jan20.dta")
QOG2 <- QOG %>% 
  dplyr::filter(year == 2015) %>% 
  dplyr::select(ccode, wdi_gdpcapcon2010, p_polity2, wdi_trade, 
                wdi_pop1564, wdi_agedr, wdi_araland) %>% drop_na()

model1 <- lm(log2(wdi_gdpcapcon2010) ~ wdi_trade + p_polity2 + 
               I(wdi_trade * p_polity2) + wdi_araland + wdi_agedr + 
               I(wdi_araland * wdi_agedr), data=QOG2)
model1 %>% broom::tidy() %>% 
  mutate_if(is.numeric, round, 3) %>% 
  knitr::kable()

set.seed(19891224)
beta_draws <- rmvnorm(n=1000, mean = coef(model1), sigma=vcov(model1))
head(beta_draws)

trade <- quantile(QOG2$wdi_trade, c(0.15, 0.85))
trade
democracy <- c(seq(-10, 10, by=1))
democracy

# 낮은 수준의 무역개방성
LowTrade <- cbind(1, trade[1], 
                  democracy, 
                  I(trade[1] * democracy),
                  mean(QOG2$wdi_araland), mean(QOG2$wdi_agedr), 
                  I(mean(QOG2$wdi_araland) * mean(QOG2$wdi_agedr)))
# 높은 수준의 무역개방성
HighTrade <- cbind(1, trade[2], 
                   democracy, 
                   I(trade[2] * democracy),
                   mean(QOG2$wdi_araland), mean(QOG2$wdi_agedr), 
                   I(mean(QOG2$wdi_araland) * mean(QOG2$wdi_agedr)))

LowTrade.ME <- t(LowTrade %*% t(beta_draws)) ## ME는 Marginal Effect입니다.
LT.mean <- apply(LowTrade.ME, 2, mean) ## 구해진 1,000개의 예측값의 평균
LT.se <- apply(LowTrade.ME,2, 
               quantile, c(0.025, 0.975)) ## 구해진 1,000개의 예측값의 표준편차

HighTrade.ME <- t(HighTrade %*% t(beta_draws))
HT.mean <- apply(HighTrade.ME, 2, mean)
HT.se <- apply(HighTrade.ME, 2, 
               quantile, c(0.025, 0.975)) ## 2.5/97.5 perentile

LT <- data.frame(Democracy=democracy,
                 Group = "Low Trade", ## 시뮬레이션이니 직접 하위 5%, 상위 5%의
                 # 관측치를 95% 신뢰구간을 위해 사용할 수 있습니다.
                 Mean=LT.mean, Lower=LT.se[1,], Upper= LT.se[2,])
HT <- data.frame(Democracy=democracy,
                 Group = "High Trade", ## 시뮬레이션이니 직접 하위 5%, 상위 5%의
                 # 관측치를 95% 신뢰구간을 위해 사용할 수 있습니다.
                 Mean=HT.mean, Lower=HT.se[1,], Upper= HT.se[2,])
Trade <- bind_rows(LT, HT)

Trade %>% 
  ggplot(aes(x=Democracy, y=Mean, color=Group, shape=Group)) + 
  geom_point() +
  geom_pointrange(aes(y = Mean, ymin = Lower, ymax = Upper)) + 
  scale_x_continuous(breaks = democracy)+
  theme(axis.text.x  = element_text(vjust=0.5)) + 
  labs(#title="Economy Size (logged) by the Level of Democracy",
    x="Level of Democracy", y="Economy Size (logged)",
    caption="Vertical bars indicate 95-percent confidence intervals") +
  theme_bw() + theme(legend.position = "bottom")

age <- quantile(QOG2$wdi_agedr, c(0.15, 0.85))
age
summary(QOG2$wdi_araland)
agriland <- c(seq(0, 60, by = 5))


# 여기서 중요한 점은 예측변수의 행렬을 만들 때, 나중에 행렬곱셈을 해줄 시뮬레이
# 션된 계수값들의 순서는 OLS 분석에 투입된 변수 순서와 같다는 것입니다.
# 이를 고려해서 변수를 조작해주어야 합니다.

LowAge <- cbind(1, mean(QOG2$wdi_trade), mean(QOG2$p_polity2), 
                I(mean(QOG2$wdi_trade) * mean(QOG2$p_polity2)),
                agriland, age[1], 
                I(agriland * age[1]))

HighAge <- cbind(1, mean(QOG2$wdi_trade), mean(QOG2$p_polity2), 
                 I(mean(QOG2$wdi_trade) * mean(QOG2$p_polity2)),
                 agriland, age[2], 
                 I(agriland * age[2]))

LowAge.ME <- t(LowAge %*% t(beta_draws))
LA.mean <- apply(LowAge.ME, 2, mean)
LA.se <- apply(LowAge.ME, 2, 
               quantile, c(0.025, 0.975))

HighAge.ME <- t(HighAge %*% t(beta_draws))
HA.mean <- apply(HighAge.ME, 2, mean)
HA.se <- apply(HighAge.ME, 2, 
               quantile, c(0.025, 0.975))

LA <- data.frame(Arable=agriland,
                 Group = "Low Age Dependency",
                 Mean=LA.mean, Lower=LA.se[1,], Upper= LA.se[2,])
HA <- data.frame(Arable=agriland,
                 Group = "High Age Dependency",
                 Mean=HA.mean, Lower=HA.se[1,], Upper= HA.se[2,])
Age <- bind_rows(LA, HA)

Age %>% 
  ggplot(aes(x=Arable, y=Mean, color=Group, shape=Group)) + 
  geom_point() +
  geom_pointrange(aes(y = Mean, ymin = Lower, ymax = Upper)) + 
  scale_x_continuous(breaks = agriland) +
  theme(axis.text.x  = element_text(vjust=0.5)) + 
  labs(#title="Economy Size (logged) by the Level of Arable land",
    x="Arable land (% of land area)", y="Economy Size (logged)",
    caption="Vertical bars indicate 95-percent confidence intervals") +
  theme_bw() + 
  theme(legend.position = "bottom",
        legend.text=element_text(size=8.5))