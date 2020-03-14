calculate_wt = function( dataframe , spread_days , delta_spread_days , std ) {
  
  w_t = dataframe %>%
    arrange(date) %>%
    mutate(
           delta_spread = spread - lag(spread) , 
           rolling_spread_sd  = rollapply(delta_spread ,width = delta_spread_days , FUN = sd , align = "right" , fill=NA) , 
           rolling_spread_mean  = rollapply(spread ,width = spread_days , FUN = mean , align = "right" , fill=NA) ,
           upper_s = lag(rolling_spread_mean) + (lag(rolling_spread_mean) -spread )* as.numeric(date-lag(date))/252 + std *lag(rolling_spread_sd),
           # upper_s = pnorm(upper_s , mean =  lag(rolling_spread_mean) , sd = lag(rolling_spread_sd)),
           low_s = lag(rolling_spread_mean) + (lag(rolling_spread_mean) -spread )* as.numeric(date-lag(date))/252 - std *lag(rolling_spread_sd) ,
           # low_s = pnorm(low_s , mean =  lag(rolling_spread_mean) , sd = lag(rolling_spread_sd)),
           # wt = case_when(spread>=upper_s ~ 1 ,
           #                spread<=low_s ~ 0 ,
           #                spread<=upper_s & spread>=low_s ~  ((spread-low_s)/(upper_s - low_s))   )
           wt = case_when( spread > upper_s  ~  0  ,
                           spread < low_s ~ 1 ,
                           spread<=upper_s & spread>=low_s ~  ((spread-low_s)/(upper_s - low_s)) ))
  return(w_t %>% select(date , wt , upper_s , low_s  , spread ,))
}

calculate_wt = function( dataframe , spread_days , delta_spread_days , std ) {
  
  w_t = dataframe %>%
    arrange(date) %>%
    mutate(
      spread = BC_20Y - lag( BY_20Y) , 
      delta_spread = spread - lag(spread) , 
      rolling_spread_sd  = rollapply(delta_spread ,width = delta_spread_days , FUN = sd , align = "right" , fill=NA) , 
      rolling_spread_mean  = rollapply(spread ,width = spread_days , FUN = mean , align = "right" , fill=NA) ,
      upper_s = lag(rolling_spread_mean) + (lag(rolling_spread_mean) -spread )* as.numeric(date-lag(date))/252 + std *lag(rolling_spread_sd),
      # upper_s = pnorm(upper_s , mean =  lag(rolling_spread_mean) , sd = lag(rolling_spread_sd)),
      low_s = lag(rolling_spread_mean) + (lag(rolling_spread_mean) -spread )* as.numeric(date-lag(date))/252 - std *lag(rolling_spread_sd) ,
      # low_s = pnorm(low_s , mean =  lag(rolling_spread_mean) , sd = lag(rolling_spread_sd)),
      # wt = case_when(spread>=upper_s ~ 1 ,
      #                spread<=low_s ~ 0 ,
      #                spread<=upper_s & spread>=low_s ~  ((spread-low_s)/(upper_s - low_s))   )
      wt = case_when( spread > upper_s  ~  0  ,
                      spread < low_s ~ 1 ,
                      spread<=upper_s & spread>=low_s ~  ((spread-low_s)/(upper_s - low_s)) ))
  return(w_t %>% select(date , wt , upper_s , low_s  , spread ))
}


bond_yield_TLT = bond_yield %>%
  mutate(spread = BC_20Y - rb) %>%
  filter(date >= min(TLT$Date)) %>%
  select(date , BC_20Y , spread) %>%
  left_join( TLT %>% mutate( date = ymd(date)),by="date") %>%
  na.omit()
# delta 殖利率 報酬COR
bond_yield_TLT = bond_yield_TLT %>%
  mutate( deltaRB = BC_20Y - lag(BC_20Y) , 
          delta_spread = spread - lag(spread) , 
          delta_bond_ret = bond_ret - lag(bond_ret)) %>%
  na.omit()

cor(bond_yield_TLT$deltaRB , bond_yield_TLT$bond_ret)
# 殖利率上升 ，債券報酬下降
cor(bond_yield_TLT$delta_spread , bond_yield_TLT$bond_ret)
# 利差上升，債券報酬下降
cor(bond_yield_TLT$deltaRB , bond_yield_TLT$delta_bond_ret)
# 殖利率上升 , 債券報酬差下降 >> 殖利率上升 , 持有債券應
cor(bond_yield_TLT$delta_spread , bond_yield_TLT$delta_bond_ret)
cor(bond_yield_TLT$spread , bond_yield_TLT$delta_bond_ret)



cat("債券ETF標的:" ,i.bond, ";std:" ,std,";spread_days:" ,spread_days  , ";delta_spread_days:" ,delta_spread_days ,"\n")
find_weight = calculate_wt( bond_yield_TLT , spread_days = spread_days ,
                            std = 3 , delta_spread_days =  delta_spread_days)

find_weight = find_weight %>% filter(date > ymd(20180101))
dataframe = find_weight %>% mutate(wc = 1-wt)
ggplot_dataframe = tidyr::gather(dataframe%>% select(date  , upper_s , low_s) ,
                                 key="name" , value = "value"   , upper_s , low_s)
ggplot_dataframe = ggplot_dataframe %>% mutate( name = ifelse( name=="upper_s" , "利差報酬上限" , "利差報酬下限"))
p = ggplot()+
  geom_line(data = ggplot_dataframe , 
            aes(x = date , y = value , color = name ) , size = 1 , linetype = "dashed")+
  geom_line(data = dataframe , 
            aes(x = date , y = spread , color = "利差"  ))+
  theme_bw() +
  labs(title = paste0("20年公債殖利率與聯邦基金利率利差"), y = "價格", x = "日期", colour = "") +
  theme(plot.title = element_text(hjust = 0.5, size = 20),
        plot.subtitle = element_text(hjust = 0.5, size = 16),
        text = element_text(family = "BL", size = 14),
        legend.position="right",
        axis.title.x=element_blank())+
  ylab("利差(%)")+
  scale_x_date(labels = scales::date_format("%Y-%m-%d") , breaks = "1 years")+
  labs(color = NULL)


data = fread("all_bond_ETF.txt")
data = data [,-4]
colnames(data) = c("code" , "name" , "date" ,"close" ,"volume" )

# 相同追蹤指數之國外ETF

yuan_bond = data %>% filter(name =="元大美債20年" ) %>%
  arrange( date ) %>% filter(date >=20180000)


yuan_bond = yuan_bond %>% mutate( group = "元大美債20年收盤價" ,
                                  bond_ret = log(close / lag(close)))
yuan_bond = yuan_bond %>% filter(date <= 20200200)
p1 <- ggplot(yuan_bond, aes(x = ymd(date), y = round(close, 2), group = group)) +
  geom_line(aes(color = group), size = 1.2) +
  labs(title = paste0("元大美債20年(00679B)價格走勢圖"), 
       subtitle = "回測期間: 2018/01至2020/01", y = "價格", x = "日期", colour = "") +
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5, size = 20),
        plot.subtitle = element_text(hjust = 0.5, size = 16),
        text = element_text(family = "BL", size = 14),
        legend.position="right")+
  scale_x_date(labels = scales::date_format("%Y-%m-%d") , breaks = "1 years")

# 報酬率&權重相關
bond_cor_ret_wt = yuan_bond %>% 
  mutate(date = ymd(date)) %>%
  select(date , bond_ret ) %>% 
  left_join(find_weight , by="date") %>%
  na.omit() %>%
  mutate( delta_wt = wt-lag(wt) ,
          bond_ret = round(bond_ret*100,2) ,
          delta_bond_ret = bond_ret -lag(bond_ret)) %>%
  na.omit()

cor(bond_cor_ret_wt$delta_bond_ret , bond_cor_ret_wt$delta_wt)

ggplot_bond_cor_ret_wt = tidyr::gather(bond_cor_ret_wt %>% select(date  , bond_ret , delta_wt) ,
                                       key="name" , value = "value"   , bond_ret , delta_wt)
ggplot()+
  geom_line(data = ggplot_bond_cor_ret_wt , 
            aes(x = date , y = value , color = name ) , size = 1 )
library(ggpubr)
ggarrange( p , p1 , align = "v" , ncol=1)
