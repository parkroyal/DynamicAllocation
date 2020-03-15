rm(list=ls())

library(data.table)
library(dplyr)
library(lubridate)
library(tidyverse)
library(zoo)
# bond rate
bond_yield = fread("bondrate.csv") %>% arrange(date)
# overnight rate 
rb = fread("fedfundrate.csv")
# sapply(rb, class)
colnames(rb) = c("date" , "rb")
rb = rb %>% mutate(ym = paste0( substr(date,1,4) , substr(date,6,7))) %>% select(-date)
bond_yield = bond_yield %>% 
  mutate(ym = paste0( substr(date,1,6)) , 
         date = ymd(date)) %>% 
  left_join( rb  , by="ym")

names = colnames(bond_yield)[grepl('BC' , colnames(bond_yield) )]
bond_yield[names] <- sapply(bond_yield[names] ,as.numeric)
# sapply(bond_yield, class)

TLT = fread("TLT.csv")
TLT$name  = "IShares 20"

TLT = TLT %>%  
  mutate( bond_ret = log(`Adj Close` / lag(`Adj Close`,1)) ,
          date = paste0(substr(Date , 1, 4 ),substr(Date , 6, 7 ) ,substr(Date , 9, 10 )),
          date = ymd(as.numeric(date)), )  %>%
  mutate(close = (`Adj Close` ) / first(`Adj Close`) ) %>%
  select(date , bond_ret , close)


bond_yield = bond_yield %>% 
  filter(!is.na(rb)) %>%
  # select(-c(BC_1MONTH:BC_6MONTH)) %>%
  na.omit() %>%
  left_join( TLT , by="date" )

rm(TLT , rb)
# 資料合併結束
setwd("C:/Users/powensu/Desktop/R/DynamicAllocation")
save(bond_yield , file="合併後Data.rdata")



# 異常值處理
bond_yield = bond_yield %>% 
  arrange(date) %>%
  mutate(BC_20Y = replace( BC_20Y , BC_20Y == 0 , NA ))%>%
  fill(BC_20Y) %>%
  filter(date >= ymd(20070101)) 

bond_yield = bond_yield %>% na.omit()
# 觀察殖利率
calculate_wt = function( dataframe , spread_days , delta_spread_days , std ) {
  
  w_t = dataframe %>%
    arrange(date) %>%
    mutate(
      spread = BC_5Y - rb , 
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
      wt = case_when( spread > upper_s  ~ 0   ,
                      spread < low_s ~ 1 ,
                      spread<=upper_s & spread>=low_s ~  1-((spread-low_s)/(upper_s - low_s)) ))
  return(w_t %>% select(date , wt , upper_s , low_s  , spread , close))
}

plot = calculate_wt(dataframe = bond_yield , spread_days = 60, delta_spread_days = 60,std = 2)
plot = plot %>% arrange(date) %>%
  mutate(bond_return = log(close / lag(close )),
         delta_wt = log(wt/ lag(wt))) %>%
  na.omit()

apply(plot %>% select(wt : close ,delta_wt), 2 , function(x){ cor(x , plot$bond_return)})

# 合併S&P500指數
SP500 = fread("SP500.txt")
SP500 = SP500[ , 3:4]
colnames(SP500)= c("date" , "sp500")
bond_yield = bond_yield %>% left_join( SP500 %>% mutate( date = ymd(date)) , by="date")
bond_yield = bond_yield %>% 
  mutate(term_spread = BC_5Y - BC_3MONTH , 
         inverse_wealth = WMA(sp500 , n = 60 , ))



ggplot()+
  geom_line(data = plot , 
            aes(x = date , y = close  ) , size = 1 )+
  geom_line(data = plot , 
            aes(x = date , y = wt))


all_equity = data.frame()
all_return = data.frame()
all_ws = data.frame()
load("智慧指數追蹤台股市值前50檔.rdata")
for ( i.bond in unique(bond_ETF$name)){
  # i.files = files[1]
  # i.bond = unique(bond_ETF$name)[1]
  # load(i.files)
  now_bond = bond_ETF %>% filter( name == i.bond)
  all_date = sort( unique(now_bond$date))
  # 月初
  all_date = portfolioRetTable %>%
    mutate(ym = substr(date,1,6)) %>%
    group_by(ym) %>%
    arrange(date) %>%
    filter( row_number() == 1) %>%
    select(date) %>% pull()
  # month_first_date = historyHoldInfoTable %>% select(inDate) %>% pull() %>% unique()
  
  # bond ETF , image , AI_portfolio
  all_data = now_bond
  
  for ( spread_days in c(30,60,120,252)){
    for ( std in seq(0.5,3,0.5)){
      for( delta_spread_days in c(30,60,120,252)){
        cat("債券ETF標的:" ,i.bond, ";lowstd:" ,std,";spread_days:" ,spread_days  , ";delta_spread_days:" ,delta_spread_days ,"\n")
        find_weight = calculate_wt( bond_yield , spread_days = spread_days ,
                                    std = -100 , delta_spread_days =  delta_spread_days)
        
        # find_weight = find_weight %>% filter(date > ymd(20180101))
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
          mutate( delta_wt = wt-lag(wt)) %>%
          na.omit()
        
        cor(bond_cor_ret_wt$bond_ret , bond_cor_ret_wt$delta_wt)
        
        ggplot_bond_cor_ret_wt = tidyr::gather(bond_cor_ret_wt %>% select(date  , upper_s , low_s) ,
                                               key="name" , value = "value"   , upper_s , low_s)
        library(ggpubr)
        ggarrange( p , p1 , align = "v" , ncol=1)
        
        
        i.data = all_data  %>% na.omit()
        value = 1
        initial_value = value
        equity = data.frame()
        weight = data.frame()
        for( i in 1:(length(all_date)-1)){
          # i=1
          cat(i ,"/" ,length(all_date)-1 , "\n")
          # 抓取之前資料 ,如果大於22則計算VC 無則跳至下月
          # vc_data = i.data %>% filter(date< month_first_date[i])
          # if(nrow(vc_data) >= 22){
          
          
          # # 股票權重
          # w_s = volatility_controll(target_v , vc_data$portfolioRet[(nrow(vc_data)-21) : nrow(vc_data)])
          # # 智能投資組合比例
          # w_AI_propotion = 1
          # # bond_ETF 權重
          # w_b = 1-w_s
          # # 圖形辨識權重
          # w_image = w_s * (1-w_AI_propotion)
          # w_AI = w_s * w_AI_propotion
          # 債券權重
          w_b = find_weight %>% filter(date <= ymd(all_date[i]))  %>% select(wt) %>% pull() %>% last()
          # 現金權重
          w_c = 1-w_b
          
          # 抓取該月份資料
          month_equity = i.data %>% filter(Date >= ymd(all_date[i]), Date < ymd(all_date[i+1]))
          # # 當月持有部位
          # month_hold = historyHoldInfoTable %>% filter(portfolioName == i.name,
          #                                              inDate >= month_first_date[i] ,
          #                                              inDate < month_first_date[i+1] ) 
          
          # 紀錄權重 持有部位數量
          weight = bind_rows(weight , data.frame(date = all_date[i] , 
                                                 w_c = w_c , 
                                                 w_b = w_b ))
          
          
          # 計算該月份各部位變化
          month_equity$bond = cumprod( 1+month_equity$bond_ret ) * w_b * value
          month_equity$cash = w_c * value
          # month_equity$image = (1+month_equity$image )* w_image * value
          # month_equity$image = 0
          month_equity = month_equity %>% 
            mutate( equity = bond +cash ) %>%
            select( date , equity, bond , cash ) %>%
            mutate( w_c = w_c , w_b= w_b)
          # equity: date , equity
          # 紀錄資金
          equity = bind_rows(equity , month_equity)
          # 紀錄股票債券權重
          
          # 月末 
          # 計算value
          value = last(equity$equity)
          
        }
        
        
        
        
        equity <- equity %>% mutate(cumRet=(equity)/initial_value -1)
        
        equity <- equity %>% mutate(ret= (equity/lag(equity,1)) -1) %>% filter(!is.na(ret))
        
        # 計算累積報酬率
        cum_ret  = last(equity$cumRet)
        # 對基金日報酬率轉為xts格式
        fundRetXts= xts(equity %>% select(ret), order.by = ymd(equity$date))
        # 年化報酬率
        annual_ret = (last(equity$cumRet )+1)^(365/as.double(ymd(last(equity$date))-ymd(first(equity$date))))-1
        # 夏普比率
        sharpe_ratio = mean(equity$ret) / sd(equity$ret) * (252^(1/2))
        # 最大回撤率
        DD = maxDrawdown(fundRetXts) %>% as.vector()
        
        all_return = bind_rows(all_return , data.frame( std = std , 
                                                        delta_spread_days = delta_spread_days , 
                                                        spread_days = spread_days , 
                                                        bond = i.bond,
                                                        cum_ret = cum_ret , 
                                                        annual_ret = annual_ret ,
                                                        annual_std = unname(StdDev.annualized(fundRetXts)),
                                                        sharpe_ratio = sharpe_ratio , 
                                                        maxDrawdown = DD))
        all_equity = bind_rows(all_equity , equity %>% 
                                 select(date , equity) %>% 
                                 mutate(std = std , 
                                        delta_spread_days = delta_spread_days , 
                                        spread_days = spread_days ,  
                                        bond = i.bond,))
        all_ws = bind_rows( all_ws , weight %>% mutate(std = std , 
                                                       delta_spread_days = delta_spread_days , 
                                                       spread_days = spread_days , 
                                                       bond = i.bond, ))
      }
    }
  }
  
}


# bond_yield_TLT = bond_yield %>%
#   mutate(spread = BC_5Y - rb) %>%
#   filter(date >= min(TLT$Date)) %>%
#   select(date , BC_5Y , spread) %>%
#   left_join( TLT %>% mutate( date = ymd(date)),by="date") %>%
#   na.omit()
# # delta 殖利率 報酬COR
# bond_yield_TLT = bond_yield_TLT %>%
#   mutate( deltaRB = BC_5Y - lag(BC_5Y) , 
#           delta_spread = spread - lag(spread) , 
#           delta_bond_ret = bond_ret - lag(bond_ret)) %>%
#   na.omit()
# 
# cor(bond_yield_TLT$deltaRB , bond_yield_TLT$bond_ret)
# # 殖利率上升 ，債券報酬下降
# cor(bond_yield_TLT$delta_spread , bond_yield_TLT$bond_ret)
# # 利差上升，債券報酬下降
# cor(bond_yield_TLT$deltaRB , bond_yield_TLT$delta_bond_ret)
# # 殖利率上升 , 債券報酬差下降 >> 殖利率上升 , 持有債券應
# cor(bond_yield_TLT$delta_spread , bond_yield_TLT$delta_bond_ret)
# cor(bond_yield_TLT$spread , bond_yield_TLT$delta_bond_ret)


# 
# cat("std:" ,std,";spread_days:" ,spread_days  , ";delta_spread_days:" ,delta_spread_days ,"\n")
# find_weight = calculate_wt( bond_yield_TLT , spread_days = spread_days ,
#                             std = 2 , delta_spread_days =  delta_spread_days)
# 
# 
# find_weight = find_weight %>% filter(date > ymd(20180101))
# dataframe = find_weight %>% mutate(wc = 1-wt)
# ggplot_dataframe = tidyr::gather(dataframe%>% select(date  , upper_s , low_s) ,
#                                  key="name" , value = "value"   , upper_s , low_s)
# ggplot_dataframe = ggplot_dataframe %>% mutate( name = ifelse( name=="upper_s" , "利差報酬上限" , "利差報酬下限"))
# p = ggplot()+
#   geom_line(data = ggplot_dataframe , 
#             aes(x = date , y = value , color = name ) , size = 1 , linetype = "dashed")+
#   geom_line(data = dataframe , 
#             aes(x = date , y = spread , color = "利差"  ))+
#   theme_bw() +
#   labs(title = paste0("20年公債殖利率與聯邦基金利率利差"), y = "價格", x = "日期", colour = "") +
#   theme(plot.title = element_text(hjust = 0.5, size = 20),
#         plot.subtitle = element_text(hjust = 0.5, size = 16),
#         text = element_text(family = "BL", size = 14),
#         legend.position="right",
#         axis.title.x=element_blank())+
#   ylab("利差(%)")+
#   scale_x_date(labels = scales::date_format("%Y-%m-%d") , breaks = "1 years")+
#   labs(color = NULL)
# 
# 
# data = fread("all_bond_ETF.txt")
# data = data [,-4]
# colnames(data) = c("code" , "name" , "date" ,"close" ,"volume" )
# 
# # 相同追蹤指數之國外ETF
# 
# yuan_bond = data %>% filter(name =="元大美債20年" ) %>%
#   arrange( date ) %>% filter(date >=20180000)
# 
# 
# yuan_bond = yuan_bond %>% mutate( group = "元大美債20年收盤價" ,
#                                   bond_ret = log(close / lag(close)))
# yuan_bond = yuan_bond %>% filter(date <= 20200200)
# p1 <- ggplot(yuan_bond, aes(x = ymd(date), y = round(close, 2), group = group)) +
#   geom_line(aes(color = group), size = 1.2) +
#   labs(title = paste0("元大美債20年(00679B)價格走勢圖"), 
#        subtitle = "回測期間: 2018/01至2020/01", y = "價格", x = "日期", colour = "") +
#   theme_bw()+
#   theme(plot.title = element_text(hjust = 0.5, size = 20),
#         plot.subtitle = element_text(hjust = 0.5, size = 16),
#         text = element_text(family = "BL", size = 14),
#         legend.position="right")+
#   scale_x_date(labels = scales::date_format("%Y-%m-%d") , breaks = "1 years")
# 
# # 報酬率&權重相關
# bond_cor_ret_wt = yuan_bond %>% 
#   mutate(date = ymd(date)) %>%
#   select(date , bond_ret ) %>% 
#   left_join(find_weight , by="date") %>%
#   na.omit() %>%
#   mutate( delta_wt = wt-lag(wt) ,
#           bond_ret = round(bond_ret*100,2) ,
#           delta_bond_ret = bond_ret -lag(bond_ret)) %>%
#   na.omit()
# 
# cor(bond_cor_ret_wt$delta_bond_ret , bond_cor_ret_wt$delta_wt)
# 
# ggplot_bond_cor_ret_wt = tidyr::gather(bond_cor_ret_wt %>% select(date  , bond_ret , delta_wt) ,
#                                        key="name" , value = "value"   , bond_ret , delta_wt)
# ggplot()+
#   geom_line(data = ggplot_bond_cor_ret_wt , 
#             aes(x = date , y = value , color = name ) , size = 1 )
# library(ggpubr)
# ggarrange( p , p1 , align = "v" , ncol=1)
