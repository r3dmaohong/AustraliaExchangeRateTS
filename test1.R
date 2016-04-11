rm(list = ls()) #去除工作空間中所有物件
gc() #記憶體釋放
path<-"D:\\Work Space\\R\\FE_history"
setwd(path)
#start.time<-Sys.time()

# returns string w/o leading or trailing whitespace
trim <- function (x) gsub("^\\s+|\\s+$", "", x)
#returns string w/o leading or trailing punct: gsub("^[[:punct:]]+|[[:punct:]]+$", "", x)

library('rvest')
library('plyr')
library('XML')
library('RCurl')
library('SnowballC')
library('cluster')   
library('data.table') 
library('xlsx')
library('httr')


fe_df = data.frame('日期'=character(),'匯率'=numeric(),stringsAsFactors=F)

date = c()
year = c(2003:2015)
month = c('01','02','03','04','05','06','07','08','09','10','11','12')
day = c('01','02','03','04','05','06','07','08','09',10:30)

date = apply(expand.grid(year, month,day), 1, paste, collapse="")
date = sort(date)
x = 1
for(i in 547:length(date)){
  tryCatch({
    ##http://www.cnyes.com/forex/history.aspx?fccode=AUD/TWD&fcname=%E6%BE%B3%E5%85%83%2f%E5%8F%B0%E5%B9%A3&mydate=20150411&rate=exchange
    url = paste0('http://www.cnyes.com/forex/history.aspx?fccode=AUD/TWD&fcname=%E6%BE%B3%E5%85%83%2f%E5%8F%B0%E5%B9%A3&mydate=',date[i],'&rate=exchange')
    gc() #記憶體釋放      
    uastring <- "Mozilla/5.0 (Windows NT 6.1) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/41.0.2228.0 Safari/537.36"
    session <- html_session(url, user_agent(uastring))
    title_css = read_html(session) %>% html_nodes(".cr") %>% html_text()
    utf8_text_title <- iconv(title_css,'UTF-8')
    utf8_text_title = as.numeric(utf8_text_title)
    utf8_text_title = utf8_text_title[which(!is.na(utf8_text_title ))]
    fe = utf8_text_title[1]
    
    if(!is.na(fe)){
      fe_df[x,1] = date[i]
      fe_df[x,2] = fe
      x = x + 1
      print(paste0(date[i],' : ',fe))
      write.csv(fe_df,'澳幣歷史匯率.csv',row.names=F)
    }
    
    print(paste0(i/length(date)*100,'%'))
    Sys.sleep(runif(1,5,10))
    
  }
  , error=function(e){
    print(paste0(date[i],' : 失敗'))
    print(paste0(i/length(date)*100,'%'))
    Sys.sleep(runif(1,3,5))
  })
}

plot.ts(ts(fe_df[,2]))
plot.ts(diff(ts(fe_df[,2]),differences=1))


fes=fe_df[,2]
fests<- ts(fes,start = c(1866))
plot.ts(fests)
festsdiff<-diff(fests,differences=1)
plot.ts(festsdiff)
festsdiff2<-diff(fests,differences=2)
plot.ts(festsdiff2)
acf(festsdiff2,lag.max=20)
acf(festsdiff2,lag.max=20,plot=FALSE)
pacf(festsdiff2,lag.max=20)
pacf(festsdiff2,lag.max=20,plot=FALSE)
fesarima<-arima(fests,order=c(1,2,5))
fesarima

fesarimaforecast<-forecast.Arima(fesarima,h=5,level=c(99.5))
fesarimaforecast
plot.forecast(fesarimaforecast)##plot.forecast(fesarimaforecast$residuals)
acf(fesarimaforecast$residuals,lag.max=20)
Box.test(fesarimaforecast$residuals, lag=20, type="Ljung-Box")
plot.ts(fesarimaforecast$residuals)
plotForecastErrors(fesarimaforecast$residuals)