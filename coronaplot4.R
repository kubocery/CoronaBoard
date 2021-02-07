#require(plotly)
countries <- c('China', 'Japan','Korea, South','US','France','Canada','Australia','Germany','Italy','United Kingdom','Russia','Sweden','Spain',
               'Belgium','Slovenia','Iceland', 'Iran','Croatia',"Switzerland",'Austria','Israel','Greece', 'Norway','Finland', 'Portugal','Netherlands','Poland','Slovakia',
               'Czechia','Hungary','Slovenia','Serbia','Bosnia and Herzegovina','Romania','Ukraine','Estonia','Latvia',
               'Lithuania','Denmark','Ireland','Bulgaria', 'Belarus')

get_n_formate <- function(link, countries){
  df <- read.csv(url(link), sep=',', stringsAsFactors = F)
  df <- df[df$Country.Region %in% countries,!c(names(df)%in% c('Lat','Long', 'Province.State'))]
  df2 <- tidyr::gather(df,key=time,value=df,-Country.Region)
  df2 <- aggregate(df2$df, by=list(df2$Country.Region,df2$time), FUN=sum) 
  colnames(df2)<- c('Country','Date','df')
  df2$Date <- zoo::as.Date(as.character(df2$Date), 'X%m.%d.%y')
  return(df2)
}

link <- 'https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv'
cases <- get_n_formate(link, countries)

link <- 'https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv'
deaths <- get_n_formate(link, countries)

link <- 'https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv'
recover <- get_n_formate(link, countries)


dftmp <- merge(cases, deaths, by.x=c('Country','Date'), by.y=c('Country','Date'))
df <- merge(dftmp, recover, by.x=c('Country','Date'), by.y=c('Country','Date'), all.x = T)
names(df) <- c('Country','Date','Cases','Deaths','Recovered')


df <- df %>% group_by(Country) %>% 
  mutate(change = ifelse(Cases == dplyr::lag(Cases) & Date == as.Date('2020-03-12'),
                                                          'TRUE', 'FALSE')) %>% ungroup()

df$Cases[df$change==TRUE]<- NA
df$Deaths[df$change==TRUE & df$Country %in% c('Italy', 'France','Germany','Switzerland','Spain')]<- NA
df$Recovered[df$change==TRUE & df$Country %in% c('Italy', 'France','Germany','Switzerland','Spain')]<- NA


require(zoo)
df <- transform(df, newCol = ave(Cases, Country, FUN = function(x) na.approx(x, rule = 2)))
df <- transform(df, newCol2 = ave(Deaths, Country, FUN = function(x) na.approx(x, rule = 2)))
df <- transform(df, newCol3 = ave(Recovered, Country, FUN = function(x) na.approx(x, rule = 2)))

df$Cases <- round(df$newCol)
df$deaths <- round(df$newCol2)
df$recover <- round(df$newCol3)


df[,c('newCol','newCol2','newCol3', 'change')] <- NULL


df$Active <- df$Cases-df$deaths-df$recover

rm(cases2, deaths2, recover2, dftmp)
#pop2 <- data.frame(Country = unique(df$Country))
#pop2 <- pop2 %>% left_join(select(pop, Country.Name,Value), by=c('Country'='Country.Name'))
#pop2[is.na(pop2$Value),2]<- c(58500000, 1378665000-58500000, 5428704,65637239, 323127513)

pop4 <- data.frame(Country =c("Australia", "Austria", "Belgium", "Canada", "Croatia", "Czechia", 
                              "Denmark", "Estonia", "Finland", "France", "Germany", "Greece", "Hubei", 
                              "Hungary", "Israel", "Italy", "Japan", "Latvia", "Lithuania", "China", 
                              "Netherlands", "Norway", "Poland", "Portugal", "Romania", "Serbia", "Slovakia",
                              "Slovenia", "Spain", "Sweden", "Switzerland", "United Kingdom", "Ukraine", "US", 'Korea, South',
                              'Iran', 'Russia', 'Ireland','Iceland','Bosnia and Herzegovina', 'Bulgaria', 'Belarus'),
                   Population = c(24127159, 8747358, 11348159, 36286425, 4170600, 10561633, 5731118, 
                                  1316481, 5495096, 66896109, 82667685, 10746740, 58500000, 9817958, 
                                  8547100, 60600590, 126994511, 1960424, 2872298, 1320165000, 17018408, 
                                  5232929, 37948016, 10324611, 19705301, 7057412, 5428704, 2064845, 
                                  46443959, 9903122, 8372098, 65637239, 45004645, 323127513,51255914, 83666665,145915734, 
                                  4830000,364260, 3507000, 7050000, 9508000
                                  ))


df2 <- merge(df,pop4,by='Country', all.x = T)

df2$Active_on_pop <- round(df2$Active/df2$Population *1000000,1)
df3 <- df2[df2$Active_on_pop>=1,]
df3 <- dplyr::mutate(dplyr::group_by(df3,Country),1:dplyr::n())
names(df3)[ncol(df3)]<-'Days'
df3[df3$Country=='Korea, South','Country'] <- 'Korea' 


dfs <- df2
dfs <- dplyr::mutate(dplyr::group_by(dfs,Country),1:dplyr::n())
names(dfs)[ncol(dfs)]<-'Days'
dfs[dfs$Country=='Korea, South','Country'] <- 'Korea' 


#writexl::write_xlsx(dfs, 'df3.xlsx')
#saveRDS(df2, 'map.rds')

coronaplot <- function(places, logscale = FALSE, smooth=FALSE){
  plot_countries <- places
  #plot_countries <- c('Slovenia','Italy','Korea','France','Germany','None','France')
  if(sum(plot_countries=='None')==length(plot_countries)){
    plot_countries <- c(rep('None',10))
  }else{
    plot_countries <- plot_countries[plot_countries!='None']
  }
  
  if(max(table(plot_countries))>1){
    plot_countries <- unique(plot_countries)
  }
  plot_countries <- plot_countries[c(plot_countries%in%df3$Country)&plot_countries!='None']
  df<- df3[df3$Country%in%plot_countries,]
  if(nrow(df)==0){
    df<-df
  }else{
    df <- df[order(unlist(sapply(df$Country, function(x) which(plot_countries == x)))),]
  }
  cols <- c('#a6cee3','#1f78b4','#b2df8a','#33a02c','#fb9a99','#e31a1c','#fdbf6f','#ff7f00','#cab2d6','#6a3d9a')
  cols2 <- cols[1:length(unique(df$Country))]
  #tmpcol <- data.frame(Country=plot_countries, cols3=cols2)
  #df <- merge(df,tmpcol,by = 'Country', all.x = T)
  if(nrow(df)!=0){
    df$Country <- factor(df$Country, levels = plot_countries[plot_countries!='None'])
    pal <- setNames(cols2,plot_countries[plot_countries!='None'])
  }else{
    pal <- setNames(cols2,plot_countries)
  }
  hovertxt <- paste0("<b>Location: </b>", df$Country, "<br>",
                     "<b>Days since outbreak: </b>", df$Days, "<br>",
                     "<b>Active cases per 1M people: </b>", df$Active_on_pop, "<br>")
  
  if(logscale==FALSE){
    if(smooth==FALSE){
      plot_ly(df, x = ~Days,y = ~Active_on_pop, color = ~Country,colors = pal) %>%
        add_lines(  text = hovertxt, hoverinfo = "text") %>% 
        layout(title = "Active Cases per 1 milion people", font = list(size = 12),
               xaxis = list(title = 'Days since outbreak', showgrid = T, showline = F,
                            ticklen = 3, dtick = 2,zeroline = FALSE),
               yaxis = list(title = "", showline = F, ticks = "outside", showgrid = TRUE, 
                            rangemode = "tozero",ticklen = 0,zeroline = FALSE,categoryarray = ~Country, categoryorder = "array"),
               annotations = list( 
                 list(x = 1, y = -0.4,
                      showarrow = F, xref='paper', yref='paper',
                      xanchor='right', yanchor='auto', xshift=0, yshift=0,
                      font=list(size=10), align = "left")),
               margin = list(l=10,r=10,b=50,t=70))  
    }else{
      plcase <- ggplot(df, aes(x=Days, y=Active_on_pop,color=Country))+
        #geom_line()+
        geom_smooth(span =0.7,method = 'loess', se=F, size=0.6)+
        scale_color_manual(values=pal)+
        #scale_y_continuous(trans='log')+
        theme_minimal()+
        scale_x_continuous(breaks = seq(2,round(max(df$Days)/2,0)*2,by=2))+
        labs(title="New cases in the past 2 weeks",
             x ="Days since outbreak", y = "New Cases")+
        theme(plot.caption=element_text( face="italic", color="black"),
              plot.title = element_text(hjust = 0.5),
              plot.subtitle = element_text(size = 10,face="italic", hjust = 0.5),
              #axis.title.x=element_blank(),
              legend.title = element_blank(),
              axis.title.y=element_blank(),
              #axis.ticks.y = element_blank(),
              #axis.text.y=element_blank(),
              axis.text.x = element_text( hjust = 1),
              legend.key.size = unit(0.2, "cm"),
              plot.margin = margin(l=10,r=10,b=10,t=25))
      
      plotly::ggplotly(plcase,tooltip = 'Country')
    }
    
    
  }else{
    if(smooth==FALSE){
    plot_ly(df, x = ~Days,y = ~Active_on_pop, color = ~Country,colors = pal) %>%
      add_lines(  text = hovertxt, hoverinfo = "text") %>% 
      layout(title = "Active Cases per 1 milion people", font = list(size = 12),
             xaxis = list(title = 'Days since outbreak', showgrid = T, showline = F,
                          ticklen = 0, dtick = 2,zeroline = FALSE),
             yaxis = list(type='log', title = "Log scale", showline = F, showgrid = T, 
                          rangemode = "tozero",zeroline = FALSE,showticklabels = T,
                          categoryarray = ~Country, categoryorder = "array"),
             annotations = list( 
               list(x = 1, y = -0.4, text = "Source: Erste Group Research",
                    showarrow = F, xref='paper', yref='paper',
                    xanchor='right', yanchor='auto', xshift=0, yshift=0,
                    font=list(size=10), align = "left")),
             margin = list(l=10,r=10,b=50,t=70))
    }else{
    plcase <- ggplot(df, aes(x=Days, y=Active_on_pop,color=Country))+
      #geom_line()+
      geom_smooth(span =0.7,method = 'loess', se=F, size=0.6)+
      scale_color_manual(values=pal)+
      scale_y_continuous(trans='log')+
      theme_minimal()+
      scale_x_continuous(breaks = seq(2,round(max(df$Days)/2,0)*2,by=2))+
      labs(title="Active Cases per 1 milion people",
           x ="Days since outbreak", y = "Log Scale")+
      theme(plot.caption=element_text( face="italic", color="black"),
            plot.title = element_text(hjust = 0.5),
            plot.subtitle = element_text(size = 10,face="italic", hjust = 0.5),
            #axis.title.x=element_blank(),
            legend.title = element_blank(),
            axis.ticks.y = element_blank(),
            axis.text.y=element_blank(),
            axis.text.x = element_text( hjust = 1),
            legend.key.size = unit(0.2, "cm"),
            plot.margin = margin(l=10,r=10,b=10,t=25))
    
    plotly::ggplotly(plcase, tooltip = 'Country')
    }
  }
 
}


coronamap <- function(){
  df <- df2[df2$Date==max(df2$Date), c('Country', 'Active', 'Active_on_pop')]
  dfl <- df2[df2$Date==(max(df2$Date)-1), c('Country', 'Active', 'Active_on_pop')]
  df <- merge(df,dfl[,c('Country','Active')], by='Country')
  df$New_cases <- df$Active.x-df$Active.y
  url <- 'https://www.worldometers.info/coronavirus/'
  require(rvest)
  population <- url %>%
    read_html() %>%
    html_nodes(xpath='//*[@id="main_table_countries_today"]') %>%
    html_table()
  
  dfpop <- population[[1]]
  
  
  isoc <- read.csv('https://raw.githubusercontent.com/lukes/ISO-3166-Countries-with-Regional-Codes/master/all/all.csv', stringsAsFactors = F)
  
  df <- merge(df,isoc[,c('name', 'alpha.3')], by.x = 'Country', by.y = 'name', all.x = T)
  dfpop2 <- merge(dfpop,isoc[,c('name', 'alpha.3')], by.x = 'Country,Other', by.y = 'name', all.x = T)

  
  df[df$Country=='United Kingdom', 'alpha.3']<- 'GBR'
  df[df$Country=='Russia', 'alpha.3']<- 'RUS'
  
  dfpop2[dfpop2$Country=='UK', 'alpha.3']<- 'GBR'
  dfpop2[dfpop2$Country=='Russia', 'alpha.3']<- 'RUS'
  df <- df[complete.cases(df),]
  
  dfpop3 <- merge(dfpop2, df, by.x='alpha.3', by.y = 'alpha.3', all.y = T)
  dfpop3$NewCases <- as.numeric(stringr::str_replace_all(substring(dfpop3$NewCases, 2), ',',''))
  dfpop3$NewCases <- ifelse(is.na(dfpop3$NewCases),dfpop3$New_cases, dfpop3$NewCases)
  dfpop3$pop <- dfpop3$Active.x/dfpop3$Active_on_pop *1000000
  dfpop3$Active_on_pop2 <- round(as.numeric(stringr::str_replace_all(dfpop3$ActiveCases, ',',''))/dfpop3$pop *1000000,1)
  
  df <- data.frame(Country=dfpop3$Country, ISO = dfpop3$alpha.3, 
                   Active= as.numeric(stringr::str_replace_all(dfpop3$ActiveCases, ',','')),
                   Active_on_pop = dfpop3$Active_on_pop2,  New_cases=dfpop3$NewCases)
  
  #df$buckets <- cut(df$Active_on_pop, quantile(df$Active_on_pop, probs = seq(0,1,length.out=8)))
  df$hover <- paste0('Country: ',df$Country,'<br>','Active cases: ',df$Active,  '<br>',
                     'Active cases per milion: ', df$Active_on_pop, '<br>','New active cases: ', df$New_cases)
  
  
  #LAND_ISO <- c("AUT","BEL","BGR","HRV","CYP","CZE","DNK","EST","FIN","FRA","DEU","GRC","HUN","IRL","ITA","LVA","LTU","LUX","MLT","NLD","POL","PRT","ROU","SVK","SVN","ESP","SWE","GBR")
  #value <- runif(length(LAND_ISO), 1, 10)
  
  #data <- data.frame(LAND_ISO, value)
  
  # Run your code:
  g <- list(
    scope = 'europe',
    projection = list(type = "natural earth"))
  
  #m <- list(
  #  l = 1,
  #  r = 1,
  #  b = 1,
  #  t = 40
  #)
  
  plot_geo(df) %>%
    add_trace(
      z = ~log(Active_on_pop+0.5), locations = ~ISO,text = ~hover,hoverinfo='text',
      color = ~log(Active_on_pop+0.5), colors = 'Blues', showscale = F
    ) %>%
    colorbar(title = "") %>%
    layout(geo = g,
           title = "Active Cases per 1 milion people", font = list(size = 12),
           annotations = list( 
             list(x = 1, y = -0.4, text = "Source: Erste Group Research",
                  showarrow = F, xref='paper', yref='paper',
                  xanchor='right', yanchor='auto', xshift=0, yshift=0,
                  font=list(size=10), align = "left"))
    )
}

new_cases_smooth <- function(places, logscale=FALSE){
  df <- df2 %>% group_by(Country)%>% mutate(first_diff = Cases - dplyr::lag(Cases))
  df[df$Country=='Korea, South','Country'] <- 'Korea' 
  
  dseq <- seq.Date(lubridate::today()-15,lubridate::today()-1, by='day')
  df <- df %>% filter(Date %in% dseq)
  
  plot_countries <- places
  #plot_countries <- c('Poland','Italy','Korea','France','Germany','None','France')
  if(sum(plot_countries=='None')==length(plot_countries)){
    plot_countries <- c(rep('None',10))
  }else{
    plot_countries <- plot_countries[plot_countries!='None']
  }
  
  if(max(table(plot_countries))>1){
    plot_countries <- unique(plot_countries)
  }
  plot_countries <- plot_countries[c(plot_countries%in%df3$Country)&plot_countries!='None']
  df<- df[df$Country%in%plot_countries,]
  if(nrow(df)==0){
    df<-df
  }else{
    df <- df[order(unlist(sapply(df$Country, function(x) which(plot_countries == x)))),]
  }
  cols <- c('#a6cee3','#1f78b4','#b2df8a','#33a02c','#fb9a99','#e31a1c','#fdbf6f','#ff7f00','#cab2d6','#6a3d9a')
  cols2 <- cols[1:length(unique(df$Country))]
  #tmpcol <- data.frame(Country=plot_countries, cols3=cols2)
  #df <- merge(df,tmpcol,by = 'Country', all.x = T)
  if(nrow(df)!=0){
    df$Country <- factor(df$Country, levels = plot_countries[plot_countries!='None'])
    pal <- setNames(cols2,plot_countries[plot_countries!='None'])
  }else{
    pal <- setNames(cols2,plot_countries)
  }
  hovertxt <- paste0("<b>Location: </b>", df$Country, "<br>",
                     "<b>Days since outbreak: </b>", df$Date, "<br>",
                     "<b>Active cases per 1M people: </b>", df$Active_on_pop, "<br>",
                     "<b>Total active cases: </b>", df$Active, "<br>")
  df <- df %>% group_by(Country) %>% mutate(n=1:dplyr::n())
  #df$txt <- hovertxt
  df$date2 <- as.character(format(df$Date, '%d-%m'))
  
  models <- df %>% group_by(Country) %>% do(model = loess(first_diff~n,data=., span = 0.7))
  
  
  if(logscale==TRUE){
    plcase <- ggplot(df, aes(x=Date, y=first_diff,color=Country))+
      #geom_line()+
      geom_smooth(span =0.7,method = 'loess', se=F, size=0.5)+
      scale_color_manual(values=pal)+
      scale_y_continuous(trans='log')+
      theme_minimal()+
      scale_x_date(date_breaks = "1 day",  date_labels = "%d-%m")+
      labs(title="New cases in the past 2 weeks",
           x ="Date", y = "New Cases")+
      theme(plot.caption=element_text( face="italic", color="black"),
            plot.title = element_text(hjust = 0.5),
            plot.subtitle = element_text(size = 10,face="italic", hjust = 0.5),
            axis.title.x=element_blank(),
            legend.title = element_blank(),
            axis.title.y=element_blank(),
            axis.ticks.y = element_blank(),
            axis.text.y=element_blank(),
            axis.text.x = element_text(angle = 45, hjust = 1),
            plot.margin = margin(l=10,r=10,b=10,t=25))   
  }else{
    plcase <- ggplot(df, aes(x=Date, y=first_diff,color=Country))+
      #geom_line()+
      geom_smooth(span =0.7,method = 'loess', se=F, size=0.5)+
      scale_color_manual(values=pal)+
      #scale_y_continuous(trans='log')+
      theme_minimal()+
      scale_x_date(date_breaks = "1 day",  date_labels = "%d-%m")+
      labs(title="New cases in the past 2 weeks",
           x ="Date", y = "New Cases")+
      theme(plot.caption=element_text( face="italic", color="black"),
            plot.title = element_text(hjust = 0.5),
            plot.subtitle = element_text(size = 10,face="italic", hjust = 0.5),
            axis.title.x=element_blank(),
            legend.title = element_blank(),
            axis.title.y=element_blank(),
            axis.text.x = element_text(angle = 45, hjust = 1),
            plot.margin = margin(l=10,r=10,b=10,t=25))
  }
  
 
  
  #plot(plcase)
  plotly::ggplotly(plcase,tooltip='Country')
}

new_cases_raw <- function(places, logscale=FALSE){
  df <- df2 %>% group_by(Country)%>% mutate(first_diff = Cases - dplyr::lag(Cases))
  df[df$Country=='Korea, South','Country'] <- 'Korea' 
  
  dseq <- seq.Date(lubridate::today()-15,lubridate::today()-1, by='day')
  df <- df %>% filter(Date %in% dseq)
  
  plot_countries <- places
  #plot_countries <- c('Poland','Italy','Korea','France','Germany','None','France')
  #plot_countries <- rep('None', 10)
  
  if(sum(plot_countries=='None')==length(plot_countries)){
    plot_countries <- c(rep('None',10))
  }else{
    plot_countries <- plot_countries[plot_countries!='None']
  }
  
  if(max(table(plot_countries))>1){
    plot_countries <- unique(plot_countries)
  }
  plot_countries <- plot_countries[c(plot_countries%in%df3$Country)&plot_countries!='None']
  df<- df[df$Country%in%plot_countries,]
  if(nrow(df)==0){
    df<-df
  }else{
    df <- df[order(unlist(sapply(df$Country, function(x) which(plot_countries == x)))),]
  }
  cols <- c('#a6cee3','#1f78b4','#b2df8a','#33a02c','#fb9a99','#e31a1c','#fdbf6f','#ff7f00','#cab2d6','#6a3d9a')
  cols2 <- cols[1:length(unique(df$Country))]
  #tmpcol <- data.frame(Country=plot_countries, cols3=cols2)
  #df <- merge(df,tmpcol,by = 'Country', all.x = T)
  if(nrow(df)!=0){
    df$Country <- factor(df$Country, levels = plot_countries[plot_countries!='None'])
    pal <- setNames(cols2,plot_countries[plot_countries!='None'])
  }else{
    pal <- setNames(cols2,plot_countries)
  }
  hovertxt <- paste0("<b>Location: </b>", df$Country, "<br>",
                     "<b>Days since outbreak: </b>", df$Date, "<br>",
                     "<b>Active cases per 1M people: </b>", df$Active_on_pop, "<br>",
                     "<b>Total active cases: </b>", df$Active, "<br>")
  df <- df %>% group_by(Country) %>% mutate(n=1:dplyr::n())
  #df$txt <- hovertxt
  df$date2 <- as.character(format(df$Date, '%d-%m'))
  names(df)[9]<-'New cases'
  
  #models <- df %>% group_by(Country) %>% do(model = loess(first_diff~n,data=., span = 0.7))
  if(logscale==TRUE){
    plcase <- ggplot(df, aes(x=Date, y=`New cases`,color=Country))+
      geom_line(size=0.5)+
      #geom_smooth(span =0.7,method = 'loess', se=F, size=0.5)+
      scale_color_manual(values=pal)+
      scale_y_continuous(trans='log')+
      theme_minimal()+
      scale_x_date(date_breaks = "1 day",  date_labels = "%d-%m")+
      labs(title="New cases in the past 2 weeks",subtitle = 'China peaked at around 4000 cases per day at the beginning of February',
           x ="Date", y = "New Cases", caption = '*Smoothed data')+
      theme(plot.caption=element_text( face="italic", color="black"),
            plot.title = element_text(hjust = 0.5),
            plot.subtitle = element_text(size = 10,face="italic", hjust = 0.5),
            axis.title.x=element_blank(),
            axis.ticks.y = element_blank(),
            axis.text.y=element_blank(),
            legend.title = element_blank(),
            axis.title.y=element_blank(),
            axis.text.x = element_text(angle = 45, hjust = 1),
            plot.margin = margin(l=10,r=10,b=10,t=25))  
  }else{
    plcase <- ggplot(df, aes(x=Date, y=`New cases`,color=Country))+
      geom_line(size=0.5)+
      #geom_smooth(span =0.7,method = 'loess', se=F, size=0.5)+
      scale_color_manual(values=pal)+
      #scale_y_continuous(trans='log')+
      theme_minimal()+
      scale_x_date(date_breaks = "1 day",  date_labels = "%d-%m")+
      labs(title="New cases in the past 2 weeks",subtitle = 'China peaked at around 4000 cases per day at the beginning of February',
           x ="Date", y = "New Cases", caption = '*Smoothed data')+
      theme(plot.caption=element_text( face="italic", color="black"),
            plot.title = element_text(hjust = 0.5),
            plot.subtitle = element_text(size = 10,face="italic", hjust = 0.5),
            axis.title.x=element_blank(),
            legend.title = element_blank(),
            axis.title.y=element_blank(),
            axis.text.x = element_text(angle = 45, hjust = 1),
            plot.margin = margin(l=10,r=10,b=10,t=25))
  }
  
  
  #plot(plcase)
  plotly::ggplotly(plcase,tooltip=c('Country', 'New cases'))
}

