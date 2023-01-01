## PREPARE THE STANDARDIZE RETURNS:

### convert the bank return dataframe into matrix
return_matrix<-data.matrix(return_dplyr%>%select(-date))

### Regress the bank returns on the market returns and take residual series
market<-read_excel("Market.xlsx") %>%mutate(date = ymd(Date)) %>%tk_xts(date_var = Date)
lm_model<-lm(return_matrix[,1]~ market$Eur_return)$residuals
residual_returns<- matrix(NA, 939, 28)
for(i in 1:28) {residual_returns[,i]<-lm(return_matrix[,i]~ market$Eur_return)$residuals}

### Apply GARCH (1,1) for residuals, take the fitted variance and standardise the return series.
multg1 <- multispec(replicate(28, ugarchspec(mean.model = list(armaOrder = c(0,0)))))
multf1 <- multifit(multg1, residual_returns)

### Standardize the residuals by the variance estimated by the GARCH(1,1) model.
Stand_return <- matrix(NA, 939, 28)
for(i in 1:28) {Stand_return[,i]<-(scale(residual_returns[,i],scale = FALSE))/multf1@fit[[i]]@fit$sigma} 
colnames(Stand_return) <- c(symbols)
as.tibble(Stand_return)
Stand_return<-cbind.data.frame(return_dplyr$date,Stand_return)
Stand_return<-Stand_return %>% rename(date="return_dplyr$date")

## QUANTIFY THE ROLLING DEGREE OF GRANGER CAUSALITY CONNECTEDNESS:

### Function to do rolling 
interval_VAR <-
  function(returns_df, start = 1, window = 144)
  {start_date <- returns_df$date[start]
  end_date <- returns_df$date[c(start + window)]
  returns_to_use <- returns_df %>% filter(date >= start_date & date < end_date) %>% select(-date)%>%data.matrix()
  DGC <- VAR_function(returns_to_use)
  results_with_date <- DGC %>% mutate(date = ymd(end_date)) %>% select(date, everything()) %>% spread(symbols, V1)   }

library(MTS)
rolling_VAR <-
  map_df(1:(nrow(Stand_return) - window),
         interval_VAR,
         returns_df = Stand_return,
         window = window)

### Make the columns appear as symbols order
rolling_VAR<-rolling_VAR%>%select(date, symbols) 

### Calculate the DGC measure
DGC<-matrix(NA,795,2) 
for(i in 1:795) {DGC[i,1]<-sum(rolling_VAR[i,2:29]) ### NUMBER OF GRANGER CAUSALITY RELATIONSHIPS
DGC[i,2]<-(sum(rolling_VAR[i,2:29]))/756} ### AS PERCENTAGE OF 756 ALL POSSIBLE CONNNECTIONS

### Visualize the DGC measure
rolling_VAR%>%ggplot(aes(x = date))+geom_line(aes(y = DGC[,2]), color = "cornflowerblue") +
  scale_y_continuous(labels = scales::percent,breaks = pretty_breaks(n = 10),position = "left") +
  scale_x_date(breaks = pretty_breaks(n = 8)) +
  labs(title = "", y = "") +
  theme(plot.title = element_text(hjust = 0.5))+theme(axis.title.x=element_blank())