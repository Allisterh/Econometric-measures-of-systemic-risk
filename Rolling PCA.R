
## Load the bank equity prices
prices <-read_excel("inputdata.xlsx") %>%mutate(date = ymd(date)) %>%tk_xts(date_var = date)
symbols <- c('HSBC.HOLDINGS','BNP.PARIBAS','BANCO.SANTANDER','UBS.GROUP','NATWEST.GROUP',	'ING.GROEP','UNICREDIT','BARCLAYS','CREDIT.SUISSE.GROUP','BBV.ARGENTARIA','SOCIETE.GENERALE','DEUTSCHE.BANK','LLOYDS.BANKING.GROUP','KBC.GROUP','INTESA.SANPAOLO','STANDARD.CHARTERED','NORDEA.BANK','DANSKE.BANK','COMMERZBANK','AIB.GROUP','SVENSKA.HANDELSBANKEN.A','BANK.OF.IRELAND.GROUP','SWEDBANK.A','ERSTE.GROUP.BANK','NATIXIS','ALPHA.BANK','BANCO.COMR.PORTUGUES','EUROBANK.HOLDINGS')
bankreturn<-Return.calculate(prices,method = "log")%>%na.omit() #XTS version

########### PRINCIPLE COMPONENT ANALYSIS
pca<-bankreturn%>%princomp()
screeplot(pca)
### Find proportion of explained variances
eigs <- pca$sdev^2
Cumulative = cumsum(eigs)/sum(eigs)
cume_prop<-Cumulative %>%enframe()

### Create function to compute proportion of total variance explained by certain components
prop_function<-function(returns){pcaroll<-princomp(returns,cor = TRUE)
eigs <- pcaroll$sdev^2
Cumulative = cumsum(eigs)/sum(eigs)
Cumulative %>%
  enframe()
}

prop_function(bankreturn)

### Create function to do the 144 weeks rolling PCA
interval_prop <-
  function(returns_df,
           start = 1,
           window = 144)
    # First create start date.
  {start_date <-
    returns_df$date[start]
  # Next create an end date that depends
  # on start date and window.
  end_date <-
    returns_df$date[c(start + window)]
  # Filter on start and end date.
  returns_to_use <- returns_df %>% filter(date >= start_date & date < end_date) %>% select(-date)
  # Call our original custom function
  # We are nesting one function inside another
  component_percentages <- prop_function(returns_to_use)
  # Add back the end date as date column
  results_with_date <- component_percentages %>% mutate(date = ymd(end_date)) %>% select(date, everything()) %>%
    spread(name, value) %>%
    # Round the results for better presentation
    mutate_if(is.numeric, function(x) x * 100)
  }

### 3 year rolling pca 
window = 144
rolling_pca <- ### STANDARDISED RETURN BEING USED HERE
  # First argument:
  # tell map_df to start at date index 1
  # This is the start argument to interval_prop()
  # and it is what map() will loop over until we tell
  # it to stop at the date that is 144 weeks before the
  # last date.
  map_df(1:(nrow(Stand_return) - window),
         # Second argument:
         # tell it to apply our rolling function
         interval_prop,
         # Third argument:
         # tell it to operate on our returns
         returns_df = Stand_return,             #### STANDARDISED RETURN BEING USED HERE
         # supply the rolling window
         window = window)

try<-rolling_pca [c("date","Comp.1","Comp.5","Comp.15","Comp.28")]
try$PC2toPC5<-try$Comp.5-try$Comp.1
try$PC6toPC15<-try$Comp.15-try$Comp.5
try$PC16toPC28<-try$Comp.28-try$Comp.15
newda <- subset(try, select = c(date,Comp.1,PC2toPC5,PC6toPC15,PC16toPC28))
stackda<-newda %>% rename(Date=date,a.PC1=Comp.1,b.PC2toPC5=PC2toPC5, c.PC6toPC15=PC6toPC15,d.PC16toPC28=PC16toPC28)

## Visualize the rolling contribution of the principal components
plot1<-stackda %>%
  gather(Component, Contribution, -Date) %>%group_by(Component) %>%ggplot(aes(x = Date,y = Contribution)) +
  geom_area(aes(colour = Component,fill= Component),position = position_fill(reverse = TRUE)) +scale_x_date(breaks =pretty_breaks(n = 10)) +
  scale_y_continuous(labels =function(x) paste0(x*100,"%"), breaks =pretty_breaks(n = 10))+
  theme(legend.position = "top",legend.title = element_text(color = "black", size = 8.5),legend.text = element_text(color = "black",size=8),legend.key.size = unit(0.5, "cm"),legend.key.width = unit(0.5,"cm"))+
  theme(axis.title.x=element_blank())
