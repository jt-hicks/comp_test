##Plot a rainfall profile
						
# The parameters for the fourier series
# Parameters for Fatick, Senegal
ssa0 <- 0.4
ssa1 <- 0.15#0.317593544
ssa2 <- -0.4#-0.331394491
ssa3 <- -.05#-0.001915169
ssb1 <- 0.6#0.293101909
ssb2 <- .1#0.3#0.116623245
ssb3 <- -.1#-0.061672743
theta_c <- 0.2845953

ssa0p <- 0.3
ssa1p <- 0.07#0.317593544
ssa2p <- 0.05#-0.15
ssa3p <- 0.02#-0.3#.15#-0.001915169
ssb1p <- -0.08#0.05#0.293101909
ssb2p <- 0#-0.05#.15#0.3#0.116623245
ssb3p <- 0.0#-.15#-0.061672743
theta_cp <- 0.2#0.2845953

# Recreation of the rainfall function

df <- data.frame(t=220:(220+380*2))%>%
  mutate(rainfall=pmax((ssa0+ssa1*cos(2*pi*-t/365)+ssa2*cos(2*2*pi*-t/365)+ssa3*cos(3*2*pi*-t/365)+ssb1*sin(2*pi*-t/365)+ssb2*sin(2*2*pi*-t/365)+ ssb3*sin(3*2*pi*-t/365))/theta_c,0.001),
         moz=pmax((ssa0+0.01+ssa1*cos(2*pi*(-t+8)/365)+ssa2*cos(2*2*pi*(-t+8)/365)+ssa3*cos(3*2*pi*(-t+8)/365)+ssb1*sin(2*pi*(-t+8)/365)+ssb2*sin(2*2*pi*(-t+8)/365)+ ssb3*sin(3*2*pi*(-t+8)/365))/theta_c,0.001),
         eir=pmax((ssa0+0.01+(ssa1+.05)*cos(2*pi*(-t+60)/365)+ssa2*cos(2*2*pi*(-t+60)/365)+ssa3*cos(3*2*pi*(-t+60)/365)+ssb1*sin(2*pi*(-t+60)/365)+ssb2*sin(2*2*pi*(-t+60)/365)+ ssb3*sin(3*2*pi*(-t+60)/365))/theta_c,0.001),
         inc=pmax((ssa0+0.01+(ssa1+.05)*cos(2*pi*(-t+75)/365)+ssa2*cos(2*2*pi*(-t+75)/365)+ssa3*cos(3*2*pi*(-t+75)/365)+ssb1*sin(2*pi*(-t+75)/365)+ssb2*sin(2*2*pi*(-t+75)/365)+ ssb3*sin(3*2*pi*(-t+75)/365))/theta_c,0.001),
         prev=pmax((ssa0p+(ssa1p)*cos(2*pi*(-t)/365)+ssa2p*cos(2*2*pi*(-t)/365)+ssa3p*cos(3*2*pi*(-t)/365)+ssb1p*sin(2*pi*(-t)/365)+ssb2p*sin(2*2*pi*(-t)/365)+ ssb3p*sin(3*2*pi*(-t)/365))/theta_cp,0.001),
         month=as.Date('2019-08-01')+t)



matplot(df$t,df$rainfall,type='l')
matlines(df$t,df$moz,col='blue')
matlines(df$t,df$eir,col='red')
matlines(df$t,df$inc,col='green')
matlines(df$t,df$prev,col='grey')

windows(9,4)
ggplot(df)+
  geom_line(aes(x=month,y=prev),color="#999999",size=1)+
  geom_line(aes(x=month,y=eir),color="#1B9E77",size=1)+
  geom_line(aes(x=month,y=inc),color="#D95F02",size=1)+
  labs(x='Month',y='Transmission Metric')+
  scale_x_date(date_labels = "%b",date_breaks = '3 months')+
  scale_y_continuous(limits = c(0,6))+
  theme(axis.text.y=element_blank())
  
blank <- ggplot(df)+
  geom_line(aes(x=month,y=prev),color="#999999",size=1,alpha=0)+
  geom_line(aes(x=month,y=eir),color="#1B9E77",size=1,alpha=0)+
  geom_line(aes(x=month,y=inc),color="#D95F02",size=1,alpha=0)+
  labs(x='Month',y='Transmission Metric')+
  scale_x_date(date_labels = "%b",date_breaks = '3 months')+
  scale_y_continuous(limits = c(0,6))+
  theme(axis.text.y=element_blank())
blank

prev <- ggplot(df)+
  geom_line(aes(x=month,y=prev),color="#999999",size=1)+
  # geom_line(aes(x=month,y=eir),color="#1B9E77",size=1)+
  # geom_line(aes(x=month,y=inc),color="#D95F02",size=1)+
  labs(x='Month',y='Transmission Metric')+
  scale_x_date(date_labels = "%b",date_breaks = '3 months')+
  scale_y_continuous(limits = c(0,6))+
  theme(axis.text.y=element_blank())
prev

inc <- ggplot(df)+
  # geom_line(aes(x=month,y=prev),color="#999999",size=1)+
  geom_line(aes(x=month,y=eir),color="#1B9E77",size=1)+
  geom_line(aes(x=month,y=inc),color="#D95F02",size=1)+
  labs(x='Month',y='Transmission Metric')+
  scale_x_date(date_labels = "%b",date_breaks = '3 months')+
  scale_y_continuous(limits = c(0,6))+
  theme(axis.text.y=element_blank())
inc
