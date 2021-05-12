library(rms)
library(tidyverse)
library(pspline)

df <- data.frame(x = 1:16, 
                 y = c(NA,23,NaN,NaN,NaN,22,NA,6,7,23,NA,NaN,1.8,2,2,NA))

df <- df %>% as_tibble() %>% mutate(y_fill = as.numeric(forecast::na.interp(y)))


# df$Loess10 <- as.numeric(predict(loess(y_fill~x, df, span = 1, na.action = "na.exclude"), df$x, se = TRUE)$fit)
df$Loess08 <- predict(loess(y_fill~x, df, span = 0.8), df$x, se = TRUE)$fit
# df$Loess06 <- predict(loess(y~x, df, span = 0.6), df$x, se = TRUE)$fit
# df$rcs5 <- predict(lm(y~rcs(x,5), data=df), data.frame(x=df$x))
# df$rcs4 <- predict(lm(y~rcs(x,4), data=df), data.frame(x=df$x))
# df$rcs3 <- predict(lm(y~rcs(x,3), data=df), data.frame(x=df$x))
# df$smspl10 <- SmoothSpline <- predict(sm.spline(df[!is.na(df$y),]$x,df[!is.na(df$y),]$y, spar = 2), df$x)
# df$smspl08 <- SmoothSpline <- predict(sm.spline(df[!is.na(df$y),]$x,df[!is.na(df$y),]$y, spar = 0.8), df$x)
# df$smspl06 <- predict(pspline::sm.spline(df[!is.na(df$y),]$x,df[!is.na(df$y),]$y, spar = 0.6), df$x)


# my_ <- function(y,x) {
#   df <- data.frame(xx=x,yy=y)
#   predict(pspline::sm.spline(df[!is.na(df$yy),]$xx,df[!is.na(df$yy),]$yy, spar = 0.6), df$xx)[,1]}
# my_(df$y, df$x)

plotly::ggplotly(df %>% pivot_longer(-x, names_to = "Data", values_to = "y") %>% 
  ggplot() + 
  geom_line(aes(x, y, color = Data), size = 0.5) + 
  geom_point(aes(x, y, color = Data), size = 2) + 
  geom_point(data = df, aes(x, y), size = 3) + 
  geom_hline(yintercept = 1.8) + 
  egg::theme_article())

