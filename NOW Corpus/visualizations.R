library(ggplot2)
library(tidyverse)

df.country.publisher <- read.csv('../data/country_publisher_aggregated_data.csv')
df.country.publisher <- df.country.publisher[!(df.country.publisher$country %in% c("  ", "? ","??","y ")), ]
df.country.publisher$date <- as.Date(paste(paste0("20", df.country.publisher$month), "-01", sep=""), format = "%Y-%m-%d")
df.country.publisher$country <- as.factor(df.country.publisher$country)
df.country.publisher$type <- as.factor(df.country.publisher$type)

df <- df.country.publisher %>%
  group_by(date) %>%
  summarise(NON.BINARY.THEY = sum(NON.BINARY.THEY), 
            GENERIC.THEY = sum(GENERIC.THEY), 
            GENERIC.HE = sum(GENERIC.HE), 
            TEXTS.SUM = sum(TEXTS.SUM))

df.country <- df.country.publisher %>%
  group_by(date, country) %>%
  summarise(NON.BINARY.THEY = sum(NON.BINARY.THEY), 
            GENERIC.THEY = sum(GENERIC.THEY), 
            GENERIC.HE = sum(GENERIC.HE), 
            TEXTS.SUM = sum(TEXTS.SUM))

# Calculate % for country publisher
df.country.publisher$GENERIC.SUM <- df.country.publisher$GENERIC.HE + df.country.publisher$GENERIC.THEY
df.country.publisher$GENERIC.SUM.pro.ALL <- df.country.publisher$GENERIC.SUM / df.country.publisher$TEXTS.SUM
df.country.publisher$GENERIC.HE.pro.GEN <- df.country.publisher$GENERIC.HE / df.country.publisher$GENERIC.SUM
df.country.publisher$GENERIC.HE.pro.ALL <- df.country.publisher$GENERIC.HE / df.country.publisher$TEXTS.SUM
df.country.publisher$GENERIC.THEY.pro.GEN <- df.country.publisher$GENERIC.THEY / df.country.publisher$GENERIC.SUM
df.country.publisher$GENERIC.THEY.pro.ALL <- df.country.publisher$GENERIC.THEY / df.country.publisher$TEXTS.SUM

# Calculate % for country df
df.country$GENERIC.SUM <- df.country$GENERIC.HE + df.country$GENERIC.THEY
df.country$GENERIC.SUM.pro.ALL <- df.country$GENERIC.SUM / df.country$TEXTS.SUM
df.country$GENERIC.HE.pro.GEN <- df.country$GENERIC.HE / df.country$GENERIC.SUM
df.country$GENERIC.HE.pro.ALL <- df.country$GENERIC.HE / df.country$TEXTS.SUM
df.country$GENERIC.THEY.pro.GEN <- df.country$GENERIC.THEY / df.country$GENERIC.SUM
df.country$GENERIC.THEY.pro.ALL <- df.country$GENERIC.THEY / df.country$TEXTS.SUM

# Calculate % for df
df$GENERIC.SUM <- df$GENERIC.HE + df$GENERIC.THEY
df$GENERIC.SUM.pro.ALL <- df$GENERIC.SUM / df$TEXTS.SUM
df$GENERIC.HE.pro.GEN <- df$GENERIC.HE / df$GENERIC.SUM
df$GENERIC.HE.pro.ALL <- df$GENERIC.HE / df$TEXTS.SUM
df$GENERIC.THEY.pro.GEN <- df$GENERIC.THEY / df$GENERIC.SUM
df$GENERIC.THEY.pro.ALL <- df$GENERIC.THEY / df$TEXTS.SUM


# COUNTRY MODEL
model.lm <- lm(GENERIC.HE.pro.ALL ~ date, data = df)
summary(model.lm)
df$pred.he <- predict(model.lm)

ggplot(df, aes(x = date, y = GENERIC.HE.pro.ALL)) +
  geom_point(aes(x = date, y = GENERIC.HE.pro.ALL, color="datapoints")) +
  geom_line(aes(y = pred.he, color="linear regression"), size = 1.2, linetype = "dashed") +
  ggtitle("Usage of Generic He") +
  scale_y_continuous(labels = function(x) paste0(x*100, "%")) +
  labs(color = "", x="per month", y="in articles") + 
  theme_minimal() 

model.they.lm <- lm(GENERIC.THEY.pro.ALL ~ date, data = df)
summary(model.they.lm)
df$pred.they <- predict(model.they.lm)

ggplot(df, aes(x = date)) +
  geom_point(aes(x = date, y = GENERIC.THEY.pro.ALL, color="datapoints")) +
  geom_line(aes(y = pred.they, color="linear regression"), size = 1.2, linetype = "dashed") +
  ggtitle("Usage of generic They") +
  scale_y_continuous(labels = function(x) paste0(x*100, "%")) +
  labs(color = "", x="per month", y="in articles") + 
  theme_minimal() 

ggplot(df, aes(x = date)) +
  geom_point(aes(y = GENERIC.HE.pro.ALL, color="Generic He", shape="Generic He"), size = 1) +
  geom_line(aes(y = pred.he, color="Generic He"), size = 1, linetype = "dashed") +
  geom_point(aes(y = GENERIC.THEY.pro.ALL, color="Generic They", shape="Generic They"), size = 1) +
  geom_line(aes(y = pred.they, color="Generic They"), size = 1, linetype = "dashed") +
  ggtitle("Usage of Generic He vs. Generic They") +
  scale_y_continuous(labels = function(x) paste0(x*100, "%")) +
  scale_color_manual(values = c("Generic He" = "cadetblue", "Generic They" = "coral1")) + 
  scale_shape_manual(values = c("Generic He" = 4, "Generic They" = 2)) +
  labs(color = "Linear Regressions", shape="Datapoints", x="per month", y="in articles") + 
  theme_minimal()

# Tesa version singular they instead of generic they
# ggplot(df, aes(x = date)) +
#   geom_point(aes(y = GENERIC.HE.pro.ALL, color="Generic He", shape="Generic He"), size = 1) +
#   geom_line(aes(y = pred.he, color="Generic He"), size = 1, linetype = "dashed") +
#   geom_point(aes(y = GENERIC.THEY.pro.ALL, color="Singular They", shape="Singular They"), size = 1) +
#   geom_line(aes(y = pred.they, color="Singular They"), size = 1, linetype = "dashed") +
#   ggtitle("Usage of Generic He vs. Singular They") +
#   scale_y_continuous(labels = function(x) paste0(x*100, "%")) +
#   scale_color_manual(values = c("Generic He" = "cadetblue", "Singular They" = "coral1")) + 
#   scale_shape_manual(values = c("Generic He" = 4, "Singular They" = 2)) +
#   labs(color = "Linear Regressions", shape="Datapoints", x="per month", y="in articles") + 
#   theme_minimal()

# Generic Constructions

model.generic.lm <- lm(GENERIC.SUM.pro.ALL ~ date, data = df)
summary(model.generic.lm)
df$pred.generic <- predict(model.generic.lm)

ggplot(df, aes(x = date)) +
  geom_point(aes(x = date, y = GENERIC.SUM.pro.ALL, color="datapoints")) +
  geom_line(aes(y = pred.generic, color="linear regression"), size = 1.2, linetype = "dashed") +
  ggtitle("Usage of Generic Constructions") +
  scale_y_continuous(labels = function(x) paste0(x*100, "%")) +
  labs(color = "", x="per month", y="in articles") + 
  theme_minimal() 
  







# COUNTRY MODEL
df.country.reduced <- df.country[(df.country$country %in% c("US", "GB","CA")), ]#, "NG", "IN", "AU", "IE")), ]
model.country.lm <- lm(GENERIC.HE.pro.ALL ~ date * country, 
                       data = df.country.reduced)
summary(model.country.lm)
df.country.reduced$pred.he <- predict(model.country.lm)

ggplot(df.country.reduced, aes(x = date)) +
  geom_point(aes(x = date, y = GENERIC.HE.pro.GEN, color=country, shape=country, alpha=0.05)) +
  geom_line(aes(y = pred.he, color=country), linetype = "dashed", size=1.2) +
  ggtitle("Usage of Generic He in US, GB, CA") +
  scale_y_continuous(labels = function(x) paste0(x*100, "%")) +
  scale_color_manual(values = c("US" = "cadetblue", "GB" = "coral1", "CA" = "darkgoldenrod")) + 
  # scale_shape_manual(values = c("US" = 4, "Generic They" = 2)) +
  labs(color = "", x="per month", y="in articles") + 
  theme_minimal() 


ggplot(df.country.reduced, aes(x = date)) +
  geom_point(aes(x = date, y = GENERIC.HE.pro.ALL, color="datapoints", alpha=0.05)) +
  geom_line(aes(y = pred.he, color="linear regression"), size=1.2) +
  ggtitle("Usage of Generic He in US, GB, CA") +
  scale_y_continuous(labels = function(x) paste0(x*100, "%")) +
  labs(color = "", x="per month", y="in articles") + 
  facet_wrap(~country, ncol = 1) +
  guides(alpha = "none") +
  theme_minimal() 

# Generic They

df.country.reduced <- df.country[(df.country$country %in% c("US", "GB","CA")), ]#, "NG", "IN", "AU", "IE")), ]
model.country.lm <- lm(GENERIC.THEY.pro.ALL ~ date * country, 
                       data = df.country.reduced)

summary(model.country.lm)
summary(model.country.lm)
df.country.reduced$pred.they <- predict(model.country.lm)

ggplot(df.country.reduced, aes(x = date)) +
  geom_point(aes(x = date, y = GENERIC.THEY.pro.ALL, color="datapoints", alpha=0.05)) +
  geom_line(aes(y = pred.they, color="linear regression"), size=1.2) +
  ggtitle("Usage of Singular They in US, GB, CA") +
  scale_y_continuous(labels = function(x) paste0(x*100, "%")) +
  labs(color = "", x="per month", y="in articles") + 
  facet_wrap(~country, ncol = 1) +
  guides(alpha = "none") +
  theme_minimal() 





# COUNTRY PUBLISHER MODEL
df.country.publisher.reduced <- df.country.publisher[(df.country.publisher$country %in% c("US", "GB","CA")), ]
df.country.publisher.reduced <- df.country.publisher.reduced[(df.country.publisher.reduced$GENERIC.HE.pro.ALL < 0.7), ]

model.country.publisher.lm <- lm(GENERIC.HE.pro.ALL ~ date * country * type, 
                       data = df.country.publisher.reduced)
summary(model.country.publisher.lm)
df.country.publisher.reduced$pred.he <- predict(model.country.publisher.lm)

ggplot(df.country.publisher.reduced, aes(x = date)) +
  geom_point(aes(x = date, y = GENERIC.HE.pro.ALL, color=type, shape=type, alpha=0.05)) +
  geom_line(aes(y = pred.he, color=type), size=1.2) +
  ggtitle("Usage of Generic He in US, GB, CA") +
  scale_y_continuous(labels = function(x) paste0(x*100, "%")) +
  scale_color_manual(values = c("publisher" = "cadetblue", "top publisher" = "coral1")) + 
  # scale_shape_manual(values = c("US" = 4, "Generic They" = 2)) +
  labs(color = "", x="per month", y="in articles") + 
  facet_wrap(~country, ncol = 1, scale="free") +
  guides(alpha = "none") +
  theme_minimal() 

# Generic They
df.country.publisher.reduced <- df.country.publisher[(df.country.publisher$country %in% c("US", "GB","CA")), ]
df.country.publisher.reduced <- df.country.publisher.reduced[(df.country.publisher.reduced$GENERIC.THEY.pro.ALL < 0.7), ]

model.country.publisher.lm <- lm(GENERIC.THEY.pro.ALL ~ date * country * type, 
                                 data = df.country.publisher.reduced)
summary(model.country.publisher.lm)
df.country.publisher.reduced$pred.they <- predict(model.country.publisher.lm)

ggplot(df.country.publisher.reduced, aes(x = date)) +
  geom_point(aes(x = date, y = GENERIC.THEY.pro.ALL, color=type, shape=type, alpha=0.05)) +
  geom_line(aes(y = pred.they, color=type), size=1.2) +
  ggtitle("Usage of Singular They in US, GB, CA") +
  scale_y_continuous(labels = function(x) paste0(x*100, "%")) +
  scale_color_manual(values = c("publisher" = "cadetblue", "top publisher" = "coral1")) + 
  # scale_shape_manual(values = c("US" = 4, "Generic They" = 2)) +
  labs(color = "", x="per month", y="in articles") + 
  facet_wrap(~country, ncol = 1, scale="free") +
  guides(alpha = "none") +
  theme_minimal() 




model.country.publisher.lm <- lm(GENERIC.HE.pro.ALL ~ date * country * type, 
                                 data = df.country.publisher)
summary(model.country.publisher.lm)
df.country.publisher$pred.he <- predict(model.country.publisher.lm)

model.country.publisher.lm.they <- lm(GENERIC.THEY.pro.ALL ~ date * country * type, 
                                 data = df.country.publisher)
summary(model.country.publisher.lm.they)
df.country.publisher$pred.they <- predict(model.country.publisher.lm.they)

ggplot(df.country.publisher, aes(x = date)) +
  # geom_point(aes(y = GENERIC.HE.pro.GEN, color=type, shape=type, alpha=0.05)) +
  geom_line(aes(y = pred.he, color=type), size=1.2) +
  geom_line(aes(y = pred.they, color=type), size=1.2, linetype = "dashed") +
  ggtitle("Usage of Generic He in US, GB, CA") +
  scale_y_continuous(labels = function(x) paste0(x*100, "%")) +
  scale_color_manual(values = c("publisher" = "cadetblue", "top publisher" = "coral1")) + 
  # scale_shape_manual(values = c("US" = 4, "Generic They" = 2)) +
  labs(color = "", x="per month", y="in articles") + 
  facet_wrap(~country, ncol = 3, scale="free") +
  guides(alpha = "none") +
  theme_minimal() 






