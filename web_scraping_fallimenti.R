# Analyse Konkurse in Südtirol

# ------------------------------------------------------------------------------

# To Do:
# create Markdown file with LaTeX
# copy analysis for other regions (e.g. Trento)
# adjust margins and other stuff in plots made with base R!

# ------------------------------------------------------------------------------

# packages

library(rvest) # web scraping data
library(dplyr) # data manipulation
library(ggplot2) # make violin plot
library(Hmisc) # make violin plot

# scrape HTML table directly from website:
# https://www.r-bloggers.com/using-rvest-to-scrape-an-html-table/

# additional sources who analyzed similar data
# https://www.tageszeitung.it/2018/08/02/34-konkurse/
# https://astat.provinz.bz.it/de/aktuelles-publikationen-info.asp?news_action=4&news_article_id=622999

# ------------------------------------------------------------------------------

# create all links

all_links <- paste0("http://www.fallimentibolzano.com/index.php?where=ultime_procedure_dichiarate_mostra_tutte&altre=fallimenti&page=",
                    1:40)

# loop over all links and extract data

data_all <- list()

for (i in 1:length(all_links)) {
  
  data <- all_links[i] %>%
    read_html() %>%
    html_nodes(xpath='//*[@id="elenco_procedure"]') %>%
    html_table()
  data_all[[i]] <- data[[1]]
  
  # wait random time step for next iteration to not overload server with requests!
  Sys.sleep(runif(1, min = 1, max = 4))
  
  # show current iteration
  print(i)
  
}

# ------------------------------------------------------------------------------

# data preprocessing and preperation for analysis

# create one big data frame
data_final <- do.call(rbind.data.frame, data_all)

# convert to date
data_final$`Data dich.` <- as.Date(data_final$`Data dich.`, "%d/%m/%Y")
data_final$`Data chius.` <- as.Date(data_final$`Data chius.`, "%d/%m/%Y")

# year of dichiarazione
data_final$year_dich <- as.numeric(format(data_final$`Data dich.`,'%Y'))

# year of closure
data_final$year_close <- as.numeric(format(data_final$`Data chius.`,'%Y'))

# closure time in days
data_final$close_time_days <- as.numeric(data_final$`Data chius.` - data_final$`Data dich.`, units="days")

# closure time in years
data_final$close_time_years <- as.numeric(difftime(data_final$`Data chius.`, data_final$`Data dich.`, 
                                                   unit="weeks"))/52.25

# last solution is based on:
# https://stackoverflow.com/questions/15569333/get-date-difference-in-years-floating-point

# To do:
# include tryCatch and automated extracton of maximal pages with data

# save prepared dataset to current working directory!
write.csv2(data_final, "daten_konkurse.csv", row.names = F)


data_final <- read.csv2("daten_konkurse.csv")


# ------------------------------------------------------------------------------

# Start with analysis

# distribution of closure time

hist(data_final$close_time_years, breaks = 30, main = "Verteilung Abschlusszeit in Jahre 2007-2019",
     ylab = "Anzahl", xlab = "Zeit in Jahren")
abline(v = mean(data_final$close_time_years, na.rm = T), col = "red", lty = 2, lwd = 2)
abline(v = median(data_final$close_time_years, na.rm = T), col = "orange", lty = 2, lwd = 2)
legend("topright", 
       c("Durchschnitt", "Median"), 
       lty=c(2, 2), 
       col=c("red","orange"),
       lwd = c(2,2), 
       bty = "n",
       cex = 0.75)

# in tabelle anzeigen
summary(data_final$close_time_years)

dich_year <- data_final %>% 
  group_by(year_dich) %>% 
  tally() # create concise summary table

close_year <- data_final %>%
  group_by(year_close) %>% 
  filter(!is.na(year_close)) %>% # exclude positions which are not finished!
  tally()

# plot absolute values dichiarazione fallimento
plot(dich_year$n ~ dich_year$year_dich, type = "b", pch = 1)
abline(h = min(dich_year$n), col = "green")
abline(h = max(dich_year$n), col = "red")

# plot absolute values closure fallimento
plot(close_year$n ~ close_year$year_close, type = "b", pch = 1)
abline(h = min(close_year$n), col = "green")
abline(h = max(close_year$n), col = "red")


# absolute yearly difference

# difference from year to an other --> include also relative difference in %!
plot(diff(dich_year$n, 1) ~ dich_year$year_dich[-1], type = "b", pch = 1, col = "blue", lwd = 2)
abline(h = 0, col = "orange")
abline(h = min(diff(dich_year$n, 1)), col = "green")
abline(h = max(diff(dich_year$n, 1)), col = "red")

# annual change in % (YoY)

# still open!!

# average closing time over the years
avg_close_time <- data_final %>%
  filter(Stato == "C") %>%
  group_by(year_close) %>%
  summarise(avg_time = mean(close_time_years)) %>%
  select(year_close, avg_time)

plot(avg_close_time$avg_time ~ avg_close_time$year_close,  
     type = "b", pch = 1, col = "blue", lwd = 2)

median_close_time <- data_final %>%
  filter(Stato == "C") %>%
  group_by(year_close) %>%
  summarise(median_time = median(close_time_years)) %>%
  select(year_close, median_time)

# summary table
closing_times <- cbind(avg_close_time, median_close_time[, -1], data_final %>% 
                         group_by(year_close) %>% 
                         tally() %>%
                         select(n))

# create violin plot to show distribution for each year in sample
violine <- data_final %>%
  filter(Stato == "C") %>%
  mutate(year_close = as.factor(year_close)) %>%
  filter(!is.na(year_close)) %>%
  filter(!is.na(close_time_years)) %>%
  select(year_close, close_time_years)

p <- ggplot(violine, aes(x=year_close, y=close_time_years, color = year_close)) + 
  geom_violin(show.legend = FALSE) +
  labs(x = "Jahre") +
  labs(y = "Abschlusszeit in Jahren") +
  labs(title = "Verteilung der Abschlusszeit 2009 - 2019") +
  stat_summary(fun.y=mean, geom="point", size=2, color="red") +
  theme_minimal()
p

# ------------------------------------------------------------------------------

# how many judges?
judges <- unique(data_final$`Giudice Delegato`)

judge_year_tot <- data_final %>% 
  group_by(`Giudice Delegato`) %>% 
  tally()

judge_year <- data_final %>% 
  group_by(`Giudice Delegato`, year_dich) %>% 
  tally()


# what about curators?

curators <- unique(data_final$Curatore)
length(curators)

curators_year_tot <- data_final %>% 
  group_by(Curatore) %>% 
  tally() %>%
  arrange(desc(n))

barplot(curators_year_tot$n)
hist(curators_year_tot$n, breaks = 15)

# closing time per curator?

avg_close_time_curator <- data_final %>%
  filter(Stato == "C") %>%
  group_by(Curatore) %>%
  summarise(avg_time = mean(close_time_years)) %>%
  select(Curatore, avg_time) %>%
  arrange(desc(avg_time))

curatore_final <- left_join(avg_close_time_curator, 
                            curators_year_tot, 
                            by = "Curatore") %>%
  filter(n > 10) %>%
  filter(!is.na(avg_time)) %>%
  arrange(desc(avg_time))
  
# ------------------------------------------------------------------------------