library(tidyverse)

df <- read_csv('data.csv')

parties <- c('Linke', 'SPD', 'Grüne', 'CDU', 'FDP', 'AfD')

# correct ordering of words
df$party <- factor(df$party, levels = parties)

# correct ordering of parties
df$word <- factor(df$word, levels = c("Demokrat", "Nationalstaat", "Bürgerin", "EZB", "Lohn", "Beschäftigte", "Kommune", "Chance", "Familie", "Armut", "Konzern", "Bürger", "Innovation", "Prozent", "Beispiel", "Kind", "Weg", "Arbeitsplatz", "Arbeit", "Frau", "Region", "Welt", "Forschung", "Kultur", "Gesellschaft", "Zugang", "Demokratie", "Zusammenarbeit", "Euro", "Menschenrecht", "Ebene", "Bildung", "Politik", "Bereich", "Sicherheit", "Zukunft", "Freiheit", "Interesse", "Parlament", "Schutz", "Jahr", "Staat", "Entwicklung", "Ziel", "Deutschland", "Land", "Union", "Recht", "Unternehmen", "Mitgliedstaat", "EU", "Mensch", "Europa"))

# rankings start at 1
df$value <- df$value + 1

# previously invalid were set to -1, but now 0
df <- df %>%
  mutate(value_label = ifelse(value == 0, '-', paste(value, '.', sep=''))) %>%
  mutate(value = ifelse(value > 100, NA, value)) %>%
  mutate(value = replace(value, value == 0, NA)) %>%
  mutate(color_label = ifelse(value > 30 | is.na(value), 'black', 'white'))


ggplot(data=df, aes(x = as.numeric(party), y=word, fill=value)) +
  geom_tile(color='white', size=1) +
  scale_fill_gradient(name = "value", low = "darkblue", high = "white", na.value = 'white', trans='log10', guide=FALSE) +
  geom_text(size=3, aes(label = value_label, color=color_label)) +
  scale_color_manual(values = c("grey30", "white"), guide=FALSE) +
  xlab(label = "") + ylab(label = "") + 
  theme_classic(base_size = 12, base_family = "Roboto") +
  scale_x_continuous(sec.axis = dup_axis(), breaks = 1:length(parties), labels = parties) +
  labs(title = "Wahlprogramme zur Europawahl 2019", subtitle = "Ranking der häufigsten Wörter pro Partei", caption = 'Nur die 20 häufigsten Nomen wurden pro Partei berücksichtigt.')
  # theme(plot.margin = unit(c(0.5,5,0.5,0), "cm"))

ggsave('wahlprogramme_1.svg', width = 5)
ggsave('wahlprogramme_1.jpg', width = 5)


