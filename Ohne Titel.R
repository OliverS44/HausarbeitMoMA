library(tidyverse)
library(lubridate)

data <- read.csv("/Users/oliversiedler/Desktop/Universität Leipzig/D.H. - Hausarbeit/MoMAExhibitions1929to1989.csv",
                 stringsAsFactors = FALSE, fileEncoding = "ISO-8859-1") #damit "ungewöhnliche" Buchstaben klar bleiben

data_clean <- data %>%
  mutate(
    ExhibitionBeginDate = mdy(ExhibitionBeginDate),
    ExhibitionEndDate = mdy(ExhibitionEndDate),
    Year = year(ExhibitionBeginDate)
  ) %>%
  filter(Year >= 1930 & Year <= 1949)

create_year_files <- function(year) {
  data_year <- data_clean %>% filter(Year == year)
  
  edges_nationalities <- data_year %>%
    group_by(ExhibitionID) %>%
    summarise(
      nationalities = list(unique(na.omit(Nationality))),  
      .groups = "drop"
    ) %>%
    filter(map_int(nationalities, length) >= 2) %>%
    mutate(
      pairs = map(nationalities, function(nat) {
        if (length(nat) < 2) return(NULL)  
        tibble(Source = combn(nat, 2)[1,], Target = combn(nat, 2)[2,])
      })
    ) %>%
    unnest(pairs) %>%
    count(Source, Target, name = "weight") %>%
    filter(Source != "" & Target != "") 
  
  
  if (nrow(edges_nationalities) > 0) {
    write.csv(edges_nationalities, paste0("edges_", year, ".csv"), row.names = FALSE)
  }
  
  nodes <- tibble(Id = unique(c(edges_nationalities$Source, edges_nationalities$Target)))
  
  if (nrow(nodes) > 0) {
    nodes <- nodes %>% mutate(Label = Id)  
    write.csv(nodes, paste0("/Users/oliversiedler/HausarbeitMoMA1/ nodes_", year, ".csv"), row.names = FALSE, quote = FALSE)
  }
}

purrr::walk(1930:1949, create_year_files)



library(tidyverse)

analyze_germany <- function(year) {
  edge_path <- paste0("edges_", year, ".csv")
  node_path <- paste0("nodes_", year, ".csv")
  
  if (!file.exists(edge_path)) return(NULL)
  
  edges <- read.csv(edge_path, stringsAsFactors = FALSE)
  german_edges <- edges %>%
    filter(Source == "German" | Target == "German") %>%
    mutate(Other = if_else(Source == "German", Target, Source)) %>%
    group_by(Other) %>%
    summarise(
      connections = n(),              
      total_weight = sum(weight)  
    ) %>%
    arrange(desc(total_weight)) 
  
  german_count <- if (file.exists(node_path)) {
    nodes <- read.csv(node_path, stringsAsFactors = FALSE)
    sum(nodes$Id == "German", na.rm = TRUE) 
  } else {
    NA
  }
  
  write.csv(german_edges, paste0("german_stats_", year, ".csv"), row.names = FALSE)
  
  return(tibble(Year = year, Exhibitions = german_count, Total_Connections = sum(german_edges$connections, na.rm = TRUE)))
}

german_summary <- map_dfr(1930:1949, analyze_germany)

write.csv(german_summary, "german_exhibitions_1930-1949.csv", row.names = FALSE)

library(tidyverse)

files <- list.files(pattern = "german_stats_\\d{4}\\.csv")

german_data <- files %>%
  map_dfr(~ read.csv(.x, stringsAsFactors = FALSE) %>% mutate(Year = str_extract(.x, "\\d{4}")))

german_data <- german_data %>% arrange(as.numeric(Year))

write.csv(german_data, "german_stats_overview.csv", row.names = FALSE)

library(tidyverse)

files <- list.files(pattern = "edges_\\d{4}\\.csv")

german_exhibitions <- files %>%
  map_dfr(~ {
    year <- str_extract(.x, "\\d{4}") 
    
    edges <- read.csv(.x, stringsAsFactors = FALSE) %>%
      filter(Source == "German" | Target == "German") %>%
      mutate(Year = as.numeric(year))
    
    exhibitions_germany <- data_clean %>%
      filter(Year == as.numeric(year) & Nationality == "German") %>%
      select(ExhibitionID) %>%
      distinct()
    
    tibble(
      Year = as.numeric(year),
      Total_Connections = nrow(edges),  # Anzahl der Kanten (Verbindungen)
      Exhibitions = n_distinct(exhibitions_germany$ExhibitionID)  # Anzahl einzelner Ausstellungen
    )
  })

write.csv(german_exhibitions, "german_exhibitions_1930-1949.csv", row.names = FALSE)










library(tidyverse)

edge_files <- list.files(pattern = "edges_\\d{4}\\.csv")

edges_all <- edge_files %>%
  map_dfr(~ {
    year <- as.numeric(str_extract(.x, "\\d{4}"))  
  })

edges_dynamic <- edges_all %>%
  group_by(Source, Target) %>%
  summarise(
    Weight = sum(weight),  
    .groups = "drop"
  )

write.csv(edges_dynamic, "edges_dynamic.csv", row.names = FALSE, quote = FALSE)

nodes_dynamic <- tibble(Id = unique(c(edges_dynamic$Source, edges_dynamic$Target)))

nodes_dynamic <- nodes_dynamic %>%
  mutate(Label = Id)

nationality_years <- edges_all %>%
  select(Source, Start) %>%
  rename(Id = Source) %>%
  bind_rows(edges_all %>% select(Target, Start) %>% rename(Id = Target)) %>%
  group_by(Id) %>%
  summarise(Start = min(Start), End = max(Start), .groups = "drop")

nodes_dynamic <- nodes_dynamic %>%
  left_join(nationality_years, by = "Id")

nodes_dynamic <- nodes_dynamic %>%
  mutate(End = ifelse(is.na(End), Start, End))

write.csv(nodes_dynamic, "nodes_dynamic.csv", row.names = FALSE, quote = FALSE)


library(tidyverse)

convert_to_iso8601 <- function(year) {
  ifelse(is.na(year), NA, paste0(year, "-01-01T00:00:00"))  # Falls NA, bleibt es NA, sonst Umwandlung
}

nodes_dynamic <- read.csv("nodes_dynamic.csv", stringsAsFactors = FALSE) %>%
  mutate(
    Start = convert_to_iso8601(Start),
    End = convert_to_iso8601(End)
  )

write.csv(nodes_dynamic, "nodes_dynamic_corrected.csv", row.names = FALSE, quote = FALSE)

edges_dynamic <- read.csv("edges_dynamic.csv", stringsAsFactors = FALSE) %>%
  mutate(
    Start = convert_to_iso8601(Start),
    End = convert_to_iso8601(End)
  )

write.csv(edges_dynamic, "edges_dynamic_corrected.csv", row.names = FALSE, quote = FALSE)


library(tidyverse)

convert_to_iso8601 <- function(year, is_end = FALSE) {
  ifelse(is.na(year), NA, paste0(year, ifelse(is_end, "-12-31T23:59:59", "-01-01T00:00:00")))
}

nodes_dynamic <- read.csv("nodes_dynamic.csv", stringsAsFactors = FALSE) %>%
  mutate(
    Start = convert_to_iso8601(Start, FALSE),
    End = convert_to_iso8601(End, TRUE)  # End auf den 31. Dezember setzen
  )

write.csv(nodes_dynamic, "nodes_dynamic_corrected.csv", row.names = FALSE, quote = FALSE)


library(tidyverse)

nodes_dynamic <- read.csv("nodes_dynamic.csv", stringsAsFactors = FALSE) %>%
  mutate(
    Start = as.numeric(Start),  # Start als numerischen Wert speichern
    End = as.numeric(End)       # End ebenfalls als numerisch
  )


edges_dynamic <- read.csv("edges_dynamic.csv", stringsAsFactors = FALSE) %>%
  mutate(
    Start = as.numeric(Start),
    End = as.numeric(End)
  )

write.csv(edges_dynamic, "edges_dynamic_corrected.csv", row.names = FALSE, quote = FALSE)

print("Die Dateien wurden erfolgreich mit numerischen Zeitwerten gespeichert!")

library(tidyverse)

format_interval <- function(start, end) {
  paste0("[", start, ";", end, "]")
}

nodes_dynamic <- read.csv("nodes_dynamic.csv", stringsAsFactors = FALSE) %>%
  mutate(
    Interval = format_interval(Start, End)  # Kombiniert Start & End ins Gephi-Format
  ) %>%
  select(Id, Label, Interval)  # Gephi braucht nur diese Spalten!

write.csv(nodes_dynamic, "nodes_dynamic_corrected.csv", row.names = FALSE, quote = FALSE)

edges_dynamic <- read.csv("edges_dynamic.csv", stringsAsFactors = FALSE) %>%
  mutate(
    Interval = format_interval(Start, End)
  ) %>%
  select(Source, Target, Weight, Interval)  # Wichtig: Gephi erwartet genau diese Spaltennamen

write.csv(edges_dynamic, "edges_dynamic_corrected.csv", row.names = FALSE, quote = FALSE)


library(tidyverse)

format_interval <- function(start, end) {
  end <- ifelse(start == end, start + 1, end)  # Falls Start == End, End um 1 erhöhen
  paste0("[", start, ";", end, "]")
}

nodes_dynamic <- read.csv("nodes_dynamic.csv", stringsAsFactors = FALSE) %>%
  mutate(
    Interval = format_interval(Start, End)  # `format_interval()` wird für jede Zeile angewendet
  ) %>%
  select(Id, Label, Interval)

write.csv(nodes_dynamic, "nodes_dynamic_corrected.csv", row.names = FALSE, quote = FALSE)

edges_dynamic <- read.csv("edges_dynamic.csv", stringsAsFactors = FALSE) %>%
  mutate(
    Interval = format_interval(Start, End)
  ) %>%
  select(Source, Target, Weight, Interval)

write.csv(edges_dynamic, "edges_dynamic_corrected.csv", row.names = FALSE, quote = FALSE)

print("Die Dateien wurden erfolgreich im Gephi-Intervallformat gespeichert!")

library(tidyverse)

format_interval <- function(start, end) {
  start <- as.numeric(start)
  end <- as.numeric(ifelse(start == end, start + 1, end))  # Falls Start == End, End um 1 erhöhen
  paste0("[", format(start, nsmall = 1), ";", format(end, nsmall = 1), "]")  # Dezimalformat erzwingen
}

nodes_dynamic <- read.csv("nodes_dynamic.csv", stringsAsFactors = FALSE) %>%
  mutate(
    Interval = format_interval(Start, End)  # Intervall mit Dezimalwerten
  ) %>%
  select(Id, Label, Interval)

write.csv(nodes_dynamic, "nodes_dynamic_corrected.csv", row.names = FALSE, quote = FALSE)

edges_dynamic <- read.csv("edges_dynamic.csv", stringsAsFactors = FALSE) %>%
  mutate(
    Interval = format_interval(Start, End)
  ) %>%
  select(Source, Target, Weight, Interval)

write.csv(edges_dynamic, "edges_dynamic_corrected.csv", row.names = FALSE, quote = FALSE)


library(tidyverse)

format_interval <- function(start, end) {
  start <- as.numeric(start)
  end <- as.numeric(ifelse(start == end, start + 1, end))  # Falls Start == End, End um 1 erhöhen
  paste0("[", format(start, nsmall = 1), ", ", format(end, nsmall = 1), "]")  # Komma als Trennzeichen
}

nodes_dynamic <- read.csv("nodes_dynamic.csv", stringsAsFactors = FALSE) %>%
  mutate(
    TimeInterval = format_interval(Start, End)  # Intervall mit Double-Werten
  ) %>%
  select(Id, Label, TimeInterval)  # Umbenennung der Spalte für Gephi

write.table(nodes_dynamic, "nodes_dynamic_corrected.csv", row.names = FALSE, sep = ";", quote = FALSE)

edges_dynamic <- read.csv("edges_dynamic.csv", stringsAsFactors = FALSE) %>%
  mutate(
    TimeInterval = format_interval(Start, End)
  ) %>%
  select(Source, Target, Weight, TimeInterval)

write.table(edges_dynamic, "edges_dynamic_corrected.csv", row.names = FALSE, sep = ";", quote = FALSE)


library(tidyverse)

format_interval <- function(start, end) {
  start <- as.numeric(start)
  end <- as.numeric(ifelse(start == end, start + 1, end))  # Falls Start == End, End um 1 erhöhen
  paste0("[", start, ", ", end, "]")  # WICHTIG: Komma statt Semikolon innerhalb der Klammern!
}

nodes_dynamic <- read.csv("nodes_dynamic.csv", stringsAsFactors = FALSE) %>%
  mutate(
    Interval = format_interval(Start, End)  # Echte Intervalle erstellen
  ) %>%
  select(Id, Label, Interval)

write.table(nodes_dynamic, "nodes_dynamic_fixed.csv", row.names = FALSE, sep = ";", quote = FALSE)

edges_dynamic <- read.csv("edges_dynamic.csv", stringsAsFactors = FALSE) %>%
  mutate(
    Interval = format_interval(Start, End)
  ) %>%
  select(Source, Target, Weight, Interval)

write.table(edges_dynamic, "edges_dynamic_fixed.csv", row.names = FALSE, sep = ";", quote = FALSE)


library(tidyverse)

format_interval <- function(start, end) {
  start <- as.numeric(start)
  end <- as.numeric(ifelse(start == end, start + 1, end))  # Falls Start == End, End um 1 erhöhen
  paste0(start, ",", end)  # KEINE Eckigen Klammern mehr → Gephi zwingt so "Interval<Double>"
}

nodes_dynamic <- read.csv("nodes_dynamic.csv", stringsAsFactors = FALSE) %>%
  mutate(
    Interval = format_interval(Start, End)  # Echte Intervalle erstellen
  ) %>%
  select(Id, Label, Interval)

write.table(nodes_dynamic, "nodes_dynamic_fixed.csv", row.names = FALSE, sep = ";", quote = FALSE)

edges_dynamic <- read.csv("edges_dynamic.csv", stringsAsFactors = FALSE) %>%
  mutate(
    Interval = format_interval(Start, End)
  ) %>%
  select(Source, Target, Weight, Interval)

write.table(edges_dynamic, "edges_dynamic_fixed.csv", row.names = FALSE, sep = ";", quote = FALSE)


library(tidyverse)

format_interval <- function(start, end) {
  start <- as.numeric(start)
  end <- as.numeric(ifelse(start == end, start + 1, end))  # Falls Start == End, End um 1 erhöhen
  paste0("[", start, ", ", end, "]")  # Korrektes Format für Gephi
}

nodes_dynamic <- read.csv("nodes_dynamic.csv", stringsAsFactors = FALSE, fileEncoding = "ISO-8859-1") %>%
  mutate(
    Interval = format_interval(Start, End)
  ) %>%
  select(Id, Label, Interval)

write.table(nodes_dynamic, "nodes_dynamic_fixed.csv", row.names = FALSE, sep = ";", quote = FALSE, fileEncoding = "UTF-8")

edges_dynamic <- read.csv("edges_dynamic.csv", stringsAsFactors = FALSE, fileEncoding = "ISO-8859-1") %>%
  mutate(
    Interval = format_interval(Start, End)
  ) %>%
  select(Source, Target, Weight, Interval)

write.table(edges_dynamic, "edges_dynamic_fixed.csv", row.names = FALSE, sep = ";", quote = FALSE, fileEncoding = "UTF-8")


data_clean %>% filter(Nationality == "architect") %>% select(DisplayName, ExhibitionTitle, Year)

data_clean <- data_clean %>%
  mutate(Nationality = ifelse(Nationality == "architect", NA, Nationality))

library(tidyverse)

  mutate(Nationality = ifelse(Nationality %in% c("architect", "Nationality unknown"), NA, Nationality))

format_interval <- function(start, end) {
  start <- as.numeric(start)
  end <- as.numeric(ifelse(start == end, start + 1, end))  # Falls Start == End, End um 1 erhöhen
  paste0("[", start, ", ", end, "]")  # Gephi-kompatibles Format
}

nodes_dynamic <- data_clean %>%
  filter(!is.na(Nationality)) %>%  
  distinct(Nationality) %>%
  rename(Id = Nationality) %>%
  mutate(Label = Id, Interval = format_interval(min(Year), max(Year)))  # Echte Zeitintervalle

write.table(nodes_dynamic, "nodes_dynamic_fixed.csv", row.names = FALSE, sep = ";", quote = FALSE, fileEncoding = "UTF-8")

edges_dynamic <- data_clean %>%
  filter(!is.na(Nationality)) %>%  
  group_by(ExhibitionID) %>%
  summarise(
    nationalities = list(unique(Nationality)),
    .groups = "drop"
  ) %>%
  filter(map_int(nationalities, length) >= 2) %>% 
  mutate(
    pairs = map(nationalities, ~ tibble(Source = combn(.x, 2)[1,], Target = combn(.x, 2)[2,]))
  ) %>%
  unnest(pairs) %>%
  count(Source, Target, name = "Weight") %>%
  mutate(Interval = format_interval(min(data_clean$Year), max(data_clean$Year)))  

write.table(edges_dynamic, "edges_dynamic_fixed.csv", row.names = FALSE, sep = ";", quote = FALSE, fileEncoding = "UTF-8")


library(tidyverse)

data_clean <- data_clean %>%
  mutate(Nationality = ifelse(Nationality %in% c("architect", "Nationality unknown"), NA, Nationality))

format_interval <- function(start, end) {
  start <- as.numeric(start)
  end <- as.numeric(ifelse(start == end, start + 1, end))  # Falls Start == End, End um 1 erhöhen
  paste0("[", start, ", ", end, "]")  # Gephi-kompatibles Format
}

nodes_dynamic <- data_clean %>%
  filter(!is.na(Nationality)) %>%  # Entferne "architect" & "Nationality unknown"
  group_by(Nationality) %>%
  summarise(
    Start = min(Year, na.rm = TRUE),  # Erster Ausstellungseintrag für diese Nationalität
    End = max(Year, na.rm = TRUE)  # Letzter Ausstellungseintrag
  ) %>%
  rename(Id = Nationality) %>%
  mutate(Label = Id, Interval = format_interval(Start, End))  # Echte Zeitintervalle

write.table(nodes_dynamic, "nodes_dynamic_fixed.csv", row.names = FALSE, sep = ";", quote = FALSE, fileEncoding = "UTF-8")

  filter(!is.na(Nationality)) %>%  # Entferne "architect" & "Nationality unknown"
  group_by(ExhibitionID) %>%
  summarise(
    nationalities = list(unique(Nationality)),
    .groups = "drop"
  ) %>%
  filter(map_int(nationalities, length) >= 2) %>%  # Nur Ausstellungen mit mind. 2 Nationalitäten
  mutate(
    pairs = map(nationalities, ~ tibble(Source = combn(.x, 2)[1,], Target = combn(.x, 2)[2,]))
  ) %>%
  unnest(pairs) %>%
  count(Source, Target, name = "Weight") %>%
  mutate(Interval = format_interval(min(data_clean$Year, na.rm = TRUE), max(data_clean$Year, na.rm = TRUE)))  # Echte Zeitintervalle

write.table(edges_dynamic, "edges_dynamic_fixed.csv", row.names = FALSE, sep = ";", quote = FALSE, fileEncoding = "UTF-8")


library(tidyverse)

data_clean <- data_clean %>%
  mutate(Nationality = ifelse(Nationality %in% c("architect", "Nationality unknown"), NA, Nationality))

format_interval <- function(start, end) {
  start <- as.numeric(start)
  end <- as.numeric(ifelse(start == end, start + 1, end))  # Falls Start == End, End um 1 erhöhen
  paste0("[", start, ", ", end, "]")  # Gephi-kompatibles Format
}

nodes_dynamic <- data_clean %>%
  filter(!is.na(Nationality)) %>%  # Entferne "architect" & "Nationality unknown"
  group_by(Nationality) %>%
  summarise(
    Start = min(Year, na.rm = TRUE),  # Erster Ausstellungseintrag für diese Nationalität
    End = max(Year, na.rm = TRUE)  # Letzter Ausstellungseintrag
  ) %>%
  rename(Id = Nationality) %>%
  mutate(Label = Id, Interval = format_interval(Start, End))  # Echte Zeitintervalle

write.table(nodes_dynamic, "nodes_dynamic_fixed.csv", row.names = FALSE, sep = ";", quote = FALSE, fileEncoding = "UTF-8")

edges_dynamic <- data_clean %>%
  filter(!is.na(Nationality)) %>%  # Entferne "Nationality unknown"
  group_by(ExhibitionID) %>%
  summarise(
    nationalities = list(unique(Nationality)),
    .groups = "drop"
  ) %>%
  filter(map_int(nationalities, length) >= 2) %>%  # Nur Ausstellungen mit mind. 2 Nationalitäten
  mutate(
    pairs = map(nationalities, ~ tibble(Source = combn(.x, 2)[1,], Target = combn(.x, 2)[2,]))
  ) %>%
  unnest(pairs) %>%
  filter(Source != "" & Target != "") %>%  # Leere Werte sicher entfernen
  count(Source, Target, name = "Weight") %>%
  mutate(Interval = format_interval(min(data_clean$Year, na.rm = TRUE), max(data_clean$Year, na.rm = TRUE)))  # Echte Zeitintervalle

write.table(edges_dynamic, "edges_dynamic_fixed.csv", row.names = FALSE, sep = ";", quote = FALSE, fileEncoding = "UTF-8")


library(tidyverse)

data_clean <- data_clean %>%
  mutate(Nationality = ifelse(Nationality %in% c("architect", "Nationality unknown", ""), NA, Nationality))

format_interval <- function(start, end) {
  start <- as.numeric(start)
  end <- as.numeric(ifelse(start == end, start + 1, end))  
  paste0("[", start, ", ", end, "]")  
}

nodes_dynamic <- data_clean %>%
  filter(!is.na(Nationality)) %>%
  group_by(Nationality) %>%
  summarise(
    Start = min(Year, na.rm = TRUE),  
    End = max(Year, na.rm = TRUE)  
  ) %>%
  rename(Id = Nationality) %>%
  mutate(Label = Id, Interval = format_interval(Start, End)) %>%
  filter(Id != "" & !is.na(Id)) 

write.table(nodes_dynamic, "nodes_dynamic_fixed.csv", row.names = FALSE, sep = ";", quote = FALSE, fileEncoding = "UTF-8")

edges_dynamic <- data_clean %>%
  filter(!is.na(Nationality)) %>%
  group_by(ExhibitionID, Year) %>%  
  summarise(
    nationalities = list(unique(Nationality)),
    .groups = "drop"
  ) %>%
  filter(map_int(nationalities, length) >= 2) %>%
  mutate(
    pairs = map(nationalities, ~ tibble(Source = combn(.x, 2)[1,], Target = combn(.x, 2)[2,]))
  ) %>%
  unnest(pairs) %>%
  filter(Source != "" & Target != "" & !is.na(Source) & !is.na(Target)) %>%  
  count(Source, Target, Year, name = "Weight") %>%
  mutate(Interval = format_interval(Year, Year)) 

write.table(edges_dynamic, "edges_dynamic_fixed.csv", row.names = FALSE, sep = ";", quote = FALSE, fileEncoding = "UTF-8")


american_artists_df <- unique(data_clean[data_clean$Nationality == "American", c("DisplayName", "Nationality")])

american_artists_df <- na.omit(american_artists_df)

View(american_artists_df)

write.csv(data_clean, "data_clean.csv", row.names = FALSE)

write.csv(data, "data.csv", row.names = FALSE)


library(dplyr)

time_periods <- list(
  "1930_1933" = c(1930, 1933),
  "1934_1938" = c(1934, 1938),
  "1939_1945" = c(1939, 1945)
)

get_top5_exhibition_partners <- function(data, start_year, end_year) {
  data %>%
    filter(Year >= start_year & Year <= end_year) %>%  # Zeitraum filtern
    select(ExhibitionID, Nationality) %>%  
    distinct() %>%  n
    filter(Nationality == "German") %>%  
    inner_join(data, by = "ExhibitionID", suffix = c("_German", "_Other")) %>%  
    filter(Nationality_Other != "German") %>%  
    distinct(ExhibitionID, Nationality_Other) %>% 
    count(Nationality_Other, sort = TRUE) %>% 
    top_n(5, n)  # Die Top 5 wählen
}

top5_exhibition_results <- lapply(names(time_periods), function(period) {
  years <- time_periods[[period]]
  result <- get_top5_exhibition_partners(data_clean, years[1], years[2])
  result$Period <- period  # Zeitraum als Spalte hinzufügen
  return(result)
})

top5_exhibition_df <- bind_rows(top5_exhibition_results)

top5_exhibition_df

library(ggplot2)
library(dplyr)

top5_exhibition_df$Period <- factor(top5_exhibition_df$Period, levels = c("1930_1933", "1934_1938", "1939_1945"))

ggplot(top5_exhibition_df, aes(x = reorder(Nationality_Other, -n), y = n, fill = Period)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Top 5 Ausstellungspartner von 'German'",
       x = "Nation",
       y = "Anzahl gemeinsamer Ausstellungen",
       fill = "Zeitraum") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

library(dplyr)

time_periods <- list(
  "1930_1933" = c(1930, 1933),
  "1934_1938" = c(1934, 1938),
  "1939_1945" = c(1939, 1945)
)

calculate_german_exhibition_share <- function(data, start_year, end_year) {
  total_exhibitions <- data %>%
    filter(Year >= start_year & Year <= end_year) %>%
    distinct(ExhibitionID) %>%
    nrow()  
  
  german_exhibitions <- data %>%
    filter(Year >= start_year & Year <= end_year, Nationality == "German") %>%
    distinct(ExhibitionID) %>%
    nrow()  
  
  percentage <- (german_exhibitions / total_exhibitions) * 100  
  
  return(data.frame(Zeitraum = paste(start_year, "-", end_year), 
                    Gesamt_Ausstellungen = total_exhibitions, 
                    German_Ausstellungen = german_exhibitions, 
                    Anteil_in_Prozent = round(percentage, 2)))
}

german_exhibition_shares <- bind_rows(lapply(time_periods, function(period) {
  calculate_german_exhibition_share(data_clean, period[1], period[2])
}))

german_exhibition_shares

calculate_german_partners <- function(data, start_year, end_year) {
  german_exhibitions <- data %>%
    filter(Year >= start_year & Year <= end_year, Nationality == "German") %>%
    select(ExhibitionID) %>% 
    distinct()  
  
  partner_nations <- data %>%
    filter(Year >= start_year & Year <= end_year) %>%
    filter(ExhibitionID %in% german_exhibitions$ExhibitionID, Nationality != "German") %>%
    select(Nationality) %>%
    distinct() %>%  
    nrow()
  
  return(data.frame(Zeitraum = paste(start_year, "-", end_year), 
                    Anzahl_Mit_Aussteller_Nationen = partner_nations))
}

german_partners_count <- bind_rows(lapply(time_periods, function(period) {
  calculate_german_partners(data_clean, period[1], period[2])
}))

german_partners_count

data_clean_filtered <- data_clean %>%
  filter(!(Nationality %in% c("architect", "Nationality Unknown")))

german_partners_count <- bind_rows(lapply(time_periods, function(period) {
  calculate_german_partners(data_clean_filtered, period[1], period[2])
}))

german_partners_count

data_1930_1933 <- data_clean %>% filter(Year >= 1930 & Year <= 1933)
data_1934_1938 <- data_clean %>% filter(Year >= 1934 & Year <= 1938)
data_1939_1945 <- data_clean %>% filter(Year >= 1939 & Year <= 1945)

write.csv(data_1930_1933, "data_1930_1933.csv", row.names = FALSE)
write.csv(data_1934_1938, "data_1934_1938.csv", row.names = FALSE)
write.csv(data_1939_1945, "data_1939_1945.csv", row.names = FALSE)

library(dplyr)

create_gephi_files <- function(data, start_year, end_year, period_name) {
  data_filtered <- data %>% filter(Year >= start_year & Year <= end_year)
  
  nodes <- data_filtered %>%
    select(Nationality) %>%
    distinct() %>%
    mutate(Id = Nationality, Label = Nationality) %>%
    select(Id, Label)
  
  edges <- data_filtered %>%
    select(ExhibitionID, Nationality) %>%
    distinct() %>%
    inner_join(data_filtered %>% select(ExhibitionID, Nationality) %>% distinct(), 
               by = "ExhibitionID", relationship = "many-to-many") %>%
    filter(Nationality.x != Nationality.y) %>%
    group_by(Nationality.x, Nationality.y) %>%
    summarise(Weight = n(), .groups = "drop") %>%
    rename(Source = Nationality.x, Target = Nationality.y)
  
  write.csv(nodes, paste0("nodes_", period_name, ".csv"), row.names = FALSE)
  write.csv(edges, paste0("edges_", period_name, ".csv"), row.names = FALSE)
}

time_periods <- list(
  "1930_1933" = c(1930, 1933),
  "1934_1938" = c(1934, 1938),
  "1939_1945" = c(1939, 1945)
)

lapply(names(time_periods), function(period) {
  create_gephi_files(data_clean, time_periods[[period]][1], time_periods[[period]][2], period)
})


get_degree_centrality <- function(data, nation = "German") {
  german_exhibitions <- data[data$Nationality == nation, ]
  co_exhibitors <- data[data$ExhibitionID %in% german_exhibitions$ExhibitionID & data$Nationality != nation, ]
  
  degree_centrality <- length(unique(co_exhibitors$Nationality))
  weighted_degree_centrality <- nrow(co_exhibitors)
  
  return(data.frame(Degree_Centrality = degree_centrality, Weighted_Degree_Centrality = weighted_degree_centrality))
}

degree_centrality_1930_1933 <- get_degree_centrality(data_1930_1933)
degree_centrality_1934_1938 <- get_degree_centrality(data_1934_1938)
degree_centrality_1939_1945 <- get_degree_centrality(data_1939_1945)

degree_centrality_results <- data.frame(
  Zeitraum = c("1930 - 1933", "1934 - 1938", "1939 - 1945"),
  Degree_Centrality = c(degree_centrality_1930_1933$Degree_Centrality, 
                        degree_centrality_1934_1938$Degree_Centrality, 
                        degree_centrality_1939_1945$Degree_Centrality),
  Weighted_Degree_Centrality = c(degree_centrality_1930_1933$Weighted_Degree_Centrality, 
                                 degree_centrality_1934_1938$Weighted_Degree_Centrality, 
                                 degree_centrality_1939_1945$Weighted_Degree_Centrality)
)

print(degree_centrality_results)

library(dplyr)

dominant_nations <- c("American", "French", "British")

get_degree_centrality <- function(data, nation) {
  nation_exhibitions <- data %>% filter(Nationality == nation)
  co_exhibitors <- data %>% filter(ExhibitionID %in% nation_exhibitions$ExhibitionID, Nationality != nation)
  
  degree_centrality <- length(unique(co_exhibitors$Nationality))
  weighted_degree_centrality <- nrow(co_exhibitors)
  
  return(data.frame(Degree_Centrality = degree_centrality, Weighted_Degree_Centrality = weighted_degree_centrality))
}

time_periods <- list(
  "1930_1933" = data_1930_1933,
  "1934_1938" = data_1934_1938,
  "1939_1945" = data_1939_1945
)

results <- list()
for (period_name in names(time_periods)) {
  period_data <- time_periods[[period_name]]
  
  for (nation in dominant_nations) {
    result <- get_degree_centrality(period_data, nation)
    result$Zeitraum <- period_name
    result$Nation <- nation
    results <- append(results, list(result))
  }
}

dominant_nation_results <- bind_rows(results)
print(dominant_nation_results)

head(data_clean)

write.csv(data_clean, "data_clean.csv", row.names = FALSE, fileEncoding = "UTF-8")

data_1946_1949 <- subset(data_clean, Year >= 1946 & Year <= 1949)

data_1946_1949 <- subset(data_1946_1949, !is.na(Nationality) & Nationality != "")

nodes_1946_1949 <- data.frame(Id = unique(data_1946_1949$Nationality))
nodes_1946_1949$Label <- nodes_1946_1949$Id

library(dplyr)
library(tidyr)
library(purrr)

edges_1946_1949 <- data_1946_1949 %>%
  group_by(ExhibitionID) %>%
  summarise(Nations = list(unique(Nationality)), .groups = "drop") %>%
  mutate(Edges = map(Nations, ~ combn(.x, 2, simplify = FALSE))) %>%
  select(Edges) %>%
  unnest(Edges) %>%
  transmute(Source = map_chr(Edges, 1), Target = map_chr(Edges, 2)) %>%
  group_by(Source, Target) %>%
  summarise(Weight = n(), .groups = "drop")

write.csv(edges_1946_1949, "edges_1946_1949.csv", row.names = FALSE, fileEncoding = "UTF-8")

library(dplyr)
library(tidyr)
library(purrr)

edges_1946_1949 <- data_1946_1949 %>%
  group_by(ExhibitionID) %>%
  summarise(Nations = list(unique(Nationality)), .groups = "drop") %>%
  filter(lengths(Nations) > 1) %>%  # Entfernt Einträge mit nur einer Nation
  mutate(Edges = map(Nations, ~ combn(.x, 2, simplify = FALSE))) %>%
  select(Edges) %>%
  unnest(Edges) %>%
  transmute(Source = map_chr(Edges, 1), Target = map_chr(Edges, 2)) %>%
  group_by(Source, Target) %>%
  summarise(Weight = n(), .groups = "drop")

write.csv(edges_1946_1949, "edges_1946_1949.csv", row.names = FALSE, fileEncoding = "UTF-8")



library(ggplot2)
library(dplyr)
library(tidyr)

top5_data <- data.frame(
  Nation = c("French", "American", "Dutch", "Italian", "Spanish", "British", "Russian", "Mexican"),
  Zeitraum_1930_1933 = c(135, 110, 10, 5, 15, 20, 25, 0),
  Zeitraum_1934_1938 = c(275, 320, 8, 4, 35, 40, 50, 0),
  Zeitraum_1939_1945 = c(380, 820, 12, 5, 90, 80, 60, 70),
  Zeitraum_1946_1949 = c(248, 465, 0, 0, 60, 38, 0, 33)  # Werte direkt aus den Berechnungen
)

top5_data_long <- top5_data %>%
  pivot_longer(cols = starts_with("Zeitraum"), names_to = "Zeitraum", values_to = "Anzahl") %>%
  drop_na()

ggplot(top5_data_long, aes(x = Nation, y = Anzahl, fill = Zeitraum)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("red", "green", "blue", "black")) +
  labs(title = "Top 5 Ausstellungspartner von 'German'",
       x = "Nation",
       y = "Anzahl gemeinsamer Ausstellungen") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

library(ggplot2)
library(dplyr)
library(tidyr)

top5_data <- data.frame(
  Nation = c("American", "French", "British", "Spanish", "Italian"),
  Zeitraum_1930_1933 = c(50, 45, 30, 25, 20),  # Ersetze mit tatsächlichen Werten
  Zeitraum_1934_1938 = c(100, 90, 60, 50, 40),
  Zeitraum_1939_1945 = c(800, 400, 150, 120, 100),
  Zeitraum_1946_1949 = c(450, 300, 120, 90, 80)
)

top5_data_long <- top5_data %>%
  pivot_longer(cols = starts_with("Zeitraum"), names_to = "Zeitraum", values_to = "Anzahl") %>%
  drop_na()

ggplot(top5_data_long, aes(x = Nation, y = Anzahl, fill = Zeitraum)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("red", "green", "blue", "purple")) +
  labs(title = "Top 5 Ausstellungspartner von 'German' (nur gemeinsame Ausstellungen)",
       x = "Nation",
       y = "Anzahl gemeinsamer Ausstellungen mit 'German'") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)







