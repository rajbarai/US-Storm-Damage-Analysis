getwd()

setwd("/Users/siiuuuuupro/Desktop/Fall 2025/IST 719/Viz/")

storm <- read.csv("StormEvents.csv")
head(storm)

cols_to_keep <- c(
  "EVENT_TYPE", 
  "INJURIES_DIRECT", "INJURIES_INDIRECT",
  "DEATHS_DIRECT", "DEATHS_INDIRECT",
  "MONTH_NAME", "YEAR",
  "STATE",
  "DAMAGE_PROPERTY", "DAMAGE_CROPS")

storm_clean <- storm[,cols_to_keep]

#Q1
injuries_summary <- aggregate(
  INJURIES_DIRECT + INJURIES_INDIRECT ~ EVENT_TYPE,
  storm_clean,sum)
names(injuries_summary)[2] <- "TOTAL_INJURIES"

deaths_summary <- aggregate(
  DEATHS_DIRECT + DEATHS_INDIRECT ~ EVENT_TYPE,
  storm_clean,sum)
names(deaths_summary)[2] <- "TOTAL_DEATHS"

injuries_split <- aggregate(
  cbind(INJURIES_DIRECT, INJURIES_INDIRECT) ~ EVENT_TYPE,
  storm_clean,sum)

deaths_split <- aggregate(
  cbind(DEATHS_DIRECT, DEATHS_INDIRECT) ~ EVENT_TYPE,
  storm_clean,sum)

event_counts <- as.data.frame(table(storm_clean$EVENT_TYPE))
names(event_counts) <- c("EVENT_TYPE", "COUNT")

event_counts <- subset(event_counts, COUNT > 0)

top10 <- event_counts[order(-event_counts$COUNT), ][1:10, ]

ggplot(top10, aes(x = reorder(EVENT_TYPE, COUNT), y = COUNT)) +
  geom_bar(stat = "identity", fill = "lightgreen") + coord_flip() +
  labs(title = "Top 10 Most Frequent Storm Event Types",
    x = "Event Type",
    y = "Count") +
  theme_minimal()

ggsave("Q1.1.pdf")

injuries_split_counts <- subset(injuries_split, INJURIES_DIRECT > 0 | INJURIES_INDIRECT > 0)
top10_injuries <- injuries_split_counts[
  order(-(injuries_split_counts$INJURIES_DIRECT + injuries_split_counts$INJURIES_INDIRECT)),
][1:10, ]

ggplot(top10_injuries, aes(x = reorder(EVENT_TYPE, INJURIES_DIRECT + INJURIES_INDIRECT))) +
  geom_bar(aes(y = INJURIES_DIRECT, fill = "Direct"), stat = "identity") +
  geom_bar(aes(y = INJURIES_INDIRECT, fill = "Indirect"), stat = "identity") +
  coord_flip() + labs(
    title = "Direct vs Indirect Injuries by Storm Type",
    x = "Event Type",
    y = "Injuries",
    fill = "")

ggsave("Q1.2.pdf")

deaths_split_counts <- subset(deaths_split, DEATHS_DIRECT > 0 | DEATHS_INDIRECT > 0)
top10_deaths <- deaths_split_counts[
  order(-(deaths_split_counts$DEATHS_DIRECT + deaths_split_counts$DEATHS_INDIRECT)),
][1:10, ]

ggplot(top10_deaths, aes(x = reorder(EVENT_TYPE, DEATHS_DIRECT + DEATHS_INDIRECT))) +
  geom_bar(aes(y = DEATHS_DIRECT, fill = "Direct"), stat = "identity") +
  geom_bar(aes(y = DEATHS_INDIRECT, fill = "Indirect"), stat = "identity") +
  coord_flip() + labs(
    title = "Direct vs Indirect Deaths by Storm Type",
    x = "Event Type",
    y = "Deaths",
    fill = "")

ggsave("Q1.3.pdf")

#Q2

month_levels <- c("January","February","March","April","May","June",
                  "July","August","September","October","November","December")
storm_clean$MONTH_NAME <- factor(storm_clean$MONTH_NAME, levels = month_levels)

tbl_month <- table(storm_clean$MONTH_NAME)

events_by_month <- aggregate(EVENT_TYPE ~ MONTH_NAME, storm_clean, length)
names(events_by_month)[2] <- "COUNT"
events_by_month <- events_by_month[order(events_by_month$MONTH_NAME), ]

ggplot(events_by_month, aes(x = MONTH_NAME, y = COUNT)) +
  geom_col(fill = "gray") +
  labs(title = "Storm Events by Month",
       x = "Month", y = "Event Count") +
  theme_minimal(base_size = 9) 

ggsave("Q2.1.pdf")

top_types <- names(sort(table(storm_clean$EVENT_TYPE), decreasing = TRUE))[1:8]
storm_season <- storm_clean %>% filter(EVENT_TYPE %in% top_types)

month_type_ct <- as.data.frame(table(storm_season$MONTH_NAME, storm_season$EVENT_TYPE))
names(month_type_ct) <- c("MONTH_NAME","EVENT_TYPE","COUNT")

month_type_ct$MONTH_NAME <- factor(month_type_ct$MONTH_NAME, levels = month_levels)

ggplot(month_type_ct, aes(x = MONTH_NAME, y = EVENT_TYPE, fill = COUNT)) +
  geom_tile(color = "white", linewidth = 0.1) +
  labs(title = "Seasonality by Storm Type",
       x = "Month", y = "Event Type", fill = "Count") +
  theme_minimal(base_size = 9) + 
  scale_fill_gradient(low = "gray", high = "black",trans = "sqrt", name = "Count") +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

ggsave("Q2.2.pdf")

#Q3
states_map <- map_data("state")

state_sum2 <- state_sum %>%
  mutate(region = tolower(STATE))

plot_df <- inner_join(states_map, state_sum2, by = "region")

ggplot(plot_df, aes(long, lat, group = group, fill = damage)) +
  geom_polygon(color = "white", linewidth = 0.2) +
  coord_map("albers", lat0 = 39, lat1 = 45) +
  scale_fill_viridis_c(
    trans = "sqrt",
    label = label_dollar(scale = 1e-9, suffix = "B"),
    name = "Damage") +
  labs(title = "Storm Damage Across the United States",
    x = NULL, y = NULL) +
  theme_void(base_size = 15) +
  theme(legend.position = "right",
    plot.title = element_text(face = "bold"))

ggsave("Q3.1.pdf")

