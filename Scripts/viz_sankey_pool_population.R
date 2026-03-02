pacman::p_load(tidyverse, here, networkD3, htmlwidgets, jsonlite, webshot2, fs)

# --- Data ---
if (!exists("pool_summary_pop")) {
  pool_summary_pop <- readRDS(here("Outputs","Intermediate outputs","pool_summary_pop.rds"))
}

lvl_pool    <- c(4L, 3L, 2L)
lvl_poolpop <- c("PHC","ACF","District","Children",
                 "PHC+District","PHC+Children","District+Children","PHC+District+Children")

mp <- pool_summary_pop %>%
  filter(!is.na(Country_Name), !is.na(poolsize), !is.na(poolpop)) %>%
  mutate(
    poolsize = factor(poolsize, levels = lvl_pool),
    poolpop  = factor(poolpop,  levels = lvl_poolpop)
  )

# Order countries high→low by total n
country_names <- mp %>%
  group_by(Country_Name) %>% summarise(n = sum(n), .groups = "drop") %>%
  arrange(desc(n)) %>% pull(Country_Name)

# --- Nodes ---
nodes <- tibble(
  name = c(
    paste0("country::", country_names),
    paste0("poolsize::", lvl_pool),
    paste0("poolpop::", lvl_poolpop)
  )
) %>%
  distinct() %>%
  mutate(
    id    = row_number() - 1,
    label = case_when(
      str_starts(name, "country::")  ~ str_remove(name, "^country::"),
      str_starts(name, "poolsize::") ~ paste("Pool", str_remove(name, "^poolsize::")),
      str_starts(name, "poolpop::")  ~ str_remove(name, "^poolpop::"),
      TRUE ~ name
    )
  )

# --- Counts for labels ---
node_counts <- bind_rows(
  mp %>% 
    group_by(Country_Name) %>% summarise(n = n_distinct(poolno), .groups = "drop") %>%
    transmute(name = paste0("country::", Country_Name), n, what = "pools"),
  mp %>% 
    group_by(poolsize) %>% summarise(n = n_distinct(poolno), .groups = "drop") %>%
    transmute(name = paste0("poolsize::", poolsize), n, what = "pools"),
  mp %>%
    group_by(poolpop) %>% summarise(n = sum(n), .groups = "drop") %>%
    transmute(name = paste0("poolpop::", poolpop), n, what = "people")
)

nodes <- nodes %>%
  left_join(node_counts, by = "name") %>%
  mutate(n = replace_na(n, 0L),
         what = replace_na(what, ""),
         label = paste0(label, " (", n, " ", what, ")")) %>%
  select(-what)


# --- Links ---
links12 <- mp %>%
  group_by(Country_Name, poolsize) %>%
  summarise(value = sum(n), .groups = "drop") %>%
  transmute(source = paste0("country::", Country_Name),
            target = paste0("poolsize::", poolsize),
            value)

links23 <- mp %>%
  group_by(poolsize, poolpop) %>%
  summarise(value = sum(n), .groups = "drop") %>%
  transmute(source = paste0("poolsize::", poolsize),
            target = paste0("poolpop::", poolpop),
            value)

links <- bind_rows(links12, links23) %>% filter(value > 0)

# Attach node IDs + groups
nodes_pub <- nodes %>%
  mutate(group = case_when(
    str_starts(name, "country::")  ~ "country",
    str_starts(name, "poolsize::") ~ "poolsize",
    str_starts(name, "poolpop::")  ~ "poolpop",
    TRUE ~ "other"
  ))

links_pub <- links %>%
  mutate(group = case_when(
    str_starts(source, "country::")  ~ "country",
    str_starts(target, "poolpop::")  ~ str_remove(target, "^poolpop::"),
    TRUE ~ "poolsize"
  )) %>%
  left_join(select(nodes_pub, name, id), by = c("source" = "name")) %>%
  rename(source_id = id) %>%
  left_join(select(nodes_pub, name, id), by = c("target" = "name")) %>%
  rename(target_id = id) %>%
  transmute(source = source_id, target = target_id, value, group)

# --- Palette ---
pal <- c(
  country  = "#2F4A6D",
  poolsize = "#86A6C9",
  poolpop  = "#765C8D",
  "PHC"                   = "#2E8B57",
  "ACF"                   = "#5DA5DA",
  "District"              = "#C8A15E",
  "Children"              = "#9E9E9E",
  "PHC+District"          = "#B69AC7",
  "PHC+Children"          = "#D17A5C",
  "District+Children"     = "#A0CBE8",
  "PHC+District+Children" = "#C05A93",
  other    = "#BDBDBD"
)

colourScale <- htmlwidgets::JS(
  sprintf(
    "d3.scaleOrdinal().domain(%s).range(%s)",
    jsonlite::toJSON(names(pal)),
    jsonlite::toJSON(unname(pal))
  )
)

# --- Sankey ---
s <- sankeyNetwork(
  Links = links_pub,
  Nodes = select(nodes_pub, name, label, group),
  Source = "source",
  Target = "target",
  Value  = "value",
  NodeID = "label",
  NodeGroup = "group",
  LinkGroup = "group",
  colourScale = colourScale,
  fontSize = 13,
  nodeWidth = 20,
  nodePadding = 10,
  sinksRight = FALSE,
  margin = list(top = 30, right = 20, bottom = 20, left = 20),
  iterations = 0
)

# style
s <- htmlwidgets::onRender(
  s,
  "
  function(el, x){
    const svg = d3.select(el).select('svg').style('background-color','white');
    svg.selectAll('.link').style('stroke-opacity', 0.35);
    svg.selectAll('.node text').style('font-family','Helvetica, Arial, sans-serif');

    const rectW = +(svg.select('.node rect').attr('width') || 22);
    let xs = [];
    svg.selectAll('.node').each(function(){
      const m = d3.select(this).attr('transform').match(/translate\\(([-0-9.]+),/);
      if (m) xs.push(+m[1]);
    });
    xs = Array.from(new Set(xs)).sort((a,b)=>a-b);

    const titles = ['Country','Pool size','Pool population'];
    xs.forEach((cx,i) => {
      if (i < titles.length) {
        svg.append('text')
          .attr('x', cx + rectW/2)
          .attr('y', 16)
          .attr('text-anchor','middle')
          .text(titles[i])
          .style('font-weight', 600)
          .style('font-size', '12px')
          .style('font-family','Helvetica, Arial, sans-serif');
      }
    });
  }
  "
)

s

# --- Export ---
fs::dir_create(here("Outputs","Viz_sankey","pool population"))
htmlwidgets::saveWidget(s, here("Outputs","Viz_sankey","pool population","sankey_poolpop.html"), selfcontained = TRUE)
webshot2::webshot(here("Outputs","Viz_sankey","pool population","sankey_poolpop.html"),
                  file = here("Outputs","Viz_sankey","pool population","sankey_poolpop.png"),
                  vwidth = 2000, vheight = 900, zoom = 1)
webshot2::webshot(here("Outputs","Viz_sankey","pool population","sankey_poolpop.html"),
                  file = here("Outputs","Viz_sankey","pool population","sankey_poolpop.pdf"),
                  vwidth = 1600, vheight = 900, zoom = 1)
