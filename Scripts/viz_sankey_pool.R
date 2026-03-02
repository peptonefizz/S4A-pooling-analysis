pacman::p_load(tidyverse, here, networkD3, htmlwidgets, jsonlite)
pacman::p_load(webshot2,fs)

# pal <- c(
#   country  = "#4E79A7",
#   poolsize = "#A0CBE8",
#   xpert_p1 = "#B07AA1",
#   xpert_p2 = "#D4A6C8",
#   pos      = "#009E73",
#   trace    = "#E69F00",
#   neg      = "#56B4E9",
#   invalid  = "#7F7F7F",
#   error    = "#CC79A7",
#   noresult = "#D55E00",
#   other    = "#CCCCCC"
# )


mp <- readRDS(here("Outputs","Intermediate outputs","pool_summary.rds"))

lvl_pool <- c(4L, 3L, 2L) 
lvl_p1 <- c("pos","trace","neg","invalid","error","noresult") # pos then trace 
lvl_p2 <- c("pos","trace","neg","invalid","noresult") # drop 'error' (no values) 
lab_map <- c( pos = "Positive", trace = "Trace", neg = "Negative", invalid = "Invalid", error = "Error", noresult = "No result" )

mp <- mp %>% 
  filter(!is.na(Country_Name), !is.na(poolsize), !is.na(xpert_p1_mtb)) %>% 
  mutate( poolsize = factor(poolsize, levels = lvl_pool), 
          xpert_p1_mtb = factor(xpert_p1_mtb, levels = lvl_p1), # set levels without 'error' to drop hanging xpert_p2:error 
          xpert_p2_mtb = factor(xpert_p2_mtb, levels = lvl_p2) )

# order countries high→low (top to bottom)
country_names <- mp %>%
  group_by(Country_Name) %>%
  summarise(n = sum(n), .groups = "drop") %>%
  arrange(desc(n)) %>%
  pull(Country_Name)


nodes <- tibble(
  name = c(
    paste0("country::", country_names),
    paste0("poolsize::", lvl_pool),
    paste0("xpert_p1::", lvl_p1),
    paste0("xpert_p2::", lvl_p2)
  )
) %>%
  distinct() %>%
  mutate(
    id         = row_number() - 1,
    label_base = case_when(
      str_starts(name, "country::")   ~ str_remove(name, "^country::"),
      str_starts(name, "poolsize::")  ~ str_remove(name, "^poolsize::"),
      str_starts(name, "xpert_p1::")  ~ lab_map[str_remove(name, "^xpert_p1::")] %>% unname(),
      str_starts(name, "xpert_p2::")  ~ lab_map[str_remove(name, "^xpert_p2::")] %>% unname(),
      TRUE ~ name
    )
  )

# --- Counts for all nodes; add (n) to labels ---
node_counts <- bind_rows(
  mp %>% group_by(Country_Name) %>% summarise(n = sum(n), .groups = "drop") %>%
    transmute(name = paste0("country::", Country_Name), n),
  mp %>% group_by(poolsize) %>% summarise(n = sum(n), .groups = "drop") %>%
    transmute(name = paste0("poolsize::", poolsize), n),
  mp %>% group_by(xpert_p1_mtb) %>% summarise(n = sum(n), .groups = "drop") %>%
    transmute(name = paste0("xpert_p1::", xpert_p1_mtb), n),
  mp %>% filter(!is.na(xpert_p2_mtb)) %>% group_by(xpert_p2_mtb) %>%
    summarise(n = sum(n), .groups = "drop") %>%
    transmute(name = paste0("xpert_p2::", xpert_p2_mtb), n)
)

nodes <- nodes %>%
  left_join(node_counts, by = "name") %>%
  mutate(n = replace_na(n, 0L),
         label = paste0(label_base, " (", n, ")")) %>%
  select(-label_base)

# --- links ---
links12 <- mp %>%
  group_by(Country_Name, poolsize) %>%
  summarise(value = sum(n), .groups = "drop") %>%
  transmute(
    source = paste0("country::", Country_Name),
    target = paste0("poolsize::", poolsize),
    value
  )

links23 <- mp %>%
  group_by(poolsize, xpert_p1_mtb) %>%
  summarise(value = sum(n), .groups = "drop") %>%
  transmute(
    source = paste0("poolsize::", poolsize),
    target = paste0("xpert_p1::", xpert_p1_mtb),
    value
  )

links34 <- mp %>%
  filter(!is.na(xpert_p2_mtb)) %>%
  group_by(xpert_p1_mtb, xpert_p2_mtb) %>%
  summarise(value = sum(n), .groups = "drop") %>%
  transmute(
    source = paste0("xpert_p1::", xpert_p1_mtb),
    target = paste0("xpert_p2::", xpert_p2_mtb),
    value
  )

links <- bind_rows(links12, links23, links34) %>%
  filter(value > 0)

links_d3 <- links %>%
  left_join(select(nodes, name, id), by = c("source" = "name")) %>%
  rename(source_id = id) %>%
  left_join(select(nodes, name, id), by = c("target" = "name")) %>%
  rename(target_id = id) %>%
  transmute(source = source_id, target = target_id, value)

nodes_pub <- nodes %>%
  mutate(group = case_when(
    str_starts(name, "country::")  ~ "country",
    str_starts(name, "poolsize::") ~ "poolsize",
    str_starts(name, "xpert_p1::") ~ "xpert_p1",
    str_starts(name, "xpert_p2::") ~ "xpert_p2",
    TRUE ~ "other"
  ))

links_pub <- links %>%
  mutate(group = case_when(
    str_starts(source, "country::")  ~ "country",
    str_starts(target, "xpert_p1::") ~ str_remove(target, "^xpert_p1::"),
    str_starts(target, "xpert_p2::") ~ str_remove(target, "^xpert_p2::"),
    TRUE ~ "poolsize"
  )) %>%
  left_join(select(nodes_pub, name, id), by = c("source" = "name")) %>%
  rename(source_id = id) %>%
  left_join(select(nodes_pub, name, id), by = c("target" = "name")) %>%
  rename(target_id = id) %>%
  transmute(source = source_id, target = target_id, value, group)

# --- Palette + colourScale ---
pal <- c(
  country  = "#2F4A6D",
  poolsize = "#86A6C9",
  xpert_p1 = "#765C8D",
  xpert_p2 = "#B69AC7",
  pos      = "#2E8B57",
  trace    = "#C8A15E",
  neg      = "#5DA5DA",
  invalid  = "#9E9E9E",
  error    = "#C05A93",
  noresult = "#D17A5C",
  other    = "#BDBDBD"
)

colourScale <- htmlwidgets::JS(
  sprintf(
    "d3.scaleOrdinal().domain(%s).range(%s)",
    jsonlite::toJSON(names(pal)),
    jsonlite::toJSON(unname(pal))
  )
)

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
  sinksRight = F,
  margin = list(top = 30, right = 20, bottom = 20, left = 20), 
  iterations = 0
)

# --- Styling ---
s <- htmlwidgets::onRender(
  s,
  "
  function(el, x){
    const svg = d3.select(el).select('svg')
      .style('background-color','white');

    svg.selectAll('.link').style('stroke-opacity', 0.35);
    svg.selectAll('.node text').style('font-family','Helvetica, Arial, sans-serif');

    const rectW = +(svg.select('.node rect').attr('width') || 22);
    let xs = [];
    svg.selectAll('.node').each(function(){
      const m = d3.select(this).attr('transform').match(/translate\\(([-0-9.]+),/);
      if (m) xs.push(+m[1]);
    });
    xs = Array.from(new Set(xs)).sort((a,b)=>a-b);

    const titles = ['Pool size','Pooled Ultra','Repeat pooled Ultra'];
    xs.slice(1,4).forEach((cx,i) => {
      svg.append('text')
        .attr('x', cx + rectW/2)
        .attr('y', 16)
        .attr('text-anchor','middle')
        .text(titles[i])
        .style('font-weight', 600)
        .style('font-size', '12px')
        .style('font-family','Helvetica, Arial, sans-serif');
    });
  }
  "
)

s

# --- Export ---

fs::dir_create(here("Outputs","Viz_sankey","pool distribution"))
htmlwidgets::saveWidget(s, here("Outputs","Viz_sankey","pool distribution","sankey pool distribution.html"), selfcontained = TRUE)
webshot2::webshot(here("Outputs","Viz_sankey","pool distribution","sankey pool distribution.html"), file = here("Outputs","Viz_sankey","pool distribution","sankey pool distribution.png"), vwidth = 2000, vheight = 900, zoom = 1)
webshot2::webshot(here("Outputs","Viz_sankey","pool distribution","sankey pool distribution.html"), file = here("Outputs","Viz_sankey","pool distribution","sankey pool distribution.pdf"), vwidth = 1600, vheight = 900, zoom = 1)

