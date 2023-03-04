# Introduction and aim

I don’t have color vision deficiency (CVD, commonly called colour
blindness), but I have a persistent interest in choosing CVD-safe
colours that comes from my work as a scientist (because we show data in
graphs) and as a person who teaches R sometimes (because syntax
highlighters show code elements in colour). To support the latter, I
made [a colourblind-friendly theme called Pebble-Safe for
RStudio](https://github.com/DesiQuintans/Pebble-safe) and I am
continuing to [make it easier for people to make their own
themes](https://github.com/DesiQuintans/RStudioThemeTemplate) to suit
their needs.

In making the Pebble-Safe theme, I documented a method of selecting
CVD-safe colours that amounted to *“Shuffle them in a grid and if two
colours look similar beside each other, delete one of them.”* This is
fine and even necessary as a final step, but it definitely shouldn’t be
the *first* step since it’s so effortful.

This time I’m going to use similarity and network analysis methods to
pre-select distant colours.

## Packages used

``` r
# remotes::install_github("DesiQuintans/librarian")
librarian::shelf(tidyverse, gt,
                 DesiQuintans/desiderata,  # Has the data for this analysis
                 qgraph, igraph,
                 spacesXYZ, khroma)  # CVD colour conversion and comparison
```

    ## 
    ##   The 'cran_repo' argument in shelf() was not set, so it will use
    ##   cran_repo = 'https://cran.r-project.org' by default.
    ## 
    ##   To avoid this message, set the 'cran_repo' argument to a CRAN
    ##   mirror URL (see https://cran.r-project.org/mirrors.html) or set
    ##   'quiet = TRUE'.

# Dataset

I’ll be using a dataset from my personal package, which contains
Google’s Material Design 2014 colour palette together with simulated CVD
conversions of those colours that I generated using
`khroma:::anomalize()` (a private function inside that package).

``` r
show_colours(material2014_colblind$normal, n = 14)
```

![](README_files/figure-gfm/unnamed-chunk-1-1.png)<!-- -->

``` r
show_colours(material2014_colblind$deutan, n = 14)
```

![](README_files/figure-gfm/unnamed-chunk-1-2.png)<!-- -->

``` r
show_colours(material2014_colblind$protan, n = 14)
```

![](README_files/figure-gfm/unnamed-chunk-1-3.png)<!-- -->

``` r
show_colours(material2014_colblind$tritan, n = 14)
```

![](README_files/figure-gfm/unnamed-chunk-1-4.png)<!-- -->

``` r
show_colours(material2014_colblind$achrom, n = 14)
```

![](README_files/figure-gfm/unnamed-chunk-1-5.png)<!-- -->

``` r
mat2014 <- 
    material2014_colblind %>%   # Shorter name
    filter(not.na(name) == TRUE)

glimpse(mat2014)
```

    ## Rows: 256
    ## Columns: 6
    ## $ name   <chr> "red_50", "red_100", "red_200", "red_300", "red_400", "red_500"…
    ## $ normal <chr> "#FFEBEE", "#FFCDD2", "#EF9A9A", "#E57373", "#EF5350", "#F44336…
    ## $ deutan <chr> "#F1F1ED", "#DFDFD0", "#BBBB96", "#A4A46D", "#9E9E44", "#9C9C1E…
    ## $ protan <chr> "#EEEEED", "#D6D6D1", "#ACAC99", "#8F8F72", "#80804E", "#7B7B32…
    ## $ tritan <chr> "#FEEBEB", "#FECDCD", "#EE9A99", "#E47372", "#EF5252", "#F44141…
    ## $ achrom <chr> "#EDEDED", "#D2D2D2", "#9C9C9C", "#767676", "#565656", "#414141…

# Choosing distant colours based on their colour difference

`khroma::compare()` lets me calculate colour distances using the CIELAB
[distance metric recommended in
2000](https://en.wikipedia.org/wiki/Color_difference#CIEDE2000), which
is a wild thing to behold.

``` r
#' Produce a pairwise distance matrix of colours 
#'
#' @param vec (Character) A vector of colours.
#' @param colour_names (Character) Human-readable names of those colours.
#'
#' @return A distance matrix.
#' @export
#'
#' @examples
#' colour_distance(c("#FFFFFF", "#FF0000", "#0000FF"), c("white", "red", "blue"))
colour_distance <- function(vec, colour_names) {
    col_dist <- 
        vec %>% 
        khroma::compare(metric = 2000) %>%   
        as.matrix()
    
    rownames(col_dist) <- colour_names
    colnames(col_dist) <- colour_names
    
    col_dist
}

colour_distance(mat2014$normal[1:4], mat2014$name[1:4])
```

    ##           red_50  red_100   red_200   red_300
    ## red_50   0.00000 10.17361 21.702152 30.098358
    ## red_100 10.17361  0.00000 12.913725 22.343886
    ## red_200 21.70215 12.91373  0.000000  9.718756
    ## red_300 30.09836 22.34389  9.718756  0.000000

I can use these pairwise distances to find which colours are least
similar, and therefore good candidates for high-difference pairings like
syntax highlighting.

``` r
#' From a colour distance matrix, which colour is most distant?
#'
#' @param mat (Dist) A distance matrix.
#' @param colour_names (Character) Human-readable colour names that will be
#'      used to label the rows and columns of `mat`.
#'
#' @return A character vector.
#' @export
#'
#' @examples
#' c_mat <- colour_distance(c("#FFFFFF", "#FF0000", "#0000FF"), c("white", "red", "blue"))
#' furthest_colour(c_mat)
furthest_colour <- function(mat) {
    colour_names <- rownames(mat)
    
    data.frame(name = colour_names,
               furthest = colour_names[max.col(mat, ties.method = "first")])
}

x <- colour_distance(mat2014$normal[12:16], mat2014$name[12:16])

furthest_colour(x)
```

    ##       name furthest
    ## 1 red_a200  pink_50
    ## 2 red_a400  pink_50
    ## 3 red_a700  pink_50
    ## 4  pink_50 red_a700
    ## 5 pink_100 red_a700

## Does it work?

Not really, because it seems to pick the same distant colours
repeatedly.

``` r
dist_normal <- colour_distance(mat2014$normal, mat2014$name)

furthest_normal <- furthest_colour(dist_normal)

sample_n(furthest_normal, 10)
```

    ##                name         furthest
    ## 1  light_green_a700      purple_a700
    ## 2   deep_purple_300        lime_a400
    ## 3       yellow_a200  deep_purple_900
    ## 4        orange_800       green_a400
    ## 5             white            black
    ## 6          teal_100            black
    ## 7  light_green_a400 deep_purple_a700
    ## 8          cyan_100            black
    ## 9           cyan_50            black
    ## 10          blue_50            black

``` r
count_unique(furthest_normal$furthest) %>% arrange(desc(count))
```

    ##              unique count
    ## 1             black    66
    ## 2  light_green_a400    49
    ## 3   deep_purple_900    33
    ## 4       yellow_a200    29
    ## 5         lime_a400    16
    ## 6  deep_purple_a700    12
    ## 7        green_a400     7
    ## 8          pink_900     7
    ## 9       purple_a400     7
    ## 10        lime_a200     6
    ## 11        pink_a400     5
    ## 12        pink_a200     4
    ## 13       indigo_900     3
    ## 14         red_a400     3
    ## 15        cyan_a400     1
    ## 16         pink_600     1
    ## 17         pink_700     1
    ## 18        pink_a100     1
    ## 19        pink_a700     1
    ## 20       purple_900     1
    ## 21      purple_a200     1
    ## 22      purple_a700     1
    ## 23          red_100     1

# Choosing distant colours by graphing

If we want to get the most out of actually *choosing* colours, then
we’re going to have to *look at* them at some point. As I said in the
introduction, my past work involved a lot of shuffling colour swatches
and comparing them side-by-side. Now that I have computed colour
distances, maybe I can make a graph that does most of that work?

``` r
similarity_graph <- function(vec, threshold = 0, file = NULL, 
                             width = 3000, height = 3000, title) {
    mat_dist <- colour_distance(vec, 1:length(vec))
    
    # mat from colour_distance() is a pairwise difference/distance matrix.
    # However, qgraph uses weights as measures of closeness (similarity),
    # not distance. So we need to convert from distance to similarity.
    # https://stats.stackexchange.com/a/158285
    
    colour_similarity <- 1 / (1 + mat_dist)
    
    # I may want to remove a lot of weaker edges to make the major correlations
    # more visible.
    similarity_threshold <- quantile(mat_dist, threshold)
    
    my_graph <- 
        qgraph(colour_similarity, layout = "spring",
               shape = "square", vsize = 2, color = vec,
               # Labelled with the row number of the colour in df.
               label.scale = TRUE, label.scale.equal = TRUE,
               edge.color = "#000000", threshold = similarity_threshold,
               title = title
               )
    
    
    if (is.null(file) == FALSE) {
        png(file, width = width, height = height, unit = "px")
        plot(my_graph)
        dev.off()
        message("Wrote to ", file)
    }

    return(list(graph = my_graph, 
                dist = colour_distance, 
                sim = colour_similarity))
}
```

``` r
graph_title <- "Similarity of Material 2014 colours with normal colour vision."
q_normal <- similarity_graph(mat2014$normal, file = "q_normal.png", 
                             title = graph_title)
```

    ## Wrote to q_normal.png

![](q_normal.png)

(Right-Click → View these to see them at full resolution. Numbers correspond
to row number in the `mat2014` dataframe.)

``` r
graph_title <- "Similarity of Material 2014 colours with simulated deuteranopia."
q_deutan <- similarity_graph(mat2014$deutan, file = "q_deutan.png", 
                             title = graph_title)
```

    ## Wrote to q_deutan.png

![](q_deutan.png)

``` r
graph_title <- "Similarity of Material 2014 colours with simulated protanopia."
q_protan <- similarity_graph(mat2014$protan, file = "q_protan.png",
                             title = graph_title)
```

    ## Wrote to q_protan.png

![](q_protan.png)

``` r
graph_title <- "Similarity of Material 2014 colours with simulated tritanopia."
q_tritan <- similarity_graph(mat2014$tritan, file = "q_tritan.png",
                             title = graph_title)
```

    ## Wrote to q_tritan.png

![](q_tritan.png)

``` r
graph_title <- "Similarity of Material 2014 colours with simulated achromatopsia."
q_achrom <- similarity_graph(mat2014$achrom, file = "q_achrom.png",
                             title = graph_title)
```

    ## Wrote to q_achrom.png

![](q_achrom.png)

Very nice!

# Getting distances from graphs

I wonder if I can choose distant colours using their graph distance?

``` r
graph_distance <- function(qg, cvd, colour_names) {
    dist_table <- 
        distances(as.igraph(qg)) %>% 
        as.data.frame() %>%
        set_names(colour_names) %>% 
        mutate(name = colour_names, .before = everything()) %>% 
        rowwise() %>%
        nest(distances = c(-name)) %>% 
        mutate("{{cvd}}_furthest" := 
                   map_chr(distances, 
                           function(df) {
                               df %>% 
                                   pivot_longer(cols = everything(), 
                                                names_to = "name", values_to = "dist") %>% 
                                   arrange(desc(dist)) %>% 
                                   head(1) %>% 
                                   pull(name)
                           })
               ) %>% 
        select(-distances)
    
    return(dist_table)
}

furthest_normal <- graph_distance(q_normal$graph, normal, mat2014$name)
furthest_deutan <- graph_distance(q_deutan$graph, deutan, mat2014$name)
furthest_protan <- graph_distance(q_protan$graph, protan, mat2014$name)
furthest_tritan <- graph_distance(q_tritan$graph, tritan, mat2014$name)
furthest_achrom <- graph_distance(q_achrom$graph, achrom, mat2014$name)

dist_from_graph <- 
    bind_cols(
        furthest_normal,
        select(furthest_deutan, -name),
        select(furthest_protan, -name),
        select(furthest_tritan, -name),
        select(furthest_achrom, -name)
    ) %>% 
    glimpse()
```

    ## Rows: 256
    ## Columns: 6
    ## $ name            <chr> "red_50", "red_100", "red_200", "red_300", "red_400", …
    ## $ normal_furthest <chr> "brown_300", "brown_300", "brown_300", "gray_500", "br…
    ## $ deutan_furthest <chr> "blue_gray_400", "teal_400", "gray_600", "gray_600", "…
    ## $ protan_furthest <chr> "teal_500", "pink_a200", "gray_600", "gray_600", "gray…
    ## $ tritan_furthest <chr> "deep_purple_300", "deep_purple_300", "deep_purple_300…
    ## $ achrom_furthest <chr> "red_a100", "light_green_a200", "yellow_500", "pink_40…

``` r
sample_n(dist_from_graph, 10)
```

    ## # A tibble: 10 × 6
    ##    name             normal_furthest deutan_furthest protan_fur…¹ trita…² achro…³
    ##    <chr>            <chr>           <chr>           <chr>        <chr>   <chr>  
    ##  1 blue_gray_600    gray_500        gray_500        brown_300    deep_p… red_300
    ##  2 deep_purple_800  blue_gray_400   gray_600        gray_600     gray_6… amber_…
    ##  3 yellow_100       gray_500        brown_300       brown_300    deep_p… teal_6…
    ##  4 light_blue_a400  gray_500        gray_500        teal_400     blue_g… green_…
    ##  5 light_green_a100 gray_500        gray_500        brown_300    blue_g… teal_7…
    ##  6 amber_400        gray_500        brown_300       gray_500     deep_p… green_…
    ##  7 brown_300        gray_600        gray_600        pink_400     gray_6… amber_…
    ##  8 pink_800         gray_600        gray_600        teal_500     deep_p… red_a1…
    ##  9 orange_800       brown_300       gray_500        teal_600     lime_8… gray_6…
    ## 10 light_green_700  gray_600        gray_600        pink_a200    deep_p… brown_…
    ## # … with abbreviated variable names ¹​protan_furthest, ²​tritan_furthest,
    ## #   ³​achrom_furthest

It’s a more interesting list, at least?

``` r
mat2014_done <- 
    left_join(mat2014, dist_from_graph, by = "name")

gt(select(mat2014_done, name:achrom))
```

<div id="kvwvdstpdt" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>html {
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, 'Helvetica Neue', 'Fira Sans', 'Droid Sans', Arial, sans-serif;
}

#kvwvdstpdt .gt_table {
  display: table;
  border-collapse: collapse;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}

#kvwvdstpdt .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#kvwvdstpdt .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}

#kvwvdstpdt .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#kvwvdstpdt .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 0;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#kvwvdstpdt .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#kvwvdstpdt .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#kvwvdstpdt .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#kvwvdstpdt .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#kvwvdstpdt .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#kvwvdstpdt .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#kvwvdstpdt .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#kvwvdstpdt .gt_group_heading {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  text-align: left;
}

#kvwvdstpdt .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}

#kvwvdstpdt .gt_from_md > :first-child {
  margin-top: 0;
}

#kvwvdstpdt .gt_from_md > :last-child {
  margin-bottom: 0;
}

#kvwvdstpdt .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#kvwvdstpdt .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
}

#kvwvdstpdt .gt_stub_row_group {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
  vertical-align: top;
}

#kvwvdstpdt .gt_row_group_first td {
  border-top-width: 2px;
}

#kvwvdstpdt .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#kvwvdstpdt .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}

#kvwvdstpdt .gt_first_summary_row.thick {
  border-top-width: 2px;
}

#kvwvdstpdt .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#kvwvdstpdt .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#kvwvdstpdt .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#kvwvdstpdt .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#kvwvdstpdt .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#kvwvdstpdt .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#kvwvdstpdt .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-left: 4px;
  padding-right: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#kvwvdstpdt .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#kvwvdstpdt .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#kvwvdstpdt .gt_left {
  text-align: left;
}

#kvwvdstpdt .gt_center {
  text-align: center;
}

#kvwvdstpdt .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#kvwvdstpdt .gt_font_normal {
  font-weight: normal;
}

#kvwvdstpdt .gt_font_bold {
  font-weight: bold;
}

#kvwvdstpdt .gt_font_italic {
  font-style: italic;
}

#kvwvdstpdt .gt_super {
  font-size: 65%;
}

#kvwvdstpdt .gt_footnote_marks {
  font-style: italic;
  font-weight: normal;
  font-size: 75%;
  vertical-align: 0.4em;
}

#kvwvdstpdt .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}

#kvwvdstpdt .gt_indent_1 {
  text-indent: 5px;
}

#kvwvdstpdt .gt_indent_2 {
  text-indent: 10px;
}

#kvwvdstpdt .gt_indent_3 {
  text-indent: 15px;
}

#kvwvdstpdt .gt_indent_4 {
  text-indent: 20px;
}

#kvwvdstpdt .gt_indent_5 {
  text-indent: 25px;
}
</style>
<table class="gt_table">
  
  <thead class="gt_col_headings">
    <tr>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="name">name</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="normal">normal</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="deutan">deutan</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="protan">protan</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="tritan">tritan</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="achrom">achrom</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td headers="name" class="gt_row gt_left">red_50</td>
<td headers="normal" class="gt_row gt_left">#FFEBEE</td>
<td headers="deutan" class="gt_row gt_left">#F1F1ED</td>
<td headers="protan" class="gt_row gt_left">#EEEEED</td>
<td headers="tritan" class="gt_row gt_left">#FEEBEB</td>
<td headers="achrom" class="gt_row gt_left">#EDEDED</td></tr>
    <tr><td headers="name" class="gt_row gt_left">red_100</td>
<td headers="normal" class="gt_row gt_left">#FFCDD2</td>
<td headers="deutan" class="gt_row gt_left">#DFDFD0</td>
<td headers="protan" class="gt_row gt_left">#D6D6D1</td>
<td headers="tritan" class="gt_row gt_left">#FECDCD</td>
<td headers="achrom" class="gt_row gt_left">#D2D2D2</td></tr>
    <tr><td headers="name" class="gt_row gt_left">red_200</td>
<td headers="normal" class="gt_row gt_left">#EF9A9A</td>
<td headers="deutan" class="gt_row gt_left">#BBBB96</td>
<td headers="protan" class="gt_row gt_left">#ACAC99</td>
<td headers="tritan" class="gt_row gt_left">#EE9A99</td>
<td headers="achrom" class="gt_row gt_left">#9C9C9C</td></tr>
    <tr><td headers="name" class="gt_row gt_left">red_300</td>
<td headers="normal" class="gt_row gt_left">#E57373</td>
<td headers="deutan" class="gt_row gt_left">#A4A46D</td>
<td headers="protan" class="gt_row gt_left">#8F8F72</td>
<td headers="tritan" class="gt_row gt_left">#E47372</td>
<td headers="achrom" class="gt_row gt_left">#767676</td></tr>
    <tr><td headers="name" class="gt_row gt_left">red_400</td>
<td headers="normal" class="gt_row gt_left">#EF5350</td>
<td headers="deutan" class="gt_row gt_left">#9E9E44</td>
<td headers="protan" class="gt_row gt_left">#80804E</td>
<td headers="tritan" class="gt_row gt_left">#EF5252</td>
<td headers="achrom" class="gt_row gt_left">#565656</td></tr>
    <tr><td headers="name" class="gt_row gt_left">red_500</td>
<td headers="normal" class="gt_row gt_left">#F44336</td>
<td headers="deutan" class="gt_row gt_left">#9C9C1E</td>
<td headers="protan" class="gt_row gt_left">#7B7B32</td>
<td headers="tritan" class="gt_row gt_left">#F44141</td>
<td headers="achrom" class="gt_row gt_left">#414141</td></tr>
    <tr><td headers="name" class="gt_row gt_left">red_600</td>
<td headers="normal" class="gt_row gt_left">#E53935</td>
<td headers="deutan" class="gt_row gt_left">#919120</td>
<td headers="protan" class="gt_row gt_left">#717132</td>
<td headers="tritan" class="gt_row gt_left">#E53838</td>
<td headers="achrom" class="gt_row gt_left">#3E3E3E</td></tr>
    <tr><td headers="name" class="gt_row gt_left">red_700</td>
<td headers="normal" class="gt_row gt_left">#D32F2F</td>
<td headers="deutan" class="gt_row gt_left">#84841B</td>
<td headers="protan" class="gt_row gt_left">#66662C</td>
<td headers="tritan" class="gt_row gt_left">#D32E2F</td>
<td headers="achrom" class="gt_row gt_left">#373737</td></tr>
    <tr><td headers="name" class="gt_row gt_left">red_800</td>
<td headers="normal" class="gt_row gt_left">#C62828</td>
<td headers="deutan" class="gt_row gt_left">#7B7B12</td>
<td headers="protan" class="gt_row gt_left">#5E5E25</td>
<td headers="tritan" class="gt_row gt_left">#C62728</td>
<td headers="achrom" class="gt_row gt_left">#313131</td></tr>
    <tr><td headers="name" class="gt_row gt_left">red_900</td>
<td headers="normal" class="gt_row gt_left">#B71C1C</td>
<td headers="deutan" class="gt_row gt_left">#707000</td>
<td headers="protan" class="gt_row gt_left">#545418</td>
<td headers="tritan" class="gt_row gt_left">#B71B1C</td>
<td headers="achrom" class="gt_row gt_left">#262626</td></tr>
    <tr><td headers="name" class="gt_row gt_left">red_a100</td>
<td headers="normal" class="gt_row gt_left">#FF8A80</td>
<td headers="deutan" class="gt_row gt_left">#BBBB7A</td>
<td headers="protan" class="gt_row gt_left">#A6A67F</td>
<td headers="tritan" class="gt_row gt_left">#FF8888</td>
<td headers="achrom" class="gt_row gt_left">#848484</td></tr>
    <tr><td headers="name" class="gt_row gt_left">red_a200</td>
<td headers="normal" class="gt_row gt_left">#FF5252</td>
<td headers="deutan" class="gt_row gt_left">#A7A744</td>
<td headers="protan" class="gt_row gt_left">#868650</td>
<td headers="tritan" class="gt_row gt_left">#FF5152</td>
<td headers="achrom" class="gt_row gt_left">#595959</td></tr>
    <tr><td headers="name" class="gt_row gt_left">red_a400</td>
<td headers="normal" class="gt_row gt_left">#FF1744</td>
<td headers="deutan" class="gt_row gt_left">#9C9C30</td>
<td headers="protan" class="gt_row gt_left">#747441</td>
<td headers="tritan" class="gt_row gt_left">#FE2020</td>
<td headers="achrom" class="gt_row gt_left">#4A4A4A</td></tr>
    <tr><td headers="name" class="gt_row gt_left">red_a700</td>
<td headers="normal" class="gt_row gt_left">#D50000</td>
<td headers="deutan" class="gt_row gt_left">#818100</td>
<td headers="protan" class="gt_row gt_left">#5E5E00</td>
<td headers="tritan" class="gt_row gt_left">#D50000</td>
<td headers="achrom" class="gt_row gt_left">#1C1C1C</td></tr>
    <tr><td headers="name" class="gt_row gt_left">pink_50</td>
<td headers="normal" class="gt_row gt_left">#FCE4EC</td>
<td headers="deutan" class="gt_row gt_left">#ECECEB</td>
<td headers="protan" class="gt_row gt_left">#E8E8EB</td>
<td headers="tritan" class="gt_row gt_left">#FBE5E5</td>
<td headers="achrom" class="gt_row gt_left">#EBEBEB</td></tr>
    <tr><td headers="name" class="gt_row gt_left">pink_100</td>
<td headers="normal" class="gt_row gt_left">#F8BBD0</td>
<td headers="deutan" class="gt_row gt_left">#D1D1CE</td>
<td headers="protan" class="gt_row gt_left">#C7C7CF</td>
<td headers="tritan" class="gt_row gt_left">#F6BDBD</td>
<td headers="achrom" class="gt_row gt_left">#CECECE</td></tr>
    <tr><td headers="name" class="gt_row gt_left">pink_200</td>
<td headers="normal" class="gt_row gt_left">#F48FB1</td>
<td headers="deutan" class="gt_row gt_left">#B8B8AD</td>
<td headers="protan" class="gt_row gt_left">#A6A6B0</td>
<td headers="tritan" class="gt_row gt_left">#F19393</td>
<td headers="achrom" class="gt_row gt_left">#AFAFAF</td></tr>
    <tr><td headers="name" class="gt_row gt_left">pink_300</td>
<td headers="normal" class="gt_row gt_left">#F06292</td>
<td headers="deutan" class="gt_row gt_left">#A3A38D</td>
<td headers="protan" class="gt_row gt_left">#888891</td>
<td headers="tritan" class="gt_row gt_left">#ED6969</td>
<td headers="achrom" class="gt_row gt_left">#909090</td></tr>
    <tr><td headers="name" class="gt_row gt_left">pink_400</td>
<td headers="normal" class="gt_row gt_left">#EC407A</td>
<td headers="deutan" class="gt_row gt_left">#979773</td>
<td headers="protan" class="gt_row gt_left">#777778</td>
<td headers="tritan" class="gt_row gt_left">#E94A4A</td>
<td headers="achrom" class="gt_row gt_left">#787878</td></tr>
    <tr><td headers="name" class="gt_row gt_left">pink_500</td>
<td headers="normal" class="gt_row gt_left">#E91E63</td>
<td headers="deutan" class="gt_row gt_left">#8F8F5A</td>
<td headers="protan" class="gt_row gt_left">#6B6B61</td>
<td headers="tritan" class="gt_row gt_left">#E72D2D</td>
<td headers="achrom" class="gt_row gt_left">#626263</td></tr>
    <tr><td headers="name" class="gt_row gt_left">pink_600</td>
<td headers="normal" class="gt_row gt_left">#D81B60</td>
<td headers="deutan" class="gt_row gt_left">#848458</td>
<td headers="protan" class="gt_row gt_left">#63635E</td>
<td headers="tritan" class="gt_row gt_left">#D62B2B</td>
<td headers="achrom" class="gt_row gt_left">#5F5F5F</td></tr>
    <tr><td headers="name" class="gt_row gt_left">pink_700</td>
<td headers="normal" class="gt_row gt_left">#C2185B</td>
<td headers="deutan" class="gt_row gt_left">#777754</td>
<td headers="protan" class="gt_row gt_left">#585859</td>
<td headers="tritan" class="gt_row gt_left">#C02727</td>
<td headers="achrom" class="gt_row gt_left">#595959</td></tr>
    <tr><td headers="name" class="gt_row gt_left">pink_800</td>
<td headers="normal" class="gt_row gt_left">#AD1457</td>
<td headers="deutan" class="gt_row gt_left">#696951</td>
<td headers="protan" class="gt_row gt_left">#4E4E56</td>
<td headers="tritan" class="gt_row gt_left">#AA2424</td>
<td headers="achrom" class="gt_row gt_left">#555555</td></tr>
    <tr><td headers="name" class="gt_row gt_left">pink_900</td>
<td headers="normal" class="gt_row gt_left">#880E4F</td>
<td headers="deutan" class="gt_row gt_left">#51514B</td>
<td headers="protan" class="gt_row gt_left">#3C3C4E</td>
<td headers="tritan" class="gt_row gt_left">#851E1E</td>
<td headers="achrom" class="gt_row gt_left">#4C4C4C</td></tr>
    <tr><td headers="name" class="gt_row gt_left">pink_a100</td>
<td headers="normal" class="gt_row gt_left">#FF80AB</td>
<td headers="deutan" class="gt_row gt_left">#B7B7A6</td>
<td headers="protan" class="gt_row gt_left">#9F9FAA</td>
<td headers="tritan" class="gt_row gt_left">#FC8686</td>
<td headers="achrom" class="gt_row gt_left">#A8A8A9</td></tr>
    <tr><td headers="name" class="gt_row gt_left">pink_a200</td>
<td headers="normal" class="gt_row gt_left">#FF4081</td>
<td headers="deutan" class="gt_row gt_left">#A2A279</td>
<td headers="protan" class="gt_row gt_left">#7F7F7F</td>
<td headers="tritan" class="gt_row gt_left">#FC4C4C</td>
<td headers="achrom" class="gt_row gt_left">#7F7F7F</td></tr>
    <tr><td headers="name" class="gt_row gt_left">pink_a400</td>
<td headers="normal" class="gt_row gt_left">#F50057</td>
<td headers="deutan" class="gt_row gt_left">#95954A</td>
<td headers="protan" class="gt_row gt_left">#6D6D55</td>
<td headers="tritan" class="gt_row gt_left">#F31C1C</td>
<td headers="achrom" class="gt_row gt_left">#585858</td></tr>
    <tr><td headers="name" class="gt_row gt_left">pink_a700</td>
<td headers="normal" class="gt_row gt_left">#C51162</td>
<td headers="deutan" class="gt_row gt_left">#78785B</td>
<td headers="protan" class="gt_row gt_left">#585861</td>
<td headers="tritan" class="gt_row gt_left">#C22727</td>
<td headers="achrom" class="gt_row gt_left">#606060</td></tr>
    <tr><td headers="name" class="gt_row gt_left">purple_50</td>
<td headers="normal" class="gt_row gt_left">#F3E5F5</td>
<td headers="deutan" class="gt_row gt_left">#E9E9F4</td>
<td headers="protan" class="gt_row gt_left">#E7E7F4</td>
<td headers="tritan" class="gt_row gt_left">#F1E7E7</td>
<td headers="achrom" class="gt_row gt_left">#F3F3F3</td></tr>
    <tr><td headers="name" class="gt_row gt_left">purple_100</td>
<td headers="normal" class="gt_row gt_left">#E1BEE7</td>
<td headers="deutan" class="gt_row gt_left">#CACAE6</td>
<td headers="protan" class="gt_row gt_left">#C4C4E6</td>
<td headers="tritan" class="gt_row gt_left">#DCC3C3</td>
<td headers="achrom" class="gt_row gt_left">#E2E2E2</td></tr>
    <tr><td headers="name" class="gt_row gt_left">purple_200</td>
<td headers="normal" class="gt_row gt_left">#CE93D8</td>
<td headers="deutan" class="gt_row gt_left">#A9A9D6</td>
<td headers="protan" class="gt_row gt_left">#9F9FD7</td>
<td headers="tritan" class="gt_row gt_left">#C69D9D</td>
<td headers="achrom" class="gt_row gt_left">#D1D1D1</td></tr>
    <tr><td headers="name" class="gt_row gt_left">purple_300</td>
<td headers="normal" class="gt_row gt_left">#BA68C8</td>
<td headers="deutan" class="gt_row gt_left">#8A8AC6</td>
<td headers="protan" class="gt_row gt_left">#7B7BC7</td>
<td headers="tritan" class="gt_row gt_left">#B07979</td>
<td headers="achrom" class="gt_row gt_left">#C0C0C0</td></tr>
    <tr><td headers="name" class="gt_row gt_left">purple_400</td>
<td headers="normal" class="gt_row gt_left">#AB47BC</td>
<td headers="deutan" class="gt_row gt_left">#7474BA</td>
<td headers="protan" class="gt_row gt_left">#6161BB</td>
<td headers="tritan" class="gt_row gt_left">#9F6060</td>
<td headers="achrom" class="gt_row gt_left">#B3B3B3</td></tr>
    <tr><td headers="name" class="gt_row gt_left">purple_500</td>
<td headers="normal" class="gt_row gt_left">#9C27B0</td>
<td headers="deutan" class="gt_row gt_left">#6262AE</td>
<td headers="protan" class="gt_row gt_left">#4C4CAF</td>
<td headers="tritan" class="gt_row gt_left">#904C4C</td>
<td headers="achrom" class="gt_row gt_left">#A7A7A7</td></tr>
    <tr><td headers="name" class="gt_row gt_left">purple_600</td>
<td headers="normal" class="gt_row gt_left">#8E24AA</td>
<td headers="deutan" class="gt_row gt_left">#5959A8</td>
<td headers="protan" class="gt_row gt_left">#4545A9</td>
<td headers="tritan" class="gt_row gt_left">#814848</td>
<td headers="achrom" class="gt_row gt_left">#A1A1A1</td></tr>
    <tr><td headers="name" class="gt_row gt_left">purple_700</td>
<td headers="normal" class="gt_row gt_left">#7B1FA2</td>
<td headers="deutan" class="gt_row gt_left">#4D4DA0</td>
<td headers="protan" class="gt_row gt_left">#3B3BA1</td>
<td headers="tritan" class="gt_row gt_left">#6D4343</td>
<td headers="achrom" class="gt_row gt_left">#999999</td></tr>
    <tr><td headers="name" class="gt_row gt_left">purple_800</td>
<td headers="normal" class="gt_row gt_left">#6A1B9A</td>
<td headers="deutan" class="gt_row gt_left">#424299</td>
<td headers="protan" class="gt_row gt_left">#333399</td>
<td headers="tritan" class="gt_row gt_left">#5A3F3F</td>
<td headers="achrom" class="gt_row gt_left">#919191</td></tr>
    <tr><td headers="name" class="gt_row gt_left">purple_900</td>
<td headers="normal" class="gt_row gt_left">#4A148C</td>
<td headers="deutan" class="gt_row gt_left">#2E2E8B</td>
<td headers="protan" class="gt_row gt_left">#23238B</td>
<td headers="tritan" class="gt_row gt_left">#353737</td>
<td headers="achrom" class="gt_row gt_left">#848484</td></tr>
    <tr><td headers="name" class="gt_row gt_left">purple_a100</td>
<td headers="normal" class="gt_row gt_left">#EA80FC</td>
<td headers="deutan" class="gt_row gt_left">#ACACFA</td>
<td headers="protan" class="gt_row gt_left">#9999FB</td>
<td headers="tritan" class="gt_row gt_left">#DD9797</td>
<td headers="achrom" class="gt_row gt_left">#F1F1F1</td></tr>
    <tr><td headers="name" class="gt_row gt_left">purple_a200</td>
<td headers="normal" class="gt_row gt_left">#E040FB</td>
<td headers="deutan" class="gt_row gt_left">#9090F8</td>
<td headers="protan" class="gt_row gt_left">#7272FA</td>
<td headers="tritan" class="gt_row gt_left">#CF7171</td>
<td headers="achrom" class="gt_row gt_left">#EEEEEE</td></tr>
    <tr><td headers="name" class="gt_row gt_left">purple_a400</td>
<td headers="normal" class="gt_row gt_left">#D500F9</td>
<td headers="deutan" class="gt_row gt_left">#8181F6</td>
<td headers="protan" class="gt_row gt_left">#5E5EF8</td>
<td headers="tritan" class="gt_row gt_left">#C26060</td>
<td headers="achrom" class="gt_row gt_left">#EBEBEB</td></tr>
    <tr><td headers="name" class="gt_row gt_left">purple_a700</td>
<td headers="normal" class="gt_row gt_left">#AA00FF</td>
<td headers="deutan" class="gt_row gt_left">#6666FD</td>
<td headers="protan" class="gt_row gt_left">#4A4AFE</td>
<td headers="tritan" class="gt_row gt_left">#8E6363</td>
<td headers="achrom" class="gt_row gt_left">#F0F1F1</td></tr>
    <tr><td headers="name" class="gt_row gt_left">deep_purple_50</td>
<td headers="normal" class="gt_row gt_left">#EDE7F6</td>
<td headers="deutan" class="gt_row gt_left">#E9E9F5</td>
<td headers="protan" class="gt_row gt_left">#E8E8F5</td>
<td headers="tritan" class="gt_row gt_left">#EBE8E8</td>
<td headers="achrom" class="gt_row gt_left">#F4F4F4</td></tr>
    <tr><td headers="name" class="gt_row gt_left">deep_purple_100</td>
<td headers="normal" class="gt_row gt_left">#D1C4E9</td>
<td headers="deutan" class="gt_row gt_left">#C8C8E8</td>
<td headers="protan" class="gt_row gt_left">#C6C6E8</td>
<td headers="tritan" class="gt_row gt_left">#CCC9C9</td>
<td headers="achrom" class="gt_row gt_left">#E4E4E4</td></tr>
    <tr><td headers="name" class="gt_row gt_left">deep_purple_200</td>
<td headers="normal" class="gt_row gt_left">#B39DDB</td>
<td headers="deutan" class="gt_row gt_left">#A4A4DA</td>
<td headers="protan" class="gt_row gt_left">#A1A1DA</td>
<td headers="tritan" class="gt_row gt_left">#AAA6A6</td>
<td headers="achrom" class="gt_row gt_left">#D4D4D4</td></tr>
    <tr><td headers="name" class="gt_row gt_left">deep_purple_300</td>
<td headers="normal" class="gt_row gt_left">#9575CD</td>
<td headers="deutan" class="gt_row gt_left">#8080CC</td>
<td headers="protan" class="gt_row gt_left">#7B7BCC</td>
<td headers="tritan" class="gt_row gt_left">#878484</td>
<td headers="achrom" class="gt_row gt_left">#C4C4C4</td></tr>
    <tr><td headers="name" class="gt_row gt_left">deep_purple_400</td>
<td headers="normal" class="gt_row gt_left">#7E57C2</td>
<td headers="deutan" class="gt_row gt_left">#6565C1</td>
<td headers="protan" class="gt_row gt_left">#5F5FC1</td>
<td headers="tritan" class="gt_row gt_left">#6C6C6C</td>
<td headers="achrom" class="gt_row gt_left">#B8B8B8</td></tr>
    <tr><td headers="name" class="gt_row gt_left">deep_purple_500</td>
<td headers="normal" class="gt_row gt_left">#673AB7</td>
<td headers="deutan" class="gt_row gt_left">#4C4CB6</td>
<td headers="protan" class="gt_row gt_left">#4444B6</td>
<td headers="tritan" class="gt_row gt_left">#505757</td>
<td headers="achrom" class="gt_row gt_left">#ADADAD</td></tr>
    <tr><td headers="name" class="gt_row gt_left">deep_purple_600</td>
<td headers="normal" class="gt_row gt_left">#5E35B1</td>
<td headers="deutan" class="gt_row gt_left">#4545B0</td>
<td headers="protan" class="gt_row gt_left">#3E3EB0</td>
<td headers="tritan" class="gt_row gt_left">#455353</td>
<td headers="achrom" class="gt_row gt_left">#A7A7A7</td></tr>
    <tr><td headers="name" class="gt_row gt_left">deep_purple_700</td>
<td headers="normal" class="gt_row gt_left">#512DA8</td>
<td headers="deutan" class="gt_row gt_left">#3B3BA7</td>
<td headers="protan" class="gt_row gt_left">#3535A7</td>
<td headers="tritan" class="gt_row gt_left">#354C4C</td>
<td headers="achrom" class="gt_row gt_left">#9E9E9E</td></tr>
    <tr><td headers="name" class="gt_row gt_left">deep_purple_800</td>
<td headers="normal" class="gt_row gt_left">#4527A0</td>
<td headers="deutan" class="gt_row gt_left">#33339F</td>
<td headers="protan" class="gt_row gt_left">#2D2D9F</td>
<td headers="tritan" class="gt_row gt_left">#234646</td>
<td headers="achrom" class="gt_row gt_left">#979797</td></tr>
    <tr><td headers="name" class="gt_row gt_left">deep_purple_900</td>
<td headers="normal" class="gt_row gt_left">#311B92</td>
<td headers="deutan" class="gt_row gt_left">#232391</td>
<td headers="protan" class="gt_row gt_left">#1F1F91</td>
<td headers="tritan" class="gt_row gt_left">#003C3C</td>
<td headers="achrom" class="gt_row gt_left">#898989</td></tr>
    <tr><td headers="name" class="gt_row gt_left">deep_purple_a100</td>
<td headers="normal" class="gt_row gt_left">#B388FF</td>
<td headers="deutan" class="gt_row gt_left">#9797FE</td>
<td headers="protan" class="gt_row gt_left">#9090FE</td>
<td headers="tritan" class="gt_row gt_left">#A09D9D</td>
<td headers="achrom" class="gt_row gt_left">#F4F4F4</td></tr>
    <tr><td headers="name" class="gt_row gt_left">deep_purple_a200</td>
<td headers="normal" class="gt_row gt_left">#7C4DFF</td>
<td headers="deutan" class="gt_row gt_left">#5F5FFE</td>
<td headers="protan" class="gt_row gt_left">#5757FE</td>
<td headers="tritan" class="gt_row gt_left">#517878</td>
<td headers="achrom" class="gt_row gt_left">#F1F1F1</td></tr>
    <tr><td headers="name" class="gt_row gt_left">deep_purple_a400</td>
<td headers="normal" class="gt_row gt_left">#651FFF</td>
<td headers="deutan" class="gt_row gt_left">#4040FE</td>
<td headers="protan" class="gt_row gt_left">#3333FE</td>
<td headers="tritan" class="gt_row gt_left">#0E6767</td>
<td headers="achrom" class="gt_row gt_left">#F0F0F0</td></tr>
    <tr><td headers="name" class="gt_row gt_left">deep_purple_a700</td>
<td headers="normal" class="gt_row gt_left">#6200EA</td>
<td headers="deutan" class="gt_row gt_left">#3838E9</td>
<td headers="protan" class="gt_row gt_left">#2727E9</td>
<td headers="tritan" class="gt_row gt_left">#235A5A</td>
<td headers="achrom" class="gt_row gt_left">#DCDCDC</td></tr>
    <tr><td headers="name" class="gt_row gt_left">indigo_50</td>
<td headers="normal" class="gt_row gt_left">#E8EAF6</td>
<td headers="deutan" class="gt_row gt_left">#E9E9F6</td>
<td headers="protan" class="gt_row gt_left">#E9E9F6</td>
<td headers="tritan" class="gt_row gt_left">#E6EBEB</td>
<td headers="achrom" class="gt_row gt_left">#F4F4F4</td></tr>
    <tr><td headers="name" class="gt_row gt_left">indigo_100</td>
<td headers="normal" class="gt_row gt_left">#C5CAE9</td>
<td headers="deutan" class="gt_row gt_left">#C8C8E9</td>
<td headers="protan" class="gt_row gt_left">#C9C9E9</td>
<td headers="tritan" class="gt_row gt_left">#C0CECE</td>
<td headers="achrom" class="gt_row gt_left">#E5E5E5</td></tr>
    <tr><td headers="name" class="gt_row gt_left">indigo_200</td>
<td headers="normal" class="gt_row gt_left">#9FA8DA</td>
<td headers="deutan" class="gt_row gt_left">#A5A5DA</td>
<td headers="protan" class="gt_row gt_left">#A6A6DA</td>
<td headers="tritan" class="gt_row gt_left">#96AFAF</td>
<td headers="achrom" class="gt_row gt_left">#D4D4D4</td></tr>
    <tr><td headers="name" class="gt_row gt_left">indigo_300</td>
<td headers="normal" class="gt_row gt_left">#7986CB</td>
<td headers="deutan" class="gt_row gt_left">#8181CB</td>
<td headers="protan" class="gt_row gt_left">#8383CB</td>
<td headers="tritan" class="gt_row gt_left">#6A9191</td>
<td headers="achrom" class="gt_row gt_left">#C3C3C3</td></tr>
    <tr><td headers="name" class="gt_row gt_left">indigo_400</td>
<td headers="normal" class="gt_row gt_left">#5C6BC0</td>
<td headers="deutan" class="gt_row gt_left">#6666C0</td>
<td headers="protan" class="gt_row gt_left">#6868C0</td>
<td headers="tritan" class="gt_row gt_left">#447A7A</td>
<td headers="achrom" class="gt_row gt_left">#B7B7B7</td></tr>
    <tr><td headers="name" class="gt_row gt_left">indigo_500</td>
<td headers="normal" class="gt_row gt_left">#3F51B5</td>
<td headers="deutan" class="gt_row gt_left">#4B4BB5</td>
<td headers="protan" class="gt_row gt_left">#4E4EB5</td>
<td headers="tritan" class="gt_row gt_left">#046565</td>
<td headers="achrom" class="gt_row gt_left">#ACACAC</td></tr>
    <tr><td headers="name" class="gt_row gt_left">indigo_600</td>
<td headers="normal" class="gt_row gt_left">#3949AB</td>
<td headers="deutan" class="gt_row gt_left">#4444AB</td>
<td headers="protan" class="gt_row gt_left">#4646AB</td>
<td headers="tritan" class="gt_row gt_left">#005D5D</td>
<td headers="achrom" class="gt_row gt_left">#A2A2A2</td></tr>
    <tr><td headers="name" class="gt_row gt_left">indigo_700</td>
<td headers="normal" class="gt_row gt_left">#303F9F</td>
<td headers="deutan" class="gt_row gt_left">#3A3A9F</td>
<td headers="protan" class="gt_row gt_left">#3C3C9F</td>
<td headers="tritan" class="gt_row gt_left">#005353</td>
<td headers="achrom" class="gt_row gt_left">#969696</td></tr>
    <tr><td headers="name" class="gt_row gt_left">indigo_800</td>
<td headers="normal" class="gt_row gt_left">#283593</td>
<td headers="deutan" class="gt_row gt_left">#313193</td>
<td headers="protan" class="gt_row gt_left">#333393</td>
<td headers="tritan" class="gt_row gt_left">#004949</td>
<td headers="achrom" class="gt_row gt_left">#8B8B8B</td></tr>
    <tr><td headers="name" class="gt_row gt_left">indigo_900</td>
<td headers="normal" class="gt_row gt_left">#1A237E</td>
<td headers="deutan" class="gt_row gt_left">#20207E</td>
<td headers="protan" class="gt_row gt_left">#21217E</td>
<td headers="tritan" class="gt_row gt_left">#003939</td>
<td headers="achrom" class="gt_row gt_left">#767676</td></tr>
    <tr><td headers="name" class="gt_row gt_left">indigo_a100</td>
<td headers="normal" class="gt_row gt_left">#8C9EFF</td>
<td headers="deutan" class="gt_row gt_left">#9898FF</td>
<td headers="protan" class="gt_row gt_left">#9B9BFF</td>
<td headers="tritan" class="gt_row gt_left">#75AEAE</td>
<td headers="achrom" class="gt_row gt_left">#F5F5F5</td></tr>
    <tr><td headers="name" class="gt_row gt_left">indigo_a200</td>
<td headers="normal" class="gt_row gt_left">#536DFE</td>
<td headers="deutan" class="gt_row gt_left">#6565FE</td>
<td headers="protan" class="gt_row gt_left">#6969FE</td>
<td headers="tritan" class="gt_row gt_left">#008B8B</td>
<td headers="achrom" class="gt_row gt_left">#F1F1F1</td></tr>
    <tr><td headers="name" class="gt_row gt_left">indigo_a400</td>
<td headers="normal" class="gt_row gt_left">#3D5AFE</td>
<td headers="deutan" class="gt_row gt_left">#5151FE</td>
<td headers="protan" class="gt_row gt_left">#5555FE</td>
<td headers="tritan" class="gt_row gt_left">#007F7F</td>
<td headers="achrom" class="gt_row gt_left">#F0F0F0</td></tr>
    <tr><td headers="name" class="gt_row gt_left">indigo_a700</td>
<td headers="normal" class="gt_row gt_left">#304FFE</td>
<td headers="deutan" class="gt_row gt_left">#4646FE</td>
<td headers="protan" class="gt_row gt_left">#4A4AFE</td>
<td headers="tritan" class="gt_row gt_left">#007979</td>
<td headers="achrom" class="gt_row gt_left">#F0F0F0</td></tr>
    <tr><td headers="name" class="gt_row gt_left">blue_50</td>
<td headers="normal" class="gt_row gt_left">#E3F2FD</td>
<td headers="deutan" class="gt_row gt_left">#EDEDFD</td>
<td headers="protan" class="gt_row gt_left">#EFEFFD</td>
<td headers="tritan" class="gt_row gt_left">#E1F3F3</td>
<td headers="achrom" class="gt_row gt_left">#FBFBFB</td></tr>
    <tr><td headers="name" class="gt_row gt_left">blue_100</td>
<td headers="normal" class="gt_row gt_left">#BBDEFB</td>
<td headers="deutan" class="gt_row gt_left">#D3D3FB</td>
<td headers="protan" class="gt_row gt_left">#D8D8FB</td>
<td headers="tritan" class="gt_row gt_left">#B5E1E1</td>
<td headers="achrom" class="gt_row gt_left">#F7F7F7</td></tr>
    <tr><td headers="name" class="gt_row gt_left">blue_200</td>
<td headers="normal" class="gt_row gt_left">#90CAF9</td>
<td headers="deutan" class="gt_row gt_left">#B9B9FA</td>
<td headers="protan" class="gt_row gt_left">#C1C1F9</td>
<td headers="tritan" class="gt_row gt_left">#84D0D0</td>
<td headers="achrom" class="gt_row gt_left">#F2F2F3</td></tr>
    <tr><td headers="name" class="gt_row gt_left">blue_300</td>
<td headers="normal" class="gt_row gt_left">#64B5F6</td>
<td headers="deutan" class="gt_row gt_left">#9F9FF7</td>
<td headers="protan" class="gt_row gt_left">#AAAAF6</td>
<td headers="tritan" class="gt_row gt_left">#4ABEBE</td>
<td headers="achrom" class="gt_row gt_left">#EEEEEE</td></tr>
    <tr><td headers="name" class="gt_row gt_left">blue_400</td>
<td headers="normal" class="gt_row gt_left">#42A5F5</td>
<td headers="deutan" class="gt_row gt_left">#8D8DF6</td>
<td headers="protan" class="gt_row gt_left">#9999F5</td>
<td headers="tritan" class="gt_row gt_left">#00B1B1</td>
<td headers="achrom" class="gt_row gt_left">#EBECEC</td></tr>
    <tr><td headers="name" class="gt_row gt_left">blue_500</td>
<td headers="normal" class="gt_row gt_left">#2196F3</td>
<td headers="deutan" class="gt_row gt_left">#7E7EF3</td>
<td headers="protan" class="gt_row gt_left">#8A8AF3</td>
<td headers="tritan" class="gt_row gt_left">#00A5A5</td>
<td headers="achrom" class="gt_row gt_left">#E9E9E9</td></tr>
    <tr><td headers="name" class="gt_row gt_left">blue_600</td>
<td headers="normal" class="gt_row gt_left">#1E88E5</td>
<td headers="deutan" class="gt_row gt_left">#7272E5</td>
<td headers="protan" class="gt_row gt_left">#7D7DE5</td>
<td headers="tritan" class="gt_row gt_left">#009797</td>
<td headers="achrom" class="gt_row gt_left">#DBDBDB</td></tr>
    <tr><td headers="name" class="gt_row gt_left">blue_700</td>
<td headers="normal" class="gt_row gt_left">#1976D2</td>
<td headers="deutan" class="gt_row gt_left">#6262D2</td>
<td headers="protan" class="gt_row gt_left">#6C6CD2</td>
<td headers="tritan" class="gt_row gt_left">#008686</td>
<td headers="achrom" class="gt_row gt_left">#C8C8C8</td></tr>
    <tr><td headers="name" class="gt_row gt_left">blue_800</td>
<td headers="normal" class="gt_row gt_left">#1565C0</td>
<td headers="deutan" class="gt_row gt_left">#5454C0</td>
<td headers="protan" class="gt_row gt_left">#5C5CC0</td>
<td headers="tritan" class="gt_row gt_left">#007575</td>
<td headers="achrom" class="gt_row gt_left">#B7B7B7</td></tr>
    <tr><td headers="name" class="gt_row gt_left">blue_900</td>
<td headers="normal" class="gt_row gt_left">#0D47A1</td>
<td headers="deutan" class="gt_row gt_left">#3A3AA1</td>
<td headers="protan" class="gt_row gt_left">#4141A1</td>
<td headers="tritan" class="gt_row gt_left">#005959</td>
<td headers="achrom" class="gt_row gt_left">#989898</td></tr>
    <tr><td headers="name" class="gt_row gt_left">blue_a100</td>
<td headers="normal" class="gt_row gt_left">#82B1FF</td>
<td headers="deutan" class="gt_row gt_left">#A3A3FF</td>
<td headers="protan" class="gt_row gt_left">#AAAAFF</td>
<td headers="tritan" class="gt_row gt_left">#6CBDBD</td>
<td headers="achrom" class="gt_row gt_left">#F6F6F6</td></tr>
    <tr><td headers="name" class="gt_row gt_left">blue_a200</td>
<td headers="normal" class="gt_row gt_left">#448AFF</td>
<td headers="deutan" class="gt_row gt_left">#7878FF</td>
<td headers="protan" class="gt_row gt_left">#8181FF</td>
<td headers="tritan" class="gt_row gt_left">#009F9F</td>
<td headers="achrom" class="gt_row gt_left">#F3F3F3</td></tr>
    <tr><td headers="name" class="gt_row gt_left">blue_a400</td>
<td headers="normal" class="gt_row gt_left">#2979FF</td>
<td headers="deutan" class="gt_row gt_left">#6666FF</td>
<td headers="protan" class="gt_row gt_left">#7070FF</td>
<td headers="tritan" class="gt_row gt_left">#009393</td>
<td headers="achrom" class="gt_row gt_left">#F2F2F2</td></tr>
    <tr><td headers="name" class="gt_row gt_left">blue_a700</td>
<td headers="normal" class="gt_row gt_left">#2962FF</td>
<td headers="deutan" class="gt_row gt_left">#5454FF</td>
<td headers="protan" class="gt_row gt_left">#5B5BFF</td>
<td headers="tritan" class="gt_row gt_left">#008484</td>
<td headers="achrom" class="gt_row gt_left">#F1F1F1</td></tr>
    <tr><td headers="name" class="gt_row gt_left">light_blue_50</td>
<td headers="normal" class="gt_row gt_left">#E1F5FE</td>
<td headers="deutan" class="gt_row gt_left">#EEEEFE</td>
<td headers="protan" class="gt_row gt_left">#F1F1FE</td>
<td headers="tritan" class="gt_row gt_left">#DFF6F6</td>
<td headers="achrom" class="gt_row gt_left">#FCFCFC</td></tr>
    <tr><td headers="name" class="gt_row gt_left">light_blue_100</td>
<td headers="normal" class="gt_row gt_left">#B3E5FC</td>
<td headers="deutan" class="gt_row gt_left">#D6D6FD</td>
<td headers="protan" class="gt_row gt_left">#DDDDFC</td>
<td headers="tritan" class="gt_row gt_left">#AEE8E8</td>
<td headers="achrom" class="gt_row gt_left">#F8F8F8</td></tr>
    <tr><td headers="name" class="gt_row gt_left">light_blue_200</td>
<td headers="normal" class="gt_row gt_left">#81D4FA</td>
<td headers="deutan" class="gt_row gt_left">#BDBDFB</td>
<td headers="protan" class="gt_row gt_left">#C8C8FA</td>
<td headers="tritan" class="gt_row gt_left">#76D9D9</td>
<td headers="achrom" class="gt_row gt_left">#F4F4F4</td></tr>
    <tr><td headers="name" class="gt_row gt_left">light_blue_300</td>
<td headers="normal" class="gt_row gt_left">#4FC3F7</td>
<td headers="deutan" class="gt_row gt_left">#A7A7F8</td>
<td headers="protan" class="gt_row gt_left">#B5B5F7</td>
<td headers="tritan" class="gt_row gt_left">#2FCACA</td>
<td headers="achrom" class="gt_row gt_left">#F0F0F0</td></tr>
    <tr><td headers="name" class="gt_row gt_left">light_blue_400</td>
<td headers="normal" class="gt_row gt_left">#29B6F6</td>
<td headers="deutan" class="gt_row gt_left">#9999F7</td>
<td headers="protan" class="gt_row gt_left">#A8A8F6</td>
<td headers="tritan" class="gt_row gt_left">#00BFBF</td>
<td headers="achrom" class="gt_row gt_left">#EEEEEE</td></tr>
    <tr><td headers="name" class="gt_row gt_left">light_blue_500</td>
<td headers="normal" class="gt_row gt_left">#03A9F4</td>
<td headers="deutan" class="gt_row gt_left">#8C8CF5</td>
<td headers="protan" class="gt_row gt_left">#9B9BF4</td>
<td headers="tritan" class="gt_row gt_left">#00B4B4</td>
<td headers="achrom" class="gt_row gt_left">#EBEBEB</td></tr>
    <tr><td headers="name" class="gt_row gt_left">light_blue_600</td>
<td headers="normal" class="gt_row gt_left">#039BE5</td>
<td headers="deutan" class="gt_row gt_left">#8181E6</td>
<td headers="protan" class="gt_row gt_left">#8E8EE5</td>
<td headers="tritan" class="gt_row gt_left">#00A6A6</td>
<td headers="achrom" class="gt_row gt_left">#DCDCDC</td></tr>
    <tr><td headers="name" class="gt_row gt_left">light_blue_700</td>
<td headers="normal" class="gt_row gt_left">#0288D1</td>
<td headers="deutan" class="gt_row gt_left">#7070D1</td>
<td headers="protan" class="gt_row gt_left">#7C7CD1</td>
<td headers="tritan" class="gt_row gt_left">#009393</td>
<td headers="achrom" class="gt_row gt_left">#C8C8C8</td></tr>
    <tr><td headers="name" class="gt_row gt_left">light_blue_800</td>
<td headers="normal" class="gt_row gt_left">#0277BD</td>
<td headers="deutan" class="gt_row gt_left">#6262BD</td>
<td headers="protan" class="gt_row gt_left">#6D6DBD</td>
<td headers="tritan" class="gt_row gt_left">#008282</td>
<td headers="achrom" class="gt_row gt_left">#B5B5B5</td></tr>
    <tr><td headers="name" class="gt_row gt_left">light_blue_900</td>
<td headers="normal" class="gt_row gt_left">#01579B</td>
<td headers="deutan" class="gt_row gt_left">#47479B</td>
<td headers="protan" class="gt_row gt_left">#4F4F9B</td>
<td headers="tritan" class="gt_row gt_left">#006262</td>
<td headers="achrom" class="gt_row gt_left">#949494</td></tr>
    <tr><td headers="name" class="gt_row gt_left">light_blue_a100</td>
<td headers="normal" class="gt_row gt_left">#80D8FF</td>
<td headers="deutan" class="gt_row gt_left">#C0C0FF</td>
<td headers="protan" class="gt_row gt_left">#CCCCFF</td>
<td headers="tritan" class="gt_row gt_left">#74DDDD</td>
<td headers="achrom" class="gt_row gt_left">#F9F9F9</td></tr>
    <tr><td headers="name" class="gt_row gt_left">light_blue_a200</td>
<td headers="normal" class="gt_row gt_left">#40C4FF</td>
<td headers="deutan" class="gt_row gt_left">#A7A7FF</td>
<td headers="protan" class="gt_row gt_left">#B5B5FF</td>
<td headers="tritan" class="gt_row gt_left">#00CCCC</td>
<td headers="achrom" class="gt_row gt_left">#F7F7F7</td></tr>
    <tr><td headers="name" class="gt_row gt_left">light_blue_a400</td>
<td headers="normal" class="gt_row gt_left">#00B0FF</td>
<td headers="deutan" class="gt_row gt_left">#9292FF</td>
<td headers="protan" class="gt_row gt_left">#A1A1FF</td>
<td headers="tritan" class="gt_row gt_left">#00BCBC</td>
<td headers="achrom" class="gt_row gt_left">#F5F5F5</td></tr>
    <tr><td headers="name" class="gt_row gt_left">light_blue_a700</td>
<td headers="normal" class="gt_row gt_left">#0091EA</td>
<td headers="deutan" class="gt_row gt_left">#7878EA</td>
<td headers="protan" class="gt_row gt_left">#8585EA</td>
<td headers="tritan" class="gt_row gt_left">#009F9F</td>
<td headers="achrom" class="gt_row gt_left">#E0E0E0</td></tr>
    <tr><td headers="name" class="gt_row gt_left">cyan_50</td>
<td headers="normal" class="gt_row gt_left">#E0F7FA</td>
<td headers="deutan" class="gt_row gt_left">#EFEFFA</td>
<td headers="protan" class="gt_row gt_left">#F3F3FA</td>
<td headers="tritan" class="gt_row gt_left">#DFF7F7</td>
<td headers="achrom" class="gt_row gt_left">#F9F9F9</td></tr>
    <tr><td headers="name" class="gt_row gt_left">cyan_100</td>
<td headers="normal" class="gt_row gt_left">#B2EBF2</td>
<td headers="deutan" class="gt_row gt_left">#DADAF3</td>
<td headers="protan" class="gt_row gt_left">#E2E2F2</td>
<td headers="tritan" class="gt_row gt_left">#B0EBEB</td>
<td headers="achrom" class="gt_row gt_left">#F0F0F0</td></tr>
    <tr><td headers="name" class="gt_row gt_left">cyan_200</td>
<td headers="normal" class="gt_row gt_left">#80DEEA</td>
<td headers="deutan" class="gt_row gt_left">#C5C5EB</td>
<td headers="protan" class="gt_row gt_left">#D1D1EA</td>
<td headers="tritan" class="gt_row gt_left">#7CDFDF</td>
<td headers="achrom" class="gt_row gt_left">#E7E7E7</td></tr>
    <tr><td headers="name" class="gt_row gt_left">cyan_300</td>
<td headers="normal" class="gt_row gt_left">#4DD0E1</td>
<td headers="deutan" class="gt_row gt_left">#B2B2E3</td>
<td headers="protan" class="gt_row gt_left">#C1C1E1</td>
<td headers="tritan" class="gt_row gt_left">#44D2D2</td>
<td headers="achrom" class="gt_row gt_left">#DDDDDD</td></tr>
    <tr><td headers="name" class="gt_row gt_left">cyan_400</td>
<td headers="normal" class="gt_row gt_left">#26C6DA</td>
<td headers="deutan" class="gt_row gt_left">#A6A6DC</td>
<td headers="protan" class="gt_row gt_left">#B6B6DA</td>
<td headers="tritan" class="gt_row gt_left">#06C8C8</td>
<td headers="achrom" class="gt_row gt_left">#D6D6D6</td></tr>
    <tr><td headers="name" class="gt_row gt_left">cyan_500</td>
<td headers="normal" class="gt_row gt_left">#00BCD4</td>
<td headers="deutan" class="gt_row gt_left">#9C9CD5</td>
<td headers="protan" class="gt_row gt_left">#ACACD4</td>
<td headers="tritan" class="gt_row gt_left">#00BFBF</td>
<td headers="achrom" class="gt_row gt_left">#CFCFCF</td></tr>
    <tr><td headers="name" class="gt_row gt_left">cyan_600</td>
<td headers="normal" class="gt_row gt_left">#00ACC1</td>
<td headers="deutan" class="gt_row gt_left">#8F8FC2</td>
<td headers="protan" class="gt_row gt_left">#9E9EC1</td>
<td headers="tritan" class="gt_row gt_left">#00AEAE</td>
<td headers="achrom" class="gt_row gt_left">#BDBDBD</td></tr>
    <tr><td headers="name" class="gt_row gt_left">cyan_700</td>
<td headers="normal" class="gt_row gt_left">#0097A7</td>
<td headers="deutan" class="gt_row gt_left">#7D7DA8</td>
<td headers="protan" class="gt_row gt_left">#8A8AA7</td>
<td headers="tritan" class="gt_row gt_left">#009999</td>
<td headers="achrom" class="gt_row gt_left">#A3A3A3</td></tr>
    <tr><td headers="name" class="gt_row gt_left">cyan_800</td>
<td headers="normal" class="gt_row gt_left">#00838F</td>
<td headers="deutan" class="gt_row gt_left">#6C6C90</td>
<td headers="protan" class="gt_row gt_left">#78788F</td>
<td headers="tritan" class="gt_row gt_left">#008484</td>
<td headers="achrom" class="gt_row gt_left">#8C8C8C</td></tr>
    <tr><td headers="name" class="gt_row gt_left">cyan_900</td>
<td headers="normal" class="gt_row gt_left">#006064</td>
<td headers="deutan" class="gt_row gt_left">#4F4F65</td>
<td headers="protan" class="gt_row gt_left">#575764</td>
<td headers="tritan" class="gt_row gt_left">#006060</td>
<td headers="achrom" class="gt_row gt_left">#626262</td></tr>
    <tr><td headers="name" class="gt_row gt_left">cyan_a100</td>
<td headers="normal" class="gt_row gt_left">#84FFFF</td>
<td headers="deutan" class="gt_row gt_left">#E0E0FF</td>
<td headers="protan" class="gt_row gt_left">#EFEFFF</td>
<td headers="tritan" class="gt_row gt_left">#83FFFE</td>
<td headers="achrom" class="gt_row gt_left">#FDFDFD</td></tr>
    <tr><td headers="name" class="gt_row gt_left">cyan_a200</td>
<td headers="normal" class="gt_row gt_left">#18FFFF</td>
<td headers="deutan" class="gt_row gt_left">#D5D5FF</td>
<td headers="protan" class="gt_row gt_left">#EBEBFF</td>
<td headers="tritan" class="gt_row gt_left">#17FFFE</td>
<td headers="achrom" class="gt_row gt_left">#FCFCFD</td></tr>
    <tr><td headers="name" class="gt_row gt_left">cyan_a400</td>
<td headers="normal" class="gt_row gt_left">#00E5FF</td>
<td headers="deutan" class="gt_row gt_left">#BFBFFF</td>
<td headers="protan" class="gt_row gt_left">#D2D2FF</td>
<td headers="tritan" class="gt_row gt_left">#00E8E8</td>
<td headers="achrom" class="gt_row gt_left">#FAFAFA</td></tr>
    <tr><td headers="name" class="gt_row gt_left">cyan_a700</td>
<td headers="normal" class="gt_row gt_left">#00B8D4</td>
<td headers="deutan" class="gt_row gt_left">#9999D5</td>
<td headers="protan" class="gt_row gt_left">#A9A9D4</td>
<td headers="tritan" class="gt_row gt_left">#00BBBB</td>
<td headers="achrom" class="gt_row gt_left">#CFCFCF</td></tr>
    <tr><td headers="name" class="gt_row gt_left">teal_50</td>
<td headers="normal" class="gt_row gt_left">#E0F2F1</td>
<td headers="deutan" class="gt_row gt_left">#ECECF1</td>
<td headers="protan" class="gt_row gt_left">#EFEFF1</td>
<td headers="tritan" class="gt_row gt_left">#E0F1F1</td>
<td headers="achrom" class="gt_row gt_left">#F0F0F0</td></tr>
    <tr><td headers="name" class="gt_row gt_left">teal_100</td>
<td headers="normal" class="gt_row gt_left">#B2DFDB</td>
<td headers="deutan" class="gt_row gt_left">#D1D1DC</td>
<td headers="protan" class="gt_row gt_left">#D8D8DB</td>
<td headers="tritan" class="gt_row gt_left">#B2DEDE</td>
<td headers="achrom" class="gt_row gt_left">#DADADA</td></tr>
    <tr><td headers="name" class="gt_row gt_left">teal_200</td>
<td headers="normal" class="gt_row gt_left">#80CBC4</td>
<td headers="deutan" class="gt_row gt_left">#B6B6C5</td>
<td headers="protan" class="gt_row gt_left">#C0C0C4</td>
<td headers="tritan" class="gt_row gt_left">#81CACA</td>
<td headers="achrom" class="gt_row gt_left">#C3C3C3</td></tr>
    <tr><td headers="name" class="gt_row gt_left">teal_300</td>
<td headers="normal" class="gt_row gt_left">#4DB6AC</td>
<td headers="deutan" class="gt_row gt_left">#9D9DAE</td>
<td headers="protan" class="gt_row gt_left">#A9A9AC</td>
<td headers="tritan" class="gt_row gt_left">#50B4B4</td>
<td headers="achrom" class="gt_row gt_left">#ABABAC</td></tr>
    <tr><td headers="name" class="gt_row gt_left">teal_400</td>
<td headers="normal" class="gt_row gt_left">#26A69A</td>
<td headers="deutan" class="gt_row gt_left">#8B8B9C</td>
<td headers="protan" class="gt_row gt_left">#99999A</td>
<td headers="tritan" class="gt_row gt_left">#2DA4A4</td>
<td headers="achrom" class="gt_row gt_left">#9A9A9A</td></tr>
    <tr><td headers="name" class="gt_row gt_left">teal_500</td>
<td headers="normal" class="gt_row gt_left">#009688</td>
<td headers="deutan" class="gt_row gt_left">#7C7C8A</td>
<td headers="protan" class="gt_row gt_left">#898988</td>
<td headers="tritan" class="gt_row gt_left">#149494</td>
<td headers="achrom" class="gt_row gt_left">#888888</td></tr>
    <tr><td headers="name" class="gt_row gt_left">teal_600</td>
<td headers="normal" class="gt_row gt_left">#00897B</td>
<td headers="deutan" class="gt_row gt_left">#71717C</td>
<td headers="protan" class="gt_row gt_left">#7D7D7B</td>
<td headers="tritan" class="gt_row gt_left">#138787</td>
<td headers="achrom" class="gt_row gt_left">#7B7B7B</td></tr>
    <tr><td headers="name" class="gt_row gt_left">teal_700</td>
<td headers="normal" class="gt_row gt_left">#00796B</td>
<td headers="deutan" class="gt_row gt_left">#64646C</td>
<td headers="protan" class="gt_row gt_left">#6E6E6B</td>
<td headers="tritan" class="gt_row gt_left">#117777</td>
<td headers="achrom" class="gt_row gt_left">#6B6B6B</td></tr>
    <tr><td headers="name" class="gt_row gt_left">teal_800</td>
<td headers="normal" class="gt_row gt_left">#00695C</td>
<td headers="deutan" class="gt_row gt_left">#56565D</td>
<td headers="protan" class="gt_row gt_left">#60605C</td>
<td headers="tritan" class="gt_row gt_left">#0D6767</td>
<td headers="achrom" class="gt_row gt_left">#5C5C5C</td></tr>
    <tr><td headers="name" class="gt_row gt_left">teal_900</td>
<td headers="normal" class="gt_row gt_left">#004D40</td>
<td headers="deutan" class="gt_row gt_left">#3E3E41</td>
<td headers="protan" class="gt_row gt_left">#464640</td>
<td headers="tritan" class="gt_row gt_left">#094B4B</td>
<td headers="achrom" class="gt_row gt_left">#404041</td></tr>
    <tr><td headers="name" class="gt_row gt_left">teal_a100</td>
<td headers="normal" class="gt_row gt_left">#A7FFEB</td>
<td headers="deutan" class="gt_row gt_left">#E6E6ED</td>
<td headers="protan" class="gt_row gt_left">#F2F2EB</td>
<td headers="tritan" class="gt_row gt_left">#ABFCFC</td>
<td headers="achrom" class="gt_row gt_left">#ECECEC</td></tr>
    <tr><td headers="name" class="gt_row gt_left">teal_a200</td>
<td headers="normal" class="gt_row gt_left">#64FFDA</td>
<td headers="deutan" class="gt_row gt_left">#DBDBDD</td>
<td headers="protan" class="gt_row gt_left">#EDEDDA</td>
<td headers="tritan" class="gt_row gt_left">#71FAFA</td>
<td headers="achrom" class="gt_row gt_left">#DDDDDD</td></tr>
    <tr><td headers="name" class="gt_row gt_left">teal_a400</td>
<td headers="normal" class="gt_row gt_left">#1DE9B6</td>
<td headers="deutan" class="gt_row gt_left">#C3C3B9</td>
<td headers="protan" class="gt_row gt_left">#D6D6B6</td>
<td headers="tritan" class="gt_row gt_left">#43E3E3</td>
<td headers="achrom" class="gt_row gt_left">#BBBBBB</td></tr>
    <tr><td headers="name" class="gt_row gt_left">teal_a700</td>
<td headers="normal" class="gt_row gt_left">#00BFA5</td>
<td headers="deutan" class="gt_row gt_left">#9F9FA7</td>
<td headers="protan" class="gt_row gt_left">#AFAFA5</td>
<td headers="tritan" class="gt_row gt_left">#24BBBB</td>
<td headers="achrom" class="gt_row gt_left">#A6A6A6</td></tr>
    <tr><td headers="name" class="gt_row gt_left">green_50</td>
<td headers="normal" class="gt_row gt_left">#E8F5E9</td>
<td headers="deutan" class="gt_row gt_left">#F0F0E9</td>
<td headers="protan" class="gt_row gt_left">#F2F2E9</td>
<td headers="tritan" class="gt_row gt_left">#E9F3F3</td>
<td headers="achrom" class="gt_row gt_left">#EAEAEA</td></tr>
    <tr><td headers="name" class="gt_row gt_left">green_100</td>
<td headers="normal" class="gt_row gt_left">#C8E6C9</td>
<td headers="deutan" class="gt_row gt_left">#DCDCC9</td>
<td headers="protan" class="gt_row gt_left">#E1E1C9</td>
<td headers="tritan" class="gt_row gt_left">#CCE2E2</td>
<td headers="achrom" class="gt_row gt_left">#CCCCCC</td></tr>
    <tr><td headers="name" class="gt_row gt_left">green_200</td>
<td headers="normal" class="gt_row gt_left">#A5D6A7</td>
<td headers="deutan" class="gt_row gt_left">#C7C7A8</td>
<td headers="protan" class="gt_row gt_left">#CECEA7</td>
<td headers="tritan" class="gt_row gt_left">#ACD0D0</td>
<td headers="achrom" class="gt_row gt_left">#ACACAC</td></tr>
    <tr><td headers="name" class="gt_row gt_left">green_300</td>
<td headers="normal" class="gt_row gt_left">#81C784</td>
<td headers="deutan" class="gt_row gt_left">#B3B386</td>
<td headers="protan" class="gt_row gt_left">#BDBD84</td>
<td headers="tritan" class="gt_row gt_left">#8CC0C0</td>
<td headers="achrom" class="gt_row gt_left">#8D8D8D</td></tr>
    <tr><td headers="name" class="gt_row gt_left">green_400</td>
<td headers="normal" class="gt_row gt_left">#66BB6A</td>
<td headers="deutan" class="gt_row gt_left">#A5A56D</td>
<td headers="protan" class="gt_row gt_left">#B0B06A</td>
<td headers="tritan" class="gt_row gt_left">#74B3B3</td>
<td headers="achrom" class="gt_row gt_left">#767676</td></tr>
    <tr><td headers="name" class="gt_row gt_left">green_500</td>
<td headers="normal" class="gt_row gt_left">#4CAF50</td>
<td headers="deutan" class="gt_row gt_left">#979754</td>
<td headers="protan" class="gt_row gt_left">#A3A350</td>
<td headers="tritan" class="gt_row gt_left">#5FA6A6</td>
<td headers="achrom" class="gt_row gt_left">#606060</td></tr>
    <tr><td headers="name" class="gt_row gt_left">green_600</td>
<td headers="normal" class="gt_row gt_left">#43A047</td>
<td headers="deutan" class="gt_row gt_left">#89894B</td>
<td headers="protan" class="gt_row gt_left">#959547</td>
<td headers="tritan" class="gt_row gt_left">#559898</td>
<td headers="achrom" class="gt_row gt_left">#565656</td></tr>
    <tr><td headers="name" class="gt_row gt_left">green_700</td>
<td headers="normal" class="gt_row gt_left">#388E3C</td>
<td headers="deutan" class="gt_row gt_left">#797940</td>
<td headers="protan" class="gt_row gt_left">#84843C</td>
<td headers="tritan" class="gt_row gt_left">#498686</td>
<td headers="achrom" class="gt_row gt_left">#4A4A4A</td></tr>
    <tr><td headers="name" class="gt_row gt_left">green_800</td>
<td headers="normal" class="gt_row gt_left">#2E7D32</td>
<td headers="deutan" class="gt_row gt_left">#6A6A35</td>
<td headers="protan" class="gt_row gt_left">#747432</td>
<td headers="tritan" class="gt_row gt_left">#3E7676</td>
<td headers="achrom" class="gt_row gt_left">#3F3F3F</td></tr>
    <tr><td headers="name" class="gt_row gt_left">green_900</td>
<td headers="normal" class="gt_row gt_left">#1B5E20</td>
<td headers="deutan" class="gt_row gt_left">#4F4F23</td>
<td headers="protan" class="gt_row gt_left">#565620</td>
<td headers="tritan" class="gt_row gt_left">#2A5858</td>
<td headers="achrom" class="gt_row gt_left">#2B2B2B</td></tr>
    <tr><td headers="name" class="gt_row gt_left">green_a100</td>
<td headers="normal" class="gt_row gt_left">#B9F6CA</td>
<td headers="deutan" class="gt_row gt_left">#E4E4CB</td>
<td headers="protan" class="gt_row gt_left">#EDEDCA</td>
<td headers="tritan" class="gt_row gt_left">#C0F1F1</td>
<td headers="achrom" class="gt_row gt_left">#CFCFCF</td></tr>
    <tr><td headers="name" class="gt_row gt_left">green_a200</td>
<td headers="normal" class="gt_row gt_left">#69F0AE</td>
<td headers="deutan" class="gt_row gt_left">#CFCFB1</td>
<td headers="protan" class="gt_row gt_left">#E0E0AE</td>
<td headers="tritan" class="gt_row gt_left">#7BE9E9</td>
<td headers="achrom" class="gt_row gt_left">#B5B5B5</td></tr>
    <tr><td headers="name" class="gt_row gt_left">green_a400</td>
<td headers="normal" class="gt_row gt_left">#00E676</td>
<td headers="deutan" class="gt_row gt_left">#C0C07C</td>
<td headers="protan" class="gt_row gt_left">#D3D377</td>
<td headers="tritan" class="gt_row gt_left">#4EDBDB</td>
<td headers="achrom" class="gt_row gt_left">#878787</td></tr>
    <tr><td headers="name" class="gt_row gt_left">green_a700</td>
<td headers="normal" class="gt_row gt_left">#00C853</td>
<td headers="deutan" class="gt_row gt_left">#A7A75A</td>
<td headers="protan" class="gt_row gt_left">#B7B754</td>
<td headers="tritan" class="gt_row gt_left">#46BEBE</td>
<td headers="achrom" class="gt_row gt_left">#686868</td></tr>
    <tr><td headers="name" class="gt_row gt_left">light_green_50</td>
<td headers="normal" class="gt_row gt_left">#F1F8E9</td>
<td headers="deutan" class="gt_row gt_left">#F5F5E9</td>
<td headers="protan" class="gt_row gt_left">#F6F6E9</td>
<td headers="tritan" class="gt_row gt_left">#F2F6F6</td>
<td headers="achrom" class="gt_row gt_left">#EAEAEA</td></tr>
    <tr><td headers="name" class="gt_row gt_left">light_green_100</td>
<td headers="normal" class="gt_row gt_left">#DCEDC8</td>
<td headers="deutan" class="gt_row gt_left">#E7E7C8</td>
<td headers="protan" class="gt_row gt_left">#EAEAC8</td>
<td headers="tritan" class="gt_row gt_left">#E0E8E8</td>
<td headers="achrom" class="gt_row gt_left">#CCCCCC</td></tr>
    <tr><td headers="name" class="gt_row gt_left">light_green_200</td>
<td headers="normal" class="gt_row gt_left">#C5E1A5</td>
<td headers="deutan" class="gt_row gt_left">#D8D8A6</td>
<td headers="protan" class="gt_row gt_left">#DCDCA5</td>
<td headers="tritan" class="gt_row gt_left">#CCDADA</td>
<td headers="achrom" class="gt_row gt_left">#ADADAD</td></tr>
    <tr><td headers="name" class="gt_row gt_left">light_green_300</td>
<td headers="normal" class="gt_row gt_left">#AED581</td>
<td headers="deutan" class="gt_row gt_left">#C9C982</td>
<td headers="protan" class="gt_row gt_left">#CFCF81</td>
<td headers="tritan" class="gt_row gt_left">#B8CCCC</td>
<td headers="achrom" class="gt_row gt_left">#8E8E8E</td></tr>
    <tr><td headers="name" class="gt_row gt_left">light_green_400</td>
<td headers="normal" class="gt_row gt_left">#9CCC65</td>
<td headers="deutan" class="gt_row gt_left">#BDBD67</td>
<td headers="protan" class="gt_row gt_left">#C4C465</td>
<td headers="tritan" class="gt_row gt_left">#A8C2C2</td>
<td headers="achrom" class="gt_row gt_left">#777777</td></tr>
    <tr><td headers="name" class="gt_row gt_left">light_green_500</td>
<td headers="normal" class="gt_row gt_left">#8BC34A</td>
<td headers="deutan" class="gt_row gt_left">#B2B24E</td>
<td headers="protan" class="gt_row gt_left">#BABA4A</td>
<td headers="tritan" class="gt_row gt_left">#99B9B9</td>
<td headers="achrom" class="gt_row gt_left">#626262</td></tr>
    <tr><td headers="name" class="gt_row gt_left">light_green_600</td>
<td headers="normal" class="gt_row gt_left">#7CB342</td>
<td headers="deutan" class="gt_row gt_left">#A3A346</td>
<td headers="protan" class="gt_row gt_left">#ABAB42</td>
<td headers="tritan" class="gt_row gt_left">#89A9A9</td>
<td headers="achrom" class="gt_row gt_left">#595959</td></tr>
    <tr><td headers="name" class="gt_row gt_left">light_green_700</td>
<td headers="normal" class="gt_row gt_left">#689F38</td>
<td headers="deutan" class="gt_row gt_left">#8F8F3C</td>
<td headers="protan" class="gt_row gt_left">#979738</td>
<td headers="tritan" class="gt_row gt_left">#749696</td>
<td headers="achrom" class="gt_row gt_left">#4D4D4D</td></tr>
    <tr><td headers="name" class="gt_row gt_left">light_green_800</td>
<td headers="normal" class="gt_row gt_left">#558B2F</td>
<td headers="deutan" class="gt_row gt_left">#7C7C32</td>
<td headers="protan" class="gt_row gt_left">#83832F</td>
<td headers="tritan" class="gt_row gt_left">#618383</td>
<td headers="achrom" class="gt_row gt_left">#424242</td></tr>
    <tr><td headers="name" class="gt_row gt_left">light_green_900</td>
<td headers="normal" class="gt_row gt_left">#33691E</td>
<td headers="deutan" class="gt_row gt_left">#5B5B21</td>
<td headers="protan" class="gt_row gt_left">#62621E</td>
<td headers="tritan" class="gt_row gt_left">#3E6363</td>
<td headers="achrom" class="gt_row gt_left">#2E2E2E</td></tr>
    <tr><td headers="name" class="gt_row gt_left">light_green_a100</td>
<td headers="normal" class="gt_row gt_left">#CCFF90</td>
<td headers="deutan" class="gt_row gt_left">#EFEF92</td>
<td headers="protan" class="gt_row gt_left">#F7F790</td>
<td headers="tritan" class="gt_row gt_left">#D9F4F4</td>
<td headers="achrom" class="gt_row gt_left">#A2A2A2</td></tr>
    <tr><td headers="name" class="gt_row gt_left">light_green_a200</td>
<td headers="normal" class="gt_row gt_left">#B2FF59</td>
<td headers="deutan" class="gt_row gt_left">#E9E95F</td>
<td headers="protan" class="gt_row gt_left">#F4F45A</td>
<td headers="tritan" class="gt_row gt_left">#C5F1F1</td>
<td headers="achrom" class="gt_row gt_left">#7C7C7C</td></tr>
    <tr><td headers="name" class="gt_row gt_left">light_green_a400</td>
<td headers="normal" class="gt_row gt_left">#76FF03</td>
<td headers="deutan" class="gt_row gt_left">#DDDD2A</td>
<td headers="protan" class="gt_row gt_left">#EEEE0E</td>
<td headers="tritan" class="gt_row gt_left">#96F0F0</td>
<td headers="achrom" class="gt_row gt_left">#5E5E5E</td></tr>
    <tr><td headers="name" class="gt_row gt_left">light_green_a700</td>
<td headers="normal" class="gt_row gt_left">#64DD17</td>
<td headers="deutan" class="gt_row gt_left">#BFBF2B</td>
<td headers="protan" class="gt_row gt_left">#CECE1B</td>
<td headers="tritan" class="gt_row gt_left">#80D0D0</td>
<td headers="achrom" class="gt_row gt_left">#545454</td></tr>
    <tr><td headers="name" class="gt_row gt_left">lime_50</td>
<td headers="normal" class="gt_row gt_left">#F9FBE7</td>
<td headers="deutan" class="gt_row gt_left">#FAFAE7</td>
<td headers="protan" class="gt_row gt_left">#FAFAE7</td>
<td headers="tritan" class="gt_row gt_left">#FBF8F8</td>
<td headers="achrom" class="gt_row gt_left">#E9E9E9</td></tr>
    <tr><td headers="name" class="gt_row gt_left">lime_100</td>
<td headers="normal" class="gt_row gt_left">#F0F4C3</td>
<td headers="deutan" class="gt_row gt_left">#F2F2C3</td>
<td headers="protan" class="gt_row gt_left">#F3F3C3</td>
<td headers="tritan" class="gt_row gt_left">#F5EEEE</td>
<td headers="achrom" class="gt_row gt_left">#C9CACA</td></tr>
    <tr><td headers="name" class="gt_row gt_left">lime_200</td>
<td headers="normal" class="gt_row gt_left">#E6EE9C</td>
<td headers="deutan" class="gt_row gt_left">#EBEB9C</td>
<td headers="protan" class="gt_row gt_left">#ECEC9C</td>
<td headers="tritan" class="gt_row gt_left">#EEE5E5</td>
<td headers="achrom" class="gt_row gt_left">#A9A9A9</td></tr>
    <tr><td headers="name" class="gt_row gt_left">lime_300</td>
<td headers="normal" class="gt_row gt_left">#DCE775</td>
<td headers="deutan" class="gt_row gt_left">#E3E375</td>
<td headers="protan" class="gt_row gt_left">#E5E575</td>
<td headers="tritan" class="gt_row gt_left">#E6DCDC</td>
<td headers="achrom" class="gt_row gt_left">#8A8A8A</td></tr>
    <tr><td headers="name" class="gt_row gt_left">lime_400</td>
<td headers="normal" class="gt_row gt_left">#D4E157</td>
<td headers="deutan" class="gt_row gt_left">#DCDC58</td>
<td headers="protan" class="gt_row gt_left">#DEDE57</td>
<td headers="tritan" class="gt_row gt_left">#DFD5D5</td>
<td headers="achrom" class="gt_row gt_left">#747474</td></tr>
    <tr><td headers="name" class="gt_row gt_left">lime_500</td>
<td headers="normal" class="gt_row gt_left">#CDDC39</td>
<td headers="deutan" class="gt_row gt_left">#D7D73B</td>
<td headers="protan" class="gt_row gt_left">#D9D939</td>
<td headers="tritan" class="gt_row gt_left">#D9CFCF</td>
<td headers="achrom" class="gt_row gt_left">#636363</td></tr>
    <tr><td headers="name" class="gt_row gt_left">lime_600</td>
<td headers="normal" class="gt_row gt_left">#C0CA33</td>
<td headers="deutan" class="gt_row gt_left">#C6C634</td>
<td headers="protan" class="gt_row gt_left">#C8C833</td>
<td headers="tritan" class="gt_row gt_left">#CBBEBE</td>
<td headers="achrom" class="gt_row gt_left">#5A5A5A</td></tr>
    <tr><td headers="name" class="gt_row gt_left">lime_700</td>
<td headers="normal" class="gt_row gt_left">#AFB42B</td>
<td headers="deutan" class="gt_row gt_left">#B2B22B</td>
<td headers="protan" class="gt_row gt_left">#B3B32B</td>
<td headers="tritan" class="gt_row gt_left">#B8A9A9</td>
<td headers="achrom" class="gt_row gt_left">#4F4F4F</td></tr>
    <tr><td headers="name" class="gt_row gt_left">lime_800</td>
<td headers="normal" class="gt_row gt_left">#9E9D24</td>
<td headers="deutan" class="gt_row gt_left">#9D9D23</td>
<td headers="protan" class="gt_row gt_left">#9D9D23</td>
<td headers="tritan" class="gt_row gt_left">#A69494</td>
<td headers="achrom" class="gt_row gt_left">#444444</td></tr>
    <tr><td headers="name" class="gt_row gt_left">lime_900</td>
<td headers="normal" class="gt_row gt_left">#827717</td>
<td headers="deutan" class="gt_row gt_left">#7A7A14</td>
<td headers="protan" class="gt_row gt_left">#787816</td>
<td headers="tritan" class="gt_row gt_left">#877070</td>
<td headers="achrom" class="gt_row gt_left">#313131</td></tr>
    <tr><td headers="name" class="gt_row gt_left">lime_a100</td>
<td headers="normal" class="gt_row gt_left">#F4FF81</td>
<td headers="deutan" class="gt_row gt_left">#FBFB81</td>
<td headers="protan" class="gt_row gt_left">#FDFD81</td>
<td headers="tritan" class="gt_row gt_left">#FFF3F3</td>
<td headers="achrom" class="gt_row gt_left">#989898</td></tr>
    <tr><td headers="name" class="gt_row gt_left">lime_a200</td>
<td headers="normal" class="gt_row gt_left">#EEFF41</td>
<td headers="deutan" class="gt_row gt_left">#F9F943</td>
<td headers="protan" class="gt_row gt_left">#FCFC41</td>
<td headers="tritan" class="gt_row gt_left">#FCF1F1</td>
<td headers="achrom" class="gt_row gt_left">#727272</td></tr>
    <tr><td headers="name" class="gt_row gt_left">lime_a400</td>
<td headers="normal" class="gt_row gt_left">#C6FF00</td>
<td headers="deutan" class="gt_row gt_left">#EEEE1C</td>
<td headers="protan" class="gt_row gt_left">#F6F606</td>
<td headers="tritan" class="gt_row gt_left">#D8F0F0</td>
<td headers="achrom" class="gt_row gt_left">#606060</td></tr>
    <tr><td headers="name" class="gt_row gt_left">lime_a700</td>
<td headers="normal" class="gt_row gt_left">#AEEA00</td>
<td headers="deutan" class="gt_row gt_left">#D8D81B</td>
<td headers="protan" class="gt_row gt_left">#E1E105</td>
<td headers="tritan" class="gt_row gt_left">#C0DCDC</td>
<td headers="achrom" class="gt_row gt_left">#575758</td></tr>
    <tr><td headers="name" class="gt_row gt_left">yellow_50</td>
<td headers="normal" class="gt_row gt_left">#FFFDE7</td>
<td headers="deutan" class="gt_row gt_left">#FDFDE6</td>
<td headers="protan" class="gt_row gt_left">#FDFDE6</td>
<td headers="tritan" class="gt_row gt_left">#FFFAFA</td>
<td headers="achrom" class="gt_row gt_left">#E9E9E9</td></tr>
    <tr><td headers="name" class="gt_row gt_left">yellow_100</td>
<td headers="normal" class="gt_row gt_left">#FFF9C4</td>
<td headers="deutan" class="gt_row gt_left">#FBFBC3</td>
<td headers="protan" class="gt_row gt_left">#FAFAC3</td>
<td headers="tritan" class="gt_row gt_left">#FFF3F3</td>
<td headers="achrom" class="gt_row gt_left">#CBCBCB</td></tr>
    <tr><td headers="name" class="gt_row gt_left">yellow_200</td>
<td headers="normal" class="gt_row gt_left">#FFF59D</td>
<td headers="deutan" class="gt_row gt_left">#F8F89C</td>
<td headers="protan" class="gt_row gt_left">#F6F69C</td>
<td headers="tritan" class="gt_row gt_left">#FFECEC</td>
<td headers="achrom" class="gt_row gt_left">#ABABAB</td></tr>
    <tr><td headers="name" class="gt_row gt_left">yellow_300</td>
<td headers="normal" class="gt_row gt_left">#FFF176</td>
<td headers="deutan" class="gt_row gt_left">#F5F574</td>
<td headers="protan" class="gt_row gt_left">#F3F375</td>
<td headers="tritan" class="gt_row gt_left">#FFE6E6</td>
<td headers="achrom" class="gt_row gt_left">#8E8E8E</td></tr>
    <tr><td headers="name" class="gt_row gt_left">yellow_400</td>
<td headers="normal" class="gt_row gt_left">#FFEE58</td>
<td headers="deutan" class="gt_row gt_left">#F3F356</td>
<td headers="protan" class="gt_row gt_left">#F1F157</td>
<td headers="tritan" class="gt_row gt_left">#FFE1E1</td>
<td headers="achrom" class="gt_row gt_left">#7A7A7A</td></tr>
    <tr><td headers="name" class="gt_row gt_left">yellow_500</td>
<td headers="normal" class="gt_row gt_left">#FFEB3B</td>
<td headers="deutan" class="gt_row gt_left">#F1F137</td>
<td headers="protan" class="gt_row gt_left">#EEEE3A</td>
<td headers="tritan" class="gt_row gt_left">#FFDEDE</td>
<td headers="achrom" class="gt_row gt_left">#6A6A6A</td></tr>
    <tr><td headers="name" class="gt_row gt_left">yellow_600</td>
<td headers="normal" class="gt_row gt_left">#FDD835</td>
<td headers="deutan" class="gt_row gt_left">#E5E52E</td>
<td headers="protan" class="gt_row gt_left">#DEDE33</td>
<td headers="tritan" class="gt_row gt_left">#FFCCCC</td>
<td headers="achrom" class="gt_row gt_left">#626262</td></tr>
    <tr><td headers="name" class="gt_row gt_left">yellow_700</td>
<td headers="normal" class="gt_row gt_left">#FBC02D</td>
<td headers="deutan" class="gt_row gt_left">#D5D51F</td>
<td headers="protan" class="gt_row gt_left">#CBCB2B</td>
<td headers="tritan" class="gt_row gt_left">#FFB5B5</td>
<td headers="achrom" class="gt_row gt_left">#585858</td></tr>
    <tr><td headers="name" class="gt_row gt_left">yellow_800</td>
<td headers="normal" class="gt_row gt_left">#F9A825</td>
<td headers="deutan" class="gt_row gt_left">#C7C709</td>
<td headers="protan" class="gt_row gt_left">#B9B921</td>
<td headers="tritan" class="gt_row gt_left">#FE9E9E</td>
<td headers="achrom" class="gt_row gt_left">#4D4D4D</td></tr>
    <tr><td headers="name" class="gt_row gt_left">yellow_900</td>
<td headers="normal" class="gt_row gt_left">#F57F17</td>
<td headers="deutan" class="gt_row gt_left">#B1B100</td>
<td headers="protan" class="gt_row gt_left">#9B9B10</td>
<td headers="tritan" class="gt_row gt_left">#F87777</td>
<td headers="achrom" class="gt_row gt_left">#3D3D3D</td></tr>
    <tr><td headers="name" class="gt_row gt_left">yellow_a100</td>
<td headers="normal" class="gt_row gt_left">#FFFF8D</td>
<td headers="deutan" class="gt_row gt_left">#FEFF8C</td>
<td headers="protan" class="gt_row gt_left">#FFFE8D</td>
<td headers="tritan" class="gt_row gt_left">#FFF4F4</td>
<td headers="achrom" class="gt_row gt_left">#A1A1A1</td></tr>
    <tr><td headers="name" class="gt_row gt_left">yellow_a200</td>
<td headers="normal" class="gt_row gt_left">#FFFF00</td>
<td headers="deutan" class="gt_row gt_left">#FEFF00</td>
<td headers="protan" class="gt_row gt_left">#FFFE00</td>
<td headers="tritan" class="gt_row gt_left">#FFF0F0</td>
<td headers="achrom" class="gt_row gt_left">#636363</td></tr>
    <tr><td headers="name" class="gt_row gt_left">yellow_a400</td>
<td headers="normal" class="gt_row gt_left">#FFEA00</td>
<td headers="deutan" class="gt_row gt_left">#F1F100</td>
<td headers="protan" class="gt_row gt_left">#EDED00</td>
<td headers="tritan" class="gt_row gt_left">#FFDCDC</td>
<td headers="achrom" class="gt_row gt_left">#5C5C5C</td></tr>
    <tr><td headers="name" class="gt_row gt_left">yellow_a700</td>
<td headers="normal" class="gt_row gt_left">#FFD600</td>
<td headers="deutan" class="gt_row gt_left">#E4E400</td>
<td headers="protan" class="gt_row gt_left">#DDDD00</td>
<td headers="tritan" class="gt_row gt_left">#FFC9C9</td>
<td headers="achrom" class="gt_row gt_left">#555555</td></tr>
    <tr><td headers="name" class="gt_row gt_left">amber_50</td>
<td headers="normal" class="gt_row gt_left">#FFF8E1</td>
<td headers="deutan" class="gt_row gt_left">#FAFAE0</td>
<td headers="protan" class="gt_row gt_left">#F9F9E0</td>
<td headers="tritan" class="gt_row gt_left">#FFF5F5</td>
<td headers="achrom" class="gt_row gt_left">#E4E4E4</td></tr>
    <tr><td headers="name" class="gt_row gt_left">amber_100</td>
<td headers="normal" class="gt_row gt_left">#FFECB3</td>
<td headers="deutan" class="gt_row gt_left">#F2F2B2</td>
<td headers="protan" class="gt_row gt_left">#EFEFB2</td>
<td headers="tritan" class="gt_row gt_left">#FFE5E5</td>
<td headers="achrom" class="gt_row gt_left">#BBBCBC</td></tr>
    <tr><td headers="name" class="gt_row gt_left">amber_200</td>
<td headers="normal" class="gt_row gt_left">#FFE082</td>
<td headers="deutan" class="gt_row gt_left">#EAEA80</td>
<td headers="protan" class="gt_row gt_left">#E5E581</td>
<td headers="tritan" class="gt_row gt_left">#FFD7D7</td>
<td headers="achrom" class="gt_row gt_left">#939393</td></tr>
    <tr><td headers="name" class="gt_row gt_left">amber_300</td>
<td headers="normal" class="gt_row gt_left">#FFD54F</td>
<td headers="deutan" class="gt_row gt_left">#E4E44A</td>
<td headers="protan" class="gt_row gt_left">#DCDC4E</td>
<td headers="tritan" class="gt_row gt_left">#FFCACA</td>
<td headers="achrom" class="gt_row gt_left">#6E6E6E</td></tr>
    <tr><td headers="name" class="gt_row gt_left">amber_400</td>
<td headers="normal" class="gt_row gt_left">#FFCA28</td>
<td headers="deutan" class="gt_row gt_left">#DDDD19</td>
<td headers="protan" class="gt_row gt_left">#D4D425</td>
<td headers="tritan" class="gt_row gt_left">#FFBEBE</td>
<td headers="achrom" class="gt_row gt_left">#595959</td></tr>
    <tr><td headers="name" class="gt_row gt_left">amber_500</td>
<td headers="normal" class="gt_row gt_left">#FFC107</td>
<td headers="deutan" class="gt_row gt_left">#D8D800</td>
<td headers="protan" class="gt_row gt_left">#CDCD00</td>
<td headers="tritan" class="gt_row gt_left">#FFB5B5</td>
<td headers="achrom" class="gt_row gt_left">#4E4E4E</td></tr>
    <tr><td headers="name" class="gt_row gt_left">amber_600</td>
<td headers="normal" class="gt_row gt_left">#FFB300</td>
<td headers="deutan" class="gt_row gt_left">#D0D000</td>
<td headers="protan" class="gt_row gt_left">#C2C200</td>
<td headers="tritan" class="gt_row gt_left">#FFA8A8</td>
<td headers="achrom" class="gt_row gt_left">#494949</td></tr>
    <tr><td headers="name" class="gt_row gt_left">amber_700</td>
<td headers="normal" class="gt_row gt_left">#FFA000</td>
<td headers="deutan" class="gt_row gt_left">#C6C600</td>
<td headers="protan" class="gt_row gt_left">#B5B500</td>
<td headers="tritan" class="gt_row gt_left">#FF9696</td>
<td headers="achrom" class="gt_row gt_left">#434343</td></tr>
    <tr><td headers="name" class="gt_row gt_left">amber_800</td>
<td headers="normal" class="gt_row gt_left">#FF8F00</td>
<td headers="deutan" class="gt_row gt_left">#BDBD00</td>
<td headers="protan" class="gt_row gt_left">#A9A900</td>
<td headers="tritan" class="gt_row gt_left">#FF8686</td>
<td headers="achrom" class="gt_row gt_left">#3D3D3D</td></tr>
    <tr><td headers="name" class="gt_row gt_left">amber_900</td>
<td headers="normal" class="gt_row gt_left">#FF6F00</td>
<td headers="deutan" class="gt_row gt_left">#B0B000</td>
<td headers="protan" class="gt_row gt_left">#959500</td>
<td headers="tritan" class="gt_row gt_left">#FF6868</td>
<td headers="achrom" class="gt_row gt_left">#343434</td></tr>
    <tr><td headers="name" class="gt_row gt_left">amber_a100</td>
<td headers="normal" class="gt_row gt_left">#FFE57F</td>
<td headers="deutan" class="gt_row gt_left">#EEEE7D</td>
<td headers="protan" class="gt_row gt_left">#E9E97E</td>
<td headers="tritan" class="gt_row gt_left">#FFDBDB</td>
<td headers="achrom" class="gt_row gt_left">#929292</td></tr>
    <tr><td headers="name" class="gt_row gt_left">amber_a200</td>
<td headers="normal" class="gt_row gt_left">#FFD740</td>
<td headers="deutan" class="gt_row gt_left">#E5E53A</td>
<td headers="protan" class="gt_row gt_left">#DEDE3F</td>
<td headers="tritan" class="gt_row gt_left">#FFCBCB</td>
<td headers="achrom" class="gt_row gt_left">#676767</td></tr>
    <tr><td headers="name" class="gt_row gt_left">amber_a400</td>
<td headers="normal" class="gt_row gt_left">#FFC400</td>
<td headers="deutan" class="gt_row gt_left">#D9D900</td>
<td headers="protan" class="gt_row gt_left">#CFCF00</td>
<td headers="tritan" class="gt_row gt_left">#FFB8B8</td>
<td headers="achrom" class="gt_row gt_left">#4E4E4E</td></tr>
    <tr><td headers="name" class="gt_row gt_left">amber_a700</td>
<td headers="normal" class="gt_row gt_left">#FFAB00</td>
<td headers="deutan" class="gt_row gt_left">#CBCB00</td>
<td headers="protan" class="gt_row gt_left">#BCBC00</td>
<td headers="tritan" class="gt_row gt_left">#FFA0A0</td>
<td headers="achrom" class="gt_row gt_left">#464646</td></tr>
    <tr><td headers="name" class="gt_row gt_left">orange_50</td>
<td headers="normal" class="gt_row gt_left">#FFF3E0</td>
<td headers="deutan" class="gt_row gt_left">#F7F7DF</td>
<td headers="protan" class="gt_row gt_left">#F5F5DF</td>
<td headers="tritan" class="gt_row gt_left">#FFF0F0</td>
<td headers="achrom" class="gt_row gt_left">#E2E2E2</td></tr>
    <tr><td headers="name" class="gt_row gt_left">orange_100</td>
<td headers="normal" class="gt_row gt_left">#FFE0B2</td>
<td headers="deutan" class="gt_row gt_left">#EAEAB0</td>
<td headers="protan" class="gt_row gt_left">#E5E5B1</td>
<td headers="tritan" class="gt_row gt_left">#FFDADA</td>
<td headers="achrom" class="gt_row gt_left">#B9B9B9</td></tr>
    <tr><td headers="name" class="gt_row gt_left">orange_200</td>
<td headers="normal" class="gt_row gt_left">#FFCC80</td>
<td headers="deutan" class="gt_row gt_left">#DEDE7C</td>
<td headers="protan" class="gt_row gt_left">#D5D57F</td>
<td headers="tritan" class="gt_row gt_left">#FFC4C4</td>
<td headers="achrom" class="gt_row gt_left">#8E8E8E</td></tr>
    <tr><td headers="name" class="gt_row gt_left">orange_300</td>
<td headers="normal" class="gt_row gt_left">#FFB74D</td>
<td headers="deutan" class="gt_row gt_left">#D2D245</td>
<td headers="protan" class="gt_row gt_left">#C5C54B</td>
<td headers="tritan" class="gt_row gt_left">#FFADAD</td>
<td headers="achrom" class="gt_row gt_left">#666666</td></tr>
    <tr><td headers="name" class="gt_row gt_left">orange_400</td>
<td headers="normal" class="gt_row gt_left">#FFA726</td>
<td headers="deutan" class="gt_row gt_left">#C9C907</td>
<td headers="protan" class="gt_row gt_left">#BABA22</td>
<td headers="tritan" class="gt_row gt_left">#FF9D9D</td>
<td headers="achrom" class="gt_row gt_left">#4E4E4E</td></tr>
    <tr><td headers="name" class="gt_row gt_left">orange_500</td>
<td headers="normal" class="gt_row gt_left">#FF9800</td>
<td headers="deutan" class="gt_row gt_left">#C2C200</td>
<td headers="protan" class="gt_row gt_left">#AFAF00</td>
<td headers="tritan" class="gt_row gt_left">#FF8E8E</td>
<td headers="achrom" class="gt_row gt_left">#404040</td></tr>
    <tr><td headers="name" class="gt_row gt_left">orange_600</td>
<td headers="normal" class="gt_row gt_left">#FB8C00</td>
<td headers="deutan" class="gt_row gt_left">#BABA00</td>
<td headers="protan" class="gt_row gt_left">#A6A600</td>
<td headers="tritan" class="gt_row gt_left">#FE8383</td>
<td headers="achrom" class="gt_row gt_left">#3C3C3C</td></tr>
    <tr><td headers="name" class="gt_row gt_left">orange_700</td>
<td headers="normal" class="gt_row gt_left">#F57C00</td>
<td headers="deutan" class="gt_row gt_left">#B0B000</td>
<td headers="protan" class="gt_row gt_left">#999900</td>
<td headers="tritan" class="gt_row gt_left">#F87474</td>
<td headers="achrom" class="gt_row gt_left">#373737</td></tr>
    <tr><td headers="name" class="gt_row gt_left">orange_800</td>
<td headers="normal" class="gt_row gt_left">#EF6C00</td>
<td headers="deutan" class="gt_row gt_left">#A6A600</td>
<td headers="protan" class="gt_row gt_left">#8E8E00</td>
<td headers="tritan" class="gt_row gt_left">#F16565</td>
<td headers="achrom" class="gt_row gt_left">#313131</td></tr>
    <tr><td headers="name" class="gt_row gt_left">orange_900</td>
<td headers="normal" class="gt_row gt_left">#E65100</td>
<td headers="deutan" class="gt_row gt_left">#989800</td>
<td headers="protan" class="gt_row gt_left">#7C7C00</td>
<td headers="tritan" class="gt_row gt_left">#E74B4B</td>
<td headers="achrom" class="gt_row gt_left">#292929</td></tr>
    <tr><td headers="name" class="gt_row gt_left">orange_a100</td>
<td headers="normal" class="gt_row gt_left">#FFD180</td>
<td headers="deutan" class="gt_row gt_left">#E1E17D</td>
<td headers="protan" class="gt_row gt_left">#D9D97F</td>
<td headers="tritan" class="gt_row gt_left">#FFC9C9</td>
<td headers="achrom" class="gt_row gt_left">#8F8F8F</td></tr>
    <tr><td headers="name" class="gt_row gt_left">orange_a200</td>
<td headers="normal" class="gt_row gt_left">#FFAB40</td>
<td headers="deutan" class="gt_row gt_left">#CBCB34</td>
<td headers="protan" class="gt_row gt_left">#BCBC3E</td>
<td headers="tritan" class="gt_row gt_left">#FFA2A2</td>
<td headers="achrom" class="gt_row gt_left">#5C5C5C</td></tr>
    <tr><td headers="name" class="gt_row gt_left">orange_a400</td>
<td headers="normal" class="gt_row gt_left">#FF9100</td>
<td headers="deutan" class="gt_row gt_left">#BEBE00</td>
<td headers="protan" class="gt_row gt_left">#AAAA00</td>
<td headers="tritan" class="gt_row gt_left">#FF8888</td>
<td headers="achrom" class="gt_row gt_left">#3E3E3E</td></tr>
    <tr><td headers="name" class="gt_row gt_left">orange_a700</td>
<td headers="normal" class="gt_row gt_left">#FF6D00</td>
<td headers="deutan" class="gt_row gt_left">#AFAF00</td>
<td headers="protan" class="gt_row gt_left">#949400</td>
<td headers="tritan" class="gt_row gt_left">#FF6666</td>
<td headers="achrom" class="gt_row gt_left">#343434</td></tr>
    <tr><td headers="name" class="gt_row gt_left">deep_orange_50</td>
<td headers="normal" class="gt_row gt_left">#FBE9E7</td>
<td headers="deutan" class="gt_row gt_left">#EFEFE6</td>
<td headers="protan" class="gt_row gt_left">#ECECE6</td>
<td headers="tritan" class="gt_row gt_left">#FBE8E8</td>
<td headers="achrom" class="gt_row gt_left">#E7E7E7</td></tr>
    <tr><td headers="name" class="gt_row gt_left">deep_orange_100</td>
<td headers="normal" class="gt_row gt_left">#FFCCBC</td>
<td headers="deutan" class="gt_row gt_left">#DEDEBA</td>
<td headers="protan" class="gt_row gt_left">#D5D5BB</td>
<td headers="tritan" class="gt_row gt_left">#FFCACA</td>
<td headers="achrom" class="gt_row gt_left">#BFBFBF</td></tr>
    <tr><td headers="name" class="gt_row gt_left">deep_orange_200</td>
<td headers="normal" class="gt_row gt_left">#FFAB91</td>
<td headers="deutan" class="gt_row gt_left">#CBCB8D</td>
<td headers="protan" class="gt_row gt_left">#BCBC90</td>
<td headers="tritan" class="gt_row gt_left">#FFA8A8</td>
<td headers="achrom" class="gt_row gt_left">#969697</td></tr>
    <tr><td headers="name" class="gt_row gt_left">deep_orange_300</td>
<td headers="normal" class="gt_row gt_left">#FF8A65</td>
<td headers="deutan" class="gt_row gt_left">#BBBB5C</td>
<td headers="protan" class="gt_row gt_left">#A6A663</td>
<td headers="tritan" class="gt_row gt_left">#FF8686</td>
<td headers="achrom" class="gt_row gt_left">#6F6F6F</td></tr>
    <tr><td headers="name" class="gt_row gt_left">deep_orange_400</td>
<td headers="normal" class="gt_row gt_left">#FF7043</td>
<td headers="deutan" class="gt_row gt_left">#B0B032</td>
<td headers="protan" class="gt_row gt_left">#959540</td>
<td headers="tritan" class="gt_row gt_left">#FF6B6B</td>
<td headers="achrom" class="gt_row gt_left">#525252</td></tr>
    <tr><td headers="name" class="gt_row gt_left">deep_orange_500</td>
<td headers="normal" class="gt_row gt_left">#FF5722</td>
<td headers="deutan" class="gt_row gt_left">#A8A800</td>
<td headers="protan" class="gt_row gt_left">#88881C</td>
<td headers="tritan" class="gt_row gt_left">#FF5252</td>
<td headers="achrom" class="gt_row gt_left">#393939</td></tr>
    <tr><td headers="name" class="gt_row gt_left">deep_orange_600</td>
<td headers="normal" class="gt_row gt_left">#F4511E</td>
<td headers="deutan" class="gt_row gt_left">#A0A000</td>
<td headers="protan" class="gt_row gt_left">#818118</td>
<td headers="tritan" class="gt_row gt_left">#F54C4C</td>
<td headers="achrom" class="gt_row gt_left">#353535</td></tr>
    <tr><td headers="name" class="gt_row gt_left">deep_orange_700</td>
<td headers="normal" class="gt_row gt_left">#E64A19</td>
<td headers="deutan" class="gt_row gt_left">#969600</td>
<td headers="protan" class="gt_row gt_left">#797912</td>
<td headers="tritan" class="gt_row gt_left">#E64545</td>
<td headers="achrom" class="gt_row gt_left">#303030</td></tr>
    <tr><td headers="name" class="gt_row gt_left">deep_orange_800</td>
<td headers="normal" class="gt_row gt_left">#D84315</td>
<td headers="deutan" class="gt_row gt_left">#8C8C00</td>
<td headers="protan" class="gt_row gt_left">#70700E</td>
<td headers="tritan" class="gt_row gt_left">#D83F3F</td>
<td headers="achrom" class="gt_row gt_left">#2B2B2B</td></tr>
    <tr><td headers="name" class="gt_row gt_left">deep_orange_900</td>
<td headers="normal" class="gt_row gt_left">#BF360C</td>
<td headers="deutan" class="gt_row gt_left">#7A7A00</td>
<td headers="protan" class="gt_row gt_left">#606004</td>
<td headers="tritan" class="gt_row gt_left">#BF3232</td>
<td headers="achrom" class="gt_row gt_left">#222222</td></tr>
    <tr><td headers="name" class="gt_row gt_left">deep_orange_a100</td>
<td headers="normal" class="gt_row gt_left">#FF9E80</td>
<td headers="deutan" class="gt_row gt_left">#C5C57A</td>
<td headers="protan" class="gt_row gt_left">#B3B37F</td>
<td headers="tritan" class="gt_row gt_left">#FF9A9A</td>
<td headers="achrom" class="gt_row gt_left">#878787</td></tr>
    <tr><td headers="name" class="gt_row gt_left">deep_orange_a200</td>
<td headers="normal" class="gt_row gt_left">#FF6E40</td>
<td headers="deutan" class="gt_row gt_left">#B0B02E</td>
<td headers="protan" class="gt_row gt_left">#94943D</td>
<td headers="tritan" class="gt_row gt_left">#FF6969</td>
<td headers="achrom" class="gt_row gt_left">#4F4F4F</td></tr>
    <tr><td headers="name" class="gt_row gt_left">deep_orange_a400</td>
<td headers="normal" class="gt_row gt_left">#FF3D00</td>
<td headers="deutan" class="gt_row gt_left">#A2A200</td>
<td headers="protan" class="gt_row gt_left">#7E7E00</td>
<td headers="tritan" class="gt_row gt_left">#FF3838</td>
<td headers="achrom" class="gt_row gt_left">#292929</td></tr>
    <tr><td headers="name" class="gt_row gt_left">deep_orange_a700</td>
<td headers="normal" class="gt_row gt_left">#DD2C00</td>
<td headers="deutan" class="gt_row gt_left">#8A8A00</td>
<td headers="protan" class="gt_row gt_left">#6A6A00</td>
<td headers="tritan" class="gt_row gt_left">#DD2828</td>
<td headers="achrom" class="gt_row gt_left">#212121</td></tr>
    <tr><td headers="name" class="gt_row gt_left">brown_50</td>
<td headers="normal" class="gt_row gt_left">#EFEBE9</td>
<td headers="deutan" class="gt_row gt_left">#ECECE8</td>
<td headers="protan" class="gt_row gt_left">#EBEBE8</td>
<td headers="tritan" class="gt_row gt_left">#EFEAEA</td>
<td headers="achrom" class="gt_row gt_left">#E9E9E9</td></tr>
    <tr><td headers="name" class="gt_row gt_left">brown_100</td>
<td headers="normal" class="gt_row gt_left">#D7CCC8</td>
<td headers="deutan" class="gt_row gt_left">#CFCFC7</td>
<td headers="protan" class="gt_row gt_left">#CDCDC7</td>
<td headers="tritan" class="gt_row gt_left">#D7CBCB</td>
<td headers="achrom" class="gt_row gt_left">#C8C8C8</td></tr>
    <tr><td headers="name" class="gt_row gt_left">brown_200</td>
<td headers="normal" class="gt_row gt_left">#BCAAA4</td>
<td headers="deutan" class="gt_row gt_left">#B0B0A3</td>
<td headers="protan" class="gt_row gt_left">#ADADA3</td>
<td headers="tritan" class="gt_row gt_left">#BCA9A9</td>
<td headers="achrom" class="gt_row gt_left">#A5A5A5</td></tr>
    <tr><td headers="name" class="gt_row gt_left">brown_300</td>
<td headers="normal" class="gt_row gt_left">#A1887F</td>
<td headers="deutan" class="gt_row gt_left">#90907E</td>
<td headers="protan" class="gt_row gt_left">#8C8C7E</td>
<td headers="tritan" class="gt_row gt_left">#A18686</td>
<td headers="achrom" class="gt_row gt_left">#808080</td></tr>
    <tr><td headers="name" class="gt_row gt_left">brown_400</td>
<td headers="normal" class="gt_row gt_left">#8D6E63</td>
<td headers="deutan" class="gt_row gt_left">#797961</td>
<td headers="protan" class="gt_row gt_left">#747462</td>
<td headers="tritan" class="gt_row gt_left">#8D6C6C</td>
<td headers="achrom" class="gt_row gt_left">#656565</td></tr>
    <tr><td headers="name" class="gt_row gt_left">brown_500</td>
<td headers="normal" class="gt_row gt_left">#795548</td>
<td headers="deutan" class="gt_row gt_left">#626246</td>
<td headers="protan" class="gt_row gt_left">#5C5C47</td>
<td headers="tritan" class="gt_row gt_left">#795353</td>
<td headers="achrom" class="gt_row gt_left">#4A4A4A</td></tr>
    <tr><td headers="name" class="gt_row gt_left">brown_600</td>
<td headers="normal" class="gt_row gt_left">#6D4C41</td>
<td headers="deutan" class="gt_row gt_left">#58583F</td>
<td headers="protan" class="gt_row gt_left">#525240</td>
<td headers="tritan" class="gt_row gt_left">#6D4A4A</td>
<td headers="achrom" class="gt_row gt_left">#434343</td></tr>
    <tr><td headers="name" class="gt_row gt_left">brown_700</td>
<td headers="normal" class="gt_row gt_left">#5D4037</td>
<td headers="deutan" class="gt_row gt_left">#4B4B35</td>
<td headers="protan" class="gt_row gt_left">#454536</td>
<td headers="tritan" class="gt_row gt_left">#5D3E3E</td>
<td headers="achrom" class="gt_row gt_left">#383838</td></tr>
    <tr><td headers="name" class="gt_row gt_left">brown_800</td>
<td headers="normal" class="gt_row gt_left">#4E342E</td>
<td headers="deutan" class="gt_row gt_left">#3E3E2C</td>
<td headers="protan" class="gt_row gt_left">#39392D</td>
<td headers="tritan" class="gt_row gt_left">#4E3333</td>
<td headers="achrom" class="gt_row gt_left">#2F2F2F</td></tr>
    <tr><td headers="name" class="gt_row gt_left">brown_900</td>
<td headers="normal" class="gt_row gt_left">#3E2723</td>
<td headers="deutan" class="gt_row gt_left">#2F2F22</td>
<td headers="protan" class="gt_row gt_left">#2B2B22</td>
<td headers="tritan" class="gt_row gt_left">#3E2626</td>
<td headers="achrom" class="gt_row gt_left">#242424</td></tr>
    <tr><td headers="name" class="gt_row gt_left">gray_50</td>
<td headers="normal" class="gt_row gt_left">#FAFAFA</td>
<td headers="deutan" class="gt_row gt_left">#FAF9FA</td>
<td headers="protan" class="gt_row gt_left">#FAF9FA</td>
<td headers="tritan" class="gt_row gt_left">#F9FAF9</td>
<td headers="achrom" class="gt_row gt_left">#F9F9FA</td></tr>
    <tr><td headers="name" class="gt_row gt_left">gray_100</td>
<td headers="normal" class="gt_row gt_left">#F5F5F5</td>
<td headers="deutan" class="gt_row gt_left">#F5F4F5</td>
<td headers="protan" class="gt_row gt_left">#F5F4F5</td>
<td headers="tritan" class="gt_row gt_left">#F4F5F4</td>
<td headers="achrom" class="gt_row gt_left">#F4F4F5</td></tr>
    <tr><td headers="name" class="gt_row gt_left">gray_200</td>
<td headers="normal" class="gt_row gt_left">#EEEEEE</td>
<td headers="deutan" class="gt_row gt_left">#EEEDEE</td>
<td headers="protan" class="gt_row gt_left">#EEEDEE</td>
<td headers="tritan" class="gt_row gt_left">#EDEEED</td>
<td headers="achrom" class="gt_row gt_left">#EDEDEE</td></tr>
    <tr><td headers="name" class="gt_row gt_left">gray_300</td>
<td headers="normal" class="gt_row gt_left">#E0E0E0</td>
<td headers="deutan" class="gt_row gt_left">#E0DFE0</td>
<td headers="protan" class="gt_row gt_left">#E0DFE0</td>
<td headers="tritan" class="gt_row gt_left">#DFE0DF</td>
<td headers="achrom" class="gt_row gt_left">#DFDFE0</td></tr>
    <tr><td headers="name" class="gt_row gt_left">gray_400</td>
<td headers="normal" class="gt_row gt_left">#BDBDBD</td>
<td headers="deutan" class="gt_row gt_left">#BDBCBD</td>
<td headers="protan" class="gt_row gt_left">#BDBCBD</td>
<td headers="tritan" class="gt_row gt_left">#BCBDBC</td>
<td headers="achrom" class="gt_row gt_left">#BCBCBD</td></tr>
    <tr><td headers="name" class="gt_row gt_left">gray_500</td>
<td headers="normal" class="gt_row gt_left">#9E9E9E</td>
<td headers="deutan" class="gt_row gt_left">#9E9D9E</td>
<td headers="protan" class="gt_row gt_left">#9E9D9E</td>
<td headers="tritan" class="gt_row gt_left">#9D9E9D</td>
<td headers="achrom" class="gt_row gt_left">#9D9D9E</td></tr>
    <tr><td headers="name" class="gt_row gt_left">gray_600</td>
<td headers="normal" class="gt_row gt_left">#757575</td>
<td headers="deutan" class="gt_row gt_left">#757475</td>
<td headers="protan" class="gt_row gt_left">#757475</td>
<td headers="tritan" class="gt_row gt_left">#747574</td>
<td headers="achrom" class="gt_row gt_left">#747475</td></tr>
    <tr><td headers="name" class="gt_row gt_left">gray_700</td>
<td headers="normal" class="gt_row gt_left">#616161</td>
<td headers="deutan" class="gt_row gt_left">#616061</td>
<td headers="protan" class="gt_row gt_left">#616061</td>
<td headers="tritan" class="gt_row gt_left">#606160</td>
<td headers="achrom" class="gt_row gt_left">#606061</td></tr>
    <tr><td headers="name" class="gt_row gt_left">gray_800</td>
<td headers="normal" class="gt_row gt_left">#424242</td>
<td headers="deutan" class="gt_row gt_left">#424142</td>
<td headers="protan" class="gt_row gt_left">#424142</td>
<td headers="tritan" class="gt_row gt_left">#414241</td>
<td headers="achrom" class="gt_row gt_left">#414142</td></tr>
    <tr><td headers="name" class="gt_row gt_left">gray_900</td>
<td headers="normal" class="gt_row gt_left">#212121</td>
<td headers="deutan" class="gt_row gt_left">#212021</td>
<td headers="protan" class="gt_row gt_left">#212021</td>
<td headers="tritan" class="gt_row gt_left">#202120</td>
<td headers="achrom" class="gt_row gt_left">#202021</td></tr>
    <tr><td headers="name" class="gt_row gt_left">blue_gray_50</td>
<td headers="normal" class="gt_row gt_left">#ECEFF1</td>
<td headers="deutan" class="gt_row gt_left">#EEEEF1</td>
<td headers="protan" class="gt_row gt_left">#EEEEF1</td>
<td headers="tritan" class="gt_row gt_left">#EBEFEF</td>
<td headers="achrom" class="gt_row gt_left">#F0F0F0</td></tr>
    <tr><td headers="name" class="gt_row gt_left">blue_gray_100</td>
<td headers="normal" class="gt_row gt_left">#CFD8DC</td>
<td headers="deutan" class="gt_row gt_left">#D5D5DC</td>
<td headers="protan" class="gt_row gt_left">#D6D6DC</td>
<td headers="tritan" class="gt_row gt_left">#CED8D8</td>
<td headers="achrom" class="gt_row gt_left">#DBDBDB</td></tr>
    <tr><td headers="name" class="gt_row gt_left">blue_gray_200</td>
<td headers="normal" class="gt_row gt_left">#B0BEC5</td>
<td headers="deutan" class="gt_row gt_left">#B9B9C5</td>
<td headers="protan" class="gt_row gt_left">#BBBBC5</td>
<td headers="tritan" class="gt_row gt_left">#AEBEBE</td>
<td headers="achrom" class="gt_row gt_left">#C3C3C3</td></tr>
    <tr><td headers="name" class="gt_row gt_left">blue_gray_300</td>
<td headers="normal" class="gt_row gt_left">#90A4AE</td>
<td headers="deutan" class="gt_row gt_left">#9D9DAE</td>
<td headers="protan" class="gt_row gt_left">#A0A0AE</td>
<td headers="tritan" class="gt_row gt_left">#8EA5A5</td>
<td headers="achrom" class="gt_row gt_left">#ACACAC</td></tr>
    <tr><td headers="name" class="gt_row gt_left">blue_gray_400</td>
<td headers="normal" class="gt_row gt_left">#78909C</td>
<td headers="deutan" class="gt_row gt_left">#88889C</td>
<td headers="protan" class="gt_row gt_left">#8C8C9C</td>
<td headers="tritan" class="gt_row gt_left">#759191</td>
<td headers="achrom" class="gt_row gt_left">#9A9A9A</td></tr>
    <tr><td headers="name" class="gt_row gt_left">blue_gray_500</td>
<td headers="normal" class="gt_row gt_left">#607D8B</td>
<td headers="deutan" class="gt_row gt_left">#74748B</td>
<td headers="protan" class="gt_row gt_left">#78788B</td>
<td headers="tritan" class="gt_row gt_left">#5D7E7E</td>
<td headers="achrom" class="gt_row gt_left">#888888</td></tr>
    <tr><td headers="name" class="gt_row gt_left">blue_gray_600</td>
<td headers="normal" class="gt_row gt_left">#546E7A</td>
<td headers="deutan" class="gt_row gt_left">#66667A</td>
<td headers="protan" class="gt_row gt_left">#6A6A7A</td>
<td headers="tritan" class="gt_row gt_left">#516F6F</td>
<td headers="achrom" class="gt_row gt_left">#787878</td></tr>
    <tr><td headers="name" class="gt_row gt_left">blue_gray_700</td>
<td headers="normal" class="gt_row gt_left">#455A64</td>
<td headers="deutan" class="gt_row gt_left">#535364</td>
<td headers="protan" class="gt_row gt_left">#565664</td>
<td headers="tritan" class="gt_row gt_left">#435B5B</td>
<td headers="achrom" class="gt_row gt_left">#626262</td></tr>
    <tr><td headers="name" class="gt_row gt_left">blue_gray_800</td>
<td headers="normal" class="gt_row gt_left">#37474F</td>
<td headers="deutan" class="gt_row gt_left">#42424F</td>
<td headers="protan" class="gt_row gt_left">#44444F</td>
<td headers="tritan" class="gt_row gt_left">#354848</td>
<td headers="achrom" class="gt_row gt_left">#4D4D4D</td></tr>
    <tr><td headers="name" class="gt_row gt_left">blue_gray_900</td>
<td headers="normal" class="gt_row gt_left">#263238</td>
<td headers="deutan" class="gt_row gt_left">#2E2E38</td>
<td headers="protan" class="gt_row gt_left">#303038</td>
<td headers="tritan" class="gt_row gt_left">#243232</td>
<td headers="achrom" class="gt_row gt_left">#373737</td></tr>
    <tr><td headers="name" class="gt_row gt_left">black</td>
<td headers="normal" class="gt_row gt_left">#000000</td>
<td headers="deutan" class="gt_row gt_left">#000000</td>
<td headers="protan" class="gt_row gt_left">#000000</td>
<td headers="tritan" class="gt_row gt_left">#000000</td>
<td headers="achrom" class="gt_row gt_left">#000000</td></tr>
    <tr><td headers="name" class="gt_row gt_left">white</td>
<td headers="normal" class="gt_row gt_left">#FFFFFF</td>
<td headers="deutan" class="gt_row gt_left">#FFFFFF</td>
<td headers="protan" class="gt_row gt_left">#FFFFFF</td>
<td headers="tritan" class="gt_row gt_left">#FFFFFF</td>
<td headers="achrom" class="gt_row gt_left">#FFFFFF</td></tr>
  </tbody>
  
  
</table>
</div>

``` r
gt(select(mat2014_done, name, ends_with("furthest")))
```

<div id="vhpcviwffu" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>html {
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, 'Helvetica Neue', 'Fira Sans', 'Droid Sans', Arial, sans-serif;
}

#vhpcviwffu .gt_table {
  display: table;
  border-collapse: collapse;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}

#vhpcviwffu .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#vhpcviwffu .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}

#vhpcviwffu .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#vhpcviwffu .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 0;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#vhpcviwffu .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#vhpcviwffu .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#vhpcviwffu .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#vhpcviwffu .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#vhpcviwffu .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#vhpcviwffu .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#vhpcviwffu .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#vhpcviwffu .gt_group_heading {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  text-align: left;
}

#vhpcviwffu .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}

#vhpcviwffu .gt_from_md > :first-child {
  margin-top: 0;
}

#vhpcviwffu .gt_from_md > :last-child {
  margin-bottom: 0;
}

#vhpcviwffu .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#vhpcviwffu .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
}

#vhpcviwffu .gt_stub_row_group {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
  vertical-align: top;
}

#vhpcviwffu .gt_row_group_first td {
  border-top-width: 2px;
}

#vhpcviwffu .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#vhpcviwffu .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}

#vhpcviwffu .gt_first_summary_row.thick {
  border-top-width: 2px;
}

#vhpcviwffu .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#vhpcviwffu .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#vhpcviwffu .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#vhpcviwffu .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#vhpcviwffu .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#vhpcviwffu .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#vhpcviwffu .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-left: 4px;
  padding-right: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#vhpcviwffu .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#vhpcviwffu .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#vhpcviwffu .gt_left {
  text-align: left;
}

#vhpcviwffu .gt_center {
  text-align: center;
}

#vhpcviwffu .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#vhpcviwffu .gt_font_normal {
  font-weight: normal;
}

#vhpcviwffu .gt_font_bold {
  font-weight: bold;
}

#vhpcviwffu .gt_font_italic {
  font-style: italic;
}

#vhpcviwffu .gt_super {
  font-size: 65%;
}

#vhpcviwffu .gt_footnote_marks {
  font-style: italic;
  font-weight: normal;
  font-size: 75%;
  vertical-align: 0.4em;
}

#vhpcviwffu .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}

#vhpcviwffu .gt_indent_1 {
  text-indent: 5px;
}

#vhpcviwffu .gt_indent_2 {
  text-indent: 10px;
}

#vhpcviwffu .gt_indent_3 {
  text-indent: 15px;
}

#vhpcviwffu .gt_indent_4 {
  text-indent: 20px;
}

#vhpcviwffu .gt_indent_5 {
  text-indent: 25px;
}
</style>
<table class="gt_table">
  
  <thead class="gt_col_headings">
    <tr>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="name">name</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="normal_furthest">normal_furthest</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="deutan_furthest">deutan_furthest</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="protan_furthest">protan_furthest</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="tritan_furthest">tritan_furthest</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="achrom_furthest">achrom_furthest</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td headers="name" class="gt_row gt_left">red_50</td>
<td headers="normal_furthest" class="gt_row gt_left">brown_300</td>
<td headers="deutan_furthest" class="gt_row gt_left">blue_gray_400</td>
<td headers="protan_furthest" class="gt_row gt_left">teal_500</td>
<td headers="tritan_furthest" class="gt_row gt_left">deep_purple_300</td>
<td headers="achrom_furthest" class="gt_row gt_left">red_a100</td></tr>
    <tr><td headers="name" class="gt_row gt_left">red_100</td>
<td headers="normal_furthest" class="gt_row gt_left">brown_300</td>
<td headers="deutan_furthest" class="gt_row gt_left">teal_400</td>
<td headers="protan_furthest" class="gt_row gt_left">pink_a200</td>
<td headers="tritan_furthest" class="gt_row gt_left">deep_purple_300</td>
<td headers="achrom_furthest" class="gt_row gt_left">light_green_a200</td></tr>
    <tr><td headers="name" class="gt_row gt_left">red_200</td>
<td headers="normal_furthest" class="gt_row gt_left">brown_300</td>
<td headers="deutan_furthest" class="gt_row gt_left">gray_600</td>
<td headers="protan_furthest" class="gt_row gt_left">gray_600</td>
<td headers="tritan_furthest" class="gt_row gt_left">deep_purple_300</td>
<td headers="achrom_furthest" class="gt_row gt_left">yellow_500</td></tr>
    <tr><td headers="name" class="gt_row gt_left">red_300</td>
<td headers="normal_furthest" class="gt_row gt_left">gray_500</td>
<td headers="deutan_furthest" class="gt_row gt_left">gray_600</td>
<td headers="protan_furthest" class="gt_row gt_left">gray_600</td>
<td headers="tritan_furthest" class="gt_row gt_left">deep_purple_300</td>
<td headers="achrom_furthest" class="gt_row gt_left">pink_400</td></tr>
    <tr><td headers="name" class="gt_row gt_left">red_400</td>
<td headers="normal_furthest" class="gt_row gt_left">brown_200</td>
<td headers="deutan_furthest" class="gt_row gt_left">gray_600</td>
<td headers="protan_furthest" class="gt_row gt_left">gray_700</td>
<td headers="tritan_furthest" class="gt_row gt_left">lime_800</td>
<td headers="achrom_furthest" class="gt_row gt_left">red_a100</td></tr>
    <tr><td headers="name" class="gt_row gt_left">red_500</td>
<td headers="normal_furthest" class="gt_row gt_left">brown_200</td>
<td headers="deutan_furthest" class="gt_row gt_left">gray_500</td>
<td headers="protan_furthest" class="gt_row gt_left">gray_600</td>
<td headers="tritan_furthest" class="gt_row gt_left">lime_800</td>
<td headers="achrom_furthest" class="gt_row gt_left">light_green_a200</td></tr>
    <tr><td headers="name" class="gt_row gt_left">red_600</td>
<td headers="normal_furthest" class="gt_row gt_left">gray_600</td>
<td headers="deutan_furthest" class="gt_row gt_left">gray_600</td>
<td headers="protan_furthest" class="gt_row gt_left">gray_600</td>
<td headers="tritan_furthest" class="gt_row gt_left">lime_800</td>
<td headers="achrom_furthest" class="gt_row gt_left">teal_600</td></tr>
    <tr><td headers="name" class="gt_row gt_left">red_700</td>
<td headers="normal_furthest" class="gt_row gt_left">gray_600</td>
<td headers="deutan_furthest" class="gt_row gt_left">gray_600</td>
<td headers="protan_furthest" class="gt_row gt_left">pink_400</td>
<td headers="tritan_furthest" class="gt_row gt_left">brown_300</td>
<td headers="achrom_furthest" class="gt_row gt_left">pink_400</td></tr>
    <tr><td headers="name" class="gt_row gt_left">red_800</td>
<td headers="normal_furthest" class="gt_row gt_left">gray_600</td>
<td headers="deutan_furthest" class="gt_row gt_left">gray_600</td>
<td headers="protan_furthest" class="gt_row gt_left">pink_400</td>
<td headers="tritan_furthest" class="gt_row gt_left">brown_300</td>
<td headers="achrom_furthest" class="gt_row gt_left">gray_600</td></tr>
    <tr><td headers="name" class="gt_row gt_left">red_900</td>
<td headers="normal_furthest" class="gt_row gt_left">gray_600</td>
<td headers="deutan_furthest" class="gt_row gt_left">gray_600</td>
<td headers="protan_furthest" class="gt_row gt_left">gray_600</td>
<td headers="tritan_furthest" class="gt_row gt_left">deep_purple_400</td>
<td headers="achrom_furthest" class="gt_row gt_left">deep_orange_300</td></tr>
    <tr><td headers="name" class="gt_row gt_left">red_a100</td>
<td headers="normal_furthest" class="gt_row gt_left">brown_300</td>
<td headers="deutan_furthest" class="gt_row gt_left">teal_a700</td>
<td headers="protan_furthest" class="gt_row gt_left">pink_400</td>
<td headers="tritan_furthest" class="gt_row gt_left">brown_300</td>
<td headers="achrom_furthest" class="gt_row gt_left">yellow_500</td></tr>
    <tr><td headers="name" class="gt_row gt_left">red_a200</td>
<td headers="normal_furthest" class="gt_row gt_left">brown_200</td>
<td headers="deutan_furthest" class="gt_row gt_left">gray_500</td>
<td headers="protan_furthest" class="gt_row gt_left">gray_600</td>
<td headers="tritan_furthest" class="gt_row gt_left">lime_800</td>
<td headers="achrom_furthest" class="gt_row gt_left">green_a400</td></tr>
    <tr><td headers="name" class="gt_row gt_left">red_a400</td>
<td headers="normal_furthest" class="gt_row gt_left">brown_200</td>
<td headers="deutan_furthest" class="gt_row gt_left">gray_600</td>
<td headers="protan_furthest" class="gt_row gt_left">gray_600</td>
<td headers="tritan_furthest" class="gt_row gt_left">brown_300</td>
<td headers="achrom_furthest" class="gt_row gt_left">brown_300</td></tr>
    <tr><td headers="name" class="gt_row gt_left">red_a700</td>
<td headers="normal_furthest" class="gt_row gt_left">brown_300</td>
<td headers="deutan_furthest" class="gt_row gt_left">gray_600</td>
<td headers="protan_furthest" class="gt_row gt_left">teal_700</td>
<td headers="tritan_furthest" class="gt_row gt_left">brown_300</td>
<td headers="achrom_furthest" class="gt_row gt_left">teal_700</td></tr>
    <tr><td headers="name" class="gt_row gt_left">pink_50</td>
<td headers="normal_furthest" class="gt_row gt_left">brown_300</td>
<td headers="deutan_furthest" class="gt_row gt_left">blue_gray_400</td>
<td headers="protan_furthest" class="gt_row gt_left">teal_500</td>
<td headers="tritan_furthest" class="gt_row gt_left">deep_purple_300</td>
<td headers="achrom_furthest" class="gt_row gt_left">red_a100</td></tr>
    <tr><td headers="name" class="gt_row gt_left">pink_100</td>
<td headers="normal_furthest" class="gt_row gt_left">brown_300</td>
<td headers="deutan_furthest" class="gt_row gt_left">teal_500</td>
<td headers="protan_furthest" class="gt_row gt_left">blue_gray_500</td>
<td headers="tritan_furthest" class="gt_row gt_left">deep_purple_300</td>
<td headers="achrom_furthest" class="gt_row gt_left">light_green_a200</td></tr>
    <tr><td headers="name" class="gt_row gt_left">pink_200</td>
<td headers="normal_furthest" class="gt_row gt_left">gray_500</td>
<td headers="deutan_furthest" class="gt_row gt_left">gray_600</td>
<td headers="protan_furthest" class="gt_row gt_left">pink_400</td>
<td headers="tritan_furthest" class="gt_row gt_left">deep_purple_300</td>
<td headers="achrom_furthest" class="gt_row gt_left">lime_a200</td></tr>
    <tr><td headers="name" class="gt_row gt_left">pink_300</td>
<td headers="normal_furthest" class="gt_row gt_left">gray_500</td>
<td headers="deutan_furthest" class="gt_row gt_left">gray_600</td>
<td headers="protan_furthest" class="gt_row gt_left">teal_500</td>
<td headers="tritan_furthest" class="gt_row gt_left">deep_purple_300</td>
<td headers="achrom_furthest" class="gt_row gt_left">brown_400</td></tr>
    <tr><td headers="name" class="gt_row gt_left">pink_400</td>
<td headers="normal_furthest" class="gt_row gt_left">brown_200</td>
<td headers="deutan_furthest" class="gt_row gt_left">gray_600</td>
<td headers="protan_furthest" class="gt_row gt_left">brown_300</td>
<td headers="tritan_furthest" class="gt_row gt_left">lime_800</td>
<td headers="achrom_furthest" class="gt_row gt_left">red_300</td></tr>
    <tr><td headers="name" class="gt_row gt_left">pink_500</td>
<td headers="normal_furthest" class="gt_row gt_left">gray_600</td>
<td headers="deutan_furthest" class="gt_row gt_left">gray_600</td>
<td headers="protan_furthest" class="gt_row gt_left">teal_400</td>
<td headers="tritan_furthest" class="gt_row gt_left">brown_300</td>
<td headers="achrom_furthest" class="gt_row gt_left">lime_300</td></tr>
    <tr><td headers="name" class="gt_row gt_left">pink_600</td>
<td headers="normal_furthest" class="gt_row gt_left">gray_600</td>
<td headers="deutan_furthest" class="gt_row gt_left">gray_600</td>
<td headers="protan_furthest" class="gt_row gt_left">brown_300</td>
<td headers="tritan_furthest" class="gt_row gt_left">brown_300</td>
<td headers="achrom_furthest" class="gt_row gt_left">lime_300</td></tr>
    <tr><td headers="name" class="gt_row gt_left">pink_700</td>
<td headers="normal_furthest" class="gt_row gt_left">gray_600</td>
<td headers="deutan_furthest" class="gt_row gt_left">gray_600</td>
<td headers="protan_furthest" class="gt_row gt_left">brown_300</td>
<td headers="tritan_furthest" class="gt_row gt_left">deep_purple_400</td>
<td headers="achrom_furthest" class="gt_row gt_left">green_a400</td></tr>
    <tr><td headers="name" class="gt_row gt_left">pink_800</td>
<td headers="normal_furthest" class="gt_row gt_left">gray_600</td>
<td headers="deutan_furthest" class="gt_row gt_left">gray_600</td>
<td headers="protan_furthest" class="gt_row gt_left">teal_500</td>
<td headers="tritan_furthest" class="gt_row gt_left">deep_purple_400</td>
<td headers="achrom_furthest" class="gt_row gt_left">red_a100</td></tr>
    <tr><td headers="name" class="gt_row gt_left">pink_900</td>
<td headers="normal_furthest" class="gt_row gt_left">gray_600</td>
<td headers="deutan_furthest" class="gt_row gt_left">pink_600</td>
<td headers="protan_furthest" class="gt_row gt_left">pink_a200</td>
<td headers="tritan_furthest" class="gt_row gt_left">deep_purple_400</td>
<td headers="achrom_furthest" class="gt_row gt_left">brown_300</td></tr>
    <tr><td headers="name" class="gt_row gt_left">pink_a100</td>
<td headers="normal_furthest" class="gt_row gt_left">gray_500</td>
<td headers="deutan_furthest" class="gt_row gt_left">gray_600</td>
<td headers="protan_furthest" class="gt_row gt_left">teal_600</td>
<td headers="tritan_furthest" class="gt_row gt_left">brown_300</td>
<td headers="achrom_furthest" class="gt_row gt_left">amber_300</td></tr>
    <tr><td headers="name" class="gt_row gt_left">pink_a200</td>
<td headers="normal_furthest" class="gt_row gt_left">brown_200</td>
<td headers="deutan_furthest" class="gt_row gt_left">gray_600</td>
<td headers="protan_furthest" class="gt_row gt_left">teal_500</td>
<td headers="tritan_furthest" class="gt_row gt_left">lime_800</td>
<td headers="achrom_furthest" class="gt_row gt_left">deep_orange_300</td></tr>
    <tr><td headers="name" class="gt_row gt_left">pink_a400</td>
<td headers="normal_furthest" class="gt_row gt_left">gray_600</td>
<td headers="deutan_furthest" class="gt_row gt_left">gray_600</td>
<td headers="protan_furthest" class="gt_row gt_left">pink_a200</td>
<td headers="tritan_furthest" class="gt_row gt_left">brown_300</td>
<td headers="achrom_furthest" class="gt_row gt_left">green_a400</td></tr>
    <tr><td headers="name" class="gt_row gt_left">pink_a700</td>
<td headers="normal_furthest" class="gt_row gt_left">gray_600</td>
<td headers="deutan_furthest" class="gt_row gt_left">gray_600</td>
<td headers="protan_furthest" class="gt_row gt_left">brown_300</td>
<td headers="tritan_furthest" class="gt_row gt_left">deep_purple_400</td>
<td headers="achrom_furthest" class="gt_row gt_left">lime_300</td></tr>
    <tr><td headers="name" class="gt_row gt_left">purple_50</td>
<td headers="normal_furthest" class="gt_row gt_left">brown_300</td>
<td headers="deutan_furthest" class="gt_row gt_left">blue_gray_400</td>
<td headers="protan_furthest" class="gt_row gt_left">teal_500</td>
<td headers="tritan_furthest" class="gt_row gt_left">deep_purple_300</td>
<td headers="achrom_furthest" class="gt_row gt_left">green_a400</td></tr>
    <tr><td headers="name" class="gt_row gt_left">purple_100</td>
<td headers="normal_furthest" class="gt_row gt_left">brown_300</td>
<td headers="deutan_furthest" class="gt_row gt_left">teal_500</td>
<td headers="protan_furthest" class="gt_row gt_left">pink_a200</td>
<td headers="tritan_furthest" class="gt_row gt_left">lime_900</td>
<td headers="achrom_furthest" class="gt_row gt_left">red_a100</td></tr>
    <tr><td headers="name" class="gt_row gt_left">purple_200</td>
<td headers="normal_furthest" class="gt_row gt_left">gray_500</td>
<td headers="deutan_furthest" class="gt_row gt_left">gray_600</td>
<td headers="protan_furthest" class="gt_row gt_left">teal_500</td>
<td headers="tritan_furthest" class="gt_row gt_left">lime_900</td>
<td headers="achrom_furthest" class="gt_row gt_left">light_green_a200</td></tr>
    <tr><td headers="name" class="gt_row gt_left">purple_300</td>
<td headers="normal_furthest" class="gt_row gt_left">gray_500</td>
<td headers="deutan_furthest" class="gt_row gt_left">gray_500</td>
<td headers="protan_furthest" class="gt_row gt_left">gray_500</td>
<td headers="tritan_furthest" class="gt_row gt_left">deep_purple_300</td>
<td headers="achrom_furthest" class="gt_row gt_left">light_green_400</td></tr>
    <tr><td headers="name" class="gt_row gt_left">purple_400</td>
<td headers="normal_furthest" class="gt_row gt_left">blue_gray_200</td>
<td headers="deutan_furthest" class="gt_row gt_left">gray_500</td>
<td headers="protan_furthest" class="gt_row gt_left">pink_a100</td>
<td headers="tritan_furthest" class="gt_row gt_left">deep_purple_300</td>
<td headers="achrom_furthest" class="gt_row gt_left">lime_a200</td></tr>
    <tr><td headers="name" class="gt_row gt_left">purple_500</td>
<td headers="normal_furthest" class="gt_row gt_left">blue_gray_300</td>
<td headers="deutan_furthest" class="gt_row gt_left">teal_a700</td>
<td headers="protan_furthest" class="gt_row gt_left">pink_a200</td>
<td headers="tritan_furthest" class="gt_row gt_left">gray_600</td>
<td headers="achrom_furthest" class="gt_row gt_left">amber_300</td></tr>
    <tr><td headers="name" class="gt_row gt_left">purple_600</td>
<td headers="normal_furthest" class="gt_row gt_left">gray_600</td>
<td headers="deutan_furthest" class="gt_row gt_left">teal_300</td>
<td headers="protan_furthest" class="gt_row gt_left">pink_400</td>
<td headers="tritan_furthest" class="gt_row gt_left">gray_600</td>
<td headers="achrom_furthest" class="gt_row gt_left">teal_700</td></tr>
    <tr><td headers="name" class="gt_row gt_left">purple_700</td>
<td headers="normal_furthest" class="gt_row gt_left">gray_600</td>
<td headers="deutan_furthest" class="gt_row gt_left">gray_600</td>
<td headers="protan_furthest" class="gt_row gt_left">pink_400</td>
<td headers="tritan_furthest" class="gt_row gt_left">gray_600</td>
<td headers="achrom_furthest" class="gt_row gt_left">green_a700</td></tr>
    <tr><td headers="name" class="gt_row gt_left">purple_800</td>
<td headers="normal_furthest" class="gt_row gt_left">blue_gray_400</td>
<td headers="deutan_furthest" class="gt_row gt_left">gray_600</td>
<td headers="protan_furthest" class="gt_row gt_left">gray_600</td>
<td headers="tritan_furthest" class="gt_row gt_left">gray_600</td>
<td headers="achrom_furthest" class="gt_row gt_left">brown_400</td></tr>
    <tr><td headers="name" class="gt_row gt_left">purple_900</td>
<td headers="normal_furthest" class="gt_row gt_left">blue_gray_400</td>
<td headers="deutan_furthest" class="gt_row gt_left">gray_600</td>
<td headers="protan_furthest" class="gt_row gt_left">teal_800</td>
<td headers="tritan_furthest" class="gt_row gt_left">gray_600</td>
<td headers="achrom_furthest" class="gt_row gt_left">yellow_500</td></tr>
    <tr><td headers="name" class="gt_row gt_left">purple_a100</td>
<td headers="normal_furthest" class="gt_row gt_left">gray_500</td>
<td headers="deutan_furthest" class="gt_row gt_left">teal_400</td>
<td headers="protan_furthest" class="gt_row gt_left">teal_400</td>
<td headers="tritan_furthest" class="gt_row gt_left">deep_purple_300</td>
<td headers="achrom_furthest" class="gt_row gt_left">green_a400</td></tr>
    <tr><td headers="name" class="gt_row gt_left">purple_a200</td>
<td headers="normal_furthest" class="gt_row gt_left">blue_gray_200</td>
<td headers="deutan_furthest" class="gt_row gt_left">gray_500</td>
<td headers="protan_furthest" class="gt_row gt_left">pink_200</td>
<td headers="tritan_furthest" class="gt_row gt_left">deep_purple_300</td>
<td headers="achrom_furthest" class="gt_row gt_left">red_a100</td></tr>
    <tr><td headers="name" class="gt_row gt_left">purple_a400</td>
<td headers="normal_furthest" class="gt_row gt_left">blue_gray_200</td>
<td headers="deutan_furthest" class="gt_row gt_left">gray_500</td>
<td headers="protan_furthest" class="gt_row gt_left">blue_gray_300</td>
<td headers="tritan_furthest" class="gt_row gt_left">deep_purple_300</td>
<td headers="achrom_furthest" class="gt_row gt_left">red_a100</td></tr>
    <tr><td headers="name" class="gt_row gt_left">purple_a700</td>
<td headers="normal_furthest" class="gt_row gt_left">blue_gray_200</td>
<td headers="deutan_furthest" class="gt_row gt_left">teal_a700</td>
<td headers="protan_furthest" class="gt_row gt_left">pink_300</td>
<td headers="tritan_furthest" class="gt_row gt_left">deep_purple_300</td>
<td headers="achrom_furthest" class="gt_row gt_left">green_a400</td></tr>
    <tr><td headers="name" class="gt_row gt_left">deep_purple_50</td>
<td headers="normal_furthest" class="gt_row gt_left">blue_gray_400</td>
<td headers="deutan_furthest" class="gt_row gt_left">blue_gray_400</td>
<td headers="protan_furthest" class="gt_row gt_left">teal_500</td>
<td headers="tritan_furthest" class="gt_row gt_left">deep_purple_300</td>
<td headers="achrom_furthest" class="gt_row gt_left">green_a400</td></tr>
    <tr><td headers="name" class="gt_row gt_left">deep_purple_100</td>
<td headers="normal_furthest" class="gt_row gt_left">blue_gray_400</td>
<td headers="deutan_furthest" class="gt_row gt_left">teal_500</td>
<td headers="protan_furthest" class="gt_row gt_left">blue_gray_500</td>
<td headers="tritan_furthest" class="gt_row gt_left">deep_purple_300</td>
<td headers="achrom_furthest" class="gt_row gt_left">red_a100</td></tr>
    <tr><td headers="name" class="gt_row gt_left">deep_purple_200</td>
<td headers="normal_furthest" class="gt_row gt_left">brown_300</td>
<td headers="deutan_furthest" class="gt_row gt_left">gray_600</td>
<td headers="protan_furthest" class="gt_row gt_left">teal_500</td>
<td headers="tritan_furthest" class="gt_row gt_left">deep_purple_400</td>
<td headers="achrom_furthest" class="gt_row gt_left">pink_a200</td></tr>
    <tr><td headers="name" class="gt_row gt_left">deep_purple_300</td>
<td headers="normal_furthest" class="gt_row gt_left">gray_500</td>
<td headers="deutan_furthest" class="gt_row gt_left">gray_500</td>
<td headers="protan_furthest" class="gt_row gt_left">gray_500</td>
<td headers="tritan_furthest" class="gt_row gt_left">blue_gray_600</td>
<td headers="achrom_furthest" class="gt_row gt_left">pink_400</td></tr>
    <tr><td headers="name" class="gt_row gt_left">deep_purple_400</td>
<td headers="normal_furthest" class="gt_row gt_left">blue_gray_300</td>
<td headers="deutan_furthest" class="gt_row gt_left">teal_a700</td>
<td headers="protan_furthest" class="gt_row gt_left">pink_a200</td>
<td headers="tritan_furthest" class="gt_row gt_left">blue_gray_400</td>
<td headers="achrom_furthest" class="gt_row gt_left">lime_400</td></tr>
    <tr><td headers="name" class="gt_row gt_left">deep_purple_500</td>
<td headers="normal_furthest" class="gt_row gt_left">gray_600</td>
<td headers="deutan_furthest" class="gt_row gt_left">gray_600</td>
<td headers="protan_furthest" class="gt_row gt_left">pink_400</td>
<td headers="tritan_furthest" class="gt_row gt_left">deep_purple_300</td>
<td headers="achrom_furthest" class="gt_row gt_left">lime_a200</td></tr>
    <tr><td headers="name" class="gt_row gt_left">deep_purple_600</td>
<td headers="normal_furthest" class="gt_row gt_left">gray_600</td>
<td headers="deutan_furthest" class="gt_row gt_left">gray_600</td>
<td headers="protan_furthest" class="gt_row gt_left">pink_400</td>
<td headers="tritan_furthest" class="gt_row gt_left">deep_purple_300</td>
<td headers="achrom_furthest" class="gt_row gt_left">amber_300</td></tr>
    <tr><td headers="name" class="gt_row gt_left">deep_purple_700</td>
<td headers="normal_furthest" class="gt_row gt_left">blue_gray_400</td>
<td headers="deutan_furthest" class="gt_row gt_left">gray_600</td>
<td headers="protan_furthest" class="gt_row gt_left">gray_600</td>
<td headers="tritan_furthest" class="gt_row gt_left">gray_600</td>
<td headers="achrom_furthest" class="gt_row gt_left">yellow_500</td></tr>
    <tr><td headers="name" class="gt_row gt_left">deep_purple_800</td>
<td headers="normal_furthest" class="gt_row gt_left">blue_gray_400</td>
<td headers="deutan_furthest" class="gt_row gt_left">gray_600</td>
<td headers="protan_furthest" class="gt_row gt_left">gray_600</td>
<td headers="tritan_furthest" class="gt_row gt_left">gray_600</td>
<td headers="achrom_furthest" class="gt_row gt_left">amber_a200</td></tr>
    <tr><td headers="name" class="gt_row gt_left">deep_purple_900</td>
<td headers="normal_furthest" class="gt_row gt_left">blue_gray_400</td>
<td headers="deutan_furthest" class="gt_row gt_left">teal_500</td>
<td headers="protan_furthest" class="gt_row gt_left">gray_700</td>
<td headers="tritan_furthest" class="gt_row gt_left">deep_purple_400</td>
<td headers="achrom_furthest" class="gt_row gt_left">brown_400</td></tr>
    <tr><td headers="name" class="gt_row gt_left">deep_purple_a100</td>
<td headers="normal_furthest" class="gt_row gt_left">gray_500</td>
<td headers="deutan_furthest" class="gt_row gt_left">gray_500</td>
<td headers="protan_furthest" class="gt_row gt_left">teal_400</td>
<td headers="tritan_furthest" class="gt_row gt_left">deep_purple_400</td>
<td headers="achrom_furthest" class="gt_row gt_left">green_a400</td></tr>
    <tr><td headers="name" class="gt_row gt_left">deep_purple_a200</td>
<td headers="normal_furthest" class="gt_row gt_left">blue_gray_200</td>
<td headers="deutan_furthest" class="gt_row gt_left">teal_a700</td>
<td headers="protan_furthest" class="gt_row gt_left">pink_400</td>
<td headers="tritan_furthest" class="gt_row gt_left">deep_purple_300</td>
<td headers="achrom_furthest" class="gt_row gt_left">green_a400</td></tr>
    <tr><td headers="name" class="gt_row gt_left">deep_purple_a400</td>
<td headers="normal_furthest" class="gt_row gt_left">blue_gray_300</td>
<td headers="deutan_furthest" class="gt_row gt_left">gray_600</td>
<td headers="protan_furthest" class="gt_row gt_left">blue_gray_400</td>
<td headers="tritan_furthest" class="gt_row gt_left">gray_600</td>
<td headers="achrom_furthest" class="gt_row gt_left">green_a400</td></tr>
    <tr><td headers="name" class="gt_row gt_left">deep_purple_a700</td>
<td headers="normal_furthest" class="gt_row gt_left">blue_gray_400</td>
<td headers="deutan_furthest" class="gt_row gt_left">teal_400</td>
<td headers="protan_furthest" class="gt_row gt_left">pink_400</td>
<td headers="tritan_furthest" class="gt_row gt_left">gray_600</td>
<td headers="achrom_furthest" class="gt_row gt_left">brown_300</td></tr>
    <tr><td headers="name" class="gt_row gt_left">indigo_50</td>
<td headers="normal_furthest" class="gt_row gt_left">blue_gray_400</td>
<td headers="deutan_furthest" class="gt_row gt_left">blue_gray_400</td>
<td headers="protan_furthest" class="gt_row gt_left">teal_500</td>
<td headers="tritan_furthest" class="gt_row gt_left">deep_purple_300</td>
<td headers="achrom_furthest" class="gt_row gt_left">green_a400</td></tr>
    <tr><td headers="name" class="gt_row gt_left">indigo_100</td>
<td headers="normal_furthest" class="gt_row gt_left">blue_gray_400</td>
<td headers="deutan_furthest" class="gt_row gt_left">teal_500</td>
<td headers="protan_furthest" class="gt_row gt_left">blue_gray_500</td>
<td headers="tritan_furthest" class="gt_row gt_left">light_green_800</td>
<td headers="achrom_furthest" class="gt_row gt_left">red_a100</td></tr>
    <tr><td headers="name" class="gt_row gt_left">indigo_200</td>
<td headers="normal_furthest" class="gt_row gt_left">gray_600</td>
<td headers="deutan_furthest" class="gt_row gt_left">gray_600</td>
<td headers="protan_furthest" class="gt_row gt_left">teal_500</td>
<td headers="tritan_furthest" class="gt_row gt_left">gray_600</td>
<td headers="achrom_furthest" class="gt_row gt_left">pink_a200</td></tr>
    <tr><td headers="name" class="gt_row gt_left">indigo_300</td>
<td headers="normal_furthest" class="gt_row gt_left">gray_500</td>
<td headers="deutan_furthest" class="gt_row gt_left">gray_500</td>
<td headers="protan_furthest" class="gt_row gt_left">teal_400</td>
<td headers="tritan_furthest" class="gt_row gt_left">deep_purple_400</td>
<td headers="achrom_furthest" class="gt_row gt_left">pink_400</td></tr>
    <tr><td headers="name" class="gt_row gt_left">indigo_400</td>
<td headers="normal_furthest" class="gt_row gt_left">blue_gray_300</td>
<td headers="deutan_furthest" class="gt_row gt_left">teal_a700</td>
<td headers="protan_furthest" class="gt_row gt_left">teal_500</td>
<td headers="tritan_furthest" class="gt_row gt_left">gray_600</td>
<td headers="achrom_furthest" class="gt_row gt_left">lime_400</td></tr>
    <tr><td headers="name" class="gt_row gt_left">indigo_500</td>
<td headers="normal_furthest" class="gt_row gt_left">gray_600</td>
<td headers="deutan_furthest" class="gt_row gt_left">gray_600</td>
<td headers="protan_furthest" class="gt_row gt_left">pink_a200</td>
<td headers="tritan_furthest" class="gt_row gt_left">gray_600</td>
<td headers="achrom_furthest" class="gt_row gt_left">deep_orange_300</td></tr>
    <tr><td headers="name" class="gt_row gt_left">indigo_600</td>
<td headers="normal_furthest" class="gt_row gt_left">gray_600</td>
<td headers="deutan_furthest" class="gt_row gt_left">gray_600</td>
<td headers="protan_furthest" class="gt_row gt_left">pink_400</td>
<td headers="tritan_furthest" class="gt_row gt_left">gray_600</td>
<td headers="achrom_furthest" class="gt_row gt_left">teal_700</td></tr>
    <tr><td headers="name" class="gt_row gt_left">indigo_700</td>
<td headers="normal_furthest" class="gt_row gt_left">gray_600</td>
<td headers="deutan_furthest" class="gt_row gt_left">gray_600</td>
<td headers="protan_furthest" class="gt_row gt_left">pink_400</td>
<td headers="tritan_furthest" class="gt_row gt_left">deep_purple_400</td>
<td headers="achrom_furthest" class="gt_row gt_left">amber_a200</td></tr>
    <tr><td headers="name" class="gt_row gt_left">indigo_800</td>
<td headers="normal_furthest" class="gt_row gt_left">blue_gray_400</td>
<td headers="deutan_furthest" class="gt_row gt_left">gray_600</td>
<td headers="protan_furthest" class="gt_row gt_left">gray_600</td>
<td headers="tritan_furthest" class="gt_row gt_left">deep_purple_400</td>
<td headers="achrom_furthest" class="gt_row gt_left">brown_400</td></tr>
    <tr><td headers="name" class="gt_row gt_left">indigo_900</td>
<td headers="normal_furthest" class="gt_row gt_left">gray_700</td>
<td headers="deutan_furthest" class="gt_row gt_left">teal_500</td>
<td headers="protan_furthest" class="gt_row gt_left">teal_800</td>
<td headers="tritan_furthest" class="gt_row gt_left">deep_purple_400</td>
<td headers="achrom_furthest" class="gt_row gt_left">pink_400</td></tr>
    <tr><td headers="name" class="gt_row gt_left">indigo_a100</td>
<td headers="normal_furthest" class="gt_row gt_left">gray_500</td>
<td headers="deutan_furthest" class="gt_row gt_left">gray_500</td>
<td headers="protan_furthest" class="gt_row gt_left">teal_400</td>
<td headers="tritan_furthest" class="gt_row gt_left">blue_gray_600</td>
<td headers="achrom_furthest" class="gt_row gt_left">green_a400</td></tr>
    <tr><td headers="name" class="gt_row gt_left">indigo_a200</td>
<td headers="normal_furthest" class="gt_row gt_left">blue_gray_200</td>
<td headers="deutan_furthest" class="gt_row gt_left">teal_a700</td>
<td headers="protan_furthest" class="gt_row gt_left">pink_a100</td>
<td headers="tritan_furthest" class="gt_row gt_left">gray_600</td>
<td headers="achrom_furthest" class="gt_row gt_left">green_a400</td></tr>
    <tr><td headers="name" class="gt_row gt_left">indigo_a400</td>
<td headers="normal_furthest" class="gt_row gt_left">blue_gray_300</td>
<td headers="deutan_furthest" class="gt_row gt_left">teal_300</td>
<td headers="protan_furthest" class="gt_row gt_left">pink_400</td>
<td headers="tritan_furthest" class="gt_row gt_left">gray_600</td>
<td headers="achrom_furthest" class="gt_row gt_left">green_a400</td></tr>
    <tr><td headers="name" class="gt_row gt_left">indigo_a700</td>
<td headers="normal_furthest" class="gt_row gt_left">blue_gray_300</td>
<td headers="deutan_furthest" class="gt_row gt_left">gray_600</td>
<td headers="protan_furthest" class="gt_row gt_left">pink_300</td>
<td headers="tritan_furthest" class="gt_row gt_left">gray_600</td>
<td headers="achrom_furthest" class="gt_row gt_left">green_a400</td></tr>
    <tr><td headers="name" class="gt_row gt_left">blue_50</td>
<td headers="normal_furthest" class="gt_row gt_left">blue_gray_400</td>
<td headers="deutan_furthest" class="gt_row gt_left">blue_gray_400</td>
<td headers="protan_furthest" class="gt_row gt_left">teal_500</td>
<td headers="tritan_furthest" class="gt_row gt_left">deep_purple_300</td>
<td headers="achrom_furthest" class="gt_row gt_left">deep_purple_900</td></tr>
    <tr><td headers="name" class="gt_row gt_left">blue_100</td>
<td headers="normal_furthest" class="gt_row gt_left">blue_gray_500</td>
<td headers="deutan_furthest" class="gt_row gt_left">teal_500</td>
<td headers="protan_furthest" class="gt_row gt_left">teal_500</td>
<td headers="tritan_furthest" class="gt_row gt_left">light_green_800</td>
<td headers="achrom_furthest" class="gt_row gt_left">green_a400</td></tr>
    <tr><td headers="name" class="gt_row gt_left">blue_200</td>
<td headers="normal_furthest" class="gt_row gt_left">blue_gray_400</td>
<td headers="deutan_furthest" class="gt_row gt_left">teal_400</td>
<td headers="protan_furthest" class="gt_row gt_left">teal_500</td>
<td headers="tritan_furthest" class="gt_row gt_left">blue_gray_500</td>
<td headers="achrom_furthest" class="gt_row gt_left">green_a400</td></tr>
    <tr><td headers="name" class="gt_row gt_left">blue_300</td>
<td headers="normal_furthest" class="gt_row gt_left">blue_gray_400</td>
<td headers="deutan_furthest" class="gt_row gt_left">gray_500</td>
<td headers="protan_furthest" class="gt_row gt_left">teal_500</td>
<td headers="tritan_furthest" class="gt_row gt_left">blue_gray_500</td>
<td headers="achrom_furthest" class="gt_row gt_left">red_a100</td></tr>
    <tr><td headers="name" class="gt_row gt_left">blue_400</td>
<td headers="normal_furthest" class="gt_row gt_left">gray_500</td>
<td headers="deutan_furthest" class="gt_row gt_left">gray_500</td>
<td headers="protan_furthest" class="gt_row gt_left">teal_500</td>
<td headers="tritan_furthest" class="gt_row gt_left">blue_gray_500</td>
<td headers="achrom_furthest" class="gt_row gt_left">red_a100</td></tr>
    <tr><td headers="name" class="gt_row gt_left">blue_500</td>
<td headers="normal_furthest" class="gt_row gt_left">gray_500</td>
<td headers="deutan_furthest" class="gt_row gt_left">teal_a700</td>
<td headers="protan_furthest" class="gt_row gt_left">teal_400</td>
<td headers="tritan_furthest" class="gt_row gt_left">blue_gray_500</td>
<td headers="achrom_furthest" class="gt_row gt_left">red_a100</td></tr>
    <tr><td headers="name" class="gt_row gt_left">blue_600</td>
<td headers="normal_furthest" class="gt_row gt_left">gray_500</td>
<td headers="deutan_furthest" class="gt_row gt_left">teal_a700</td>
<td headers="protan_furthest" class="gt_row gt_left">teal_400</td>
<td headers="tritan_furthest" class="gt_row gt_left">gray_600</td>
<td headers="achrom_furthest" class="gt_row gt_left">brown_300</td></tr>
    <tr><td headers="name" class="gt_row gt_left">blue_700</td>
<td headers="normal_furthest" class="gt_row gt_left">blue_gray_300</td>
<td headers="deutan_furthest" class="gt_row gt_left">teal_a700</td>
<td headers="protan_furthest" class="gt_row gt_left">pink_200</td>
<td headers="tritan_furthest" class="gt_row gt_left">gray_600</td>
<td headers="achrom_furthest" class="gt_row gt_left">yellow_400</td></tr>
    <tr><td headers="name" class="gt_row gt_left">blue_800</td>
<td headers="normal_furthest" class="gt_row gt_left">blue_gray_300</td>
<td headers="deutan_furthest" class="gt_row gt_left">gray_600</td>
<td headers="protan_furthest" class="gt_row gt_left">pink_a200</td>
<td headers="tritan_furthest" class="gt_row gt_left">gray_600</td>
<td headers="achrom_furthest" class="gt_row gt_left">lime_400</td></tr>
    <tr><td headers="name" class="gt_row gt_left">blue_900</td>
<td headers="normal_furthest" class="gt_row gt_left">gray_600</td>
<td headers="deutan_furthest" class="gt_row gt_left">gray_600</td>
<td headers="protan_furthest" class="gt_row gt_left">pink_400</td>
<td headers="tritan_furthest" class="gt_row gt_left">gray_600</td>
<td headers="achrom_furthest" class="gt_row gt_left">green_a700</td></tr>
    <tr><td headers="name" class="gt_row gt_left">blue_a100</td>
<td headers="normal_furthest" class="gt_row gt_left">blue_gray_400</td>
<td headers="deutan_furthest" class="gt_row gt_left">gray_500</td>
<td headers="protan_furthest" class="gt_row gt_left">teal_500</td>
<td headers="tritan_furthest" class="gt_row gt_left">blue_gray_500</td>
<td headers="achrom_furthest" class="gt_row gt_left">green_a400</td></tr>
    <tr><td headers="name" class="gt_row gt_left">blue_a200</td>
<td headers="normal_furthest" class="gt_row gt_left">gray_500</td>
<td headers="deutan_furthest" class="gt_row gt_left">teal_a700</td>
<td headers="protan_furthest" class="gt_row gt_left">teal_400</td>
<td headers="tritan_furthest" class="gt_row gt_left">gray_600</td>
<td headers="achrom_furthest" class="gt_row gt_left">green_a400</td></tr>
    <tr><td headers="name" class="gt_row gt_left">blue_a400</td>
<td headers="normal_furthest" class="gt_row gt_left">blue_gray_200</td>
<td headers="deutan_furthest" class="gt_row gt_left">teal_a700</td>
<td headers="protan_furthest" class="gt_row gt_left">pink_200</td>
<td headers="tritan_furthest" class="gt_row gt_left">gray_600</td>
<td headers="achrom_furthest" class="gt_row gt_left">green_a400</td></tr>
    <tr><td headers="name" class="gt_row gt_left">blue_a700</td>
<td headers="normal_furthest" class="gt_row gt_left">blue_gray_300</td>
<td headers="deutan_furthest" class="gt_row gt_left">teal_300</td>
<td headers="protan_furthest" class="gt_row gt_left">blue_gray_300</td>
<td headers="tritan_furthest" class="gt_row gt_left">gray_600</td>
<td headers="achrom_furthest" class="gt_row gt_left">green_a400</td></tr>
    <tr><td headers="name" class="gt_row gt_left">light_blue_50</td>
<td headers="normal_furthest" class="gt_row gt_left">blue_gray_400</td>
<td headers="deutan_furthest" class="gt_row gt_left">blue_gray_400</td>
<td headers="protan_furthest" class="gt_row gt_left">teal_500</td>
<td headers="tritan_furthest" class="gt_row gt_left">blue_gray_400</td>
<td headers="achrom_furthest" class="gt_row gt_left">deep_purple_900</td></tr>
    <tr><td headers="name" class="gt_row gt_left">light_blue_100</td>
<td headers="normal_furthest" class="gt_row gt_left">blue_gray_400</td>
<td headers="deutan_furthest" class="gt_row gt_left">teal_500</td>
<td headers="protan_furthest" class="gt_row gt_left">teal_500</td>
<td headers="tritan_furthest" class="gt_row gt_left">green_700</td>
<td headers="achrom_furthest" class="gt_row gt_left">teal_500</td></tr>
    <tr><td headers="name" class="gt_row gt_left">light_blue_200</td>
<td headers="normal_furthest" class="gt_row gt_left">blue_gray_500</td>
<td headers="deutan_furthest" class="gt_row gt_left">teal_400</td>
<td headers="protan_furthest" class="gt_row gt_left">teal_500</td>
<td headers="tritan_furthest" class="gt_row gt_left">light_green_800</td>
<td headers="achrom_furthest" class="gt_row gt_left">green_a400</td></tr>
    <tr><td headers="name" class="gt_row gt_left">light_blue_300</td>
<td headers="normal_furthest" class="gt_row gt_left">blue_gray_400</td>
<td headers="deutan_furthest" class="gt_row gt_left">gray_500</td>
<td headers="protan_furthest" class="gt_row gt_left">teal_500</td>
<td headers="tritan_furthest" class="gt_row gt_left">blue_gray_500</td>
<td headers="achrom_furthest" class="gt_row gt_left">green_a400</td></tr>
    <tr><td headers="name" class="gt_row gt_left">light_blue_400</td>
<td headers="normal_furthest" class="gt_row gt_left">blue_gray_400</td>
<td headers="deutan_furthest" class="gt_row gt_left">gray_500</td>
<td headers="protan_furthest" class="gt_row gt_left">teal_500</td>
<td headers="tritan_furthest" class="gt_row gt_left">blue_gray_500</td>
<td headers="achrom_furthest" class="gt_row gt_left">red_a100</td></tr>
    <tr><td headers="name" class="gt_row gt_left">light_blue_500</td>
<td headers="normal_furthest" class="gt_row gt_left">gray_500</td>
<td headers="deutan_furthest" class="gt_row gt_left">gray_500</td>
<td headers="protan_furthest" class="gt_row gt_left">teal_500</td>
<td headers="tritan_furthest" class="gt_row gt_left">blue_gray_500</td>
<td headers="achrom_furthest" class="gt_row gt_left">red_a100</td></tr>
    <tr><td headers="name" class="gt_row gt_left">light_blue_600</td>
<td headers="normal_furthest" class="gt_row gt_left">gray_500</td>
<td headers="deutan_furthest" class="gt_row gt_left">gray_500</td>
<td headers="protan_furthest" class="gt_row gt_left">teal_400</td>
<td headers="tritan_furthest" class="gt_row gt_left">blue_gray_500</td>
<td headers="achrom_furthest" class="gt_row gt_left">brown_300</td></tr>
    <tr><td headers="name" class="gt_row gt_left">light_blue_700</td>
<td headers="normal_furthest" class="gt_row gt_left">gray_500</td>
<td headers="deutan_furthest" class="gt_row gt_left">teal_a700</td>
<td headers="protan_furthest" class="gt_row gt_left">gray_500</td>
<td headers="tritan_furthest" class="gt_row gt_left">gray_600</td>
<td headers="achrom_furthest" class="gt_row gt_left">yellow_400</td></tr>
    <tr><td headers="name" class="gt_row gt_left">light_blue_800</td>
<td headers="normal_furthest" class="gt_row gt_left">gray_500</td>
<td headers="deutan_furthest" class="gt_row gt_left">teal_a700</td>
<td headers="protan_furthest" class="gt_row gt_left">teal_400</td>
<td headers="tritan_furthest" class="gt_row gt_left">gray_600</td>
<td headers="achrom_furthest" class="gt_row gt_left">lime_400</td></tr>
    <tr><td headers="name" class="gt_row gt_left">light_blue_900</td>
<td headers="normal_furthest" class="gt_row gt_left">gray_600</td>
<td headers="deutan_furthest" class="gt_row gt_left">gray_600</td>
<td headers="protan_furthest" class="gt_row gt_left">pink_a200</td>
<td headers="tritan_furthest" class="gt_row gt_left">gray_600</td>
<td headers="achrom_furthest" class="gt_row gt_left">orange_300</td></tr>
    <tr><td headers="name" class="gt_row gt_left">light_blue_a100</td>
<td headers="normal_furthest" class="gt_row gt_left">blue_gray_500</td>
<td headers="deutan_furthest" class="gt_row gt_left">teal_400</td>
<td headers="protan_furthest" class="gt_row gt_left">pink_300</td>
<td headers="tritan_furthest" class="gt_row gt_left">light_green_800</td>
<td headers="achrom_furthest" class="gt_row gt_left">teal_500</td></tr>
    <tr><td headers="name" class="gt_row gt_left">light_blue_a200</td>
<td headers="normal_furthest" class="gt_row gt_left">blue_gray_400</td>
<td headers="deutan_furthest" class="gt_row gt_left">gray_500</td>
<td headers="protan_furthest" class="gt_row gt_left">pink_300</td>
<td headers="tritan_furthest" class="gt_row gt_left">blue_gray_500</td>
<td headers="achrom_furthest" class="gt_row gt_left">green_a400</td></tr>
    <tr><td headers="name" class="gt_row gt_left">light_blue_a400</td>
<td headers="normal_furthest" class="gt_row gt_left">gray_500</td>
<td headers="deutan_furthest" class="gt_row gt_left">gray_500</td>
<td headers="protan_furthest" class="gt_row gt_left">teal_400</td>
<td headers="tritan_furthest" class="gt_row gt_left">blue_gray_500</td>
<td headers="achrom_furthest" class="gt_row gt_left">green_a400</td></tr>
    <tr><td headers="name" class="gt_row gt_left">light_blue_a700</td>
<td headers="normal_furthest" class="gt_row gt_left">gray_500</td>
<td headers="deutan_furthest" class="gt_row gt_left">teal_a700</td>
<td headers="protan_furthest" class="gt_row gt_left">teal_400</td>
<td headers="tritan_furthest" class="gt_row gt_left">gray_600</td>
<td headers="achrom_furthest" class="gt_row gt_left">brown_300</td></tr>
    <tr><td headers="name" class="gt_row gt_left">cyan_50</td>
<td headers="normal_furthest" class="gt_row gt_left">blue_gray_400</td>
<td headers="deutan_furthest" class="gt_row gt_left">blue_gray_400</td>
<td headers="protan_furthest" class="gt_row gt_left">teal_500</td>
<td headers="tritan_furthest" class="gt_row gt_left">blue_gray_400</td>
<td headers="achrom_furthest" class="gt_row gt_left">teal_500</td></tr>
    <tr><td headers="name" class="gt_row gt_left">cyan_100</td>
<td headers="normal_furthest" class="gt_row gt_left">blue_gray_400</td>
<td headers="deutan_furthest" class="gt_row gt_left">teal_500</td>
<td headers="protan_furthest" class="gt_row gt_left">teal_500</td>
<td headers="tritan_furthest" class="gt_row gt_left">green_700</td>
<td headers="achrom_furthest" class="gt_row gt_left">green_a400</td></tr>
    <tr><td headers="name" class="gt_row gt_left">cyan_200</td>
<td headers="normal_furthest" class="gt_row gt_left">blue_gray_400</td>
<td headers="deutan_furthest" class="gt_row gt_left">teal_500</td>
<td headers="protan_furthest" class="gt_row gt_left">cyan_800</td>
<td headers="tritan_furthest" class="gt_row gt_left">light_green_800</td>
<td headers="achrom_furthest" class="gt_row gt_left">red_a100</td></tr>
    <tr><td headers="name" class="gt_row gt_left">cyan_300</td>
<td headers="normal_furthest" class="gt_row gt_left">blue_gray_400</td>
<td headers="deutan_furthest" class="gt_row gt_left">teal_400</td>
<td headers="protan_furthest" class="gt_row gt_left">pink_a200</td>
<td headers="tritan_furthest" class="gt_row gt_left">blue_gray_500</td>
<td headers="achrom_furthest" class="gt_row gt_left">brown_300</td></tr>
    <tr><td headers="name" class="gt_row gt_left">cyan_400</td>
<td headers="normal_furthest" class="gt_row gt_left">blue_gray_400</td>
<td headers="deutan_furthest" class="gt_row gt_left">gray_600</td>
<td headers="protan_furthest" class="gt_row gt_left">pink_a200</td>
<td headers="tritan_furthest" class="gt_row gt_left">blue_gray_500</td>
<td headers="achrom_furthest" class="gt_row gt_left">pink_a200</td></tr>
    <tr><td headers="name" class="gt_row gt_left">cyan_500</td>
<td headers="normal_furthest" class="gt_row gt_left">blue_gray_400</td>
<td headers="deutan_furthest" class="gt_row gt_left">gray_500</td>
<td headers="protan_furthest" class="gt_row gt_left">pink_a200</td>
<td headers="tritan_furthest" class="gt_row gt_left">blue_gray_500</td>
<td headers="achrom_furthest" class="gt_row gt_left">light_green_a200</td></tr>
    <tr><td headers="name" class="gt_row gt_left">cyan_600</td>
<td headers="normal_furthest" class="gt_row gt_left">gray_600</td>
<td headers="deutan_furthest" class="gt_row gt_left">gray_500</td>
<td headers="protan_furthest" class="gt_row gt_left">teal_500</td>
<td headers="tritan_furthest" class="gt_row gt_left">blue_gray_500</td>
<td headers="achrom_furthest" class="gt_row gt_left">red_300</td></tr>
    <tr><td headers="name" class="gt_row gt_left">cyan_700</td>
<td headers="normal_furthest" class="gt_row gt_left">gray_600</td>
<td headers="deutan_furthest" class="gt_row gt_left">gray_500</td>
<td headers="protan_furthest" class="gt_row gt_left">brown_300</td>
<td headers="tritan_furthest" class="gt_row gt_left">gray_600</td>
<td headers="achrom_furthest" class="gt_row gt_left">amber_300</td></tr>
    <tr><td headers="name" class="gt_row gt_left">cyan_800</td>
<td headers="normal_furthest" class="gt_row gt_left">gray_600</td>
<td headers="deutan_furthest" class="gt_row gt_left">gray_500</td>
<td headers="protan_furthest" class="gt_row gt_left">brown_300</td>
<td headers="tritan_furthest" class="gt_row gt_left">gray_600</td>
<td headers="achrom_furthest" class="gt_row gt_left">brown_400</td></tr>
    <tr><td headers="name" class="gt_row gt_left">cyan_900</td>
<td headers="normal_furthest" class="gt_row gt_left">gray_600</td>
<td headers="deutan_furthest" class="gt_row gt_left">pink_800</td>
<td headers="protan_furthest" class="gt_row gt_left">teal_500</td>
<td headers="tritan_furthest" class="gt_row gt_left">gray_600</td>
<td headers="achrom_furthest" class="gt_row gt_left">lime_300</td></tr>
    <tr><td headers="name" class="gt_row gt_left">cyan_a100</td>
<td headers="normal_furthest" class="gt_row gt_left">blue_gray_400</td>
<td headers="deutan_furthest" class="gt_row gt_left">cyan_700</td>
<td headers="protan_furthest" class="gt_row gt_left">teal_500</td>
<td headers="tritan_furthest" class="gt_row gt_left">blue_gray_400</td>
<td headers="achrom_furthest" class="gt_row gt_left">deep_purple_900</td></tr>
    <tr><td headers="name" class="gt_row gt_left">cyan_a200</td>
<td headers="normal_furthest" class="gt_row gt_left">blue_gray_400</td>
<td headers="deutan_furthest" class="gt_row gt_left">teal_500</td>
<td headers="protan_furthest" class="gt_row gt_left">teal_500</td>
<td headers="tritan_furthest" class="gt_row gt_left">blue_gray_400</td>
<td headers="achrom_furthest" class="gt_row gt_left">deep_purple_900</td></tr>
    <tr><td headers="name" class="gt_row gt_left">cyan_a400</td>
<td headers="normal_furthest" class="gt_row gt_left">blue_gray_400</td>
<td headers="deutan_furthest" class="gt_row gt_left">teal_400</td>
<td headers="protan_furthest" class="gt_row gt_left">teal_500</td>
<td headers="tritan_furthest" class="gt_row gt_left">blue_gray_400</td>
<td headers="achrom_furthest" class="gt_row gt_left">teal_500</td></tr>
    <tr><td headers="name" class="gt_row gt_left">cyan_a700</td>
<td headers="normal_furthest" class="gt_row gt_left">blue_gray_400</td>
<td headers="deutan_furthest" class="gt_row gt_left">gray_500</td>
<td headers="protan_furthest" class="gt_row gt_left">pink_a200</td>
<td headers="tritan_furthest" class="gt_row gt_left">blue_gray_500</td>
<td headers="achrom_furthest" class="gt_row gt_left">light_green_a200</td></tr>
    <tr><td headers="name" class="gt_row gt_left">teal_50</td>
<td headers="normal_furthest" class="gt_row gt_left">blue_gray_400</td>
<td headers="deutan_furthest" class="gt_row gt_left">blue_gray_400</td>
<td headers="protan_furthest" class="gt_row gt_left">teal_500</td>
<td headers="tritan_furthest" class="gt_row gt_left">deep_purple_300</td>
<td headers="achrom_furthest" class="gt_row gt_left">green_a400</td></tr>
    <tr><td headers="name" class="gt_row gt_left">teal_100</td>
<td headers="normal_furthest" class="gt_row gt_left">blue_gray_400</td>
<td headers="deutan_furthest" class="gt_row gt_left">teal_500</td>
<td headers="protan_furthest" class="gt_row gt_left">pink_a200</td>
<td headers="tritan_furthest" class="gt_row gt_left">light_green_800</td>
<td headers="achrom_furthest" class="gt_row gt_left">pink_a200</td></tr>
    <tr><td headers="name" class="gt_row gt_left">teal_200</td>
<td headers="normal_furthest" class="gt_row gt_left">blue_gray_500</td>
<td headers="deutan_furthest" class="gt_row gt_left">gray_600</td>
<td headers="protan_furthest" class="gt_row gt_left">pink_400</td>
<td headers="tritan_furthest" class="gt_row gt_left">blue_gray_500</td>
<td headers="achrom_furthest" class="gt_row gt_left">pink_400</td></tr>
    <tr><td headers="name" class="gt_row gt_left">teal_300</td>
<td headers="normal_furthest" class="gt_row gt_left">blue_gray_400</td>
<td headers="deutan_furthest" class="gt_row gt_left">gray_600</td>
<td headers="protan_furthest" class="gt_row gt_left">teal_700</td>
<td headers="tritan_furthest" class="gt_row gt_left">blue_gray_600</td>
<td headers="achrom_furthest" class="gt_row gt_left">deep_orange_300</td></tr>
    <tr><td headers="name" class="gt_row gt_left">teal_400</td>
<td headers="normal_furthest" class="gt_row gt_left">blue_gray_400</td>
<td headers="deutan_furthest" class="gt_row gt_left">brown_300</td>
<td headers="protan_furthest" class="gt_row gt_left">pink_400</td>
<td headers="tritan_furthest" class="gt_row gt_left">blue_gray_500</td>
<td headers="achrom_furthest" class="gt_row gt_left">green_a700</td></tr>
    <tr><td headers="name" class="gt_row gt_left">teal_500</td>
<td headers="normal_furthest" class="gt_row gt_left">gray_600</td>
<td headers="deutan_furthest" class="gt_row gt_left">brown_300</td>
<td headers="protan_furthest" class="gt_row gt_left">pink_a200</td>
<td headers="tritan_furthest" class="gt_row gt_left">gray_600</td>
<td headers="achrom_furthest" class="gt_row gt_left">orange_300</td></tr>
    <tr><td headers="name" class="gt_row gt_left">teal_600</td>
<td headers="normal_furthest" class="gt_row gt_left">gray_600</td>
<td headers="deutan_furthest" class="gt_row gt_left">brown_300</td>
<td headers="protan_furthest" class="gt_row gt_left">teal_500</td>
<td headers="tritan_furthest" class="gt_row gt_left">gray_600</td>
<td headers="achrom_furthest" class="gt_row gt_left">lime_a200</td></tr>
    <tr><td headers="name" class="gt_row gt_left">teal_700</td>
<td headers="normal_furthest" class="gt_row gt_left">gray_600</td>
<td headers="deutan_furthest" class="gt_row gt_left">brown_300</td>
<td headers="protan_furthest" class="gt_row gt_left">brown_300</td>
<td headers="tritan_furthest" class="gt_row gt_left">gray_600</td>
<td headers="achrom_furthest" class="gt_row gt_left">red_a100</td></tr>
    <tr><td headers="name" class="gt_row gt_left">teal_800</td>
<td headers="normal_furthest" class="gt_row gt_left">gray_600</td>
<td headers="deutan_furthest" class="gt_row gt_left">pink_700</td>
<td headers="protan_furthest" class="gt_row gt_left">brown_300</td>
<td headers="tritan_furthest" class="gt_row gt_left">gray_600</td>
<td headers="achrom_furthest" class="gt_row gt_left">deep_purple_900</td></tr>
    <tr><td headers="name" class="gt_row gt_left">teal_900</td>
<td headers="normal_furthest" class="gt_row gt_left">gray_600</td>
<td headers="deutan_furthest" class="gt_row gt_left">pink_a700</td>
<td headers="protan_furthest" class="gt_row gt_left">red_400</td>
<td headers="tritan_furthest" class="gt_row gt_left">deep_purple_400</td>
<td headers="achrom_furthest" class="gt_row gt_left">light_green_a200</td></tr>
    <tr><td headers="name" class="gt_row gt_left">teal_a100</td>
<td headers="normal_furthest" class="gt_row gt_left">blue_gray_400</td>
<td headers="deutan_furthest" class="gt_row gt_left">blue_gray_400</td>
<td headers="protan_furthest" class="gt_row gt_left">teal_500</td>
<td headers="tritan_furthest" class="gt_row gt_left">blue_gray_400</td>
<td headers="achrom_furthest" class="gt_row gt_left">red_a100</td></tr>
    <tr><td headers="name" class="gt_row gt_left">teal_a200</td>
<td headers="normal_furthest" class="gt_row gt_left">gray_500</td>
<td headers="deutan_furthest" class="gt_row gt_left">teal_500</td>
<td headers="protan_furthest" class="gt_row gt_left">teal_500</td>
<td headers="tritan_furthest" class="gt_row gt_left">blue_gray_400</td>
<td headers="achrom_furthest" class="gt_row gt_left">brown_300</td></tr>
    <tr><td headers="name" class="gt_row gt_left">teal_a400</td>
<td headers="normal_furthest" class="gt_row gt_left">gray_500</td>
<td headers="deutan_furthest" class="gt_row gt_left">teal_500</td>
<td headers="protan_furthest" class="gt_row gt_left">teal_500</td>
<td headers="tritan_furthest" class="gt_row gt_left">light_green_800</td>
<td headers="achrom_furthest" class="gt_row gt_left">red_300</td></tr>
    <tr><td headers="name" class="gt_row gt_left">teal_a700</td>
<td headers="normal_furthest" class="gt_row gt_left">blue_gray_400</td>
<td headers="deutan_furthest" class="gt_row gt_left">gray_600</td>
<td headers="protan_furthest" class="gt_row gt_left">gray_600</td>
<td headers="tritan_furthest" class="gt_row gt_left">blue_gray_500</td>
<td headers="achrom_furthest" class="gt_row gt_left">amber_300</td></tr>
    <tr><td headers="name" class="gt_row gt_left">green_50</td>
<td headers="normal_furthest" class="gt_row gt_left">blue_gray_400</td>
<td headers="deutan_furthest" class="gt_row gt_left">teal_400</td>
<td headers="protan_furthest" class="gt_row gt_left">teal_500</td>
<td headers="tritan_furthest" class="gt_row gt_left">deep_purple_300</td>
<td headers="achrom_furthest" class="gt_row gt_left">red_a100</td></tr>
    <tr><td headers="name" class="gt_row gt_left">green_100</td>
<td headers="normal_furthest" class="gt_row gt_left">blue_gray_400</td>
<td headers="deutan_furthest" class="gt_row gt_left">teal_400</td>
<td headers="protan_furthest" class="gt_row gt_left">teal_500</td>
<td headers="tritan_furthest" class="gt_row gt_left">deep_purple_300</td>
<td headers="achrom_furthest" class="gt_row gt_left">teal_600</td></tr>
    <tr><td headers="name" class="gt_row gt_left">green_200</td>
<td headers="normal_furthest" class="gt_row gt_left">blue_gray_400</td>
<td headers="deutan_furthest" class="gt_row gt_left">teal_400</td>
<td headers="protan_furthest" class="gt_row gt_left">teal_500</td>
<td headers="tritan_furthest" class="gt_row gt_left">blue_gray_500</td>
<td headers="achrom_furthest" class="gt_row gt_left">deep_orange_300</td></tr>
    <tr><td headers="name" class="gt_row gt_left">green_300</td>
<td headers="normal_furthest" class="gt_row gt_left">gray_500</td>
<td headers="deutan_furthest" class="gt_row gt_left">gray_600</td>
<td headers="protan_furthest" class="gt_row gt_left">teal_600</td>
<td headers="tritan_furthest" class="gt_row gt_left">blue_gray_500</td>
<td headers="achrom_furthest" class="gt_row gt_left">brown_400</td></tr>
    <tr><td headers="name" class="gt_row gt_left">green_400</td>
<td headers="normal_furthest" class="gt_row gt_left">gray_500</td>
<td headers="deutan_furthest" class="gt_row gt_left">gray_600</td>
<td headers="protan_furthest" class="gt_row gt_left">pink_a200</td>
<td headers="tritan_furthest" class="gt_row gt_left">blue_gray_600</td>
<td headers="achrom_furthest" class="gt_row gt_left">pink_400</td></tr>
    <tr><td headers="name" class="gt_row gt_left">green_500</td>
<td headers="normal_furthest" class="gt_row gt_left">gray_500</td>
<td headers="deutan_furthest" class="gt_row gt_left">gray_600</td>
<td headers="protan_furthest" class="gt_row gt_left">pink_a200</td>
<td headers="tritan_furthest" class="gt_row gt_left">gray_600</td>
<td headers="achrom_furthest" class="gt_row gt_left">lime_300</td></tr>
    <tr><td headers="name" class="gt_row gt_left">green_600</td>
<td headers="normal_furthest" class="gt_row gt_left">gray_500</td>
<td headers="deutan_furthest" class="gt_row gt_left">gray_600</td>
<td headers="protan_furthest" class="gt_row gt_left">teal_700</td>
<td headers="tritan_furthest" class="gt_row gt_left">gray_600</td>
<td headers="achrom_furthest" class="gt_row gt_left">red_a100</td></tr>
    <tr><td headers="name" class="gt_row gt_left">green_700</td>
<td headers="normal_furthest" class="gt_row gt_left">gray_600</td>
<td headers="deutan_furthest" class="gt_row gt_left">gray_600</td>
<td headers="protan_furthest" class="gt_row gt_left">gray_600</td>
<td headers="tritan_furthest" class="gt_row gt_left">gray_600</td>
<td headers="achrom_furthest" class="gt_row gt_left">brown_300</td></tr>
    <tr><td headers="name" class="gt_row gt_left">green_800</td>
<td headers="normal_furthest" class="gt_row gt_left">gray_600</td>
<td headers="deutan_furthest" class="gt_row gt_left">gray_600</td>
<td headers="protan_furthest" class="gt_row gt_left">gray_600</td>
<td headers="tritan_furthest" class="gt_row gt_left">gray_600</td>
<td headers="achrom_furthest" class="gt_row gt_left">teal_600</td></tr>
    <tr><td headers="name" class="gt_row gt_left">green_900</td>
<td headers="normal_furthest" class="gt_row gt_left">gray_700</td>
<td headers="deutan_furthest" class="gt_row gt_left">gray_600</td>
<td headers="protan_furthest" class="gt_row gt_left">teal_600</td>
<td headers="tritan_furthest" class="gt_row gt_left">gray_600</td>
<td headers="achrom_furthest" class="gt_row gt_left">lime_a200</td></tr>
    <tr><td headers="name" class="gt_row gt_left">green_a100</td>
<td headers="normal_furthest" class="gt_row gt_left">gray_500</td>
<td headers="deutan_furthest" class="gt_row gt_left">brown_400</td>
<td headers="protan_furthest" class="gt_row gt_left">teal_500</td>
<td headers="tritan_furthest" class="gt_row gt_left">blue_gray_400</td>
<td headers="achrom_furthest" class="gt_row gt_left">light_green_a200</td></tr>
    <tr><td headers="name" class="gt_row gt_left">green_a200</td>
<td headers="normal_furthest" class="gt_row gt_left">gray_500</td>
<td headers="deutan_furthest" class="gt_row gt_left">teal_400</td>
<td headers="protan_furthest" class="gt_row gt_left">teal_500</td>
<td headers="tritan_furthest" class="gt_row gt_left">green_700</td>
<td headers="achrom_furthest" class="gt_row gt_left">lime_400</td></tr>
    <tr><td headers="name" class="gt_row gt_left">green_a400</td>
<td headers="normal_furthest" class="gt_row gt_left">gray_500</td>
<td headers="deutan_furthest" class="gt_row gt_left">teal_a700</td>
<td headers="protan_furthest" class="gt_row gt_left">teal_500</td>
<td headers="tritan_furthest" class="gt_row gt_left">light_green_800</td>
<td headers="achrom_furthest" class="gt_row gt_left">amber_a200</td></tr>
    <tr><td headers="name" class="gt_row gt_left">green_a700</td>
<td headers="normal_furthest" class="gt_row gt_left">gray_500</td>
<td headers="deutan_furthest" class="gt_row gt_left">gray_600</td>
<td headers="protan_furthest" class="gt_row gt_left">teal_500</td>
<td headers="tritan_furthest" class="gt_row gt_left">blue_gray_500</td>
<td headers="achrom_furthest" class="gt_row gt_left">green_a400</td></tr>
    <tr><td headers="name" class="gt_row gt_left">light_green_50</td>
<td headers="normal_furthest" class="gt_row gt_left">blue_gray_400</td>
<td headers="deutan_furthest" class="gt_row gt_left">brown_300</td>
<td headers="protan_furthest" class="gt_row gt_left">teal_500</td>
<td headers="tritan_furthest" class="gt_row gt_left">deep_purple_300</td>
<td headers="achrom_furthest" class="gt_row gt_left">red_a100</td></tr>
    <tr><td headers="name" class="gt_row gt_left">light_green_100</td>
<td headers="normal_furthest" class="gt_row gt_left">blue_gray_400</td>
<td headers="deutan_furthest" class="gt_row gt_left">brown_400</td>
<td headers="protan_furthest" class="gt_row gt_left">teal_500</td>
<td headers="tritan_furthest" class="gt_row gt_left">deep_purple_300</td>
<td headers="achrom_furthest" class="gt_row gt_left">teal_600</td></tr>
    <tr><td headers="name" class="gt_row gt_left">light_green_200</td>
<td headers="normal_furthest" class="gt_row gt_left">gray_500</td>
<td headers="deutan_furthest" class="gt_row gt_left">brown_400</td>
<td headers="protan_furthest" class="gt_row gt_left">teal_500</td>
<td headers="tritan_furthest" class="gt_row gt_left">deep_purple_300</td>
<td headers="achrom_furthest" class="gt_row gt_left">lime_a200</td></tr>
    <tr><td headers="name" class="gt_row gt_left">light_green_300</td>
<td headers="normal_furthest" class="gt_row gt_left">blue_gray_300</td>
<td headers="deutan_furthest" class="gt_row gt_left">teal_a700</td>
<td headers="protan_furthest" class="gt_row gt_left">teal_500</td>
<td headers="tritan_furthest" class="gt_row gt_left">blue_gray_500</td>
<td headers="achrom_furthest" class="gt_row gt_left">brown_400</td></tr>
    <tr><td headers="name" class="gt_row gt_left">light_green_400</td>
<td headers="normal_furthest" class="gt_row gt_left">gray_500</td>
<td headers="deutan_furthest" class="gt_row gt_left">teal_a700</td>
<td headers="protan_furthest" class="gt_row gt_left">teal_500</td>
<td headers="tritan_furthest" class="gt_row gt_left">blue_gray_500</td>
<td headers="achrom_furthest" class="gt_row gt_left">red_300</td></tr>
    <tr><td headers="name" class="gt_row gt_left">light_green_500</td>
<td headers="normal_furthest" class="gt_row gt_left">gray_500</td>
<td headers="deutan_furthest" class="gt_row gt_left">gray_500</td>
<td headers="protan_furthest" class="gt_row gt_left">teal_500</td>
<td headers="tritan_furthest" class="gt_row gt_left">gray_600</td>
<td headers="achrom_furthest" class="gt_row gt_left">lime_300</td></tr>
    <tr><td headers="name" class="gt_row gt_left">light_green_600</td>
<td headers="normal_furthest" class="gt_row gt_left">gray_600</td>
<td headers="deutan_furthest" class="gt_row gt_left">gray_600</td>
<td headers="protan_furthest" class="gt_row gt_left">teal_500</td>
<td headers="tritan_furthest" class="gt_row gt_left">gray_600</td>
<td headers="achrom_furthest" class="gt_row gt_left">green_a400</td></tr>
    <tr><td headers="name" class="gt_row gt_left">light_green_700</td>
<td headers="normal_furthest" class="gt_row gt_left">gray_600</td>
<td headers="deutan_furthest" class="gt_row gt_left">gray_600</td>
<td headers="protan_furthest" class="gt_row gt_left">pink_a200</td>
<td headers="tritan_furthest" class="gt_row gt_left">deep_purple_400</td>
<td headers="achrom_furthest" class="gt_row gt_left">brown_300</td></tr>
    <tr><td headers="name" class="gt_row gt_left">light_green_800</td>
<td headers="normal_furthest" class="gt_row gt_left">gray_600</td>
<td headers="deutan_furthest" class="gt_row gt_left">gray_600</td>
<td headers="protan_furthest" class="gt_row gt_left">pink_400</td>
<td headers="tritan_furthest" class="gt_row gt_left">gray_600</td>
<td headers="achrom_furthest" class="gt_row gt_left">light_green_a200</td></tr>
    <tr><td headers="name" class="gt_row gt_left">light_green_900</td>
<td headers="normal_furthest" class="gt_row gt_left">gray_700</td>
<td headers="deutan_furthest" class="gt_row gt_left">gray_600</td>
<td headers="protan_furthest" class="gt_row gt_left">teal_700</td>
<td headers="tritan_furthest" class="gt_row gt_left">gray_600</td>
<td headers="achrom_furthest" class="gt_row gt_left">lime_400</td></tr>
    <tr><td headers="name" class="gt_row gt_left">light_green_a100</td>
<td headers="normal_furthest" class="gt_row gt_left">gray_500</td>
<td headers="deutan_furthest" class="gt_row gt_left">gray_500</td>
<td headers="protan_furthest" class="gt_row gt_left">brown_300</td>
<td headers="tritan_furthest" class="gt_row gt_left">blue_gray_400</td>
<td headers="achrom_furthest" class="gt_row gt_left">teal_700</td></tr>
    <tr><td headers="name" class="gt_row gt_left">light_green_a200</td>
<td headers="normal_furthest" class="gt_row gt_left">gray_500</td>
<td headers="deutan_furthest" class="gt_row gt_left">gray_500</td>
<td headers="protan_furthest" class="gt_row gt_left">brown_300</td>
<td headers="tritan_furthest" class="gt_row gt_left">blue_gray_400</td>
<td headers="achrom_furthest" class="gt_row gt_left">lime_a200</td></tr>
    <tr><td headers="name" class="gt_row gt_left">light_green_a400</td>
<td headers="normal_furthest" class="gt_row gt_left">gray_500</td>
<td headers="deutan_furthest" class="gt_row gt_left">gray_500</td>
<td headers="protan_furthest" class="gt_row gt_left">brown_300</td>
<td headers="tritan_furthest" class="gt_row gt_left">green_700</td>
<td headers="achrom_furthest" class="gt_row gt_left">lime_300</td></tr>
    <tr><td headers="name" class="gt_row gt_left">light_green_a700</td>
<td headers="normal_furthest" class="gt_row gt_left">gray_500</td>
<td headers="deutan_furthest" class="gt_row gt_left">gray_500</td>
<td headers="protan_furthest" class="gt_row gt_left">gray_500</td>
<td headers="tritan_furthest" class="gt_row gt_left">blue_gray_500</td>
<td headers="achrom_furthest" class="gt_row gt_left">red_a100</td></tr>
    <tr><td headers="name" class="gt_row gt_left">lime_50</td>
<td headers="normal_furthest" class="gt_row gt_left">brown_300</td>
<td headers="deutan_furthest" class="gt_row gt_left">brown_300</td>
<td headers="protan_furthest" class="gt_row gt_left">teal_500</td>
<td headers="tritan_furthest" class="gt_row gt_left">deep_purple_300</td>
<td headers="achrom_furthest" class="gt_row gt_left">red_a100</td></tr>
    <tr><td headers="name" class="gt_row gt_left">lime_100</td>
<td headers="normal_furthest" class="gt_row gt_left">gray_500</td>
<td headers="deutan_furthest" class="gt_row gt_left">brown_300</td>
<td headers="protan_furthest" class="gt_row gt_left">brown_300</td>
<td headers="tritan_furthest" class="gt_row gt_left">deep_purple_300</td>
<td headers="achrom_furthest" class="gt_row gt_left">yellow_400</td></tr>
    <tr><td headers="name" class="gt_row gt_left">lime_200</td>
<td headers="normal_furthest" class="gt_row gt_left">gray_500</td>
<td headers="deutan_furthest" class="gt_row gt_left">gray_500</td>
<td headers="protan_furthest" class="gt_row gt_left">teal_400</td>
<td headers="tritan_furthest" class="gt_row gt_left">deep_purple_300</td>
<td headers="achrom_furthest" class="gt_row gt_left">deep_orange_300</td></tr>
    <tr><td headers="name" class="gt_row gt_left">lime_300</td>
<td headers="normal_furthest" class="gt_row gt_left">gray_500</td>
<td headers="deutan_furthest" class="gt_row gt_left">gray_500</td>
<td headers="protan_furthest" class="gt_row gt_left">teal_400</td>
<td headers="tritan_furthest" class="gt_row gt_left">deep_purple_300</td>
<td headers="achrom_furthest" class="gt_row gt_left">brown_400</td></tr>
    <tr><td headers="name" class="gt_row gt_left">lime_400</td>
<td headers="normal_furthest" class="gt_row gt_left">gray_500</td>
<td headers="deutan_furthest" class="gt_row gt_left">gray_500</td>
<td headers="protan_furthest" class="gt_row gt_left">gray_500</td>
<td headers="tritan_furthest" class="gt_row gt_left">deep_purple_300</td>
<td headers="achrom_furthest" class="gt_row gt_left">yellow_400</td></tr>
    <tr><td headers="name" class="gt_row gt_left">lime_500</td>
<td headers="normal_furthest" class="gt_row gt_left">gray_500</td>
<td headers="deutan_furthest" class="gt_row gt_left">gray_500</td>
<td headers="protan_furthest" class="gt_row gt_left">gray_500</td>
<td headers="tritan_furthest" class="gt_row gt_left">deep_purple_300</td>
<td headers="achrom_furthest" class="gt_row gt_left">lime_300</td></tr>
    <tr><td headers="name" class="gt_row gt_left">lime_600</td>
<td headers="normal_furthest" class="gt_row gt_left">gray_500</td>
<td headers="deutan_furthest" class="gt_row gt_left">gray_500</td>
<td headers="protan_furthest" class="gt_row gt_left">teal_400</td>
<td headers="tritan_furthest" class="gt_row gt_left">lime_900</td>
<td headers="achrom_furthest" class="gt_row gt_left">teal_500</td></tr>
    <tr><td headers="name" class="gt_row gt_left">lime_700</td>
<td headers="normal_furthest" class="gt_row gt_left">brown_300</td>
<td headers="deutan_furthest" class="gt_row gt_left">gray_500</td>
<td headers="protan_furthest" class="gt_row gt_left">teal_500</td>
<td headers="tritan_furthest" class="gt_row gt_left">gray_600</td>
<td headers="achrom_furthest" class="gt_row gt_left">brown_300</td></tr>
    <tr><td headers="name" class="gt_row gt_left">lime_800</td>
<td headers="normal_furthest" class="gt_row gt_left">brown_300</td>
<td headers="deutan_furthest" class="gt_row gt_left">gray_500</td>
<td headers="protan_furthest" class="gt_row gt_left">teal_600</td>
<td headers="tritan_furthest" class="gt_row gt_left">gray_600</td>
<td headers="achrom_furthest" class="gt_row gt_left">light_green_a200</td></tr>
    <tr><td headers="name" class="gt_row gt_left">lime_900</td>
<td headers="normal_furthest" class="gt_row gt_left">gray_600</td>
<td headers="deutan_furthest" class="gt_row gt_left">gray_600</td>
<td headers="protan_furthest" class="gt_row gt_left">teal_800</td>
<td headers="tritan_furthest" class="gt_row gt_left">deep_purple_300</td>
<td headers="achrom_furthest" class="gt_row gt_left">gray_600</td></tr>
    <tr><td headers="name" class="gt_row gt_left">lime_a100</td>
<td headers="normal_furthest" class="gt_row gt_left">gray_500</td>
<td headers="deutan_furthest" class="gt_row gt_left">brown_300</td>
<td headers="protan_furthest" class="gt_row gt_left">brown_300</td>
<td headers="tritan_furthest" class="gt_row gt_left">deep_purple_300</td>
<td headers="achrom_furthest" class="gt_row gt_left">green_a700</td></tr>
    <tr><td headers="name" class="gt_row gt_left">lime_a200</td>
<td headers="normal_furthest" class="gt_row gt_left">gray_500</td>
<td headers="deutan_furthest" class="gt_row gt_left">brown_300</td>
<td headers="protan_furthest" class="gt_row gt_left">red_300</td>
<td headers="tritan_furthest" class="gt_row gt_left">deep_purple_300</td>
<td headers="achrom_furthest" class="gt_row gt_left">light_green_a200</td></tr>
    <tr><td headers="name" class="gt_row gt_left">lime_a400</td>
<td headers="normal_furthest" class="gt_row gt_left">gray_500</td>
<td headers="deutan_furthest" class="gt_row gt_left">brown_300</td>
<td headers="protan_furthest" class="gt_row gt_left">red_300</td>
<td headers="tritan_furthest" class="gt_row gt_left">blue_gray_400</td>
<td headers="achrom_furthest" class="gt_row gt_left">lime_300</td></tr>
    <tr><td headers="name" class="gt_row gt_left">lime_a700</td>
<td headers="normal_furthest" class="gt_row gt_left">gray_500</td>
<td headers="deutan_furthest" class="gt_row gt_left">gray_500</td>
<td headers="protan_furthest" class="gt_row gt_left">brown_300</td>
<td headers="tritan_furthest" class="gt_row gt_left">light_green_800</td>
<td headers="achrom_furthest" class="gt_row gt_left">green_a400</td></tr>
    <tr><td headers="name" class="gt_row gt_left">yellow_50</td>
<td headers="normal_furthest" class="gt_row gt_left">brown_300</td>
<td headers="deutan_furthest" class="gt_row gt_left">brown_300</td>
<td headers="protan_furthest" class="gt_row gt_left">brown_300</td>
<td headers="tritan_furthest" class="gt_row gt_left">deep_purple_300</td>
<td headers="achrom_furthest" class="gt_row gt_left">red_a100</td></tr>
    <tr><td headers="name" class="gt_row gt_left">yellow_100</td>
<td headers="normal_furthest" class="gt_row gt_left">gray_500</td>
<td headers="deutan_furthest" class="gt_row gt_left">brown_300</td>
<td headers="protan_furthest" class="gt_row gt_left">brown_300</td>
<td headers="tritan_furthest" class="gt_row gt_left">deep_purple_300</td>
<td headers="achrom_furthest" class="gt_row gt_left">teal_600</td></tr>
    <tr><td headers="name" class="gt_row gt_left">yellow_200</td>
<td headers="normal_furthest" class="gt_row gt_left">gray_500</td>
<td headers="deutan_furthest" class="gt_row gt_left">brown_300</td>
<td headers="protan_furthest" class="gt_row gt_left">brown_300</td>
<td headers="tritan_furthest" class="gt_row gt_left">deep_purple_300</td>
<td headers="achrom_furthest" class="gt_row gt_left">deep_orange_300</td></tr>
    <tr><td headers="name" class="gt_row gt_left">yellow_300</td>
<td headers="normal_furthest" class="gt_row gt_left">gray_500</td>
<td headers="deutan_furthest" class="gt_row gt_left">brown_300</td>
<td headers="protan_furthest" class="gt_row gt_left">brown_300</td>
<td headers="tritan_furthest" class="gt_row gt_left">deep_purple_300</td>
<td headers="achrom_furthest" class="gt_row gt_left">brown_400</td></tr>
    <tr><td headers="name" class="gt_row gt_left">yellow_400</td>
<td headers="normal_furthest" class="gt_row gt_left">gray_500</td>
<td headers="deutan_furthest" class="gt_row gt_left">brown_300</td>
<td headers="protan_furthest" class="gt_row gt_left">brown_300</td>
<td headers="tritan_furthest" class="gt_row gt_left">deep_purple_300</td>
<td headers="achrom_furthest" class="gt_row gt_left">lime_400</td></tr>
    <tr><td headers="name" class="gt_row gt_left">yellow_500</td>
<td headers="normal_furthest" class="gt_row gt_left">gray_500</td>
<td headers="deutan_furthest" class="gt_row gt_left">brown_300</td>
<td headers="protan_furthest" class="gt_row gt_left">brown_300</td>
<td headers="tritan_furthest" class="gt_row gt_left">deep_purple_300</td>
<td headers="achrom_furthest" class="gt_row gt_left">red_a100</td></tr>
    <tr><td headers="name" class="gt_row gt_left">yellow_600</td>
<td headers="normal_furthest" class="gt_row gt_left">gray_500</td>
<td headers="deutan_furthest" class="gt_row gt_left">brown_300</td>
<td headers="protan_furthest" class="gt_row gt_left">brown_300</td>
<td headers="tritan_furthest" class="gt_row gt_left">deep_purple_300</td>
<td headers="achrom_furthest" class="gt_row gt_left">lime_300</td></tr>
    <tr><td headers="name" class="gt_row gt_left">yellow_700</td>
<td headers="normal_furthest" class="gt_row gt_left">brown_300</td>
<td headers="deutan_furthest" class="gt_row gt_left">gray_500</td>
<td headers="protan_furthest" class="gt_row gt_left">teal_400</td>
<td headers="tritan_furthest" class="gt_row gt_left">purple_400</td>
<td headers="achrom_furthest" class="gt_row gt_left">green_a400</td></tr>
    <tr><td headers="name" class="gt_row gt_left">yellow_800</td>
<td headers="normal_furthest" class="gt_row gt_left">brown_300</td>
<td headers="deutan_furthest" class="gt_row gt_left">gray_500</td>
<td headers="protan_furthest" class="gt_row gt_left">teal_500</td>
<td headers="tritan_furthest" class="gt_row gt_left">lime_900</td>
<td headers="achrom_furthest" class="gt_row gt_left">brown_300</td></tr>
    <tr><td headers="name" class="gt_row gt_left">yellow_900</td>
<td headers="normal_furthest" class="gt_row gt_left">brown_300</td>
<td headers="deutan_furthest" class="gt_row gt_left">gray_500</td>
<td headers="protan_furthest" class="gt_row gt_left">teal_600</td>
<td headers="tritan_furthest" class="gt_row gt_left">deep_purple_300</td>
<td headers="achrom_furthest" class="gt_row gt_left">yellow_400</td></tr>
    <tr><td headers="name" class="gt_row gt_left">yellow_a100</td>
<td headers="normal_furthest" class="gt_row gt_left">gray_500</td>
<td headers="deutan_furthest" class="gt_row gt_left">brown_300</td>
<td headers="protan_furthest" class="gt_row gt_left">gray_500</td>
<td headers="tritan_furthest" class="gt_row gt_left">deep_purple_300</td>
<td headers="achrom_furthest" class="gt_row gt_left">teal_700</td></tr>
    <tr><td headers="name" class="gt_row gt_left">yellow_a200</td>
<td headers="normal_furthest" class="gt_row gt_left">brown_200</td>
<td headers="deutan_furthest" class="gt_row gt_left">brown_300</td>
<td headers="protan_furthest" class="gt_row gt_left">red_300</td>
<td headers="tritan_furthest" class="gt_row gt_left">deep_purple_300</td>
<td headers="achrom_furthest" class="gt_row gt_left">lime_300</td></tr>
    <tr><td headers="name" class="gt_row gt_left">yellow_a400</td>
<td headers="normal_furthest" class="gt_row gt_left">gray_500</td>
<td headers="deutan_furthest" class="gt_row gt_left">brown_300</td>
<td headers="protan_furthest" class="gt_row gt_left">brown_300</td>
<td headers="tritan_furthest" class="gt_row gt_left">deep_purple_300</td>
<td headers="achrom_furthest" class="gt_row gt_left">deep_purple_900</td></tr>
    <tr><td headers="name" class="gt_row gt_left">yellow_a700</td>
<td headers="normal_furthest" class="gt_row gt_left">gray_500</td>
<td headers="deutan_furthest" class="gt_row gt_left">brown_300</td>
<td headers="protan_furthest" class="gt_row gt_left">brown_300</td>
<td headers="tritan_furthest" class="gt_row gt_left">deep_purple_300</td>
<td headers="achrom_furthest" class="gt_row gt_left">red_a100</td></tr>
    <tr><td headers="name" class="gt_row gt_left">amber_50</td>
<td headers="normal_furthest" class="gt_row gt_left">brown_300</td>
<td headers="deutan_furthest" class="gt_row gt_left">brown_300</td>
<td headers="protan_furthest" class="gt_row gt_left">teal_500</td>
<td headers="tritan_furthest" class="gt_row gt_left">deep_purple_300</td>
<td headers="achrom_furthest" class="gt_row gt_left">red_a100</td></tr>
    <tr><td headers="name" class="gt_row gt_left">amber_100</td>
<td headers="normal_furthest" class="gt_row gt_left">brown_300</td>
<td headers="deutan_furthest" class="gt_row gt_left">brown_300</td>
<td headers="protan_furthest" class="gt_row gt_left">brown_300</td>
<td headers="tritan_furthest" class="gt_row gt_left">deep_purple_300</td>
<td headers="achrom_furthest" class="gt_row gt_left">red_300</td></tr>
    <tr><td headers="name" class="gt_row gt_left">amber_200</td>
<td headers="normal_furthest" class="gt_row gt_left">brown_300</td>
<td headers="deutan_furthest" class="gt_row gt_left">gray_500</td>
<td headers="protan_furthest" class="gt_row gt_left">teal_400</td>
<td headers="tritan_furthest" class="gt_row gt_left">deep_purple_300</td>
<td headers="achrom_furthest" class="gt_row gt_left">brown_400</td></tr>
    <tr><td headers="name" class="gt_row gt_left">amber_300</td>
<td headers="normal_furthest" class="gt_row gt_left">gray_500</td>
<td headers="deutan_furthest" class="gt_row gt_left">gray_500</td>
<td headers="protan_furthest" class="gt_row gt_left">gray_500</td>
<td headers="tritan_furthest" class="gt_row gt_left">deep_purple_300</td>
<td headers="achrom_furthest" class="gt_row gt_left">brown_300</td></tr>
    <tr><td headers="name" class="gt_row gt_left">amber_400</td>
<td headers="normal_furthest" class="gt_row gt_left">gray_500</td>
<td headers="deutan_furthest" class="gt_row gt_left">brown_300</td>
<td headers="protan_furthest" class="gt_row gt_left">gray_500</td>
<td headers="tritan_furthest" class="gt_row gt_left">deep_purple_300</td>
<td headers="achrom_furthest" class="gt_row gt_left">green_a400</td></tr>
    <tr><td headers="name" class="gt_row gt_left">amber_500</td>
<td headers="normal_furthest" class="gt_row gt_left">gray_500</td>
<td headers="deutan_furthest" class="gt_row gt_left">gray_500</td>
<td headers="protan_furthest" class="gt_row gt_left">gray_500</td>
<td headers="tritan_furthest" class="gt_row gt_left">purple_400</td>
<td headers="achrom_furthest" class="gt_row gt_left">brown_300</td></tr>
    <tr><td headers="name" class="gt_row gt_left">amber_600</td>
<td headers="normal_furthest" class="gt_row gt_left">brown_300</td>
<td headers="deutan_furthest" class="gt_row gt_left">gray_500</td>
<td headers="protan_furthest" class="gt_row gt_left">teal_400</td>
<td headers="tritan_furthest" class="gt_row gt_left">brown_400</td>
<td headers="achrom_furthest" class="gt_row gt_left">brown_300</td></tr>
    <tr><td headers="name" class="gt_row gt_left">amber_700</td>
<td headers="normal_furthest" class="gt_row gt_left">brown_300</td>
<td headers="deutan_furthest" class="gt_row gt_left">gray_500</td>
<td headers="protan_furthest" class="gt_row gt_left">teal_400</td>
<td headers="tritan_furthest" class="gt_row gt_left">lime_900</td>
<td headers="achrom_furthest" class="gt_row gt_left">light_green_a200</td></tr>
    <tr><td headers="name" class="gt_row gt_left">amber_800</td>
<td headers="normal_furthest" class="gt_row gt_left">brown_300</td>
<td headers="deutan_furthest" class="gt_row gt_left">gray_500</td>
<td headers="protan_furthest" class="gt_row gt_left">teal_500</td>
<td headers="tritan_furthest" class="gt_row gt_left">brown_300</td>
<td headers="achrom_furthest" class="gt_row gt_left">yellow_400</td></tr>
    <tr><td headers="name" class="gt_row gt_left">amber_900</td>
<td headers="normal_furthest" class="gt_row gt_left">brown_300</td>
<td headers="deutan_furthest" class="gt_row gt_left">gray_500</td>
<td headers="protan_furthest" class="gt_row gt_left">teal_600</td>
<td headers="tritan_furthest" class="gt_row gt_left">lime_800</td>
<td headers="achrom_furthest" class="gt_row gt_left">red_300</td></tr>
    <tr><td headers="name" class="gt_row gt_left">amber_a100</td>
<td headers="normal_furthest" class="gt_row gt_left">gray_500</td>
<td headers="deutan_furthest" class="gt_row gt_left">gray_500</td>
<td headers="protan_furthest" class="gt_row gt_left">teal_400</td>
<td headers="tritan_furthest" class="gt_row gt_left">deep_purple_300</td>
<td headers="achrom_furthest" class="gt_row gt_left">brown_400</td></tr>
    <tr><td headers="name" class="gt_row gt_left">amber_a200</td>
<td headers="normal_furthest" class="gt_row gt_left">gray_500</td>
<td headers="deutan_furthest" class="gt_row gt_left">brown_300</td>
<td headers="protan_furthest" class="gt_row gt_left">brown_300</td>
<td headers="tritan_furthest" class="gt_row gt_left">deep_purple_300</td>
<td headers="achrom_furthest" class="gt_row gt_left">green_a400</td></tr>
    <tr><td headers="name" class="gt_row gt_left">amber_a400</td>
<td headers="normal_furthest" class="gt_row gt_left">gray_500</td>
<td headers="deutan_furthest" class="gt_row gt_left">brown_300</td>
<td headers="protan_furthest" class="gt_row gt_left">gray_500</td>
<td headers="tritan_furthest" class="gt_row gt_left">brown_400</td>
<td headers="achrom_furthest" class="gt_row gt_left">brown_300</td></tr>
    <tr><td headers="name" class="gt_row gt_left">amber_a700</td>
<td headers="normal_furthest" class="gt_row gt_left">brown_300</td>
<td headers="deutan_furthest" class="gt_row gt_left">gray_500</td>
<td headers="protan_furthest" class="gt_row gt_left">teal_400</td>
<td headers="tritan_furthest" class="gt_row gt_left">lime_900</td>
<td headers="achrom_furthest" class="gt_row gt_left">pink_a200</td></tr>
    <tr><td headers="name" class="gt_row gt_left">orange_50</td>
<td headers="normal_furthest" class="gt_row gt_left">brown_300</td>
<td headers="deutan_furthest" class="gt_row gt_left">brown_300</td>
<td headers="protan_furthest" class="gt_row gt_left">teal_500</td>
<td headers="tritan_furthest" class="gt_row gt_left">deep_purple_300</td>
<td headers="achrom_furthest" class="gt_row gt_left">red_a100</td></tr>
    <tr><td headers="name" class="gt_row gt_left">orange_100</td>
<td headers="normal_furthest" class="gt_row gt_left">brown_300</td>
<td headers="deutan_furthest" class="gt_row gt_left">gray_500</td>
<td headers="protan_furthest" class="gt_row gt_left">teal_500</td>
<td headers="tritan_furthest" class="gt_row gt_left">deep_purple_300</td>
<td headers="achrom_furthest" class="gt_row gt_left">gray_600</td></tr>
    <tr><td headers="name" class="gt_row gt_left">orange_200</td>
<td headers="normal_furthest" class="gt_row gt_left">brown_300</td>
<td headers="deutan_furthest" class="gt_row gt_left">gray_500</td>
<td headers="protan_furthest" class="gt_row gt_left">teal_500</td>
<td headers="tritan_furthest" class="gt_row gt_left">deep_purple_300</td>
<td headers="achrom_furthest" class="gt_row gt_left">brown_400</td></tr>
    <tr><td headers="name" class="gt_row gt_left">orange_300</td>
<td headers="normal_furthest" class="gt_row gt_left">brown_300</td>
<td headers="deutan_furthest" class="gt_row gt_left">gray_500</td>
<td headers="protan_furthest" class="gt_row gt_left">teal_500</td>
<td headers="tritan_furthest" class="gt_row gt_left">brown_400</td>
<td headers="achrom_furthest" class="gt_row gt_left">teal_500</td></tr>
    <tr><td headers="name" class="gt_row gt_left">orange_400</td>
<td headers="normal_furthest" class="gt_row gt_left">brown_300</td>
<td headers="deutan_furthest" class="gt_row gt_left">gray_500</td>
<td headers="protan_furthest" class="gt_row gt_left">teal_400</td>
<td headers="tritan_furthest" class="gt_row gt_left">lime_900</td>
<td headers="achrom_furthest" class="gt_row gt_left">brown_300</td></tr>
    <tr><td headers="name" class="gt_row gt_left">orange_500</td>
<td headers="normal_furthest" class="gt_row gt_left">brown_300</td>
<td headers="deutan_furthest" class="gt_row gt_left">gray_500</td>
<td headers="protan_furthest" class="gt_row gt_left">teal_500</td>
<td headers="tritan_furthest" class="gt_row gt_left">brown_300</td>
<td headers="achrom_furthest" class="gt_row gt_left">light_green_a200</td></tr>
    <tr><td headers="name" class="gt_row gt_left">orange_600</td>
<td headers="normal_furthest" class="gt_row gt_left">brown_300</td>
<td headers="deutan_furthest" class="gt_row gt_left">gray_500</td>
<td headers="protan_furthest" class="gt_row gt_left">teal_500</td>
<td headers="tritan_furthest" class="gt_row gt_left">brown_300</td>
<td headers="achrom_furthest" class="gt_row gt_left">yellow_400</td></tr>
    <tr><td headers="name" class="gt_row gt_left">orange_700</td>
<td headers="normal_furthest" class="gt_row gt_left">brown_300</td>
<td headers="deutan_furthest" class="gt_row gt_left">gray_500</td>
<td headers="protan_furthest" class="gt_row gt_left">teal_500</td>
<td headers="tritan_furthest" class="gt_row gt_left">deep_purple_300</td>
<td headers="achrom_furthest" class="gt_row gt_left">pink_400</td></tr>
    <tr><td headers="name" class="gt_row gt_left">orange_800</td>
<td headers="normal_furthest" class="gt_row gt_left">brown_300</td>
<td headers="deutan_furthest" class="gt_row gt_left">gray_500</td>
<td headers="protan_furthest" class="gt_row gt_left">teal_600</td>
<td headers="tritan_furthest" class="gt_row gt_left">lime_800</td>
<td headers="achrom_furthest" class="gt_row gt_left">gray_600</td></tr>
    <tr><td headers="name" class="gt_row gt_left">orange_900</td>
<td headers="normal_furthest" class="gt_row gt_left">brown_300</td>
<td headers="deutan_furthest" class="gt_row gt_left">gray_500</td>
<td headers="protan_furthest" class="gt_row gt_left">teal_700</td>
<td headers="tritan_furthest" class="gt_row gt_left">lime_800</td>
<td headers="achrom_furthest" class="gt_row gt_left">lime_a200</td></tr>
    <tr><td headers="name" class="gt_row gt_left">orange_a100</td>
<td headers="normal_furthest" class="gt_row gt_left">brown_300</td>
<td headers="deutan_furthest" class="gt_row gt_left">gray_500</td>
<td headers="protan_furthest" class="gt_row gt_left">teal_400</td>
<td headers="tritan_furthest" class="gt_row gt_left">deep_purple_300</td>
<td headers="achrom_furthest" class="gt_row gt_left">brown_400</td></tr>
    <tr><td headers="name" class="gt_row gt_left">orange_a200</td>
<td headers="normal_furthest" class="gt_row gt_left">brown_300</td>
<td headers="deutan_furthest" class="gt_row gt_left">gray_500</td>
<td headers="protan_furthest" class="gt_row gt_left">teal_500</td>
<td headers="tritan_furthest" class="gt_row gt_left">brown_400</td>
<td headers="achrom_furthest" class="gt_row gt_left">deep_purple_900</td></tr>
    <tr><td headers="name" class="gt_row gt_left">orange_a400</td>
<td headers="normal_furthest" class="gt_row gt_left">brown_300</td>
<td headers="deutan_furthest" class="gt_row gt_left">gray_500</td>
<td headers="protan_furthest" class="gt_row gt_left">teal_500</td>
<td headers="tritan_furthest" class="gt_row gt_left">brown_300</td>
<td headers="achrom_furthest" class="gt_row gt_left">teal_600</td></tr>
    <tr><td headers="name" class="gt_row gt_left">orange_a700</td>
<td headers="normal_furthest" class="gt_row gt_left">brown_300</td>
<td headers="deutan_furthest" class="gt_row gt_left">gray_500</td>
<td headers="protan_furthest" class="gt_row gt_left">teal_600</td>
<td headers="tritan_furthest" class="gt_row gt_left">lime_800</td>
<td headers="achrom_furthest" class="gt_row gt_left">red_300</td></tr>
    <tr><td headers="name" class="gt_row gt_left">deep_orange_50</td>
<td headers="normal_furthest" class="gt_row gt_left">brown_300</td>
<td headers="deutan_furthest" class="gt_row gt_left">teal_400</td>
<td headers="protan_furthest" class="gt_row gt_left">teal_500</td>
<td headers="tritan_furthest" class="gt_row gt_left">deep_purple_300</td>
<td headers="achrom_furthest" class="gt_row gt_left">red_a100</td></tr>
    <tr><td headers="name" class="gt_row gt_left">deep_orange_100</td>
<td headers="normal_furthest" class="gt_row gt_left">brown_300</td>
<td headers="deutan_furthest" class="gt_row gt_left">brown_400</td>
<td headers="protan_furthest" class="gt_row gt_left">pink_a200</td>
<td headers="tritan_furthest" class="gt_row gt_left">deep_purple_300</td>
<td headers="achrom_furthest" class="gt_row gt_left">light_green_400</td></tr>
    <tr><td headers="name" class="gt_row gt_left">deep_orange_200</td>
<td headers="normal_furthest" class="gt_row gt_left">brown_400</td>
<td headers="deutan_furthest" class="gt_row gt_left">gray_500</td>
<td headers="protan_furthest" class="gt_row gt_left">pink_a200</td>
<td headers="tritan_furthest" class="gt_row gt_left">brown_400</td>
<td headers="achrom_furthest" class="gt_row gt_left">amber_a200</td></tr>
    <tr><td headers="name" class="gt_row gt_left">deep_orange_300</td>
<td headers="normal_furthest" class="gt_row gt_left">brown_300</td>
<td headers="deutan_furthest" class="gt_row gt_left">gray_500</td>
<td headers="protan_furthest" class="gt_row gt_left">pink_a200</td>
<td headers="tritan_furthest" class="gt_row gt_left">brown_300</td>
<td headers="achrom_furthest" class="gt_row gt_left">pink_a200</td></tr>
    <tr><td headers="name" class="gt_row gt_left">deep_orange_400</td>
<td headers="normal_furthest" class="gt_row gt_left">brown_300</td>
<td headers="deutan_furthest" class="gt_row gt_left">gray_500</td>
<td headers="protan_furthest" class="gt_row gt_left">pink_a200</td>
<td headers="tritan_furthest" class="gt_row gt_left">lime_800</td>
<td headers="achrom_furthest" class="gt_row gt_left">red_a100</td></tr>
    <tr><td headers="name" class="gt_row gt_left">deep_orange_500</td>
<td headers="normal_furthest" class="gt_row gt_left">brown_300</td>
<td headers="deutan_furthest" class="gt_row gt_left">gray_500</td>
<td headers="protan_furthest" class="gt_row gt_left">pink_a200</td>
<td headers="tritan_furthest" class="gt_row gt_left">lime_800</td>
<td headers="achrom_furthest" class="gt_row gt_left">pink_400</td></tr>
    <tr><td headers="name" class="gt_row gt_left">deep_orange_600</td>
<td headers="normal_furthest" class="gt_row gt_left">brown_200</td>
<td headers="deutan_furthest" class="gt_row gt_left">gray_500</td>
<td headers="protan_furthest" class="gt_row gt_left">teal_700</td>
<td headers="tritan_furthest" class="gt_row gt_left">lime_800</td>
<td headers="achrom_furthest" class="gt_row gt_left">light_green_400</td></tr>
    <tr><td headers="name" class="gt_row gt_left">deep_orange_700</td>
<td headers="normal_furthest" class="gt_row gt_left">brown_300</td>
<td headers="deutan_furthest" class="gt_row gt_left">gray_500</td>
<td headers="protan_furthest" class="gt_row gt_left">teal_800</td>
<td headers="tritan_furthest" class="gt_row gt_left">lime_800</td>
<td headers="achrom_furthest" class="gt_row gt_left">gray_600</td></tr>
    <tr><td headers="name" class="gt_row gt_left">deep_orange_800</td>
<td headers="normal_furthest" class="gt_row gt_left">brown_300</td>
<td headers="deutan_furthest" class="gt_row gt_left">gray_600</td>
<td headers="protan_furthest" class="gt_row gt_left">teal_800</td>
<td headers="tritan_furthest" class="gt_row gt_left">lime_800</td>
<td headers="achrom_furthest" class="gt_row gt_left">lime_a200</td></tr>
    <tr><td headers="name" class="gt_row gt_left">deep_orange_900</td>
<td headers="normal_furthest" class="gt_row gt_left">gray_600</td>
<td headers="deutan_furthest" class="gt_row gt_left">gray_600</td>
<td headers="protan_furthest" class="gt_row gt_left">teal_700</td>
<td headers="tritan_furthest" class="gt_row gt_left">deep_purple_400</td>
<td headers="achrom_furthest" class="gt_row gt_left">deep_orange_300</td></tr>
    <tr><td headers="name" class="gt_row gt_left">deep_orange_a100</td>
<td headers="normal_furthest" class="gt_row gt_left">brown_300</td>
<td headers="deutan_furthest" class="gt_row gt_left">teal_a700</td>
<td headers="protan_furthest" class="gt_row gt_left">pink_a200</td>
<td headers="tritan_furthest" class="gt_row gt_left">lime_900</td>
<td headers="achrom_furthest" class="gt_row gt_left">amber_a200</td></tr>
    <tr><td headers="name" class="gt_row gt_left">deep_orange_a200</td>
<td headers="normal_furthest" class="gt_row gt_left">brown_300</td>
<td headers="deutan_furthest" class="gt_row gt_left">gray_500</td>
<td headers="protan_furthest" class="gt_row gt_left">pink_a200</td>
<td headers="tritan_furthest" class="gt_row gt_left">lime_800</td>
<td headers="achrom_furthest" class="gt_row gt_left">brown_300</td></tr>
    <tr><td headers="name" class="gt_row gt_left">deep_orange_a400</td>
<td headers="normal_furthest" class="gt_row gt_left">brown_300</td>
<td headers="deutan_furthest" class="gt_row gt_left">gray_500</td>
<td headers="protan_furthest" class="gt_row gt_left">teal_700</td>
<td headers="tritan_furthest" class="gt_row gt_left">lime_800</td>
<td headers="achrom_furthest" class="gt_row gt_left">lime_a200</td></tr>
    <tr><td headers="name" class="gt_row gt_left">deep_orange_a700</td>
<td headers="normal_furthest" class="gt_row gt_left">brown_300</td>
<td headers="deutan_furthest" class="gt_row gt_left">gray_600</td>
<td headers="protan_furthest" class="gt_row gt_left">teal_700</td>
<td headers="tritan_furthest" class="gt_row gt_left">brown_300</td>
<td headers="achrom_furthest" class="gt_row gt_left">amber_300</td></tr>
    <tr><td headers="name" class="gt_row gt_left">brown_50</td>
<td headers="normal_furthest" class="gt_row gt_left">blue_gray_400</td>
<td headers="deutan_furthest" class="gt_row gt_left">blue_gray_400</td>
<td headers="protan_furthest" class="gt_row gt_left">teal_500</td>
<td headers="tritan_furthest" class="gt_row gt_left">deep_purple_300</td>
<td headers="achrom_furthest" class="gt_row gt_left">red_a100</td></tr>
    <tr><td headers="name" class="gt_row gt_left">brown_100</td>
<td headers="normal_furthest" class="gt_row gt_left">gray_600</td>
<td headers="deutan_furthest" class="gt_row gt_left">teal_500</td>
<td headers="protan_furthest" class="gt_row gt_left">teal_600</td>
<td headers="tritan_furthest" class="gt_row gt_left">deep_purple_300</td>
<td headers="achrom_furthest" class="gt_row gt_left">yellow_400</td></tr>
    <tr><td headers="name" class="gt_row gt_left">brown_200</td>
<td headers="normal_furthest" class="gt_row gt_left">gray_600</td>
<td headers="deutan_furthest" class="gt_row gt_left">gray_600</td>
<td headers="protan_furthest" class="gt_row gt_left">gray_600</td>
<td headers="tritan_furthest" class="gt_row gt_left">gray_600</td>
<td headers="achrom_furthest" class="gt_row gt_left">amber_300</td></tr>
    <tr><td headers="name" class="gt_row gt_left">brown_300</td>
<td headers="normal_furthest" class="gt_row gt_left">gray_600</td>
<td headers="deutan_furthest" class="gt_row gt_left">gray_600</td>
<td headers="protan_furthest" class="gt_row gt_left">pink_400</td>
<td headers="tritan_furthest" class="gt_row gt_left">gray_600</td>
<td headers="achrom_furthest" class="gt_row gt_left">amber_300</td></tr>
    <tr><td headers="name" class="gt_row gt_left">brown_400</td>
<td headers="normal_furthest" class="gt_row gt_left">gray_500</td>
<td headers="deutan_furthest" class="gt_row gt_left">gray_600</td>
<td headers="protan_furthest" class="gt_row gt_left">pink_a200</td>
<td headers="tritan_furthest" class="gt_row gt_left">deep_purple_a100</td>
<td headers="achrom_furthest" class="gt_row gt_left">deep_purple_900</td></tr>
    <tr><td headers="name" class="gt_row gt_left">brown_500</td>
<td headers="normal_furthest" class="gt_row gt_left">gray_600</td>
<td headers="deutan_furthest" class="gt_row gt_left">gray_600</td>
<td headers="protan_furthest" class="gt_row gt_left">teal_500</td>
<td headers="tritan_furthest" class="gt_row gt_left">deep_purple_300</td>
<td headers="achrom_furthest" class="gt_row gt_left">brown_300</td></tr>
    <tr><td headers="name" class="gt_row gt_left">brown_600</td>
<td headers="normal_furthest" class="gt_row gt_left">gray_600</td>
<td headers="deutan_furthest" class="gt_row gt_left">brown_300</td>
<td headers="protan_furthest" class="gt_row gt_left">teal_500</td>
<td headers="tritan_furthest" class="gt_row gt_left">deep_purple_300</td>
<td headers="achrom_furthest" class="gt_row gt_left">light_green_a200</td></tr>
    <tr><td headers="name" class="gt_row gt_left">brown_700</td>
<td headers="normal_furthest" class="gt_row gt_left">gray_600</td>
<td headers="deutan_furthest" class="gt_row gt_left">brown_400</td>
<td headers="protan_furthest" class="gt_row gt_left">pink_a200</td>
<td headers="tritan_furthest" class="gt_row gt_left">gray_600</td>
<td headers="achrom_furthest" class="gt_row gt_left">pink_400</td></tr>
    <tr><td headers="name" class="gt_row gt_left">brown_800</td>
<td headers="normal_furthest" class="gt_row gt_left">gray_600</td>
<td headers="deutan_furthest" class="gt_row gt_left">brown_400</td>
<td headers="protan_furthest" class="gt_row gt_left">pink_a200</td>
<td headers="tritan_furthest" class="gt_row gt_left">gray_600</td>
<td headers="achrom_furthest" class="gt_row gt_left">gray_600</td></tr>
    <tr><td headers="name" class="gt_row gt_left">brown_900</td>
<td headers="normal_furthest" class="gt_row gt_left">gray_600</td>
<td headers="deutan_furthest" class="gt_row gt_left">pink_a700</td>
<td headers="protan_furthest" class="gt_row gt_left">brown_400</td>
<td headers="tritan_furthest" class="gt_row gt_left">deep_purple_400</td>
<td headers="achrom_furthest" class="gt_row gt_left">deep_orange_300</td></tr>
    <tr><td headers="name" class="gt_row gt_left">gray_50</td>
<td headers="normal_furthest" class="gt_row gt_left">blue_gray_400</td>
<td headers="deutan_furthest" class="gt_row gt_left">blue_gray_400</td>
<td headers="protan_furthest" class="gt_row gt_left">teal_500</td>
<td headers="tritan_furthest" class="gt_row gt_left">deep_purple_300</td>
<td headers="achrom_furthest" class="gt_row gt_left">teal_500</td></tr>
    <tr><td headers="name" class="gt_row gt_left">gray_100</td>
<td headers="normal_furthest" class="gt_row gt_left">blue_gray_400</td>
<td headers="deutan_furthest" class="gt_row gt_left">blue_gray_400</td>
<td headers="protan_furthest" class="gt_row gt_left">teal_500</td>
<td headers="tritan_furthest" class="gt_row gt_left">deep_purple_300</td>
<td headers="achrom_furthest" class="gt_row gt_left">green_a400</td></tr>
    <tr><td headers="name" class="gt_row gt_left">gray_200</td>
<td headers="normal_furthest" class="gt_row gt_left">blue_gray_400</td>
<td headers="deutan_furthest" class="gt_row gt_left">blue_gray_400</td>
<td headers="protan_furthest" class="gt_row gt_left">teal_500</td>
<td headers="tritan_furthest" class="gt_row gt_left">deep_purple_300</td>
<td headers="achrom_furthest" class="gt_row gt_left">red_a100</td></tr>
    <tr><td headers="name" class="gt_row gt_left">gray_300</td>
<td headers="normal_furthest" class="gt_row gt_left">blue_gray_400</td>
<td headers="deutan_furthest" class="gt_row gt_left">blue_gray_400</td>
<td headers="protan_furthest" class="gt_row gt_left">pink_a200</td>
<td headers="tritan_furthest" class="gt_row gt_left">deep_purple_300</td>
<td headers="achrom_furthest" class="gt_row gt_left">brown_300</td></tr>
    <tr><td headers="name" class="gt_row gt_left">gray_400</td>
<td headers="normal_furthest" class="gt_row gt_left">gray_600</td>
<td headers="deutan_furthest" class="gt_row gt_left">gray_600</td>
<td headers="protan_furthest" class="gt_row gt_left">pink_400</td>
<td headers="tritan_furthest" class="gt_row gt_left">gray_600</td>
<td headers="achrom_furthest" class="gt_row gt_left">red_300</td></tr>
    <tr><td headers="name" class="gt_row gt_left">gray_500</td>
<td headers="normal_furthest" class="gt_row gt_left">gray_600</td>
<td headers="deutan_furthest" class="gt_row gt_left">gray_600</td>
<td headers="protan_furthest" class="gt_row gt_left">gray_600</td>
<td headers="tritan_furthest" class="gt_row gt_left">deep_purple_400</td>
<td headers="achrom_furthest" class="gt_row gt_left">yellow_500</td></tr>
    <tr><td headers="name" class="gt_row gt_left">gray_600</td>
<td headers="normal_furthest" class="gt_row gt_left">gray_500</td>
<td headers="deutan_furthest" class="gt_row gt_left">brown_300</td>
<td headers="protan_furthest" class="gt_row gt_left">brown_300</td>
<td headers="tritan_furthest" class="gt_row gt_left">light_green_800</td>
<td headers="achrom_furthest" class="gt_row gt_left">yellow_400</td></tr>
    <tr><td headers="name" class="gt_row gt_left">gray_700</td>
<td headers="normal_furthest" class="gt_row gt_left">gray_500</td>
<td headers="deutan_furthest" class="gt_row gt_left">brown_300</td>
<td headers="protan_furthest" class="gt_row gt_left">brown_300</td>
<td headers="tritan_furthest" class="gt_row gt_left">blue_gray_400</td>
<td headers="achrom_furthest" class="gt_row gt_left">lime_300</td></tr>
    <tr><td headers="name" class="gt_row gt_left">gray_800</td>
<td headers="normal_furthest" class="gt_row gt_left">gray_600</td>
<td headers="deutan_furthest" class="gt_row gt_left">pink_700</td>
<td headers="protan_furthest" class="gt_row gt_left">red_a400</td>
<td headers="tritan_furthest" class="gt_row gt_left">gray_600</td>
<td headers="achrom_furthest" class="gt_row gt_left">light_green_a200</td></tr>
    <tr><td headers="name" class="gt_row gt_left">gray_900</td>
<td headers="normal_furthest" class="gt_row gt_left">gray_600</td>
<td headers="deutan_furthest" class="gt_row gt_left">pink_800</td>
<td headers="protan_furthest" class="gt_row gt_left">pink_a400</td>
<td headers="tritan_furthest" class="gt_row gt_left">deep_purple_400</td>
<td headers="achrom_furthest" class="gt_row gt_left">amber_300</td></tr>
    <tr><td headers="name" class="gt_row gt_left">blue_gray_50</td>
<td headers="normal_furthest" class="gt_row gt_left">blue_gray_400</td>
<td headers="deutan_furthest" class="gt_row gt_left">blue_gray_400</td>
<td headers="protan_furthest" class="gt_row gt_left">teal_500</td>
<td headers="tritan_furthest" class="gt_row gt_left">deep_purple_300</td>
<td headers="achrom_furthest" class="gt_row gt_left">green_a400</td></tr>
    <tr><td headers="name" class="gt_row gt_left">blue_gray_100</td>
<td headers="normal_furthest" class="gt_row gt_left">blue_gray_400</td>
<td headers="deutan_furthest" class="gt_row gt_left">teal_500</td>
<td headers="protan_furthest" class="gt_row gt_left">pink_a200</td>
<td headers="tritan_furthest" class="gt_row gt_left">deep_purple_300</td>
<td headers="achrom_furthest" class="gt_row gt_left">brown_300</td></tr>
    <tr><td headers="name" class="gt_row gt_left">blue_gray_200</td>
<td headers="normal_furthest" class="gt_row gt_left">gray_600</td>
<td headers="deutan_furthest" class="gt_row gt_left">gray_600</td>
<td headers="protan_furthest" class="gt_row gt_left">pink_400</td>
<td headers="tritan_furthest" class="gt_row gt_left">gray_600</td>
<td headers="achrom_furthest" class="gt_row gt_left">pink_400</td></tr>
    <tr><td headers="name" class="gt_row gt_left">blue_gray_300</td>
<td headers="normal_furthest" class="gt_row gt_left">gray_600</td>
<td headers="deutan_furthest" class="gt_row gt_left">gray_600</td>
<td headers="protan_furthest" class="gt_row gt_left">teal_600</td>
<td headers="tritan_furthest" class="gt_row gt_left">gray_600</td>
<td headers="achrom_furthest" class="gt_row gt_left">deep_orange_300</td></tr>
    <tr><td headers="name" class="gt_row gt_left">blue_gray_400</td>
<td headers="normal_furthest" class="gt_row gt_left">brown_300</td>
<td headers="deutan_furthest" class="gt_row gt_left">brown_300</td>
<td headers="protan_furthest" class="gt_row gt_left">teal_500</td>
<td headers="tritan_furthest" class="gt_row gt_left">deep_purple_400</td>
<td headers="achrom_furthest" class="gt_row gt_left">green_a700</td></tr>
    <tr><td headers="name" class="gt_row gt_left">blue_gray_500</td>
<td headers="normal_furthest" class="gt_row gt_left">gray_500</td>
<td headers="deutan_furthest" class="gt_row gt_left">gray_500</td>
<td headers="protan_furthest" class="gt_row gt_left">brown_300</td>
<td headers="tritan_furthest" class="gt_row gt_left">gray_600</td>
<td headers="achrom_furthest" class="gt_row gt_left">orange_300</td></tr>
    <tr><td headers="name" class="gt_row gt_left">blue_gray_600</td>
<td headers="normal_furthest" class="gt_row gt_left">gray_500</td>
<td headers="deutan_furthest" class="gt_row gt_left">gray_500</td>
<td headers="protan_furthest" class="gt_row gt_left">brown_300</td>
<td headers="tritan_furthest" class="gt_row gt_left">deep_purple_300</td>
<td headers="achrom_furthest" class="gt_row gt_left">red_300</td></tr>
    <tr><td headers="name" class="gt_row gt_left">blue_gray_700</td>
<td headers="normal_furthest" class="gt_row gt_left">gray_600</td>
<td headers="deutan_furthest" class="gt_row gt_left">brown_400</td>
<td headers="protan_furthest" class="gt_row gt_left">teal_500</td>
<td headers="tritan_furthest" class="gt_row gt_left">gray_600</td>
<td headers="achrom_furthest" class="gt_row gt_left">lime_300</td></tr>
    <tr><td headers="name" class="gt_row gt_left">blue_gray_800</td>
<td headers="normal_furthest" class="gt_row gt_left">gray_600</td>
<td headers="deutan_furthest" class="gt_row gt_left">brown_400</td>
<td headers="protan_furthest" class="gt_row gt_left">pink_a200</td>
<td headers="tritan_furthest" class="gt_row gt_left">gray_600</td>
<td headers="achrom_furthest" class="gt_row gt_left">brown_300</td></tr>
    <tr><td headers="name" class="gt_row gt_left">blue_gray_900</td>
<td headers="normal_furthest" class="gt_row gt_left">gray_600</td>
<td headers="deutan_furthest" class="gt_row gt_left">pink_800</td>
<td headers="protan_furthest" class="gt_row gt_left">teal_600</td>
<td headers="tritan_furthest" class="gt_row gt_left">gray_600</td>
<td headers="achrom_furthest" class="gt_row gt_left">pink_400</td></tr>
    <tr><td headers="name" class="gt_row gt_left">black</td>
<td headers="normal_furthest" class="gt_row gt_left">gray_700</td>
<td headers="deutan_furthest" class="gt_row gt_left">brown_500</td>
<td headers="protan_furthest" class="gt_row gt_left">pink_500</td>
<td headers="tritan_furthest" class="gt_row gt_left">gray_700</td>
<td headers="achrom_furthest" class="gt_row gt_left">brown_400</td></tr>
    <tr><td headers="name" class="gt_row gt_left">white</td>
<td headers="normal_furthest" class="gt_row gt_left">blue_gray_400</td>
<td headers="deutan_furthest" class="gt_row gt_left">teal_400</td>
<td headers="protan_furthest" class="gt_row gt_left">teal_500</td>
<td headers="tritan_furthest" class="gt_row gt_left">brown_300</td>
<td headers="achrom_furthest" class="gt_row gt_left">lime_300</td></tr>
  </tbody>
  
  
</table>
</div>
