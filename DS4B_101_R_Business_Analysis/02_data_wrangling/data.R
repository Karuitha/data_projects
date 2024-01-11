## Load packages manager----
if (!require(pacman)) {
    install.packages("pacman")
}

## Install (if not already installed) ----
## and load required packages
pacman::p_load(
    fs, readxl, writexl,
    odbc, tidyverse, RSQLite,
    tidyquant, tidymodels,
    umap, devtools
)

## Import data ----
bikes_tbl <- read_excel(
    path =
        "00_data/bike_sales/data_raw/bikes.xlsx"
)
bikeshops_tbl <- read_excel(
    path =
        "00_data/bike_sales/data_raw/bikeshops.xlsx"
)
orderlines_tbl <- read_excel(
    path =
        "00_data/bike_sales/data_raw/orderlines.xlsx"
)

## Examining the data ----
bikes_tbl
glimpse(bikes_tbl)
glimpse(bikeshops_tbl)
glimpse(orderlines_tbl)

## The data model ----
## Key fields
## bikeshops::bikeshop.id; orderlines::customer.id
## orderlines::product.id; bikes::bikes.id

## joining the data ----
## using joins in R
## Using the pipe
bikes_orderlines_joined_tbl <- orderlines_tbl %>%
    left_join(bikes_tbl, by = c("product.id" = "bike.id")) %>%
    left_join(bikeshops_tbl, by = c("customer.id" = "bikeshop.id"))

glimpse(bikes_orderlines_joined_tbl)

## Cleaning the data ---
## Separate the categories into separate entities
## Separate location into city and state
## Get total amount paid
bike_orderlines_wrangled_tbl <- bikes_orderlines_joined_tbl %>%
    ## Separate description into categories, material
    separate(description,
        into = c(
            "category1", "category2",
            "frame_material"
        ),
        sep = " - "
    ) %>%
    ## Separate location into city and state
    separate(location,
        into = c("city", "state"),
        sep = ", ", remove = FALSE
    ) %>%
    ## Create a new column for total price
    mutate(total_price = price * quantity) %>%
    ## Remove columns not required
    dplyr::select(-`...1`, -location, -ends_with("id")) %>%
    ## Return back order.id column into the data
    bind_cols(bikes_orderlines_joined_tbl %>%
        dplyr::select(order.id)) %>%
    ## Reordering the columns with select/ relocate
    dplyr::select(
        contains("date"), contains("id"), contains("order"),
        quantity, price, total_price,
        everything()
    ) %>%
    ## Rename the columns with rename
    set_names(names(.) %>% str_replace_all("\\.", "_"))

glimpse(bike_orderlines_wrangled_tbl)


### Sales analysis with ggplot2 ----
### sales by year
sales_year_tbl <- bike_orderlines_wrangled_tbl %>%
    dplyr::select(order_date, total_price) %>%
    mutate(year = year(order_date)) %>%
    group_by(year) %>%
    summarise(sales = sum(total_price)) %>%
    ungroup() %>%
    mutate(sales_text = scales::dollar(sales))

### Plot of sales by year ----
sales_year_tbl %>%
    ggplot(mapping = aes(x = year, y = sales)) +
    geom_col(fill = "#2C3E50") +
    geom_label(aes(label = sales_text)) +
    geom_smooth(
        method = "lm", se = FALSE,
        color = "black"
    ) +
    artyfarty::theme_scientific() +
    scale_y_continuous(labels = scales::dollar) +
    labs(
        x = "Year", y = "Sales",
        title = "Revenue by Year",
        subtitle = "There is an upward trend"
    )

### sales by year and category ----
sales_year_category_tbl <- bike_orderlines_wrangled_tbl %>%
    dplyr::select(order_date, category2, total_price) %>%
    mutate(year = year(order_date)) %>%
    group_by(year, category2) %>%
    summarise(sales = sum(total_price)) %>%
    ungroup() %>%
    mutate(sales_text = scales::dollar(sales))


### Plot of sales by year and category ----
sales_year_category_tbl %>%
    ggplot(mapping = aes(
        x = year, y = sales,
        fill = category2
    )) +
    geom_col(show.legend = FALSE) +
    geom_label(aes(label = sales_text),
        show.legend = FALSE
    ) +
    geom_smooth(
        method = "lm", se = FALSE,
        color = "black", show.legend = FALSE,
        lty = "dashed"
    ) +
    artyfarty::theme_scientific() +
    scale_y_continuous(labels = scales::dollar) +
    labs(
        x = "Year", y = "Sales",
        title = "Revenue by Year",
        subtitle = "There is an upward trend"
    ) +
    facet_wrap(~category2)
