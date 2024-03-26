special_sum <- function(variable_name, selected_year, selected_state_name) {
        with(gundeaths_cut, sum(variable_name[Year == selecter_year & State_Name == selected_state_name]))
}
