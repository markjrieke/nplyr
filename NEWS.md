# nplyr 0.2.0.9000

* Add more informative error when trying to use nplyr functions with a rowwise df (#18).
* Moved [dplyr](https://dplyr.tidyverse.org/) and [tidyr](https://dplyr.tidyverse.org/) from `Depends` to `Imports` (these packages are installed with nplyr, but no longer attached upon loading).
* Added the pipe ([`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)) as an export.

# nplyr 0.2.0

* Wrappers around `tidyr` functions that can be applied to nested tibbles (@joranE, #14):
  + `nest_drop_na()`
  + `nest_extract()`
  + `nest_fill()`
  + `nest_replace_na()`
  + `nest_separate()`
  + `nest_unite()`

# nplyr 0.1.0

* Wrappers around `dplyr` functions that can be applied to nested tibbles:
  + `nest_arrange()`
  + `nest_count()`, `nest_add_count()`
  + `nest_distinct()`
  + `nest_filter()`
  + `nest_group_by()`, `nest_ungroup()`
  + `nest_inner_join()`, `nest_left_join()`, `nest_right_join()`, `nest_full_join()`
  + `nest_semi_join()`, `nest_anti_join()`, `nest_nest_join()`
  + `nest_mutate()`, `nest_transmute()`
  + `nest_relocate()`
  + `nest_rename()`, `nest_rename_with()`
  + `nest_select()`
  + `nest_slice()`, `nest_slice_head()`, `nest_slice_tail()`, `nest_slice_min()`, `nest_slice_max()`, `nest_slice_sample()`
  + `nest_summarise()`, `nest_summarize()`
  
