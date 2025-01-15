# pmparams 0.2.2

## Changes
- Organization and formatting updates related to test suite and documentation (#81)
- Removed glue constraint added in `pmparams 0.2.1`. (#85)

# pmparams 0.2.1

## New features
- Update wording in argument descriptions, examples, and vignettes. (#63, #74, #77)

- Add `.cleanup_cols` argument to `format_param_table` and deprecate `.select_cols` argument. The flexibility in `.select_cols` has limited utility. `.cleanup_cols` is a simpler version of `.select_cols` that users can set to T or F.  If user wants to select certain columns, they can set `.cleanup_cols` = F and then use `pmtables::st_select` to further select their columns if needed. (#74)
  
## Bug fixes
- Update `.maxex` and `.digits` arguments to carry through all functions. (#63)

# pmparams 0.2.0

## New features 

- Allow users to input `THETA` in `ERROR` block. (#21)

- Allow additive error log terms (`addErrLogDV`) as a transformation option in parameter key. (#19)

- Carry specified CI level through all functions. (#15)

- `make_pmtable` generate specific parameter tables by filtering and using pmtables. (#15)

- `param_notes` generate data frame of generic footnote equations to append to parameter tables. (#39)

# pmparams 0.1.0

Initial release, featuring the following exported functions:

- `define_param_table`: Suppress warning message for NA's in internal `getpCV` function when take sqrt of value < 0.

- `format_param_table`: Change default scientific notation to put values >1,000 into scientific notation.

- `define_boot_table`: Combine bootstrap estimates with non-bootstrap estimates and parameter key.

- `format_boot_table`:  Format bootstrap parameter estimate values and output selected columns to be shown in the bootstrap parameter table.
