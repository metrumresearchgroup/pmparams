# pmparams 0.2.1

## New features
- Update R to 4.3. (#63)
  
- Update wording in argument descriptions, examples, and vignettes. (#77)
  
- Remove unnecessary console messages. (#63)
  
- Update dependency and suggest packages. (#74)
  
- Show formatted LaTeX table in vignettes. (#74)
  
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
