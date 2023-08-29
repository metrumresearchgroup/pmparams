# pmparams 0.0.0.9001

## New features and changes

- `define_param_table`: Combine model output parameter estimates with information in parameter key. Performs some formatting of this combined data.frame (#34).

- `format_param_table`: Estimate values and output selected columns to be shown in the parameter table (#34).

# pmparams 0.0.1.0

## New features and changes

- `define_param_table`: Suppress warning message for NA's in internal `getpCV` function when take sqrt of value < 0.

- `format_param_table`: Change default scientific notation to put values >1,000 into scientific notation.

- `define_boot_table`: Combine bootstrap estimates with non-bootstrap estimates and parameter key.

- `format_boot_table`:  Format bootstrap parameter estimate values and output selected columns to be shown in the bootstrap parameter table.
