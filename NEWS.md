# mrgparamtab 0.0.0.9000

## New features and changes

- `defineParamTable`: Combine model output parameter estimates with information in parameter key. Performs some formatting of this combined data.frame (#34).

- `formatParamTable`: Estimate values and output selected columns to be shown in the parameter table (#34).

# mrgparamtab 0.0.1.0

## New features and changes

- `defineParamTable`: Suppress warning message for NA's in internal `getpCV` function when take sqrt of value < 0.

- `formatParamTable`: Change default scientific notation to put values >1,000 into scientific notation.

- `defineBootTable`: Combine bootstrap estimates with non-bootstrap estimates and parameter key.

- `formatBootTable`:  Format bootstrap parameter estimate values and output selected columns to be shown in the bootstrap parameter table.
