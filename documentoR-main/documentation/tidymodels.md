Here's a concise summary of the tidymodels package documentation:

```markdown
### SUMMARY
The tidymodels package is a meta-package that provides easy installation and loading of the tidymodels collection. It offers a set of packages for modeling and statistical analysis that share the underlying design philosophy, grammar, and data structures of the tidyverse.

### KEY_ITEMS
- `tidymodels_packages()`: List all packages in tidymodels
- `tidymodels_conflicts()`: Show conflicts between tidymodels and other packages
- `tidymodels_prefer()`: Resolve conflicts in favor of tidymodels
- `tidymodels_update()`: Update tidymodels packages
- `tag_show()`, `tag_attach()`, `tag_update()`: Manage package groups by tags

### FUNCTIONS/EXPORTS
- `pkg_deps()`: List all dependencies of a package
- `tidymodels_packages()`: List all tidymodels packages
- `tidymodels_conflicts()`: Show package conflicts
- `tidymodels_prefer()`: Resolve conflicts in favor of tidymodels
- `tidymodels_update()`: Update tidymodels packages

### DEPENDENCIES/MIGHT_USE
Imports: broom, cli, conflicted, dials, dplyr, ggplot2, hardhat, infer, modeldata, parsnip, purrr, recipes, rlang, rsample, rstudioapi, tibble, tidyr, tune, workflows, workflowsets, yardstick

### TODO / WARNINGS
- Some functions may produce interactive prompts (e.g., `tidymodels_update()`)
- Certain conflicts are deliberately ignored (intersect, union, setequal, setdiff from dplyr)

### SUGGESTED_TESTS
1. Test `tidymodels_packages()` with and without `include_self = TRUE`
2. Verify `tidymodels_conflicts()` correctly identifies conflicts in a session with known conflicting packages
3. Test `tidymodels_update()` with `recursive = TRUE` to ensure it checks all dependencies

### SHORT_RECOMMENDATION
Consider adding more detailed examples for tag-related functions and expand documentation on how to effectively use the package grouping system.
```