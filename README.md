# forge-e2e-r

E2E validation of [forge](https://github.com/royalbit/forge) analytics against R.

## Overview

Validates forge's statistical and analytics functions against R at runtime.
Covers Monte Carlo, bootstrap, Bayesian networks, decision trees, and more.

## Requirements

- **forge**: `FORGE_BIN` environment variable or `../forge/target/release/forge`
- **R**: `Rscript` in PATH with required packages
  ```bash
  # macOS
  brew install r

  # Ubuntu/Debian
  apt install r-base

  # Install R packages
  Rscript -e 'install.packages(c("jsonlite", "boot", "mc2d"))'
  ```

## Usage

```bash
# Run all tests
FORGE_BIN=/path/to/forge cargo run --release -- --all

# Specify test directory
cargo run --release -- --tests tests/analytics --all
```

## How It Works

1. Load test YAML files with analytics parameters
2. Run `forge simulate/tornado/etc` to get JSON output
3. Call R validator script with same parameters
4. Compare statistics (mean, std, percentiles) within tolerance

## Validators

| Validator | R Package | Purpose |
|-----------|-----------|---------|
| monte_carlo_validator.R | mc2d | Monte Carlo distributions |
| bootstrap_validator.R | boot | Bootstrap confidence intervals |
| bayesian_validator.R | bnlearn | Bayesian networks |
| decision_tree_validator.R | data.tree | Decision trees |
| real_options_validator.R | derivmkts | Real options valuation |

## Test Structure

```
tests/
└── analytics/    # R-validated analytics tests
validators/
└── r/            # R validator scripts
```

## License

Elastic License 2.0 - See [LICENSE](LICENSE)
