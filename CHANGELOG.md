# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [1.0.0] - 2024-12-XX

Core R validation framework for forge analytics.

### Added

- Rust CLI test runner with colored output
- R validator integration via Rscript
- Monte Carlo validation (6 distributions: Normal, Uniform, Lognormal, Triangular, PERT, Exponential)
- Bootstrap confidence interval validation
- Bayesian network inference validation
- Decision tree valuation validation
- Real options pricing validation
- Tornado sensitivity analysis validation
- Scenario analysis validation
- Variance (budget vs actual) validation
- Breakeven analysis validation
- Statistical tolerance framework (mean 1%, std 5%, percentiles 2%)
- 12 R validator scripts
- 10 YAML test specification files

### Philosophy

**Forge is NEVER the authority.**
All tests validate forge against R, the gold-standard for statistical computing.
If forge disagrees with R, forge has a bug.
