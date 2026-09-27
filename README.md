# CUHK SPHPC research projects (2020-2024)

R code from my work at the JC School of Public Health and Primary Care, The Chinese University of Hong Kong.

I worked as a Research Associate from December 2020 to December 2022, followed by a Postdoctoral Fellow from January 2023 to February 2024. My work covered community-based eHealth and chronic disease programmes, multimorbidity in older adults, mental health, questionnaire validation and health services evaluation.

This repository contains analysis code only. Datasets, survey instruments, clinical records, reports, slides, manuscripts and other documents are not included.

## At a glance

- **Programmes:** Jockey Club Community eHealth Care Project, primary care multimorbidity cohort, Exercise is Medicine and COVID-19 telecare
- **Cohorts:** older adults with multimorbidity, cognitive impairment, sarcopenia, health literacy and cancer screening, undiagnosed diabetes and patient surveys
- **Methods:** mixed models, GEE, ordinal models, cross-lagged panel models, matching, regression discontinuity, lasso, survival analysis, meta-analysis and psychometric validation
- **Outputs:** cleaned analysis datasets, publication tables, figures and report-ready Excel workbooks
- **Data policy:** code is published without participant-level or identifiable data

## Repository layout

| Folder | Project | Contents |
|---|---|---|
| `ehealth/` | Jockey Club Community eHealth Care Project | Cleaning and merging survey and risk-screening data, programme evaluation, matching, regression discontinuity, penalised regression and reporting |
| `multimorbidity/` | Primary care multimorbidity cohort | Four-wave data cleaning, longitudinal models, cross-lagged panel models and comorbidity measures |
| `meaning/` | Meaning in life and mental health | Mixed models for quality of life, depression, loneliness, sleep and meaning in life |
| `sarcopenia/` | Sarcopenia, handgrip strength and cognitive impairment | Reliability checks, mixed models, Cox models and proportional-hazards diagnostics |
| `health_literacy/` | Health literacy and breast cancer screening | Health literacy scoring, perceived barriers, risk perception and regression models |
| `t2dm/` | Undiagnosed type 2 diabetes | Population survey analysis, multinomial logistic models, inferential lasso and diabetes risk scores |
| `validation/` | Questionnaire validation | Content validity, internal consistency, test-retest reliability, concurrent validity and factor analysis |
| `eim/` | Exercise is Medicine | Descriptive analysis of disease-risk estimates using GBD relative risks |
| `telecare/` | COVID-19 telecare evaluation | Cross-organisation identifier harmonisation and comparison of programme data |
| `mindfulness_fgid/` | Repeated-measures intervention analysis | GEE and mixed models across five timepoints and three intervention arms |
| `vaccination/` | Vaccination uptake | Administrative uptake compared with survey-measured uptake |
| `editing/walking/` | Co-authored meta-analysis | Cross-checks of pooled estimates and structured subgroup analysis |
| `helper_functions.R` | Shared analysis library | Reporting, data management, model summaries and reusable R functions |
| `crosstab.r` | Cross-tabulation function | Multi-way frequency tables adapted from a published University of Liverpool function |

## Data management and quality control

### Multimorbidity cohort

- Four survey waves were merged into one analysis file.
- MoCA, PHQ and GAD scores were derived from item-level responses.
- A character-encoding fault in an earlier import was identified by locating corrupted values. The affected records were recovered from the previous dataset using an identifier concordance table.
- Missing values were separated from valid "not applicable" responses before scales were scored.

### eHealth programme

- Survey records were merged with a risk-screening registry.
- Duplicate records were removed using a dedicated audit sheet, followed by a second duplicate check after cleaning.
- Timestamps and participant identifiers were repaired where formatting had changed between exports.
- "Not applicable", "unwilling to answer" and "don't know" responses were converted to missing values rather than being treated as ordinary categories.
- The analysis compares outcomes across pre- and post-programme periods and considers the COVID-19 period separately.
- Loss to follow-up and refusal analyses were written for programme monitoring.
- A value-of-information analysis was prepared for a cost-effectiveness sample-size question.

### Telecare

`telecare_compare.R` aligns identifiers and variables from data supplied by different organisations. It constructs composite keys, standardises identifiers and re-extracts the loneliness items used in the earlier assessment so that the two sources can be compared.

### Undiagnosed type 2 diabetes

- Population survey, case-extract and health-examination sources were merged.
- Coded values for refused body measurements and laboratory results below the detection limit were handled explicitly.
- Three published diabetes risk scores were reimplemented in R and applied to the local dataset.

## Statistical methods

### Longitudinal and repeated-measures models

- Linear mixed models with random intercepts
- Random slopes for time
- Models with participants nested within interviewers or other higher-level units
- Generalized mixed models
- GEE fitted alongside mixed models as a sensitivity analysis
- Ordinal mixed models, ordinal GEE and autoregressive correlated random effects in an earlier analysis file
- Intraclass correlation reported for mixed models

### Meaning in life and multimorbidity

`meaning/meaning_analysis.R` fits mixed models for EQ-5D quality of life, PHQ depression, loneliness and sleep, with meaning in life as an exposure and random slopes for time. It also derives comorbidity and medication counts from baseline disease groups.

`multimorbidity/clpm_analysis.R` contains cross-lagged panel and random-intercept cross-lagged panel models for meaning in life, social support and quality of life across three waves. The random-intercept model separates between-person differences from within-person change.

### Sarcopenia and cognitive impairment

- AWGS handgrip-strength cutoffs
- Handgrip asymmetry
- SARC-F and handgrip comparisons
- Logistic models for cognitive impairment
- Cox models for time to incident cognitive impairment
- Scaled Schoenfeld residual tests for the proportional-hazards assumption
- Weighted robust Cox models as a sensitivity analysis
- A seeded Monte Carlo test comparing coefficients of variation across cognitive-status groups

### eHealth programme evaluation

- Optimal propensity-score matching using the Mahalanobis metric
- Standardized mean-difference balance plots
- A random-subsample robustness check
- Regression discontinuity at the programme eligibility cutoff
- Density-based manipulation tests
- Covariate-adjusted and separate-slope specifications
- A directed acyclic graph used to justify the adjustment set
- Inferential lasso and elastic-net variable selection
- Stepwise selection by AIC for comparison
- Service-uptake modeling with smoothed monthly trends
- Pre/post comparisons using paired tests and center-level intraclass correlations

### Health literacy and cancer screening

- Health literacy index scoring
- Reverse-scored screening items
- Perceived barriers and risk perception
- Reliability analysis for multi-item scales
- Regression models across screening items

### Undiagnosed type 2 diabetes

- Multinomial logistic regression
- Hausman-McFadden tests of the independence-of-irrelevant-alternatives assumption
- Inferential lasso
- Directed acyclic graph for the assumed structure
- Reimplementation of published risk scores with feature rescaling

### Questionnaire validation

- Cronbach's alpha by subscale and survey round
- Alpha if item deleted
- Item-total and corrected item-total correlations
- Test-retest reliability
- Concurrent validity
- Exploratory factor analysis with one-factor and multi-factor solutions

### Meta-analysis

`editing/walking/walking_analysis.R` was prepared for a co-authored meta-analysis. Pooled estimates were calculated using several functions so that the same result could be checked across different implementations. It includes multivariate models for dependent effect sizes, a small-sample correction and a structured subgroup analysis.

## Reporting and visualization

The analysis code was written to produce report-ready outputs rather than only model summaries.

- Table 1 generation with automatic between-group tests
- Side-by-side model tables with confidence intervals and significance markers
- Multi-sheet Excel workbooks
- Figures at 300 to 600 dpi
- Multi-panel figures assembled with `patchwork`
- Rolling means and smoothed trends
- Charts with Chinese survey labels retained where required
- Results written to the clipboard for transfer into reports and manuscripts

## Working conventions

Most scripts begin with the same setup block: clear the workspace, close graphics devices, set the project root and load the shared helper library.

Random seeds are set before stochastic procedures such as elastic net and inferential lasso. Comments document coding decisions, excluded records and cases where a more straightforward approach was rejected.

## Shared analysis library

`helper_functions.R` began during my HKU work, where I moved from Stata to R and recreated functions I used often, such as Stata-style `summ()` and `tab()` and Excel-style `iferror()`.

At CUHK I extended the library for questionnaire and cohort work. It contains:

- `summ()` and `tab()` for Stata-style summaries and cross-tabulations
- `gen_desc()` for descriptive tables
- `gen_reg()` for extracting linear, generalized linear, mixed, Cox, multinomial and GEE models into publication tables
- `combine_tables()` for placing several models side by side
- `combinetab_loop()` for repeating a model across several outcomes
- `write_excel()` for multi-sheet Excel output
- `import_func()` for reusing functions from another script without running the rest of that script
- `recode_age()` and `convert2NA()`
- Frequency-table and robust-error helpers
- `c_alpha()` and `c_alpha1()` for Cronbach's alpha with separate handling of "not applicable" responses
- `eq5d_fast()`, a faster implementation of EQ-5D utility scoring

The file header documents each function with its arguments and an example. Several analysis scripts import functions from other scripts in this way.

## Wider CUHK work not included here

The following work was carried out during the CUHK period but is not in this repository because the files contain data, third-party material or documents belonging to collaborators:

- Cleaning and analysis of a three-wave community eHealth randomized controlled trial
- Generation of a stratified block randomization list for a three-arm trial
- A systematic review and meta-analysis of post-viral symptoms
- A health literacy and cancer-screening follow-up study combining returned and non-returned questionnaires
- An analytic hierarchy process study of patient-reported barriers to community health services
- A multicentre telecare programme evaluation
- Manuscript preparation and responses to reviewers
- Mentoring a research assistant and supporting a capstone project
- Handover documentation prepared before I left my role at CUHK

## Reproducibility notes

These are working analysis scripts rather than a packaged pipeline. There is no dependency lockfile or automated build system. Some paths are set in a single variable at the top of each script, and some exploratory specifications remain in commented blocks.

The repository does not include a runnable example dataset because the underlying data cannot be redistributed.

## Related publications

- Tam KW, Zhang D, Li Y, Xu Z, Li Q, Zhao Y, Niu L, Wong SYS. Meaning in life: bidirectional relationship with depression, anxiety, and loneliness in a longitudinal cohort of older primary care patients with multimorbidity. *BMC Geriatrics*. 2025;25(1):195. https://doi.org/10.1186/s12877-025-05762-7
- Poon PKM, Tam KW, Yip BHK, Chung RY, Lee EKP, Wong SYS. Social and health service-related factors associated with undiagnosed diabetes mellitus: a population-based survey in a highly urbanized Chinese setting. *BMC Public Health*. 2025;25(1):900. https://doi.org/10.1186/s12889-025-22048-0
- Poon PK, Tam KW, Lam T, Luk AK, Chu WC, Cheung P, Wong SY, Sung JJ. Poor health literacy associated with stronger perceived barriers to breast cancer screening and overestimated breast cancer risk. *Frontiers in Oncology*. 2023;12:1053698. https://doi.org/10.3389/fonc.2022.1053698
- Poon PK, Tam KW, Zhang D, Yip BH, Woo J, Wong SY. Handgrip strength but not SARC-F score predicts cognitive impairment in older adults with multimorbidity in primary care: a cohort study. *BMC Geriatrics*. 2022;22(1):342. https://doi.org/10.1186/s12877-022-03034-2
- Xu Z, Wang W, Zhang D, Tam KW, Li Y, Chan DCC, Yang Z, Wong SYS. Excess risks of long COVID symptoms compared with identical symptoms in the general population: a systematic review and meta-analysis of studies with control groups. *Journal of Global Health*. 2024;14:05022. https://doi.org/10.7189/jogh.14.05022
- Xu Z, Zheng X, Ding H, Zhang D, Cheung PMH, Yang Z, Tam KW, Zhou W, et al. The effect of walking on depressive and anxiety symptoms: systematic review and meta-analysis. *JMIR Public Health and Surveillance*. 2024;10(1):e48355. https://doi.org/10.2196/48355
