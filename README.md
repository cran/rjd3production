
<!-- README.md is generated from README.Rmd. Please edit that file -->

# {rjd3production}

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/rjd3production)](https://CRAN.R-project.org/package=rjd3production)

[![R-CMD-check](https://github.com/InseeFr/rjd3production/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/InseeFr/rjd3production/actions/workflows/R-CMD-check.yaml)
[![lint](https://github.com/InseeFr/rjd3production/actions/workflows/lint.yaml/badge.svg)](https://github.com/InseeFr/rjd3production/actions/workflows/lint.yaml)

[![GH Pages
built](https://github.com/InseeFr/rjd3production/actions/workflows/pkgdown.yaml/badge.svg)](https://github.com/InseeFr/rjd3production/actions/workflows/pkgdown.yaml)
<!-- badges: end -->

<div align="center">

## [🇫🇷 README en français](#présentation) \| [🇬🇧 README in english](#overview)

</div>

### Présentation

**{rjd3production}** aide les producteurs de données CVS-CJO à mettre en
place des chaînes de production.

Il permet notamment de :

- Créer des calendriers français et régresseurs de calendrier
  compatibles JDemetra+

- Identifier des SAI par leur nom

- Selectionner les jeux de calendrier pour une ou plusieurs séries

- Manipuler les régresseurs de calendrier et les outliers d’un Workspace
  selon la dynamique suivante:

  - Les fonctions `import_XXX()` et `export_XXX()` permettent de
    convertir les data.frame contenant les outliers et régresseurs de
    calendrier en fichiers et inversement

  - Les fonctions `retrieve_XXX()` et `assign_XXX()` permettent
    d’extraire (resp. d’assigner) les outliers et régresseurs de
    calendrier d’un workspace

``` mermaid

flowchart LR
    %% Objects
    WS["WS<br/>(JDemetra+<br/>workspace)"]
    DF_OUT["outliers_df<br/>(data.frame)"]
    DF_TD["td_df<br/>(data.frame)"]
    YAML_OUT["outliers YAML<br/>(outliers_&lt;ws_name&gt;.yaml)"]
    YAML_TD["TD YAML<br/>(td_&lt;ws_name&gt;.yaml)"]
    SERIES["Series<br/>(time series data)"]

    %% Outliers workflow
    DF_OUT -->|"assign_outliers()"| WS
    WS -->|"retrieve_outliers()"| DF_OUT
    DF_OUT -->|"export_outliers()"| YAML_OUT
    YAML_OUT -->|"import_outliers()"| DF_OUT

    %% TD workflow
    SERIES -->|"select_td()"| DF_TD
    WS -->|"retrieve_td()"| DF_TD
    DF_TD -->|"export_td()"| YAML_TD
    YAML_TD -->|"import_td()"| DF_TD
    DF_TD -->|"assign_td()"| WS

    %% Styles
    classDef ws fill:#e6f2ff,stroke:#4a7ebb,stroke-width:1px;
    classDef df fill:#e9f7ef,stroke:#2e8b57,stroke-width:1px;
    classDef yaml fill:#fff3e0,stroke:#cc8400,stroke-width:1px;
    classDef series fill:#f5e6ff,stroke:#7a3db8,stroke-width:1px;

    class WS ws
    class DF_OUT,DF_TD df
    class YAML_OUT,YAML_TD yaml
    class SERIES series
```

## Installation

**{rjd3production}** s’appuie sur le package
[**{rJava}**](https://CRAN.R-project.org/package=rJava)

L’exécution des packages rjd3 nécessite **Java 21 ou plus**. La manière
de mettre en place une telle configuration dans R est expliquée
[ici](https://jdemetra-new-documentation.netlify.app/#Rconfig).

### Latest release

Pour obtenir la version stable actuelle (à partir de la dernière
version) :

Depuis le CRAN :

``` r
install.packages("rjd3production")
```

- Depuis GitHub :

``` r
# install.packages("remotes")
remotes::install_github("InseeFr/rjd3production@*release")
```

- De
  [r-universe](https://TanguyBarthelemy.r-universe.dev/rjd3production) :

``` r
install.packages("rjd3production", repos = c("https://TanguyBarthelemy.r-universe.dev", "https://cloud.r-project.org"))
```

### Version de développement

Vous pouvez installer la version de développement de
**{rjd3production}** depuis \[GitHub\] (<https://github.com/>) avec :

``` r
# install.packages("remotes")
remotes::install_github("InseeFr/rjd3production")
```

<!-- ### Usage -->

<!-- #### Chargement du package -->

<!-- ```{r fr-loading-rjd3production, eval = TRUE} -->

<!-- library("rjd3production") -->

<!-- ``` -->
## 🇬🇧 README in english
### Overview

**{rjd3production}** helps producers of seasonal data to set up
production lines.

In particular, it enables you to:

- Create JDemetra+-compatible French calendars and calendar regressors

- Identify SAIs by name

- Select calendar sets for one or more series

- Manipulate Workspace calendar regressors and outliers according to the
  following dynamics:

  - The `import_XXX()` and `export_XXX()` functions convert data.frames
    containing calendar outliers and regressors into files, and vice
    versa.
  - The `retrieve_XXX()` and `assign_XXX()` functions extract (resp.
    assign) calendar outliers and regressors from a workspace.

``` mermaid

flowchart LR
    %% Objects
    WS["WS<br/>(JDemetra+<br/>workspace)"]
    DF_OUT["outliers_df<br/>(data.frame)"]
    DF_TD["td_df<br/>(data.frame)"]
    YAML_OUT["outliers YAML<br/>(outliers_&lt;ws_name&gt;.yaml)"]
    YAML_TD["TD YAML<br/>(td_&lt;ws_name&gt;.yaml)"]
    SERIES["Series<br/>(time series data)"]

    %% Outliers workflow
    DF_OUT -->|"assign_outliers()"| WS
    WS -->|"retrieve_outliers()"| DF_OUT
    DF_OUT -->|"export_outliers()"| YAML_OUT
    YAML_OUT -->|"import_outliers()"| DF_OUT

    %% TD workflow
    SERIES -->|"select_td()"| DF_TD
    WS -->|"retrieve_td()"| DF_TD
    DF_TD -->|"export_td()"| YAML_TD
    YAML_TD -->|"import_td()"| DF_TD
    DF_TD -->|"assign_td()"| WS

    %% Styles
    classDef ws fill:#e6f2ff,stroke:#4a7ebb,stroke-width:1px;
    classDef df fill:#e9f7ef,stroke:#2e8b57,stroke-width:1px;
    classDef yaml fill:#fff3e0,stroke:#cc8400,stroke-width:1px;
    classDef series fill:#f5e6ff,stroke:#7a3db8,stroke-width:1px;

    class WS ws
    class DF_OUT,DF_TD df
    class YAML_OUT,YAML_TD yaml
    class SERIES series
```

### Installation

To get the current stable version (from the latest release):

From CRAN:

``` r
install.packages("rjd3production")
```

- From GitHub:

``` r
# install.packages("remotes")
remotes::install_github("InseeFr/rjd3production@*release")
```

- From
  [r-universe](https://TanguyBarthelemy.r-universe.dev/rjd3production):

``` r
install.packages("rjd3production", repos = c("https://TanguyBarthelemy.r-universe.dev", "https://cloud.r-project.org"))
```

### Development version

You can install the development version of **{rjd3production}** from
[GitHub](https://github.com/) with:

``` r
# install.packages("remotes")
remotes::install_github("InseeFr/rjd3production")
```

<!-- ### Usage -->

<!-- #### Loading the package -->

<!-- ```{r en-loading-rjd3production, eval = TRUE} -->

<!-- library("rjd3production") -->

<!-- ``` -->
