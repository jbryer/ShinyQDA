
# <img src="man/figures/ShinyQDA.png" align="right" width="120" align="right" /> ShinyQDA: R Package and Shiny Application for the Analysis of Qualitative Data

<!-- badges: start -->

[![R-CMD-check](https://github.com/jbryer/ShinyQDA/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/jbryer/ShinyQDA/actions/workflows/R-CMD-check.yaml)
[![](https://img.shields.io/badge/devel%20version-0.5.0-blue.svg)](https://github.com/jbryer/ShinyQDA)
[![](https://www.r-pkg.org/badges/version/ShinyQDA)](https://cran.r-project.org/package=ShinyQDA)
[![](https://img.shields.io/github/last-commit/jbryer/ShinyQDA.svg)](https://github.com/jbryer/ShinyQDA/commits/main)
<!-- badges: end -->

Package documentation: <https://jbryer.github.io/ShinyQDA/>

The `ShinyQDA` package is designed to assist researchers with the
analysis of qualitative data. As the name suggests, the premise is that
much of the interaction with the package will be done through a
[Shiny](https://shiny.rstudio.com) application. However, all the
functionality in the Shiny application is available through through the
R command line as well. `ShinyQDA` attempts to set in between
traditional qualitative data analysis that involves researchers coding
and/or scoring (using rubrics) documents/text and natural language
processing such as sentiment analysis, topic modeling, and text encoding
(i.e. tokenization). By using Shiny, multiple researchers can access the
application to code and/or score documents.

*Note: Qualitative data researchers tend to refer to the text they
analyze as documents. Natural language processing researchers tend to
refer to the data as text data. We try to use the phrase most
appropriate for how a feature may be used, but they can often be used
interchangeably.*

![Screencast of ShinyQDA](man/figures/ShinyQDA_screencast.gif)

## Getting Started

To install, use the `remotes` package:

``` r
remotes::install_github('jbryer/ShinyQDA')
```

You can demo the ShinyQDA application using sample data from the
[Diagnostic Assessment and Achievement of College
Skills](https://daacs.net) using the following command:

``` r
demo('ShinyQDA', package = 'ShinyQDA')
```

## Creating a new ShinyQDA application

The `ShinyQDA::new_app()` function will help initialize an new
application. At a minimum you need a data frame with two columns: 1. a
primary key column and 2. a column containing text data. However, you
may include any other columns that you may want to use in your analysis.
In the example below we will use a small subset of essasy completed by
students as part of the [Diagnostic Assessment and Achievement of
College Skills](https://daacs.net) (DAACS) project.

``` r
data("daacs_data", package = 'ShinyQDA')
ShinyQDA::new_app(name = 'daacs_demo',
                  dir = getwd(),
                  qda_data = daacs_data)
```

The `new_app` function will create an `app.R` file, a `qda.sqlite` file
(which is a SQLite database containing all the data), as well as
optionally directories containing the dictionaries for conducting
sentiment analysis. This will also start the Shiny application allowing
you to login using the username “admin” and password “pass”. You can run
the application again using the followign command:

``` r
shiny::runApp('daacs_demo')
```

The `ShinyQDA::qda()` acts as a wrapper to the database (currently using
SQLlite) containing all the data. There are a number of functions
available for adding, editing, and deleting data as well as accessing
specific data elements. However, the `ShinyQDA::qda_merge()` function
will provide a merged data frame with the original data imported along
with any qualitative data analysis conducted (including sentiment
analysis, tokenization, etc.).

``` r
daacs_qda <- ShinyQDA::qda('daacs_test/qda.sqlite')
daacs_merged <- ShinyQDA::qda_merge(daacs_qda, include_sentiments = TRUE)
```

## Authentication

Authentication is handled by the
[`shinymanager`](https://datastorm-open.github.io/shinymanager/) R
package. By default ShinyQDA will create an administrator user with
`admin` and `pass` as the username and password. We recommend changing
the password after your first sign-in. User management (including
password changing) is handled by clicking the plus (`+`) icon in the
lower right hand corner. This will take you into `shinymanager`’s user
management mode. All data entered into ShinyQDA has a username
associated with it. When in authentication mode the username will be
retrieved from `shinymanager`, otherwise ShinyQDA will use the value of
`Sys.info()['user']`.

## ShinyQDA Features

Data entry, coding, and scoring features

  - User management via the
    [`shinymanager`](https://datastorm-open.github.io/shinymanager/)
    package to allow multiple coders to work on the same dataset over
    the internet.
  - Define an arbitrary set of questions/codes to assign to each
    document. Current question types include checkbox (for multiple
    selections), radio (for choose one from a list), and open text.
  - Highlight text (e.g. word, sentence, paragraph, etc.) of text to
    code. This will open a modal dialog to add codes.
  - Define an arbitrary set of questions/codes to assign to each coding
    (i.e. highlighted text). Current question types include checkbox
    (for multiple selections), radio (for choose one from a list), and
    open text.
  - Score documents using a rubric.

Analysis features:

  - Basic descriptive information for individual documents including
    character, word, sentence, and paragraph counts.
  - Sentiment analysis at the individual document level. Words within
    the document that appear in the specified sentiment dictionary are
    highlighted and color coded to correspond to a histogram of the
    sentiments within a single document.
  - Sentiment analysis across the entire database. Varying plots are
    provided approproate to the sentiment dictionary specified.
  - Co-occurrence plot to examine how codes co-occur across documents.
  - Word frequency analysis across the entire database.
  - Code frequency analysis across all codes entered.
  - Word clouds.
  - Topic modeling.
  - Inter-rater reliability analysis. For coding have a split view.

Data export features:

  - A data table view showing all documents along with imported
    metadata, codes, code question responses, document descriptive
    statistics, and sentiment. Data can be exported/downloaded as CSV or
    Excel.
  - A raw database view is provided with the ability to export/download
    as Excel (using multiple tabs) or raw SQLite database.

## Roadmap

  - Ability to see other coders responses (for the text questions).
  - Merge coding results into data export tab. Allow user to choose
    aggregation method.
  - Tokenization - allow the user to define and export/download various
    tokenization schemes.
  - Predictive modeling - use any data (including sentiment, topics,
    tokenizations, etc.) to predict an outcome.
  - Queuing and user roles - Create a scheme to limit access to certain
    features depending on user role. Allow documents to be assigned to
    users to code/score.
  - Make the app prettier / improve the design. An alternative design
    has been started in inst/bs4dash/
  - Convert the coding tab to use the Shiny module framework.
  - New hex logo (my design skills are not great, looking for a
    volunteer 😜)
  - Submit to CRAN when development is stable.
  - Ability to use databases other than SQLite. This will require
    writing a connector for shinymanager.
  - Reduce the number of packages dependencies. Some of the packages
    could be moved to “Suggests” that are only required for specific
    modules. For example, add an option to not incude sentiment analyses
    which would eliminate the need for those to be required.

## Code of Conduct

Please note that the ShinyQDA project is released with a [Contributor
Code of
Conduct](https://jbryer.github.io/ShinyQDA/CODE_OF_CONDUCT.html). By
contributing to this project, you agree to abide by its terms.
