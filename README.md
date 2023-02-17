# ShinyQDA: R Shiny Application for Qualitative Data Analysis <img src="man/figures/ShinyQDA.png" align="right" width="120" align="right" />


![Screencast of ShinyQDA](man/figures/ShinyQDA_screencast.gif)

To install, use the `remotes` package:

```
remotes::install_github('jbryer/ShinyQDA')
```

Run the Shiny app with:

```
ShinyQDA::shinyQDA()
```

## Setup

```
qda_data <- qda(file = 'ShinyQDA.sqlite')
```

## Sentiment Analysis

```
app_dir <- './'
textdata::lexicon_afinn(dir = app_dir)
textdata::lexicon_bing(dir = app_dir)
textdata::lexicon_loughran(dir = app_dir)
textdata::lexicon_nrc(dir = app_dir)
```

## Authentication

Authentication is handled by the [`shinymanager`](https://datastorm-open.github.io/shinymanager/) R package. By default, if authentication is enabled, ShinyQDA will create an administrator user with `admin` and `pass` as the username and password. We recommend changing the password after your first sign-in. User management (including password changing) is handled by clicking the plus (`+`) icon in the lower right hand corner. This will take you into `shinymanager`'s user management mode. All data entered into ShinyQDA has a username associated with it. When in authentication mode the username will be retrieved from `shinymanager`, otherwise ShinyQDA will use the value of `Sys.info()['user']`.
