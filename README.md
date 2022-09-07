# unmplymnt

![GitHub](https://img.shields.io/github/license/akherlan/unmplymnt?color=informational)
![GitHub R package version (branch & subdirectory of monorepo)](https://img.shields.io/github/r-package/v/akherlan/unmplymnt/main?color=informational&filename=DESCRIPTION&logo=r)
![GitHub issues](https://img.shields.io/github/issues/akherlan/unmplymnt)
![GitHub last commit](https://img.shields.io/github/last-commit/akherlan/unmplymnt?color=informational&logo=github)

**Looking for Job Opportunity**

The internet should be a place for everyone unless you have no money for just paying a bill for little bit of request-response data. You must have a job.

## Installation

You can install the development version of {unmplymnt} from GitHub with:

```r
install.package("remotes")
remotes::install_github("akherlan/unmplymnt")
```

## Usage

There are three main functions of this package: `glints()`, `jobstreet()`, and `indeed()`. All of them are pulling job vacancies from the websites and return as a data frame.

```r
library("unmplymnt")
glints() # return data analyst job from Glints
jobstreet("data engineer") # return latest data engineering job
indeed("data analyst") # return data analyst job from Indeed (ID)
```

## Disclaimer

The {unmplymnt} package is not associated with the mentioned job information providers. All data is provided subject to any restrictions, terms of use, and licensing arrangements noted on each website (we tried to).
