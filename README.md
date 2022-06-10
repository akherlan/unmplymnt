# unmplymnt

**Looking for Job Opportunity**

The internet should be a place for everyone unless you have no money for just paying a bill for little bit of request-response data. You must have a job.

## Installation

You can install the development version of {unmplymnt} from GitHub with:

```r
install.package("remotes")
remotes::install_github("akherlan/unmplymnt")
```

## Usage

There are three main functions of this package: `glints()`, `jobstreet()`, and `indeed()`. All of them are for pulling job vacancies as a data frame.

```r
library("unmplymnt")
glints() # return data analyst job from Glints
jobstreet("data engineer") # return latest data engineering job
indeed("data analyst") # return data analyst job from Indeed (ID)
```

## Disclaimer

The {unmplymnt} package is not associated with the mentioned job information providers. All data is provided subject to any restrictions, terms of use, and licensing arrangements noted on each website (we tried to).
