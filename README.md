
<!-- README.md is generated from README.Rmd. Please edit that file -->

# DulogInteractions

This R package is designed for processing data from Dulog Sensors.

## Installing DulogInteractions

Without uploading the Dulogs data as raw package data, installing the
DulogsInteractions package is slightly more tricky. Follow these steps:

1.  Clone the repository to your local machine. See this
    [guide](https://www.w3docs.com/learn-git/git-clone.html), for
    example.
2.  Add the `dulogs-raw` data folder to the `DulogsInteractions`
    directory.
3.  Open the R project corresponding to the cloned repository.
4.  Run `inst/scripts/setup.R`.

You should now have `DulogsInteractions` as a library and be able to
recreate the following examples. If the package is ever updated on
GitHub, you will want to pull the updates (see
[guide](https://www.w3docs.com/learn-git/git-pull.html), for example)
and rerun `inst/scripts/setup.R`.

## DulogsInteractions Pipeline

The DulogsInteractions pipeline is separated into three steps:

1.  Reading the data with `read_dulogs`.
2.  Preprocessing the data with `preprocess_dulogs`.
3.  Constructing interactions with `construct_interactions`.
4.  Restructuring interactions as an edgelist with `as_edgelist`.

We will start by providing a basic example of the functionality before
describing each component of the pipeline in detail.

``` r
# set up logs for each function
baselog <- 'logs/vignette'

# read and combine mobile node and in range data according to the requested `time_eps`
#  and `barn_rule`
el <- read_dulogs(
  paths = c(mobilenode = 'dulogs/2022', inrange = 'dulogs/2022'),
  patterns = c(mobilenode = 'MN_DATA', inrange = 'MN_RANGE'),
  year = 2022,
  time_eps = 2,
  barn_rule = 'or',
  log = file.path(baselog, 'read_dulogs')
) %>%
  # preprocess dulog meetings to remove data oddities and filter meetings by rssi,
  #  meeting times, and whether to keep all meetings or only those in a barn. The
  #  `time_eps` is used to classify meetings as reciprocal meetings.
  preprocess_dulogs(
    .,
    min_rssi = -Inf,
    max_rssi = 15,
    start_time = '0000-01-01 UTC',
    end_time = '9999-01-01 UTC',
    only_barn = FALSE,
    time_eps = 2,
    log = file.path(baselog, 'preprocess_dulogs')
  ) %>%
  # construct interactions according to the `time_gap`. A gap of at least `time_gap`
  #  seconds between contiguous meetings necessitates a new interaction. The user
  #  also specifies whether to treat meetings as directed or undirected, and whether
  #  to remove self loops. If undirected edges are requested, reciprocal edges taking
  #  place at the same time are collapsed into one meeting.
  construct_interactions(.,
                         time_gap = 8,
                         directed = FALSE,
                         loops = FALSE,
                         log = file.path(baselog, 'construct_interactions')) %>%
  # filter the interactions according to preferences and convert to an edgelist
  as_edgelist(
    .,
    min_secs_elapsed = 4,
    max_secs_elapsed = Inf,
    min_proportion_barn = 0,
    max_proportion_barn = 1
  )

head(el)
```

    #> # A tibble: 6 × 5
    #>   year  from_node to_node num_interactions secs_interactions
    #>   <chr> <chr>     <chr>              <int>             <dbl>
    #> 1 2022  12        60                   181             27256
    #> 2 2022  12        61                     7                32
    #> 3 2022  12        63                   218              4254
    #> 4 2022  12        64                    94              4028
    #> 5 2022  12        66                    14                76
    #> 6 2022  12        68                   563             54946

### Raw package data

Currently, there are two required raw package data files for this
pipeline to function: `birds-raw.csv` and `timeframe-raw.csv`, each
located in the `data-raw` folder. Any time these respective comma
separated value files are updated, their respective R scripts
(e.g. `birds.R` and `timeframe.R`) must be run. Do not edit the R
scripts, simply run them.

**`birds-raw.csv`**

At the very least, this comma separated value file *must* contain
columns:

- `year`: year under study, formatted as YYYY,
- `tag`: numeric value indicating the Dulog tag ID deployed in the year
  under study,
- `band_id`: unique 9-digit band number describing the bird.

Other columns are tag attributes which will be helpful in the analysis
phase. Examples may include things like sex of the bird, mating tag,
and/or other identifying information.

**`timeframe-raw.csv`**

At the very least, this comma separated value file *must* contain
columns:

- `year`: year under study, formatted as YYYY,
- `tz`: timezone as it appears in the [timezone
  database](https://en.wikipedia.org/wiki/List_of_tz_database_time_zones),
- `start_year`: year when tags are turned on, formatted as YYYY,
- `start_month`: month when tags are turned on, formatted as MM,
- `start_day`: day when tags are turned on, formatted as DD,
- `start_hour`: hour (using a [24-hour
  clock](https://en.wikipedia.org/wiki/24-hour_clock)) when tags are
  turned on, formatted as HH,
- `end_year`: year when tags are turned off, formatted as YYYY,
- `end_month`: month when tags are turned off, formatted as MM,
- `end_day`: day when tags are turned off, formatted as DD,
- `end_hour`: hour (using a [24-hour
  clock](https://en.wikipedia.org/wiki/24-hour_clock)) when tags are
  turned off, formatted as HH.

### Reading the dulogs data

There are two sources of data: mobile node data files and in range data
files. Mobile node data files record meetings between birds, logging the
meeting time, sender tag, and receiver tag. The in range data files
record when a bird is registered by a base station. The base stations
are located in the barns, so we use these data as proxy for whether a
bird is in the barn. There is imprecision in the clocks, so these files
are combined using a fuzzy join on year, node, and meeting time. We deem
a bird in the barn at the time of their meeting if they were registered
by the base station in the in range data within `time_eps` (inclusive,
either before or after) seconds of their meeting time. For example,
suppose an epsilon of 2 is provided. If bird $A$ met bird $B$ at time
$t$, and bird $A$ is listed in the in range data at time $t+1$ whereas
bird $B$ is listed at $t+3$, then bird $A$ is deemed in the barn while
bird $B$ is deemed out of the barn. Even still, if either the sender,
receiver, or both birds are deemed in the barn during their meeting
time, then the meeting is said to be in the barn if the barn rule is set
to ‘or’. The following describe each column of the resulting data frame:

- `year` (`chr`): year under study,
- `origin` (`chr`): mobile node data file containing this meeting,
- `meeting_date` (`POSIXct`): Unix timestamp for meeting, formatted as
  YYYY-MM-DD HH:MM:SS,
- `rx_node` (`chr`): receiver tag, must be coercible to a numeric,
- `tx_node` (`chr`): sender tag, must be coercible to a numeric,
- `rssi` (`num`): signal strength for meeting, highly positive implies
  the birds were closer in physical proximity,
- `received date` (`POSIXct`): Unix timestamp for data retrieval,
  formatted as YYYY-MM-DD HH:MM:SS,
- `rx_barn` (`logi`): indicator of whether the meeting time is within
  `time_eps` of a base station log corresponding to the receiver,
- `tx_barn` (`logi`): indicator of whether the meeting time is within
  `time_eps` of a base station log corresponding to the sender,
- `barn` (`logi`): indicator of whether either the sender, receiver, or
  both was identified in the barn at the time of meeting.

### Preprocessing the dulogs data

The dulog data have known limitations that we address in the
preprocessing step. These issue include addressing meetings:

1.  taking place outside of the specified dates under study,
2.  taking place outside of the specified daily times under study,
3.  involving a phantom actor (i.e. an undocumented bird),
4.  which are duplicated (i.e. same timestamp, sender, receiver, and
    signal strength)
5.  which are nearly duplicated (i.e. same timestamp, sender, receiver,
    but different signal strengths).

Any meeting characterized by points one through four are removed from
the dulogs data (the combined mobile node and in range data files). To
resolve point five, the near duplicate meeting with the smallest
(i.e. most negative) signal strength is preserved, providing us with the
most conservative estimate. Meetings are also removed according to user
input, including any meeting:

1.  with a signal strength outside the range of `min_rssi` and
    `max_rssi`,
2.  before the `start_date` or after the `end_date`,
3.  taking place outside of the barn (if `only_barn` is set to `TRUE`).

After removing meetings between birds according to the above
specifications, the data are prepared the network construction step.
This includes the creation of several columns which are useful for
constructing interactions at the next stage according to the user’s
preferences (e.g., see `i_node` and `j_node` which are helpful at
constructing undirected interactions). The following describe each
column of the resulting data frame:

- `year` (`chr`): year under study,
- `origin` (`chr`): mobile node data file containing this meeting,
- `meeting_date` (`POSIXct`): Unix timestamp for meeting, formatted as
  YYYY-MM-DD HH:MM:SS,
- `rx_node` (`chr`): receiver tag, must be coercible to a numeric,
- `tx_node` (`chr`): sender tag, must be coercible to a numeric,
- `rx_id` (`chr`): receiver tag description from raw package data,
- `tx_id` (`chr`): sender tag description from raw package data,
- `rx_barn` (`logi`): indicator of whether the meeting time is within
  `time_eps` of a base station log corresponding to the receiver,
- `tx_barn` (`logi`): indicator of whether the meeting time is within
  `time_eps` of a base station log corresponding to the sender,
- `rssi` (`num`): signal strength for meeting, highly positive implies
  the birds were closer in physical proximity,
- `barn` (`logi`): indicator of whether either the sender, receiver, or
  both was identified in the barn at the time of meeting.
- `i_node` (`chr`): tag with smallest sorted value, $i$, must be
  coercible to a numeric,
- `j_node` (`chr`): tag with largest sorted value, $j$, must be
  coercible to a numeric,
- `i_id` (`chr`): tag $i$ description from raw package data,
- `j_id` (`chr`): tag $j$ description from raw package data
- `i_barn` (`lgl`): indicator of whether the meeting time is within
  `time_eps` of a base station log corresponding to tag $i$
- `j_barn` (`lgl`): indicator of whether the meeting time is within
  `time_eps` of a base station log corresponding to tag $j$
- `received_date` (`POSIXct`): Unix timestamp for data retrieval,
  formatted as YYYY-MM-DD HH:MM:SS,
- `swapped` (`lgl`): indicator of whether the values of `rx_node` and
  `tx_node` were swapped to create `i_node` and `j_node`.

The choice of `time_eps` in the reading step will not impact the
preprocessing step unless `only_barn` is set to `TRUE`. In this case, a
larger `time_eps` will result in fewer meetings excluded because of the
`only_barn` constraint. Assuming a `time_eps` of two, the following
represents the number of meetings remaining (and removed) after each
consideration of the preprocessing step when the arguments are set to
the defaults (i.e. `max_rssi` of 15; all other arguments such as
`min_rssi` and `only_barn` are set so not to exclude meetings):

    #> # A tibble: 12 × 3
    #>    description                  n_meetings n_removed
    #>    <chr>                             <int>     <dbl>
    #>  1 input                            999050         0
    #>  2 bind_pkg_data                    999050         0
    #>  3 within_dates                     928211     70839
    #>  4 within_times                     928201        10
    #>  5 phantom_actors                   912534     15667
    #>  6 duplicates                       870217     42317
    #>  7 near_duplicates                  870190        27
    #>  8 rssi_range                       870190         0
    #>  9 only_barn                        870190         0
    #> 10 startend_time_range              870190         0
    #> 11 organize_rows_cols               870190         0
    #> 12 classify_reciprocal_meetings     870190         0

While not strictly necessary, the same preprocessing used to address
known data limitations is conducted on the in range data during data
reading step. This preprocessing and logging of the in range data is
largely to document any issues with the in range data. Assuming a
`time_eps` of two, the following represents the number of pulses by the
base stations (in range data) remaining (and removed) at each stage of
preprocessing:

    #> # A tibble: 8 × 3
    #>   description     n_meetings n_removed
    #>   <chr>                <int>     <dbl>
    #> 1 input               779007         0
    #> 2 bind_pkg_data       779007         0
    #> 3 within_dates        778650       357
    #> 4 within_times        775987      2663
    #> 5 phantom_actors      775900        87
    #> 6 duplicates          771663      4237
    #> 7 near_duplicates     659681    111982
    #> 8 output              659681         0

For more detailed information regarding these matters, see the logging
files.

### Constructing interactions from the dulogs data

Once the data are preprocessed accordingly, the next step is to
construct interactions from meeting data. Interactions are composed of
sequential meetings between the actors where an interaction is defined
by the number of seconds between meetings. That is, if two birds meet
and their next recorded meeting occurs at least `time_gap` seconds
later, then the birds necessarily interacted twice. Otherwise, the birds
interacted once for a longer period of time. The user should specify
whether the interactions should be considered directed. If a meeting
between bird $A$ (the receiver, `rx_node`) and bird $B$ (the sender,
`tx_node`) should be treated differently than a meeting bird $B$ (the
receiver, `rx_node`) to bird $A$ (the sender, `tx_node`), then the
interactions should be directed. Furthermore, the user should specify
whether loops should be retained. These are meetings where the same bird
is logged as both the sender and the receiver. By default, interactions
are assumed undirected and loops are excluded from consideration.

For now, the following describe each column of the resulting data frame:

- `year` (`chr`): year under study,
- `from_node` (`chr`): if directed, sender tag; otherwise, tag with
  smallest sorted value,
- `to_node` (`chr`): if directed, receiver tag; otherwise, tag with the
  largest sorted value,
- `interaction_group` (`dbl`): counter for interaction by pairs of nodes
  in a given year; each row is uniquely identified by year, from and to
  nodes, and the interaction group,
- `start_time` (`POSIXct`): Unix timestamp marking the beginning of the
  interaction, formatted as YYYY-MM-DD HH:MM:SS,
- `end_time` (`POSIXct`): Unix timestamp marking the end of the
  interaction, formatted as YYYY-MM-DD HH:MM:SS,
- `secs_elapsed` (`dbl`): seconds elapsed in the interaction,
- `num_meetings` (`int`): number of meetings recorded in the
  interaction,
- `meetings_per_sec` (`dbl`): number of meetings recorded per second in
  the interaction,
- `rssi_min` (`dbl`): minimum RSSI in the interaction,
- `rssi_med` (`dbl`): median RSSI in the interaction,
- `rssi_mean` (`dbl`): mean RSSI in the interaction,
- `rssi_max` (`dbl`): maximum RSSI in the interaction.

### Restructuring interactions as an edgelist

There may be reason to exclude interactions. For example, if the
researcher is interested in only short interactions, it may be useful to
filter according to the seconds elapsed. Furthermore, if the user wishes
to capture interactions that largely (but perhaps not completely) take
place in the barn, the user can filter according to the proportion of
meetings taking place in the barn. Once the interactions have been
filtered in this way, an edgelist is constructed.

An edgelist consists of four columns: `year`, `from_node`, `to_node`,
and `num_interactions`. Since Dulog tags are not unique across years,
relations between birds are defined by the tuple: `year`, `from_node`,
and `to_node`. The number of interactions, `num_interactions`, provides
the total number of interactions (of interest) between the from and to
nodes in a given year. If the interactions requested are undirected,
from and to are simply nomenclature to describe the adjacent pair (where
‘from’ dicates the node with the smallest dulog tag number). Otherwise,
if the interactions requested are directed, from and to distingush the
sender node (from) from the receiver node (to).

For now, the following describe each column of the resulting data frame:

- `year` (`chr`): year under study,
- `from_node` (`chr`): if directed, sender tag; otherwise, tag with
  smallest sorted value,
- `to_node` (`chr`): if directed, receiver tag; otherwise, tag with the
  largest sorted value,
- `num_interactions` (`int`): total number of interactions of interest
  (i.e. satisfying the arguments of the function).

## Acknkowledgements

This work was supported in part by NSF award 1856229.
