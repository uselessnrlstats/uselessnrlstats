---
editor_options: 
  markdown: 
    wrap: 72
---

# <img src="UNS_logos/UNS_logo2_small.png" align="left" height="150" style="padding: 20px;"/> uselessnrlstats

## Welcome to Useless NRL Stats!

We are dedicated to finding and visualising the most useless NRL
(National Rugby League) and NRLW (NRL Women's) stats possible. We have
pages on
[Instagram](%5Bwww.instagram.com/uselessnrlstats/%5D(https://www.instagram.com/uselessnrlstats/)%7B.uri%7D),
[Facebook](%5Bwww.facebook.com/uselessnrlstats%5D(https://www.facebook.com/uselessnrlstats)%7B.uri%7D)
and
[BlueSky](%5Bbsky.app/profile/uselessnrlstats.bsky.social%5D(https://bsky.app/profile/uselessnrlstats.bsky.social)%7B.uri%7D) -
follow us there to see regular posts!

All of our data is sourced from the publicly available
[rugbyleagueproject.org
(RLP)](%5Bwww.rugbyleagueprjoect.org%5D(https://www.rugbyleagueproject.org/)).
We have scraped their data using {selenium} in Python. All data
cleaning, analysis and visualisation is performed in R, our preferred
data science language, with an emphasis on tidy code and workflows
courtesy of {tidyverse}.

This is a repository containing all of our open source data, code, and
visuals. We are strong believers in open data science, and that the best
way to get more people involved is to make it more accessible, which is
why we've made our data-scraping processes, data and cleaned data all
available here. Below you'll find brief and detailed descriptions of the
contents of this repository.

You are welcome to download, copy or fork our workflows, and we would
love to hear any feedback you might have for us! If you are interested
in collaborating with us, please shoot us a message on any of our
platforms!

#### What does this repository contain?

-   *analysis_scripts/*\
    This is our folder containing all of our scripts that we have used
    to actually produce the specific stats and figures. Some of them are
    not up to date or used, but you'll be able to find everything we've
    ever written in here
-   *cleaned_data/*\
    This folder contains all cleaned data for the NRL, NRLW and mens
    State of Origin in .csv format. Each sub-directory also contains
    data cleaning scripts
-   *data/*\
    This contains all raw data in the form it was scraped from RLP. Note
    that there is a sub-directory for each type of data, and a .csv file
    for every type of data for every NRL/NRLW/SOO season
-   *helper_stats/*\
    Any other handy data we've created along the way, such as
    premiership data
-   *images/*\
    Any club/NRL logos
-   *plots/*\
    Any plots / figures we've produced
-   *posts/*\
    The .svg and .png files of our posts. We have no background in
    graphic design so simply use Inkscape for these purposes.
-   *scraping_scripts/*\
    The .py scripts we use to scrape data from RLP. Note that there are
    a number of specifications you will need to follow if you want these
    to work for yourself
-   *tables/*\
    The .html and .png outputs of our tables that we produce for our
    stats
-   *UNS_logos/*\
    Our logos

#### What data is here?

If you've ended up here, you probably want to play around with the data.
Firstly, it is all cleaned and stored in *cleaned_data/*. The most
crucial point to note is that every unique entity (matches, players,
coaches, refs and venues) have a unique id number in our datasets -
these are used rather than names, with th exception of teams, which are
identified by a unique name.

But what data do we actually have? Well:

-   **match_data.csv** contains match-specific information: competition,
    round, date, time, crowd, as well as the home/away teams,
    half-time+full-time scores, and penalties.

-   **player_data.csv** contains player-specific information: first
    name, last name, birthday, birthplace, total matches, and total
    points.

-   **coach_data.csv** and **ref_data.csv** contains imilar
    individual-specific data for coaches and referees - names, birthdays
    etc.

-   **team_data.csv** contains data for every different team, with one
    entry for every different name used (e.g. Sydney Roosters vs Eastern
    Suburbs Roosters). Included data is then shortened names, mascots,
    abbreviations, and most importantly, unique names.

-   **team_logos.csv** includes logo and colour information for each
    unique team

-   **venue_data.csv** contains name, non-commercial-name and location
    (city, state, country) information for every venue that has hosted
    an NRL match

-   **player_match_data.csv**, **coach_match_data.csv**, and
    **ref_match_data.csv** contain match-specific information for each
    individual involved in each match. For referees, this is simply
    their on-field assignments to matches, while for coaches, it
    contains information on which team they coached in every game the
    were involved in.\
    **player_match_data.csv** contains team-list data (team, opposition,
    jersey number, position, captaincy) and and high-level in-game data
    (tries, penalty tries, goals, field goals, sin bins, send offs) for
    each player in each match

-   **ladder_round_data.csv** contains team ladder data (position, wins,
    losses, draws, byes, points for, points against) for every
    regular-season round of every season

#### What is the detailed workflow of the repository?

1.  Data is scraped from RLP using the scripts in *scraping_scripts/*.\
    Files ending in 'scrape' are used to scrape all data from the
    beginning of the competition. *rlp_nrl_scrape.py* collects all
    NSWRL/NSWRFL/ARL/Super League/NRL data from 1908-present. The script
    takes \~13-14hrs to run on our laptop. we run this once every \~3
    months.\
    Files ending in 'update' are used to scrape only data from the
    current season. We run this at least once a round, sometimes more.
2.  Data from RLP is collected in *data/*.\
    For each data-type, one .csv is generated per season (118 of men's
    NRL) - these are stored in their respective folders
3.  Raw data is cleaned by *cleaned_data/.../rlp_data_cleaning.R* and
    saved to *cleaned_data/\
    *A large R script is run for \~10 minutes to clean and aggregate the
    raw data into the 10 clean .csv files - the largest of which is
    34.3MB. *cleaned_data/.../produce_ladders.R* produces the 11th clean
    .csv file.
4.  All analysis, visualisation, etc. is performed with R scripts in
    *analysis_scripts/...*\
    Note the existence of *analysis_scripts/helper_stats.R* which
    contains regularly used lines or chunks of codes.\
    \
