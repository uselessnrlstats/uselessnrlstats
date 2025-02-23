# Rugbyleagueproject player data scrape for nrlw

## Initial Setup of chrome browser
import pandas as pd
import numpy as np
import time
import os
import glob
from selenium import webdriver
from selenium.webdriver.chrome.service import Service
from selenium.webdriver.chrome.options import Options
from selenium.webdriver.support.ui import WebDriverWait
from selenium.webdriver.common.by import By
from selenium.webdriver.support import expected_conditions as EC
from selenium.common.exceptions import StaleElementReferenceException
from webdriver_manager.chrome import ChromeDriverManager
from selenium.webdriver.support.select import Select

#Define function to overcome stale element error
def overcome_stale(EC_element):
    succeed = False
    while not succeed:
        try:
            wait.until(EC_element).click()
            succeed = True
        except:
            pass

options = Options()
options.headless = True
options.binary_location = '' # Path to chrome.exe on your local machine in the format "r'...\\Program Files\Google\Chrome\Application\chrome.exe'"
s = Service('') # Insert path to up to date Chrome driver on your local machine e.g. "r'....\anaconda3\envs\[env name]\Lib\site-packages\chromedriver_binary\chromedriver[###].exe'"
driver = webdriver.Chrome(service = s)
url = "https://www.rugbyleagueproject.org/competitions/nrlw/seasons.html"
driver.get(url)
driver.maximize_window()
wait = WebDriverWait(driver, 10)

match_data = pd.DataFrame(columns = ['competition',
                                     'round',
                                     'date',
                                     'time',
                                     'match_id',
                                     'venue_id',
                                     'crowd',
                                     'home_team',
                                     'home_team_ht_score',
                                     'home_team_score',
                                     'home_team_penalties',
                                     'away_team',
                                     'away_team_ht_score',
                                     'away_team_score',
                                     'away_team_penalties'])

team_data = pd.DataFrame(columns = ['team_short',
                                    'team_name'])

player_names_data = pd.DataFrame(columns = ['player_id',
                                            'player_name1',
                                            'player_name2',
                                            'birthday',
                                            'birthplace'])

coach_data = pd.DataFrame(columns = ['coach_id',
                                     'coach_name'])

ref_data = pd.DataFrame(columns = ['ref_id',
                                   'ref_name'])

venue_data = pd.DataFrame(columns = ['venue_id',
                                     'venue_name',
                                     'non-commercial_name',
                                     'location'])


player_match_data = pd.DataFrame(columns = ['player_id',
                                            'match_id',
                                            'team',
                                            'opposition_team',
                                            'number',
                                            'position',
                                            'captain']) 

player_scoring_data = pd.DataFrame(columns = ['match_id',
                                              'player_id',
                                              'score',
                                              'n_scores'])

coach_match_data = pd.DataFrame(columns = ['match_id',
                                           'coach_id',
                                           'team'])

ref_match_data = pd.DataFrame(columns = ['match_id',
                                         'ref_id'])

dir_name = '' # Path to working directory where data will be stored

used_teams = []
used_players = []
used_coaches = []
used_refs = []
used_venues = []
# Load in used IDs

matches_to_skip = []

# Initialise match_round variable
match_round = ""
#Initialise current position variable
current_position = ""

# Find all listed seasons
seasons = driver.find_elements("xpath", "//div[@id='content']/table[@class='list']/tbody/tr/td[1]/a")
# loop through all listed seasons
for season_index in range(0, len(seasons)):
    # Go to season results
    seasons[season_index].click()
    wait.until(EC.element_to_be_clickable((By.XPATH, "//main[@id='page_content']/ul/li[5]/a"))).click()
    # Deal with different super league format
    competition_page = driver.find_element("xpath", "//main[@id='page_content']/h1").text
    competition = ""
    season_url = driver.current_url
    # Identify all the matches to click on
    matches = driver.find_elements("xpath", "//main[@id='page_content']/div[@id='content']/table[@class='list']/tbody/tr")
    # Loop through all rows of the matches table
    for match_index in range(len(matches)):
        # Figure out the current match round
        match_row_th = matches[match_index].find_elements("xpath", "th")
        if len(match_row_th) == 1:
            match_round = match_row_th[0].text
        # Only consider it a match if the row has 11 columns
        match_columns = matches[match_index].find_elements("xpath", "td")
        if len(match_columns) == 11: # and match_round not in ["Major Prelim Semi", "Minor Prelim Semi", "Minor Semi Rep."]:
            # Wipe data about previous match in case it isn't present for this one
            match_date = match_time = match_venue_id = match_ref_id = match_crowd = match_ht_hs = match_ht_penalties = match_at_hs = match_at_penalties = ""
            # Store all text in the row
            match_information = [x.text for x in match_columns]
            # the match is complete if we have a score
            match_complete = all([x != " " for x in map(match_information.__getitem__, [4,6])])
            # Store the season (to remove trial matches)
            competition = match_information[0]
            # get match ID
            match_id = match_columns[10].find_element("xpath", "a").get_attribute('href')[43:]
            if match_id not in matches_to_skip and match_complete:
                match_columns[10].find_element("xpath", "a").click() 
                match_url = driver.current_url
                # Get match round
                # Get match teams and score
                score_body = driver.find_element("xpath", "//table[@class='program']/tbody[1]/tr[2]")
                match_home_team = score_body.find_element("xpath", "th[1]/table/tbody/tr/td[1]").text
                match_home_team_score = score_body.find_element("xpath", "th[1]/table/tbody/tr/td[2]").text
                match_away_team_score = score_body.find_element("xpath", "th[3]/table/tbody/tr/td[1]").text
                match_away_team = score_body.find_element("xpath", "th[3]/table/tbody/tr/td[2]").text
                # store teams if not already used
                if match_home_team not in used_teams:
                    entry = pd.DataFrame([[match_information[3], match_home_team]],
                                         columns = ['team_short', 'team_name'])
                    team_data = pd.concat([team_data, entry], axis = 0)
                    used_teams.append(match_home_team)
                if match_away_team not in used_teams:
                    entry = pd.DataFrame([[match_information[5], match_away_team]],
                                         columns = ['team_short', 'team_name'])
                    team_data = pd.concat([team_data, entry], axis = 0)
                    used_teams.append(match_away_team)
                # get match info
                match_info = driver.find_elements("xpath", "//table[@class='program']/tbody[@id='match_info']/tr")
                for match_info_index in range(len(match_info)):
                    match_info_heading = match_info[match_info_index].find_element("xpath", "th").text
                    if match_info_heading == "Date":
                        match_date = match_info[match_info_index].find_element("xpath", "td").text
                    elif match_info_heading == "Kick Off":
                        match_time = match_info[match_info_index].find_element("xpath", "td").text
                    elif match_info_heading in ["Referee", "Referees"]:
                        match_refs = match_info[match_info_index].find_elements("xpath", "td/a")
                        match_ref_id = [x.get_attribute('href')[44:] for x in match_refs]
                        referee_name = [x.text for x in match_refs]
                        # add ref to database if he isn't there already
                        for ref_index in range(len(match_ref_id)):
                            if match_ref_id[ref_index] not in used_refs:
                                entry = pd.DataFrame([[match_ref_id[ref_index], referee_name[ref_index]]],
                                                     columns = ['ref_id', 'ref_name'])
                                ref_data = pd.concat([ref_data, entry], axis = 0)
                                used_refs.append(match_ref_id[ref_index])
                            # store ref match data for each ref
                            entry = pd.DataFrame([[match_id, match_ref_id[ref_index]]],
                                                  columns = ['match_id', 'ref_id'])
                            ref_match_data = pd.concat([ref_match_data, entry], axis = 0)
                    elif match_info_heading == "Crowd":
                        match_crowd = match_info[match_info_index].find_element("xpath", "td").text
                    elif match_info_heading == "Venue":
                        match_venue_id = match_info[match_info_index].find_element("xpath", "td/a").get_attribute('href')[42:]
                        # add venue to database if it isn't there already
                        if match_venue_id not in used_venues:
                            match_info[match_info_index].find_element("xpath", "td/a").click()
                            # find key info
                            venue_name = wait.until(EC.presence_of_element_located((By.XPATH, "//main[@id='page_content']/h1"))).text
                            venue_page_dts = driver.find_elements("xpath", "//div[@id='content']/dl/dt")
                            venue_page_dds = driver.find_elements("xpath", "//div[@id='content']/dl/dd")
                            venue_nc_name = venue_page_dds[[x.text for x in venue_page_dts].index("Non-Commercial Name")].text
                            venue_location = venue_page_dds[[x.text for x in venue_page_dts].index("Location")].text
                            entry = pd.DataFrame([[match_venue_id, venue_name, venue_nc_name, venue_location]],
                                                 columns = ['venue_id', 'venue_name', 'non-commercial_name', 'location'])
                            venue_data = pd.concat([venue_data, entry], axis = 0)
                            used_venues.append(match_venue_id)
                            driver.get(match_url)
                            match_info = driver.find_elements("xpath", "//table[@class='program']/tbody[@id='match_info']/tr")
                # Continue with match data for the current match we've just come back to
                match_stats = driver.find_elements("xpath", "//table[@class='program']/tbody[@id='match_stats']/tr")
                for match_stats_index in range(len(match_stats)):
                    match_stats_heading = match_stats[match_stats_index].find_element("xpath", "th").text
                    if match_stats_heading == "Halftime Score":
                        match_ht_hs = match_stats[match_stats_index].find_element("xpath", "td").text
                        match_at_hs = match_stats[match_stats_index].find_element("xpath", "td[2]").text
                    if match_stats_heading == "Penalties":
                        match_ht_penalties = match_stats[match_stats_index].find_element("xpath", "td").text
                        match_at_penalties = match_stats[match_stats_index].find_element("xpath", "td[2]").text
                # We now have everything for the match_data df
                entry = pd.DataFrame([[competition, match_round, match_date, match_time, match_id, match_venue_id, match_crowd, match_home_team, match_ht_hs, match_home_team_score, match_ht_penalties, match_away_team, match_at_hs, match_away_team_score, match_at_penalties]],
                                     columns = ['competition', 'round', 'date', 'time', 'match_id', 'venue_id', 'crowd', 'home_team', 'home_team_ht_score', 'home_team_score', 'home_team_penalties',  'away_team', 'away_team_ht_score', 'away_team_score', 'away_team_penalties'])
                match_data = pd.concat([match_data, entry], axis = 0)
                # Now do player scoring data
                match_scoresheet = driver.find_elements("xpath", "//table[@class='program']/tbody[@id='match_scoresheet']/tr")
                for match_ss_index in range(len(match_scoresheet)):
                    # Check if we have a middle column
                    middle_col = match_scoresheet[match_ss_index].find_elements(by = By.TAG_NAME, value = "th")
                    if len(middle_col):
                        # If we do have a middle column then its not a blank row so we can proceed
                        score = middle_col[0].text
                        if score != ' ':
                            current_score = score
                        # Check the player on the left (club 1)
                        player_present = match_scoresheet[match_ss_index].find_elements("xpath", "td[@class='name left']/a")
                        if len(player_present):
                            player_id = player_present[0].get_attribute('href')[43:]
                            player_scores = match_scoresheet[match_ss_index].find_elements("xpath", "td[2]")[0].text
                            if player_scores == '':
                                player_scores = "1"
                            entry = pd.DataFrame([[match_id, player_id, current_score, player_scores]],
                                                 columns = ['match_id', 'player_id', 'score', 'n_scores'])
                            player_scoring_data = pd.concat([player_scoring_data, entry], axis = 0)
                        # Check the player on the right (club 2)
                        player_present = match_scoresheet[match_ss_index].find_elements("xpath", "td[@class='name']/a")
                        if len(player_present):
                            player_id = player_present[0].get_attribute('href')[43:]
                            player_scores = match_scoresheet[match_ss_index].find_elements("xpath", "td[3]")[0].text
                            if player_scores == '':
                                player_scores = '1'
                            entry = pd.DataFrame([[match_id, player_id, current_score, player_scores]],
                                                 columns = ['match_id', 'player_id', 'score', 'n_scores'])
                            player_scoring_data = pd.concat([player_scoring_data, entry], axis = 0)
                # Now we do individual player data (and coach)
                match_teams = driver.find_elements("xpath", "//table[@class='program']/tbody[@id='match_teams']/tr")
                for match_teams_index in range(len(match_teams)):
                    # Check if we have a name in our column
                    td_columns = match_teams[match_teams_index].find_elements(by = By.TAG_NAME, value = "td")
                    if [x.text for x in td_columns] not in [[" ", ""], [" ", " ", " "], [" "]]:
                        # If we have a name present then we have a number and a player
                        player_position = match_teams[match_teams_index].find_element(by = By.TAG_NAME, value = "th").text
                        if player_position != current_position and player_position != " ":
                            current_position = player_position
                        # Check if its the coach
                        if current_position == "HC":
                            # then check if team 1 has a coach
                            if td_columns[0].text != " ":
                                coach_id = td_columns[0].find_element("xpath", "a").get_attribute('href')[43:]
                                # check if we've stored the coach's name
                                if coach_id not in used_coaches:
                                    coach_name = td_columns[0].text
                                    entry = pd.DataFrame([[coach_id, coach_name]],
                                                         columns = ['coach_id', 'coach_name'])
                                    coach_data = pd.concat([coach_data, entry], axis = 0)
                                    used_coaches.append(coach_id)
                                # store the coach for this game
                                entry = pd.DataFrame([[match_id, coach_id, match_home_team]],
                                                     columns = ['match_id', 'coach_id', 'team'])
                                coach_match_data = pd.concat([coach_match_data, entry], axis = 0)
                            # and do the same for coach of club 2
                            if td_columns[3].text != " ":
                                coach_id = td_columns[3].find_element("xpath", "a").get_attribute('href')[43:]
                                # check if we've stored the coach's name
                                if coach_id not in used_coaches:
                                    coach_name = td_columns[3].text
                                    entry = pd.DataFrame([[coach_id, coach_name]],
                                                         columns = ['coach_id', 'coach_name'])
                                    coach_data = pd.concat([coach_data, entry], axis = 0)
                                    used_coaches.append(coach_id)
                                # store the coach for this game
                                entry = pd.DataFrame([[match_id, coach_id, match_away_team]],
                                                     columns = ['match_id', 'coach_id', 'team'])
                                coach_match_data = pd.concat([coach_match_data, entry], axis = 0)
                        # If its not the coach then we need to store info for the one (or two) players there
                        else:
                            # check if club 1 has a player in that position
                            if td_columns[0].text != " ":
                                player_id = td_columns[0].find_element("xpath", "a").get_attribute('href')[43:]
                                # check if we've stored the player's name. If not, collect their data.
                                if player_id not in used_players:
                                    # collect all player data and then
                                    player_name1 = td_columns[0].find_element("xpath", "a").text
                                    td_columns[0].find_element("xpath", "a").click()
                                    # Find titlecase name
                                    player_name2 = driver.find_element("xpath", "//main[@id='page_content']/h1").text
                                    # set birthday and birthplace to blank
                                    player_birthday = player_birthplace = ""
                                    # Find birthday and birthplace
                                    vital_statistics = driver.find_elements("xpath", "//div[@class='stats']/h3[1]")
                                    if len(vital_statistics) == 1:
                                        if vital_statistics[0].text == "Vital Statistics":
                                            player_vs_dts = [x.text for x in driver.find_elements("xpath", "//div[@class='stats']/dl/dt")]
                                            player_vs_dds = [x.text for x in driver.find_elements("xpath", "//div[@class='stats']/dl[1]/dd")]
                                            for player_vs_index in range(len(player_vs_dts)):
                                                if player_vs_dts[player_vs_index] == "Born":
                                                    player_birthday = player_vs_dds[player_vs_index]
                                                elif player_vs_dts[player_vs_index] == "Place Of Birth":
                                                    player_birthplace = player_vs_dds[player_vs_index]
                                    # Store player-specific data
                                    entry = pd.DataFrame([[player_id, player_name1, player_name2, player_birthday, player_birthplace]],
                                                          columns = ['player_id', 'player_name1', 'player_name2', 'birthday', 'birthplace'])
                                    player_names_data = pd.concat([player_names_data, entry], axis = 0)
                                    used_players.append(player_id)
                                    # return to match page
                                    driver.get(match_url)
                                    match_teams = driver.find_elements("xpath", "//table[@class='program']/tbody[@id='match_teams']/tr")
                                    td_columns = match_teams[match_teams_index].find_elements(by = By.TAG_NAME, value = "td")
                                # store data for the player in this game
                                player_number = td_columns[1].text
                                player_captain = (td_columns[0].text[-4:] == " (c)")
                                entry = pd.DataFrame([[player_id, match_id, match_home_team, match_away_team, player_number, current_position, player_captain]],
                                                     columns = ['player_id', 'match_id', 'team', 'opposition_team', 'number', 'position', 'captain'])
                                player_match_data = pd.concat([player_match_data, entry], axis = 0)
                            # check if club 2 has a player in that position
                            if td_columns[3].text != " ":
                                player_id = td_columns[3].find_element("xpath", "a").get_attribute('href')[43:]
                                # check if we've stored the player's name. If not, collect their data.
                                if player_id not in used_players:
                                    # collect all player data and then
                                    player_name1 = td_columns[3].find_element("xpath", "a").text
                                    td_columns[3].find_element("xpath", "a").click()
                                    # Find titlecase name
                                    player_name2 = driver.find_element("xpath", "//main[@id='page_content']/h1").text
                                    # set birthday and birthplace to blank
                                    player_birthday = player_birthplace = ""
                                    # Find birthday and birthplace
                                    vital_statistics = driver.find_elements("xpath", "//div[@class='stats']/h3[1]")
                                    if len(vital_statistics) == 1:
                                        if vital_statistics[0].text == "Vital Statistics":
                                            player_vs_dts = [x.text for x in driver.find_elements("xpath", "//div[@class='stats']/dl/dt")]
                                            player_vs_dds = [x.text for x in driver.find_elements("xpath", "//div[@class='stats']/dl[1]/dd")]
                                            for player_vs_index in range(len(player_vs_dts)):
                                                if player_vs_dts[player_vs_index] == "Born":
                                                    player_birthday = player_vs_dds[player_vs_index]
                                                elif player_vs_dts[player_vs_index] == "Place Of Birth":
                                                    player_birthplace = player_vs_dds[player_vs_index]
                                    # Store player-specific data
                                    entry = pd.DataFrame([[player_id, player_name1, player_name2, player_birthday, player_birthplace]],
                                                          columns = ['player_id', 'player_name1', 'player_name2', 'birthday', 'birthplace'])
                                    player_names_data = pd.concat([player_names_data, entry], axis = 0)
                                    used_players.append(player_id)
                                    # return to match page
                                    driver.get(match_url)
                                    match_teams = driver.find_elements("xpath", "//table[@class='program']/tbody[@id='match_teams']/tr")
                                    td_columns = match_teams[match_teams_index].find_elements(by = By.TAG_NAME, value = "td")
                                # store data for the player in this game
                                player_number = td_columns[2].text
                                player_captain = (td_columns[3].text[-4:] == " (c)")
                                entry = pd.DataFrame([[player_id, match_id, match_away_team, match_home_team, player_number, current_position, player_captain]],
                                                     columns = ['player_id', 'match_id', 'team', 'opposition_team', 'number', 'position', 'captain'])
                                player_match_data = pd.concat([player_match_data, entry], axis = 0)               
        # go back to the page of matches page
        driver.get(season_url)
        # Identify all the matches to click on
        matches = driver.find_elements("xpath", "//main[@id='page_content']/div[@id='content']/table[@class='list']/tbody/tr")
    # Save all data for the season just gone
    match_data.to_csv(os.path.join(dir_name, r'match_data\\match_data_' + str(season_index) + r'.csv'))
    team_data.to_csv(os.path.join(dir_name, r'team_data\\team_data_' + str(season_index) + r'.csv'))
    player_names_data.to_csv(os.path.join(dir_name, r'player_names_data\\player_names_data_' + str(season_index) + r'.csv'))
    coach_data.to_csv(os.path.join(dir_name, r'coach_data\\coach_data_' + str(season_index) + r'.csv'))
    ref_data.to_csv(os.path.join(dir_name, r'ref_data\\ref_data_' + str(season_index) + r'.csv'))
    venue_data.to_csv(os.path.join(dir_name, r'venue_data\\venue_data_' + str(season_index) + r'.csv'))
    player_match_data.to_csv(os.path.join(dir_name, r'player_match_data\\player_match_data_' + str(season_index) + r'.csv'))
    player_scoring_data.to_csv(os.path.join(dir_name, r'player_scoring_data\\player_scoring_data_' + str(season_index) + r'.csv'))
    coach_match_data.to_csv(os.path.join(dir_name, r'coach_match_data\\coach_match_data_' + str(season_index) + r'.csv'))
    ref_match_data.to_csv(os.path.join(dir_name, r'ref_match_data\\ref_match_data_' + str(season_index) + r'.csv'))
    # Reset dataframes
    match_data = pd.DataFrame(columns = ['competition', 'round', 'date', 'time', 'match_id', 'venue_id', 'crowd', 'home_team', 'home_team_ht_score', 'home_team_score', 'home_team_penalties', 'away_team', 'away_team_ht_score', 'away_team_score', 'away_team_penalties'])
    team_data = pd.DataFrame(columns = ['team_short', 'team_name'])
    player_names_data = pd.DataFrame(columns = ['player_id', 'player_name1', 'player_name2', 'birthday', 'birthplace'])
    coach_data = pd.DataFrame(columns = ['coach_id', 'coach_name'])
    ref_data = pd.DataFrame(columns = ['ref_id', 'ref_name'])
    venue_data = pd.DataFrame(columns = ['venue_id', 'venue_name', 'non-commercial_name', 'location'])
    player_match_data = pd.DataFrame(columns = ['player_id', 'match_id', 'team', 'opposition_team', 'number', 'position', 'captain'])
    player_scoring_data = pd.DataFrame(columns = ['match_id', 'player_id', 'score', 'n_scores'])
    coach_match_data = pd.DataFrame(columns = ['match_id', 'coach_id', 'team'])
    ref_match_data = pd.DataFrame(columns = ['match_id', 'ref_id'])
    # go back to the page of seasons
    driver.get(url)
    seasons = driver.find_elements("xpath", "//div[@id='content']/table[@class='list']/tbody/tr/td[1]/a")

# PLAYER SUMMARY DATA
player_summary_data = pd.DataFrame(columns = ['player_id',
                                              'player_name_comma',
                                              'player_birthday',
                                              'total_matches',
                                              'total_points'])

players_url = "https://www.rugbyleagueproject.org/competitions/nrlw/players.html"
driver = webdriver.Chrome(service = s)
driver.get(players_url)
player_rows = driver.find_elements("xpath", "//div[@id='content']/table[@class='list x']/tbody/tr")
driver.maximize_window()

for player_index in range(len(player_rows)):
    player_columns = player_rows[player_index].find_elements("xpath", "td")
    player_id = player_columns[0].find_element("xpath", "a").get_attribute('href')[43:]
    player_name_comma = player_columns[0].text
    player_birthday = player_columns[1].text
    player_total_matches = player_columns[6].text
    player_total_points = player_columns[14].text
    entry = pd.DataFrame([[player_id, player_name_comma, player_birthday, player_total_matches, player_total_points]],
                          columns = ['player_id', 'player_name_comma', 'player_birthday', 'total_matches', 'total_points'])
    player_summary_data = pd.concat([player_summary_data, entry], axis = 0) 
player_summary_data.to_csv(os.path.join(dir_name, r'player_summary_data.csv'))

# COACH SUMMARY DATA
coach_summary_data = pd.DataFrame(columns = ['coach_id',
                                              'coach_name_comma',
                                              'coach_name',
                                              'coach_birthday'])

coaches_url = "https://www.rugbyleagueproject.org/competitions/nrlw/coaches.html"
driver = webdriver.Chrome(service = s)
driver.get(coaches_url)
coach_rows = driver.find_elements("xpath", "//div[@id='content']/table[@class='list']/tbody/tr")
driver.maximize_window()

for coach_index in range(len(coach_rows)):
    coach_columns = coach_rows[coach_index].find_elements("xpath", "td")
    coach_id = coach_columns[0].find_element("xpath", "a").get_attribute('href')[43:]
    coach_name_comma = coach_columns[0].text
    coach_birthday = coach_columns[1].text
    coach_columns[0].find_element("xpath", "a").click()
    coach_name = driver.find_element("xpath", "//main[@id='page_content']/h1").text
    entry = pd.DataFrame([[coach_id, coach_name_comma, coach_name, coach_birthday]],
                          columns = ['coach_id', 'coach_name_comma', 'coach_name', 'coach_birthday'])
    coach_summary_data = pd.concat([coach_summary_data, entry], axis = 0) 
    
    driver.get(coaches_url)
    coach_rows = driver.find_elements("xpath", "//div[@id='content']/table[@class='list']/tbody/tr")
coach_summary_data.to_csv(os.path.join(dir_name, r'coach_summary_data.csv'))

# REF SUMMARY DATA
ref_summary_data = pd.DataFrame(columns = ['ref_id',
                                            'ref_name_comma',
                                            'ref_name',
                                            'ref_birthday'])

refs_url = "https://www.rugbyleagueproject.org/competitions/nrlw/referees.html"
driver = webdriver.Chrome(service = s)
driver.get(refs_url)
ref_rows = driver.find_elements("xpath", "//div[@id='content']/table[@class='list']/tbody/tr")
driver.maximize_window()

for ref_index in range(1, len(ref_rows)):
    ref_columns = ref_rows[ref_index].find_elements("xpath", "td")
    ref_id = ref_columns[0].find_element("xpath", "a").get_attribute('href')[44:]
    ref_name_comma = ref_columns[0].text
    ref_birthday = ref_columns[1].text
    ref_columns[0].find_element("xpath", "a").click()
    ref_name = driver.find_element("xpath", "//main[@id='page_content']/h1").text
    entry = pd.DataFrame([[ref_id, ref_name_comma, ref_name, ref_birthday]],
                          columns = ['ref_id', 'ref_name_comma', 'ref_name', 'ref_birthday'])
    ref_summary_data = pd.concat([ref_summary_data, entry], axis = 0) 
    
    driver.get(refs_url)
    ref_rows = driver.find_elements("xpath", "//div[@id='content']/table[@class='list']/tbody/tr")
ref_summary_data.to_csv(os.path.join(dir_name, r'ref_summary_data.csv'))