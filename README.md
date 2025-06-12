# Somerset-Patriots-Lineup-Optimizer
Optimizing the Somerset Patriots lineup for the 2025 regular season by projected runs scored. Tracking batter performance vs scout-graded pitch types.

**Overview**

This report analyzes hitting performance via on-base percentage and slugging percentage versus starting pitchers over 56 games played by the Somerset Patriots in the 2025 regular season. The analysis completed in this report aims to determine optimal batting lineups versus left-handed and right-handed starting pitchers as well as organize hitting performance against scout-graded pitch types. The two applications created in this analysis are available online via the below links at Shinyapps.io: 
https://holsey2.shinyapps.io/Somerset_Patriots_Optimal_Lineups/
https://holsey2.shinyapps.io/Somerset_Patriots_Hitting_Pitch_Types/

**Background**

Through 56 games of the 2025 regular season, the Somerset Patriots (double-A affiliate of the New York Yankees) have a losing record of 25 wins and 31 losses. This record ranks 5th out of 6 teams in Northeast Division of the Eastern League and is unlikely to win the first half in the division. The Patriots have endured several key player injuries and a lack of offensive production which have contributed to a -10 run differential of 251 runs scored compared to 261 runs allowed. The purpose of this report is to evaluate and analyze the sucessess and failures of various batting lineups used against left-handed and right-handed starting pitchers.

**Key Findings**

The projected average runs scored for the Optimal Lineup against left-handed starters is 2.26, which is higher than both the team's projected runs of 2.04 and the actual average runs scored of 1.33. The projected average runs scored for the Optimal Lineup against right-handed starters is 3.26, which is higher than both the team’s projected runs of 2.19 and the actual average runs scored of 2.24.

**Data Sources**

MiLB.com game logs
Baseball Savant scouting grades from player profiles
Fangraphs.com prospects report

**Metrics Analyzed**

Plate Appearances (PA)
On-base Percentage (OBP)
Slugging Percentage (SLG)
Earned Runs (ER)


**Model Development**

Using the formula: Avg OBP * Avg * SLG * 2 (2 plate appearances per starter, based on the fact that the team averages 18 PA against a starting pitcher), I calculated the projected runs scored per player for each lineup slot they occupied over the 56 games. The optimal lineup was created by selecting the top player (highest projected runs scored) for each lineup position, repeating this process until the lineup was complete. 

**Limitations and Future Work**

The data used in this analysis covers only a limited number of games—specifically, 56 during the first half of the Somerset Patriots' 2025 regular season. This data was collected manually, making it susceptible to typographical errors. One significant challenge was the variation in lineups, particularly against left-handed pitchers, who are faced less frequently. This variation resulted in difficulties in achieving the required minimum number of plate appearances. 

Future studies should incorporate data from additional games to enhance the matchup scenarios. To improve the projected runs scored formula, I recommend conducting batted ball regressions using advanced metrics such as weighted Runs Created Plus (wRC+), weighted On-Base Average (wOBA), and Runs Above Average based on the base/out states (RE24). This approach should be considered if such data becomes available on Minor League Baseball websites.

**Author**: Patrick Holsey

**References**

Arneson, K. (2006). “Must. Bat. Kendall. Ninth.” 
https://catfishstew.baseballtoaster.com/archives/322075.html

BaseballSavant.mlb.com. (2025). “Player Standard Pitching Statistics – Scouting Report.” Example:
	https://baseballsavant.mlb.com/savant-player/jonah-tong-804636?stats=career-r-pitching-milb
 
Fangraphs.com (2025). “Player Statistics – Prospects Report.” Example:
	https://www.fangraphs.com/players/jonah-tong/sa3022324/stats?position=P
 
MiLB.com. (2025). “AA Eastern League Standings.” https://www.milb.com/somerset/standings/

MiLB.com. (2025). “Gameday Box Scores and Play-by-Play.” Example:
https://www.milb.com/gameday/patriots-vs-yard-goats/2025/04/04/783416/final/box

Morong, C. (2006). “Value of OBP and SLG by Lineup Position.” 
	https://www.beyondtheboxscore.com/2006/2/12/133645/296


**License**
This project is licensed under the MIT License - see the LICENSE file for details
