# Election Forecast

In this repo, I attempt to forecast the 2024 election. This is my  <ins>Very First Attempt</ins>, and I strongly reccomend not taking it seriously. It was interesting, it was fun, but it is not an accurate predictor of the 2024 election. I did it on the fly in a single night as a college undergrad with no prior forecasting experience.

This is a breakdown of my major thought process/steps in the model design:
1)	I took fivethirtyeight’s presidential poll csv file and purged it of everything not Trump v Biden (selecting only for polls and questions that allowed the respondent to pick one of either of them)
2)	 Grouped the data by state and weighted each poll by sample size, pollster grade, and recency.
3)	Ran a difference-of-means test for each state to determine the probability that the ‘true’ proportion of voters supporting Trump is greater than the proportion supporting Biden (because it’s a difference test, I don’t need to worry as much about losing other party candidates)
4)	There are no good polls for either Delaware or District of Columbia. I set their values equal to Oregon and Maryland respectively based on historical election similarity. Also based on it being quick and simple.
5)	Defined a transformation (tanh) to force the p-values to more extreme versions of themselves (i.e. 0.3 -> 0.04 ish while 0.7 -> 0.96, but 0.5 remains constant) – this counters the effect of having a small number of polls (and thus higher variance) in “safe” states while toss-ups are narrow. It also felt like this was needed as the forcing Biden was given a 35% chance of winning Wyoming.
6)	Began simulations. In each sim, I added a random but systematic polling (percentage) error across all states to add some dependence to the various state outcomes.
7)	Tallied up the electoral college vote for each simulation, calculated percentage outcomes for each state (and for the general) and plotted them on a state map. I think it’s a decent first attempt, but
there’s a lot of ways to improve this (I took shortcuts galore), especially considering the code itself was all written in one binge (I got really into it). On top of that I don’t really know what I’m doing. If anyone has any thoughts, feel free to reach out!
I fine-tuned the p-values a little bit -especially with the forcing function- but once I generated the first results I didn’t change anything except the number of simulations.


Okay, disclaimer aside here's the forecast:
![forecast](https://github.com/BGray-wrl/election_forecast_2024/blob/main/Forecast.png)
The raw numbers of victories by state can be found [here](https://github.com/BGray-wrl/election_forecast_2024/blob/main/first_sim_n_10000.csv).

My intuition on seeing these results is that my attempt at forcing p-values wasn’t strong enough. I think Biden’s wins are a little too dependent on high variances in states that just aren’t that likely to go his way but seem like they could due to a small polling sample size (and therefore higher variance). Then again, I’m going off of priors about Trump having a small advantage and IDK how much I should really trust them.

Again, this is just a fun project I wanted to put online. Please do not treat my forecast as anything more than that.
