#' # Introduction
#' 
#' I am a huge fan of sports and they have been an important part of my life  in 
#' helping me become the person I am today.  As a result, I've always tried to 
#' find a way to mix sports and mathematics into a career.  What I came up with 
#' was finding a career in sports statistics, which lead me to doing this project
#' based around that.  
#' 
#' In this case, with hockey being my favourite sport, I tried to find a question that 
#' I could ask which would help me to use the content of this course in an analysis 
#' of certain data.  So what I did is I found team data for the 2018-19 NHL season.
#' 
#' In order to satisfy a categorical and causal response variable condition, I decided to ask
#' the question of what it takes a team to make the playoffs and decided to view it
#' from the standpoint of what variables could lead to that result and so the 
#' following analyses were performed.
#' 
#' # Analysis
#' 
nhl_team_stats <- read.csv("~/PROJECT STUFF/nhl_team_stats.csv")
# View(nhl_team_stats)
# nhl_team_stats
salarydata <- read.csv("~/salarydata.csv")
# View(salarydata)
#'
gg <- within(nhl_team_stats,
      {
          wins <- W
          loss <- L + OT
          goaldiff <- GF.GP - GA.GP
          GF_g <- GF.GP
          GA_g <- GA.GP
          GF_g_c <- scale(GF.GP,scale = FALSE)
          GA_g_c <- scale(GA.GP,scale = FALSE)
          pp <- PP.
          pk <- PK.
          pp_c <- scale(pp, scale = FALSE)
          pk_c <- scale(pp, scale = FALSE)
          shots_on <- Shots.GP
          shots_against <- SA.GP
          shots_on_c <- scale(Shots.GP, scale = FALSE)
          shots_against_c <- scale(SA.GP, scale = FALSE)
          shotdiff <- shots_on - shots_against
          shotdiff_c <- scale(shotdiff, scale = FALSE)
          faceoff <- FOW.
          goaldiff_c <- scale(goaldiff,scale = FALSE)
          faceoff_c <- scale(faceoff, scale = FALSE)
          
})
wins <- gg$wins
loss <- gg$loss
goaldiff <- gg$goaldiff
pp <- gg$pp
pk <- gg$pk
shots_on <- gg$shots_on
shots_against <- gg$shots_against
faceoff <- gg$faceoff
GF_g <- gg$GF_g
GA_g <- gg$GA_g
GF_g_c <- scale(gg$GF.GP,scale = FALSE)
GA_g_c <- scale(gg$GA.GP,scale = FALSE)
shots_on_c <- scale(gg$Shots.GP, scale = FALSE)
shots_against_c <- scale(gg$SA.GP, scale = FALSE)
faceoff_c <- gg$faceoff_c
pp_c <- gg$pp_c
pk_c <- gg$pk_c
shotdiff <- shots_on - shots_against
shotdiff_c <- scale(shotdiff, scale = FALSE)
ggg <- merge(gg,salarydata)
ggg
#'
#' An important factor for teams making the playoffs rests on how many games they
#' can win throughout the regular season, the more they can win, the better their
#' chances are of making the playoffs, so that is where we'll start our analysis.
#' 
#' Figuring out what causes wins is a very complex question in the NHL these days
#' , as it can depend on many different variables, such as how well individual 
#' players perform throughout the season in many categories, how the schedule 
#' is formed--does a team have back to back games often or not, or even based 
#' on team stats overall. In our case here, we have data on overall team statistics
#' so our analysis will be mainly focused on those, while keeping other factor
#' contingencies in mind. 
#' 
#' Like with many sports, scoring more goals than the other team is a tried-and-true
#' way to win games and so, we'll see now if that is the case with our data:
#'
library(latticeExtra)
plot(goaldiff, wins, data = ggg,col = "blue", xlab = "Goal Differential Per Game",ylab = "Wins",
     main = "Team goal differential per game .vs. wins for 2018-19 NHL season")
abline(lm(wins~goaldiff), col = "red")
#'
#' Arguably, the most important causal factor on wins would have to be 
#' goal differential per game (Goals scored per game - Goals against per game), and
#' given the plot developed above, that exact trend is shown, that the higher the 
#' goal differential per game, the more wins a team will most likely secure.  This
#' lines up our intuition and provides a solid level of confidence that goals
#' affect wins.  Moving forward, our analysis will take into account other factors
#' that may play a part in this relationship with goal differential and wins.
#'  
#'  The first idea that we will tackle with that relationship in mind, starts at
#'  faceoff dot.  Faceoffs in hockey are a situation where two players line up 
#'  across from one another and wait until the referee drops the puck to fight 
#'  for possession of it.  These usually happen at the start of periods and are
#'  used to resume play after a stoppage. They can also occur in any zone of the ice
#'  so they can affect both the offensive and defensive side of the game.
#'  As a result, with winning these faceoffs being 
#'  a way to gain possession of the puck, more time of possession in many sports
#'  gives a team more chances to score goals and acquire a win.  We will now 
#'  perform some statistical tests to see if faceoff win percentage, represented
#'  by the variable "faceoff", is as important as we may think in terms of goals
#'  and wins.
#' 
goaldiff_model <- glm(cbind(wins,loss) ~ goaldiff,data = ggg,family = binomial)
summary(goaldiff_model)
faceoff_model <- glm(cbind(wins,loss) ~ faceoff,data = ggg,family = binomial)
summary(faceoff_model)
#' 
#' Individually, these summaries show that goal differential per game is highly 
#' significant in predicting wins with other variables free to change, but faceoff
#' win percentage is less so, as it's p-value is relatively much larger than that
#' for goal differential per game in its effect on wins.
#' 
#' When putting these predictors together in a model and perform an Anova test,
#' we find the following result:
#' 
basic_model <- glm(cbind(wins,loss) ~ faceoff + goaldiff,data = gg,family = binomial)
summary(basic_model)
library(car)
Anova(basic_model, test = "LR")
#'
#' We see that adding goal differential to a formula for faceoff win percentage
#' makes it a much better model in having evidence of a causal effect on wins, but
#' given that the p-value in Anova test for "faceoff" is not significant, we
#' have evidence that faceoffs are not as important in gaining wins as we first thought.
#' Essentially, this means that when holding the goal differential of each team
#' constant, faceoff win percentage does not have a significant impact on a team
#' winning a game. When we look at a plot of faceoff win percentage .vs. wins, 
#' we see a similar result, that faceoff win percentage does not significantly 
#' affect a team's number of wins or not.
#'
plot(faceoff,wins, col = "blue", xlab = "Faceoff wins per game (%)", 
     ylab = "Wins (by team)", main = "Faceoff win percentage .vs. Wins by team
     in the 2018-19 NHL season")
abline(h = 45, col = "red")
#'
#' No relationship whatsoever.  So now we can move onto check for interactions, as
#' they may play a part in this result.  For the interactions, we will be centering
#' the predictors as the extrapolation to zero that happens when interpreting interactions
#' does not hold any value here.  Therefore, we achieve the following results:
int_c_model <- glm(cbind(wins,loss) ~ goaldiff_c*faceoff_c,data = ggg,family = binomial)
summary(int_c_model)
Anova(int_c_model, test = "LR")
fGAGF_model <- glm(cbind(wins,loss) ~ goaldiff*faceoff,data = ggg,family = binomial)

par(mfrow = c(1,1), oma = c(0,0,0,0))
pred <- expand.grid(goaldiff = c(-1.5,0,1.5), faceoff = c(45,50,55))
pred$fGAGF_model <- predict(fGAGF_model,newdata = pred,type = 'link')
library(latticeExtra)
xyplot(fGAGF_model~goaldiff, groups = faceoff,pred,type = "l", auto.key = TRUE,
       xlab = "Goal differential per game",ylab = "FOW%-Goal differential int. model" 
       ,main = "FOW%:Goal differential interaction plot")
#'
#' All of the tests just performed give us the idea that the interaction between
#' goal differential per game and faceoff win percentage has no significant effect on 
#' wins, as the estimates of those parameters in these tests turned out to be highly
#' non-significant, along with the fact that the plot for testing the interaction
#' effect of goal differential per game and faceoff win percentage shows 
#' nearly parallel, if not parallel, lines which means that no significant
#' interaction exists given our data based on these variables, giving us the idea
#' that holding each variable at a certain level does not have a specific effect
#' when changing the other.
#' 
#' With an individual significant effect on wins, but an opposite result
#' when adding goal differential per game to the equation, we have evidence that
#' goal differential must be a mediating factor on faceoff win's percentage effect
#' on wins, but the strength of this result is not very high.  This result allows us to believe
#' that the result of faceoff win percentage affecting possession, in turn developing
#' a better goal differential on the way to effecting wins is a definitely a possibility.
#' Although, there are tons of factors that could have confounding effects on 
#' faceoff win percentage, such as which two players are facing off against one 
#' another--are they skilled at faceoffs or not--and being able to control for 
#' variables like that would show a better overview of the effect that faceoffs 
#' truly have on the game and their relationships within it.
#' 
#' The ideas from this analysis on faceoff win percentage's effect on wins 
#' opens up so many more questions about how certain aspects of hockey can help teams 
#' gain wins and how other factors can affect their goal differential.  With this
#' in mind, faceoffs may not be significant on wins directly, but they do lead 
#' to more possession and with more possession of the puck, they have more of a 
#' chance of scoring goals, or keeping them out of their goal, and a major factor 
#' in these situations results from shots on goal and shots against respectively.
#' Therefore, we can measure the effect of shots on goal per game in accordance 
#' with goals for per game and shots against per game in accordance with goals 
#' against per game to see how shots can be an effective measure of goals here, 
#' which again, may lead to wins.  In this case, we are 
#' splitting up goals for and goals against when dealing with the shots variables,
#' rather than dealing with overall goal differential because faceoffs effect 
#' offense and defense in complex ways and so, faceoffs
#' effect goal differential rather than each separate variable, while the shots
#' on goal affects offense, and shots against affects defense, individually. 
#' 
par(mfrow = c(1,2), oma = c(0,0,0,0))
plot(shots_on,wins,col = "blue", xlab = "Shots on goal per game", ylab = "Wins (by team)",
     main = "Shots on goal per game \n .vs. Wins")
plot(shots_against,wins, col = "blue", xlab = "Shots against per game", ylab = "Wins (by team)",
     main = "Shots against per game \n .vs. Wins")
plot(shots_on, GF_g, col = "red", xlab = "Shots on goal per game", ylab = "Goals for per game",
     main = "Shots on goal per game \n .vs. Goals for per game")
plot(shots_against,GA_g, col = "red", xlab = "Shots against per game", ylab = "Goals against per game",
     main = "Shots against per game \n .vs. Goals against per game")
#'
#' In the plots above we see that shots on goal per game tend to have a, weak, but 
#' positive linear relationship with wins and goals for per game, and shots against 
#' per game show a negative linear relationship with that of wins, but a positive 
#' relationship with that of goals against per game, which is in line with our intuition, 
#' that more shots on goal, in general, lead to more goals, and more shots against, 
#' in general, lead to more goals against.
#' 
#' We'll now perform formal analyses to see their effects in these cases.
#' 
shots_on_model <- glm(cbind(wins,loss)~shots_on,data = ggg,family = binomial)
summary(shots_on_model)

sht_goal_model <- lm(GF_g~shots_on)
summary(sht_goal_model)

#' The shots_on model that tests if the predictors individually are highly significant, 
#' shows that it is important in its effect on wins when the values of
#' the other factors in this data are changing with them,
#' but testing the effect of shots on goal per game on goals for per game shows that
#' shots on goal have a significant effect on goals, again, when not keeping
#' other factors constant, which develops the idea that 
#' goals for per game could possibly be a mediating factor on shots on goal per game,
#' and that would correlate with our intuition stated above. 
#' 
acc_add_model <- glm(cbind(wins,loss)~GF_g + shots_on,data = ggg,family = binomial)
summary(acc_add_model)
Anova(acc_add_model, test ="LR")
#'
accuracy_model <- glm(cbind(wins,loss)~GF_g_c*shots_on_c,data = ggg,family = binomial)
summary(accuracy_model)
Anova(accuracy_model, test ="LR")
#'
#' With these two tests, we see that the interaction between shots on goal per game
#' and goals for per game is insignificant.  Since this is a test of accuracy, as 
#' goals for per game represents how many shots became goals, and so, accuracy (as
#' in the interaction) proves to not matter as much to wins, or has a non-significant effect 
#' on it.
#' 
#' Furthermore, with the Anova tests, we see that predicting the cause of
#' wins with these two predictors in the same model, displays the following idea:
#' when goals are kept constant between all teams, our model is showing that more
#' shots on goal on it's own does not necessarily play a big role in predicting wins,
#' although with keeping shots on goal per game constant between teams, we see that 
#' goals for per game shows us a measure of how accurate or how good players are at 
#' scoring on certain teams, which may give us the idea that including better goal
#' scorers, or more accurate shooters can help a team win, and gives us evidence 
#' that goals for per game is the mediating factor on shots on goal per game
#' that we first thought about.  Now it makes sense why they pay the high level 
#' goal scorers the big bucks.
#'  
#' Overall, this provides evidence that shots on goal per game has a significant 
#' effect on goals for per game, but only has an effect on wins through the
#' goals for per game variable.  Shots lead to goals and goals lead to wins, simply
#' put.
#' 
shots_against_model <- glm(cbind(wins,loss)~shots_against,data = ggg,family = binomial)
summary(shots_against_model)
#'
goals_against_model <- glm(cbind(wins,loss)~GA_g,data = ggg,family = binomial)
summary(goals_against_model)
#'
shtag_goalag_model <- lm(GA_g~shots_against)
summary(shtag_goalag_model)
#'  
#' These tests show us those same results that we have seen above, that shots against 
#' per game has a significant effect on goals against per game and wins, so we can see that 
#' goals  against per game must be a mediating factor between the effect
#' of shots against per game on wins.  To ensure that this is the case, we can perform
#' the following test in an overall model:
#' 
defend_model <- glm(cbind(wins,loss)~GA_g + shots_against, data = ggg, family = binomial)
summary(defend_model)
Anova(defend_model)
#'
defend_int_model <- glm(cbind(wins,loss)~GA_g_c*shots_against_c, data = ggg, family = binomial)
summary(defend_int_model)
Anova(defend_int_model)
#'
#' Once again, the results from the models above are in line with what we first 
#' assumed ot be the case.
#'
shots_add_model <- glm(cbind(wins,loss)~shots_on + shots_against, data = ggg, family = binomial)
summary(shots_add_model)
Anova(shots_add_model)
#'
#' Given these tests, we see that the model that includes both shots on goal per
#' game and shots against per game fits the data better than the models that only
#' include the predictors separately, which develops the interesting idea that much
#' like with goal differential, shot differential per game (shots on goal per game
#' - shots against per game) is a more important predictor than the two predictors
#' separately.  Also, the relationship between these two variables should be included in the
#' model because if a team were to have more shots on goal per game, they would 
#' be more likely to be in the offensive zone more often than in their own defensive
#' zone, which is a measure of the skill of a given team or their offensive abilities
#' in a sense.  Of course there are times when both teams that face one another may have a 
#' low number of shots on goal each, or a high number of shots on goal each, but 
#' that is not as common in this sport, so we are not as worried about it in the 
#' analysis.  This brings up many more questions as well, as this shot differential
#' could be affected by injuries throughout the season, or even what teams play against
#' one another, as a weaker team would tend to be outshot when facing a stronger
#' team, and so it may well be very dependent on factors that we cannot explore 
#' within this dataset.
#' 
shotdiff <- shots_on - shots_against
shotdiff_model <- glm(cbind(wins,loss)~shotdiff,data = ggg, family = binomial)
summary(shotdiff_model)
summary(lm(goaldiff~shotdiff))
#' These final tests, with an even smaller AIC, provides more evidence that shot
#' differential per game has a significant effect on wins (while other
#' variables are permitted to change), but through 
#' goal differential per game (a mediating factor of it).  This can also be seen in the 
#' following plots:
par(mfrow = c(1,2), oma = c(0,0,0,0))
plot(shotdiff,goaldiff, col = "red", xlab = "Shot differential per game", 
     ylab = "Goal differential per game",
     main = "Shots differential per game \n .vs. Goal differential per game")
plot(shotdiff,wins, col = "blue", xlab = "Shots differential per game", 
     ylab = "Wins (by team)",
     main = "Shots differential per game \n .vs. Wins")
#'
#' Overall, we see that, just simply through these associations, that shots on goal
#' and shots against paint a vivid picture of the game of hockey and how we can truly
#' interpret the scoring ability of a team, along with why wins would follow as a result.
#' Our analysis does not end here though, we'll now move on to our final factor
#' in our quest of figuring out what it takes to win hockey games in the NHL.
#' In all sports, momentum swings can have a very significant effect on the 
#' direction of the game and can revert the score in the blink of an eye, to put
#' a losing team in a winning position.  In hockey, these so-called momentum swings
#' are largely the work of powerplays, in which one team is given an advantage
#' over the other by virtue of the opposition losing a player for a certain 
#' amount of time due to that player violating a rule.  With a team gaining a
#' man-advantage, or sometimes a 2-man advantage over the opposition, scoring a 
#' goal becomes an easier task and so having an easier pathway to a goal can
#' cause a team to score more, which in turn, allows for a higher probability of
#' winning games.
#' 
#' Therefore, we can discuss the factors of powerplay success percentage (PP%),
#' and success is determined when a team scores a goal on the powerplay , and
#' penalty kill success percentage (PK%), in which success is determined by whether
#' a team does not let up a goal when facing a powerplay, to see how they may 
#' affect wins.
#' 
#' We can start with a simple plot to see their relationship:
#' 
plot(pp,wins,col = "black", xlab = "Powerplay success percentage (PP%)", 
     ylab = "Wins (by team)",
     main = "Powerplay success percentage (PP%) .vs. Wins")
plot(pk,wins,col = "black", xlab = "Penalty kill success percentage (PK%)", 
     ylab = "Wins (by team)",
     main = "Penalty kill success percentage (PK%) .vs. Wins")
#'
#' Simply from the plots alone, we see that PP% does not have a linear relationship 
#' with wins, but the data points tend to mostly fall below about 50 wins with a 
#' varied distribution and the PK%  also do not show a linear relationship with 
#' wins alone.
#' 
#' When looking at these variables based on real life applications, this makes sense
#' as the values that are developed for PP% are dependent on goals for, as a 
#' successful powerplay is designated by a team scoring on the opponent within 
#' the duration of the powerplay, along with how long the powerplays are on average 
#' and how many powerplays the teams get per game.  On
#' the other hand, the PK% is affected by goals against, as a failed penalty kill 
#' comes from the fact that a team is scored on by their opponent within the duration
#' of the powerplay, and so that affects
#' the percentage.  In effect, the less they get scored on on those penalty kills,
#' the higher their PK% will be, alongside with the other factors that also affect
#' PP% that have an effect on PK% as well.
#' 
#' Overall, there is definitely a confounding factor that is affecting the causal 
#' relationship between PP% and PK% and there may be interaction that a model which 
#' includes them may need to develop a proper model.
#' 
pp_model <- glm(cbind(wins,loss)~pp, data = ggg, family = binomial)
summary(pp_model)
#'
pk_model <- glm(cbind(wins,loss)~pk, data = ggg, family = binomial)
summary(pk_model)
#'
#' Both individual predictor models seem to be significant predictors of wins when 
#' nothing else is held constant, but we'll need more analysis to truly understand 
#' the relationship, as many factors changing can affect this result. We'll now see
#' how goals for and against can affect PP% and PK%:
#' 
par(mfrow = c(1,1), oma = c(0,0,0,0))
pp_GF_model <- glm(cbind(wins,loss)~pp + GF_g,data = ggg,family = binomial)
summary(pp_GF_model)
Anova(pp_GF_model)
plot(pp, GF_g, col = "orange", xlab = "PP success (%)", ylab = "Goals for per game",
     main = "Goals for per game \n .vs. Powerplay Success (%)")
#'
pk_GA_model <- glm(cbind(wins,loss)~pk + GA_g, data = ggg, family = binomial)
summary(pk_GA_model)
Anova(pk_GA_model)
plot(pk,GA_g, col = "hot pink", xlab = "PK success (%)", ylab = "Goals against per game",
     main = "Goals against per game \n .vs. Penalty kill Success (%)")
#'
pp_GA_model <- glm(cbind(wins,loss)~pp + GA_g,data = ggg,family = binomial)
summary(pp_GA_model)
Anova(pp_GA_model)
#'
pk_GF_model <- glm(cbind(wins,loss)~pk + GF_g,data = ggg,family = binomial)
summary(pk_GF_model)
Anova(pk_GF_model)
#' 
#' The results from these models are very interesting as when we add goals for per game 
#' to the model with PP%, we see that the effect of PP% on wins becomes non-significant,
#' which develops the idea that goals for per game may be a mediator rather than 
#' a confounding factor on PP%, and the same goes for PK% on wins when adding goals
#' against per game to that respective model.  Although, when we instead apply 
#' goals for per game to the model with PK% and goals against per game to the model
#' with PP%, each predictor in each model becomes significant.  Each of these new
#' models reinforces the ideas from the first two and those are as follows:
#' when we keep goals against per game constant between all teams, we can see the effect that 
#' goals for has on PP% and that when goals for are allowed to change with that 
#' of PP%, we see a significant effect that each of them have on a team winning.  In other 
#' words, having more success on the powerplay (higher PP%) means scoring more goals
#' and affecting wins in a significantly positive way.  A similar result holds when
#' holding goals for per game constant and allowing PK% to change as goals against
#' per game changes because this shows that the more goals against per game can 
#' have an effect on a team's ability to defend their goal on the penalty kill, or
#' said another way, if teams tend to let in more goals, they are not as good defensively
#' as another team and their PK% will suffer due to their weak defensive abilities
#' leading to a team having a lower chance of winning. Essentially, goals for per game
#' is a mediating factor of PP%'s effect on wins and goals against per game is a mediating 
#' factor on PK%'s effect on wins, so a team improving their proficiency on their 
#' powerplays and penalty kills give teams a better chance to win.  As a sidenote,
#' these statistics do depend on what teams were playing against one another and
#' the number of powerplays that resulted in each of those games for each team, along
#' with the factor of which team was the home team as fan support plays a motivating role 
#' in hockey games that helps teams play better in front of their home fans; these
#' variables could have an effect on these results, but what we have shown here is
#' the general situation with the data that we have.
#' 
#' This develops an interesting idea.  It seems that since goals against affects 
#' PP% more than PK% on an effect on wins, we have evidence to suggest that
#' goals against affect PP% and wins simultaneously, and that goals for affect
#' wins and PK% simultaneously and so we need to include those predictors in the
#' model when discovering evidence of a causal effect of PP% and PK% on wins.
#' 
#' Given that we see that goals for per game and goals against per game seem to
#' be mediators for PP% and PK%, respectively, it brings up another question: If 
#' goals have an effect on PP% and PK%, and shots affect goals, would shots on goal
#' per game and shots against per game affect these factors too?
#' 
#' It seems very reasonable that it should, but we will use statistical analysis 
#' to see if that is the case:
#' 
library(latticeExtra)
library(spida2)
gd(col = 1:4,pch = 16)
xyplot(shots_on~pp, auto.key  = TRUE, groups = cut(GF_g, 4),
       xlab = "PP Success (%)", ylab = "Shots on goal per game",
       main = "Powerplay Success (%) .vs. Shots on goal per game \n relative to Goals for per game")
gd(col = 5:8, pch = 16)
xyplot(shots_against~pk, auto.key  = TRUE, groups = cut(GA_g, 4),
       xlab = "PK Success (%)", ylab = "Shots on goal per game",
       main = "Penalty kill Success (%) .vs. Shots on goal per game \n relative to Goals against per game")
#'
#' From the first plot above, we see that teams in the upper echelon of goals for 
#' per game tend to get more shots on goal per game and have a relatively
#' high percentage of success, but for teams that have a lower number of goals
#' for per game, despite more or less shots per game, their success on the powerplay
#' is relatively equivalent, which develops the idea that more shots on the powerplay
#' could be beneficial in its success, but this relationship is not prominent.
#' Although, in the second plot, we see a much stronger relationship between shots
#' against per game and success on the penalty kill (this includes with goals against 
#' per game at different levels, and the less goals against per game, the higher 
#' the penalty kill success percentage).  This gives us the idea that less shots
#' against per game could have a significant effect on improving a team's penalty 
#' killing ability, which allows them to keep goals out of their net.  They seem 
#' to be interconnected, but we cannot acquire the full picture of these relationships
#' because we do not have statistics on powerplay goals and powerplay shots individually
#' from their overall totals, or number of penalty kills or powerplays that each team
#' endured throughout the season, so we cannot truly see if more shots on the powerplay,
#' or less against on penalty kills truly does influence PP% and PK% respectively,
#' but this gives us a general idea and motivates further research for sure.  Also,
#' there is no stats on what teams had powerplays against any other team and so the 
#' skill that each team has in these situations is not accounted for here, but once
#' again, the general case takes these assumptions into account.
#'
pp_shton_model <- glm(cbind(wins,loss)~pp + shots_on, data = ggg, family = binomial)
summary(pp_shton_model)
Anova(pp_shton_model)
#'
#'
pk_shtag_model <- glm(cbind(wins,loss)~pk + shots_against, data = ggg, family = binomial)
summary(pk_shtag_model)
Anova(pk_shtag_model)
#'
#' Finally, we try a model with the previous predictors but add shots on goal per game
#' for the PP% model and shots against per game for the PK% model as those are known
#' to directly affect the PP% and PK% respectively, so by adding them into these 
#' models, we find an even better model (based on the AIC), where each of the
#' predictors are significant and this tells us that wins can be affected by PP% levels
#' and PK% levels, but only when we control for shots in these cases, as PP% and PK% are
#' built on those values, or shots in both factions confound their effects on wins.
#' Given these results, it begs the question whether there could be significant
#' interaction between these respective sets of variables given the confounding 
#' nature of shots on goal per game on powerplay success percentage and shots against
#' per game on penalty kill percentage. 
#'
pp_shots_int_model <- glm(cbind(wins,loss)~pp*shots_on, data = ggg, family = binomial)
pp_shots_int_model_c <- glm(cbind(wins,loss)~pp_c*shots_on_c, data = ggg, family = binomial)
#'
par(mfrow = c(1,1), oma = c(0,0,0,0))
predshots <- expand.grid(pp = c(10,20,30), shots_on = c(25, 30, 35))
predshots$pp_shots_int_model <- predict(pp_shots_int_model,newdata = predshots,type = 'link')
library(latticeExtra)
gd(col = 2:4)
xyplot(pp_shots_int_model~pp, groups = shots_on,predshots,type = "l", auto.key = TRUE,
       lwd = 2, xlab = "PP %", ylab = "PP %:shots on goal/game int. model",
       main = "Powerplay success %:shots on goal per game \n interaction plot")
#'
summary(pp_shots_int_model_c)
Anova(pp_shots_int_model_c)
#' 
#' This interaction plot between powerplay success percentage and shots on goal
#' per game showed slight deviations from parallelism between the lines, but when 
#' we dealt with the interaction numerically, the Anova test showed us that there 
#' is no significant interaction.  Given the additive model results and the results
#' from this interaction model, it's fairly clear to see that the shots on goal 
#' per game a team gets has a confounding effect on the success of a powerplay,
#' when keeping shots on goal at it's zero, or centered level in this case, 
#' but that doesn't carry over to it's effect on powerplays when keeping it 
#' constant at certain levels--it is pretty much the same effectiveness at each 
#' different level of shots on goal per game.  To note, the scaled variables were
#' used for the numerical analysis as extrapolating to zero for those variables 
#' would make no sense for analysis of their effects.
#' 
#'
pk_shots_int_model_c <- glm(cbind(wins,loss)~pk_c*shots_against_c, data = ggg, family = binomial)
pk_shots_int_model <- glm(cbind(wins,loss)~pk*shots_against, data = ggg, family = binomial)
#'
par(mfrow = c(1,1), oma = c(0,0,0,0))
predshotsag <- expand.grid(pk = c(70,80,90), shots_against = c(25, 31, 36))
predshotsag$pk_shots_int_model <- predict(pk_shots_int_model,newdata = predshotsag,type = 'link')
library(latticeExtra)
gd(col = 1:3)
xyplot(pk_shots_int_model~pk, groups = shots_against,predshotsag,type = "l", auto.key = TRUE,
       lwd = 2, xlab = "PK %", ylab = "PK %:shots against/game int. model",
       main = "Penaly kill success %:shots on goal per game \n interaction plot")
#'
summary(pk_shots_int_model_c)
Anova(pk_shots_int_model_c)
#'
#' We performed similar tests for penalty kill success percentage and shots against
#' per game as we did with powerplay success percentage and shots on goal per game,
#' but we got opposite results in this case.  The interaction between the PK% variable
#' and shots against per game variable was much stronger and significant numerically,
#' as well as can be seen on its interaction plot, as the lines are clearly not parallel.
#' Therefore, the confounding effect of shots against per game on PK% tells us 
#' that by keeping shots against per game to a certain level, a team can achieve 
#' success on the penalty kill and translate that to wins.  In other words, there
#' is a significant effect of penalty kill percentage on wins when changing the levels
#' of shots against per game.
#' 
#' Overall, powerplay and penalty kill success definitely hinges on of shots, and
#' this provides us with another stepping stone to understanding what can help a
#' team to win, even if our data is minimal in this case, it gives us insight of 
#' where to look, which is truly a spectacular result.  
#'
#' Thus far, we have dealt with each numerical variable in our data set that is 
#' determined within the hockey games themselves and how they may affect wins, 
#' but we will now test to see how a factor outside of the game itself may develop
#' an important connection to winning.
#' 
cap_space <- factor(ggg$Cap.space.left.over, levels = c("Low", "Medium", "High"))
#'
#' We will be looking at how the salary cap, or how teams spent their money
#' on players in this season, affected their ability to win games.  The idea behind
#' testing the effect of a variable like this is that usually the more skilled players
#' are paid a higher salary, and so if a team were to spend more of their cap in
#' a season, we are testing this variable under the assumption that that team has
#' more skilled players, and with more skilled players, wins tend to be easier to
#' come by, as we have seen with the Edmonton Oilers organization throughout the
#' 1980s.
#'
#' Therefore, the code above (about "cap_space") deals with introducing the variable
#' of cap space into the data set in which it's levels are determined by how much
#' salary cap space a team has left after they had paid all of their players in that
#' year.  In this case, "low" represents a team having less that $2 million in cap
#' space left over, "Medium" deals with having $2-5 million in cap space left over,
#' and "High" represents having more than $5 million in cap space left over.  
#' This variable may help us figure out if spending more or less in cap space 
#' for a team's players is beneficial in terms of winning, within these given 
#' categories.
#'
library(latticeExtra)
xyplot(wins~cap_space, col = "green", xlab = "Cap space left over", ylab = "Wins(by team)",
       main = "Cap space left over .vs. Wins \n in the 2018-19 NHL season")
#'
#' After looking at the plot  above, we see that, despite the one outlier of a team 
#' winning over 60 games with a low amount of salary cap space left over, all of the 
#' levels show fairly similar results, which means that we don't have a clear indication
#' that a certain amount of cap space left over is beneficial in terms of winning.
#' 
cap_model <- glm(cbind(wins,loss)~ cap_space, data = ggg, family = binomial)
summary(cap_model)
#' 
#' Although, after developing the model to test it against wins, we see that
#' this variable may have an interesting affect after all, but before we can interpret
#' these results, it seems that the outlier we spotted in the plot earlier may be
#' affecting our results, so we will develop an analysis of this variable of interest
#' after removing that outlier from our dataset. 
#' 
no_light <- ggg[-c(26),]
cap_space_no_light <- factor(no_light$Cap.space.left.over, levels = c("Low", "Medium", "High"))
xyplot(wins~cap_space, data = no_light, xlab = "Cap space left over",
       ylab = "Wins (by team)", main = "Cap space left over .vs. Wins \n (absence of 
       the Tampa Bay Lightning)")
#'
#' When we look at the plot that excludes the outlier (Tampa Bay Lightning who had 
#' over 60 wins and a low amount of salary cap space left over), we can see a
#' clearer picture of how the salary cap space left over does not produce as much
#' of an effect on wins as I had initially thought. To see if this is truly the case,
#' we will perform a more technical analysis:
#'
no_light_model <- glm(cbind(wins,loss)~ cap_space_no_light, data = no_light, family = binomial)
summary(no_light_model)
#' 
#' Interestingly enough, the outlier from our data affected our results quite strongly
#' and now to interpret these results we have evidence to say that the difference 
#' between the medium "cap space left over" level and its respective low level
#' is insignificant; the same goes for the difference between the high level and
#' the low level.  What this means is that the amount that a team would spend on
#' players to improve it does not improve their probability of winning games all that 
#' much.  Once again, these results stem from a minimal number of available factors
#' and observations, so the types of players they spend money on, or even the 
#' experience and past success of the players that they are spending money on, may
#' quite easily change these results and make this variable a significant in terms
#' of its effect on wins, but with what we have available, that is not the case.
#' 
#' What this analysis above has shown us though, is that the Tampa Bay lightning 
#' was a team that had a beyond outstanding regular season in comparison to the 
#' other teams in the NHL in the 2018-19 season.  As a result, this leads us to 
#' think about how our analysis for the other variables we tested earlier may have
#' been affected by their outstanding play in those categories that could have led
#' to results that did not illustrate the average tendencies of the league on a 
#' more regular basis.  We will test one of those previous models with the data 
#' set that the Tampa Bay Lightning has been removed from to see if their effect 
#' on the data would have affected our results fro those models, much like it did 
#' with the cap space models.
#'
faceoff_model <- glm(cbind(wins,loss) ~ faceoff,data = ggg,family = binomial)
summary(faceoff_model)
#'
no_light_faceoff_model <- glm(cbind(wins,loss) ~ faceoff,data = no_light,family = binomial)
summary(no_light_faceoff_model)
#' 
#' Initially, the model that was testing the effect faceoff win percentage on wins
#' was significant when using the full data set, but weakly at best, although 
#' when we test this same model but using the data set that has excluded the Tampa Bay
#' Lightning,the effect of faceoff win percentage on wins becomes insignificant 
#' on wins, which provides evidence to show us that this outlier had a strong effect 
#' on the data and gave us inflated results. We see similar results with the 
#' powerplay success percentage model as well, but along with becoming not significant,
#' the estimated effect of powerplay success percentage became smaller (much weaker):
#' 
pp_model <- glm(cbind(wins,loss)~pp, data = ggg, family = binomial)
summary(pp_model)
#'
no_light_pp_model <- glm(cbind(wins,loss)~pp, data = no_light, family = binomial)
summary(no_light_pp_model)
#'
#' Despite this being a very basic approach to illustrating the effect that an
#' outlier can have on a dataset, it does display how the effects of variables in
#' this dataset on wins could be swayed one way or the other by the influence and
#' leverage that a superior team's ability in certain categories is.  Therefore, 
#' our analysis performed above with the full data set on the variables of interest
#' and determining their effects on wins holds valid, as it takes into account all
#' teams and still displays how strongly performing at optimal levels in many aspects
#' of hockey can optimize wins, but by excluding that outlying team's performance
#' we could show even stronger relationships that could affect a team's ability
#' to win that does not have the skill or players that the superior team does; 
#' how a mid to low tier team can improve their play and find success, essentially,
#' which could provide very interesting connections if we had the data to do so.
#' Very exciting nonetheless.
#'   
#'   
#' # Conclusion
#' The statistical nature of the NHL is quite vast and complex, with a multitude
#' of causal questions that breed excitement and intrigue.  In the case of this 
#' report, I only ventured into the shallow waters of statistical analysis and 
#' what greases the gears of success in the NHL, but was able to find some interesting
#' results nonetheless.  With the overall question of what it takes to make the 
#' playoffs, I was able to reduce this complex question into the nature of winning.
#' Through this I found that shots, goals, and powerplays were effective on a team's 
#' ability to win games, but faceoffs were less so.  With interaction testing and
#' looking for confounders and mediators, goal differential seemed to be the
#' most important factor in finding evidence of causing wins, with the other variables
#' playing more minor roles, but important nonetheless.  Also, many causal questions
#' were faced and I was able to decipher some interesting answers, be them significant
#' or not.  Finally, with 2018-19 being an interesting year in the NHL, as the Tampa
#' Bay Lightning won an extraordinary amount of games (62 out of 82), I was able 
#' to see how outliers in our data could affect results and what that could mean 
#' moving forward.  Overall, statistics are an important foundational tool to help 
#' analyze uncertainty, or almost anything that we face in our lives, and being 
#' able to show that within the context of my favourite sport and something that 
#' has changed my life was a fruitful and awe-inspiring adventure.
#' 
#' # References
#' 1. https://www.tsn.ca/nhl/statistics
#' 
#' 2. https://www.capfriendly.com/archive/2020