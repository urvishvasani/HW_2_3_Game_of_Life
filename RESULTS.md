# Results

## Method

1. **Initial setup:** For the initial setup of this experiment, we implemented Conway's Game of Life in three languages: Kotlin, Julia & Haskell and introduced two bugs in each implementation.
   - Compilation bug: To evaluate how helpful the compiler is to navigate through the bug.
   - Logical bug: To evaluate the readability of language.
2. **Participants sign-up:** After the implementation, we let 10 participants sign up for our experiment and assigned each of them with a token to identify them anonymously. We set up a virtual meet of 30 minutes on Zoom per participant according to their preferred time for the debugging session.
3. **Debugging session:** For each debugging session:
   - It will have one participant and observed by two group members (hosts).
   - The participant will be required to share their screen for us to note down observations.
   - The participant will follow instructions from the README file to participate in the experiment. Session hosts may guide participant though the steps if needed.
   - Session hosts have permission to provide hints to participants to understand the bug after predefined threshold time. This factor will be considered for the final evaluation.
4. **Observations:** While participant is debugging, following observations will be noted by the session hosts:
   - For debugging experiment in each language (Kotlin, Julia and Haskell):
      - Time taken to fix the compilation bug.
      - Time taken to fix the logical bug.
      - Used resources (i.e Internet search, Help from the session hosts)
   - Whether the participant used traditional backtracking approach to find the root cause of the error?
5. **Post session survey:** Post debugging session, participants are required to complete the survey in order to record their information about their background, knowledge and comments for the debugging session. This survey include would include:
   - Familiarity with languages: Kotlin, Julia and Haskell prior this experiment.
   - For each (Kotlin, julia and Haskell) debugging experiment:
      - How helpful compiler's error messages were for compilation error?
      - How readable the code is to understand logical error?
      - How difficult these bugs were to solve?
      - Which programming language resembles the most to this language?
   - Ranking of relative difficulty from debugging perspective.
   - Which language they are most likely to use in near future?

## Materials
1. [Sign up sheet](https://docs.google.com/spreadsheets/d/1BKcw3SPB2JIysBe6Kw6mkylA_3Ja0o-eQ5TmNMgAZww/edit#gid=0) to participate in our experiment
2. Implementation with compilation and logical bugs are available at:
   - [Kotlin](code/kotlin/GOL.kt)
   - [Julia](code/julia/GOL.jl)
   - [Haskell](code/haskell/GOL.hs)
3. [Instructions for the debugging session](https://github.com/urvishvasani/HW_2_3_Game_of_Life#how-to-run) guides participants to compile and run code on an online IDE
4. [Online IDE](https://repl.it/github/urvishvasani/HW_2_3_Game_of_Life) where participants will execute the code.
5. [Post debugging survey](https://docs.google.com/forms/d/e/1FAIpQLSeqIzBdJArD6M2HLxb0OIcmEGPh17jvUO845rWSREaaegU3qQ/closedform) for participants to fill at the end of 30 mins debugging session.
6. [Post debugging survey responses](https://docs.google.com/spreadsheets/d/1hthxqCVm0Dbk5fdAo-5iRvSGBeUH_mGzRias4YtLCZo/edit?usp=sharing) to analyse the inputs from participants.
7. [Observation from the session hosts](https://docs.google.com/spreadsheets/d/1o9TrwybYMLmB7scy8Pe4eh5sLlc40FrnlNM922uLtZE/edit?usp=sharing) to draw conclusions about the hypothesis.

## Observations from responses collected via survey
### Familiarity with all 3 languages

<p align="center">
  <img src="https://github.com/urvishvasani/HW_2_3_Game_of_Life/blob/master/data/familiarity_with_languages.PNG">
</p>

The graph shown above showcases the participants' familiarity with the three languages. 
   - Among the three, Kotlin was the most used previously(3) followed by Julia(2).
   - No one had any prior experience with Haskell.
   - It can be argued that since majority of the participants had little or no experience with the 3 languages, this experiment accurately compares debugging and understanding code in 3 alien languages which are fundamentally different.
   
### Kotlin
#### Helpfulness of compilation error message (1 being very helpful and 5 being not helpful at all)
<p align="center">
  <img src="https://github.com/urvishvasani/HW_2_3_Game_of_Life/blob/master/data/kotlin_compilation_error_message.PNG">
</p>

The above graph shows how effective the compilation error message was for the participants in the case of Kotlin
   - 9/10 participants rated this particular question between 1 and 3. Thus, it can be said that for the particular compilation error that we introduced, the error message was somewhat helpful for the participants to debug

#### Understandability of Kotlin language (1 being easy to understand and 5 being tough to interpret)
<p align="center">
  <img src="https://github.com/urvishvasani/HW_2_3_Game_of_Life/blob/master/data/kotlin_understandability.PNG">
</p>

The above graph shows the understandability of code in Kotlin for solving the logical error introduced. 
   - As seen in the graph, the data is evenly distributed between helpful and not helpful. Hence, it is not possible to make any good logical inference from this

#### Difficulty level of Kotlin (1 being very easy and 5 being very difficult)
<p align="center">
  <img src="https://github.com/urvishvasani/HW_2_3_Game_of_Life/blob/master/data/kotlin_difficulty.PNG">
</p>

The above graph shows how tough to interpret Kotlin was for the participants
   - On an average, people found Kotlin to be easy to interpret (4 v/s 3)
   - 3/10 participants were neutral in their assessment of Kotlin's understandability and logical flow
   
Amongst the common languages that we were exempted from using, all the participants found Kotlin to be very similar to Java with 2 responses for C++ and Python as well
   
### Julia
#### Helpfulness of compilation error message (1 being very helpful and 5 being not helpful at all)
<p align="center">
  <img src="https://github.com/urvishvasani/HW_2_3_Game_of_Life/blob/master/data/julia_compilation_error_message.PNG">
</p>

The above graph shows how effective the compilation error message was for the participants in the case of Julia
   - 5/9 participants found the error message to be of little help or of no help at all
   - 3/9 participants found the message to be somewhat helpful with no one finding it to be most helpful
   - Thus, it can be said that for the error we introduced in Julia, people were finding the error message not much helpful

#### Understandability of Julia language (1 being easy to understand and 5 being tough to interpret)
<p align="center">
  <img src="https://github.com/urvishvasani/HW_2_3_Game_of_Life/blob/master/data/julia_understandability.PNG">
</p>

The above graph shows the understandability of code in Julia for solving the logical error introduced. 
   - Majority(5/9) of the participants found Julia to be somewhat easy to understand with 3/9 participants finding it to be a little tougher to interpret
   - Surprisingly, no one found it to be very easy to understand considering its strong affinity to Python in terms of coding style

#### Difficulty level of Julia (1 being very easy and 5 being very difficult)
<p align="center">
  <img src="https://github.com/urvishvasani/HW_2_3_Game_of_Life/blob/master/data/julia_difficulty.PNG">
</p>

The above graph shows how tough to interpret Julia was for the participants
   - Here as well no one found Julia to be very easy to interpret
   - 4/9 participants found Julia to be of their liking with the opinion then being largely divided between rest of the choices and only 1 participant rated Julia as very tough to understand
   
Amongst the common languages that we were exempted from using, all the participants found Julia to be very similar to Python with 1 participant also voting for JavaScript as being closely resembled

   
### Haskell
#### Helpfulness of compilation error message (1 being very helpful and 5 being not helpful at all)
<p align="center">
  <img src="https://github.com/urvishvasani/HW_2_3_Game_of_Life/blob/master/data/haskell_compilation_error_message.PNG">
</p>

The above graph shows how effective the compilation error message was for the participants in the case of Haskell
   - Majority(6/9) of the participants found the error message to be of no use at all
   - No one found the error message to be helping them with 2 participants rating it 3(decent) being the best response
   
#### Understandability of Haskell language (1 being easy to understand and 5 being tough to interpret)
<p align="center">
  <img src="https://github.com/urvishvasani/HW_2_3_Game_of_Life/blob/master/data/haskell_understandability.PNG">
</p>

The above graph shows the understandability of code in Haskell for solving the logical error introduced. 
   - Almost everyone(8/9) found Haskell to be too tough to understand
   - Although there is 1 response that classifies Haskell to be easy to understand. We believe it is an error on the part of the participant while filling out the survey because no one has mentioned the overall difficulty of Haskell to be even slightly easy

#### Difficulty level of Haskell (1 being very easy and 5 being very difficult)
<p align="center">
  <img src="https://github.com/urvishvasani/HW_2_3_Game_of_Life/blob/master/data/haskell_difficulty.PNG">
</p>

The above graph shows how tough to interpret Haskell was for the participants
   - Everyone found Haskell to be very tough to understand(7/9)
   - No one rated Haskell to be at least decent in terms of difficulty which suggests that more time is required to learn this language

Amongst the common languages that we were exempted from using, no participant was able to select any language as being similar to Haskell. This shows that Haskell was fundamentally different than the most popular languages and is evidence to it being tougher to grasp for people with no prior experience of it

### Overall Observations
<p align="center">
  <img src="https://github.com/urvishvasani/HW_2_3_Game_of_Life/blob/master/data/relative_difficulty.PNG">
</p>

The above graph shows how the participants rated the comparative difficulty of all 3 languages
   - Majority of the participants(6/10) voted as Kotlin to be the easiest to work on
   - 8/10 participants voted Julia as second
   - As expected, 8/10 participants voted Haskell to be the toughest

The overall difficulty ranking can be said to be : 
   1. Haskell (Most difficult)
   2. Julia
   3. Kotlin (Easiest)
   
<p align="center">
  <img src="https://github.com/urvishvasani/HW_2_3_Game_of_Life/blob/master/data/preferred_language.PNG">
</p>

The above pie-chart shows which language among the 3 are the participants most likely to use if given a choice in a future project
   - 6/10 participants went with Kotlin and rest 4 went with Julia as their most preferred language to use in the future. There were no takers for Haskell though
   
## Manual observations
### Total debugging time for each languages
<p align="center">
  <img src="https://github.com/urvishvasani/HW_2_3_Game_of_Life/blob/master/data/total.PNG">
</p>

The above displayed graphs compares the overall time taken by the participants in order to rectify the bugs in different languages. Major takeaways from the graph are as following:
   - Only 1/10 participants were able to debug code written in Haskell which denotes that Haskell is comparatively difficult to debug.
   - Amount of time taken to debug Kotlin indicates that it has a dependence on users prior knowledge about the language. Users familiar with the language were able to debug quickly.
   - 8/10 users were able to debug the code written in Julia and all of them comparatively took very less time.

### Compilation error debugging time for each languages
<p align="center">
  <img src="https://github.com/urvishvasani/HW_2_3_Game_of_Life/blob/master/data/compilation.PNG">
</p>

The above displayed graphs compares the time taken by the participants to solve the compilation error in different languages. Major takeaways from the graph are as following:
   - 3/10 debparticipantsuggers were able to solve compilation bug for haskell. All the users complaint about ugly console output and not being able to locate the line of error.
   - Surprisingly, even though a lot of participants were used to Kotlin, they took a comparatively higher time for solving the error. We hypothesize that quality of bug introduced would be the reason for this issue.
   - participants were able to solve the compilation error in Julia very quickly even though they complaint about it having not that great stacktrace. One reason could be the similarity between Julia and Python.

### Logical error debugging time for each languages
<p align="center">
  <img src="https://github.com/urvishvasani/HW_2_3_Game_of_Life/blob/master/data/logical.PNG">
</p>

The above displayed graphs compares the time taken by the participants to solve the logical error in different languages. Major takeaways from the graph are as following:
   - 2/10 participants were able to solve logical bug introduced in haskell.
   - It took comparatively less time for each participant to find and fix the logical bug introduced in julia.
  
## Conclusions
- Among three languages people found it hardest to debug Haskell code
- Julia was easiest for people to debug due to its readable syntax and similarities to Python
- Participants voted Kotlin as the language that they would most likely work on, mostly because they were either familiar with the Kotlin or Java which they found identical to Kotlin.
- Programmers are more comfortable with the languages that have more self-explanatory console output and readable source code. 
- Moreover, participants prefered those languages that had desirable online documentation.

## Threats to validity
**Bugs:**
- After few debugging sessions, we realized that the complexity of bugs should have been similar for each implementation to draw impartial conclusions.

**Language selection:**
- Choosing Haskell as one of the implementations did not worked out well for this experiment. It turned out that none of the participants were familiar even a little bit with Haskell before and it is hard for someone to debug all three languages with one being Haskell in a time frame of 30 minutes. 
- We performed the dry run for our experiment before hosting any session. However, familiarity with Haskell for majority of our team member did not raised any issue mentioned above earlier.

**Observations:**
- We allowed participants to debug for more than 30 minutes if they ask for. Although, we completely overlooked the possibility that this can affect the evaluation process.
- Session hosts were allowed to provide hints to participants after some threshold time with an intention to let them debug in all languages. However, we later realized that it would be unfair to others who fixed the bug without any help and resulted in skewed conclusion. We tried to fix this issue to some extend by manually considering this factor in our evaluation process.
