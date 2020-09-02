# Results

## Method

1. **Initial setup:** For the initial setup of this experiment, we implemented Conway's Game of Life in three languages: Kotlin, Julia & Haskell and introduced two bugs in each implementation.
   - Compilation bug: To evaluate how helpful compiler is to navigate through the bug.
   - Logical bug: To evaluate the readability of language.
2. **Participants sign-up:** After the implementation, we let 10 participants sign up for our experiment and assigned each of the with a token to identify them anonymously. We setup a virtual meet of 30 minutes on zoom as per participant's preferred time for the upcoming debugging session.
3. **Debugging session:** During the scheduled debugging session:
   - Each session will be hosted and observed by 2 group members.
   - Participants will be required to share their screen for us to observe.
   - Debugger will follow instruction from the README file to perform the experiment. Session hosts will guide participant though the steps if needed.
   - Session hosts have permission to provide hints to participants to understand the bug after predefined threshold time. This factor will be considered for the final evaluation.
4. **Observations:** While participant is debugging, following observations will be noted by the session hosts:
   - For each Kotlin, Julia and Haskell debugging experiment:
      - Time taken to fix the compilation bug.
      - Time taken to fix the logical bug.
      - Used resources (i.e Internet search, Help from the session hosts)
   - Did the participant use traditional backtracking approach to find the root cause of the error?
5. **Post session survey:** Post debugging session, participants are required to complete the survey in order to record their information about their background knowledge and comments for the debugging session. This survey includes:
   - Familiarity with languages: Kotlin, Julia and Haskell prior this experiment.
   - For each Kotlin, julia and Haskell debugging experiment:
      - How helpful compiler's error messages were for Compilation error?
      - How readable the code is to understand Logical error?
      - How difficult these bugs were to solve?
      - Which programming language resembles the most to this language?
   - Ranking of relative difficulty from debugging perspective.
   - Which language they are most likely to use in near future?

## Material
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

## Observations
<p align="center">
  <img src="https://github.com/urvishvasani/HW_2_3_Game_of_Life/blob/master/data/test.png">
</p>


1. The above displayed graphs compares the overall time taken by the debugger in order to rectify the bugs in different languages. Major takeaways from the graph are as following :-
    - 1/10 people only one person was able to debug code written in Haskell which denotes that Haskell if comparatively difficult to debug.
    - Amount of time taken to debug Kotlin indicates that it has a dependence on users prior knowledge about the language. Users familiar with the language were able to debug quickly.
    - 8/10 users were able to debug the code written in Julia and all of them comparatively took very less time.

## Conclusions

## Threats to validity