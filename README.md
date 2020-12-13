# Automatic Non-chord Tone Identification

A commonly-cited reason for the poor performance of automatic chord estimation (ACE) systems within music information retrieval (MIR) is that non-chord tones (i.e., notes outside the supporting harmony) contribute to error during the labeling process. Despite the prevalence of machine-learning approaches in MIR, there are cases where rule-based algorithms provide a simpler alternative while allowing for insights into musicological practices. <br>

In this project, we present a statistical model for predicting chord tones. Our model is currently focused on predicting chord tones in classical music melody. The non-chord tone and chord tone are primarily classified by the note’s metric position and intervals of approach and departure. By using meter, duration, and melodic intervals as predictors, we build a logistic regression model for predicting chord tones on the complete TAVERN dataset. While our probabilistic approach is similar to other efforts in the domain of automatic harmonic analysis, our focus is on melodic reduction rather than predicting harmony.  <br>

### Tools
Humdrum <br>
R <br>
<br>

To use the program, navigate to the `InputKrn` folder and open the `InputToModel.R` for instructions. <br>
