library("rjson")
library("ggplot2")
library("cowplot")
library("dplyr")
library("lme4")
library("lmerTest")

#setwd to current folder

rm(list=ls())


### Pilot ###

##Import data 

setwd("./data")

filenames <- list.files()


## Create dataframes

tests <- data.frame("pid" = numeric(), "trial" = numeric(), "rule" = numeric(), "rule_nr" = numeric(), "rule_re" = numeric(), "condition" = numeric(), "trial_order" = numeric(), "test" = numeric(), "size" = numeric(), "colour" = numeric(), "object_nr" = numeric(), "follow_rule" = logical())
generalisations <- data.frame("pid" = numeric(), "trial" = numeric(), "rule" = numeric(), "rule_nr" = numeric(), "rule_re" = numeric(), "condition" = numeric(), "accuracy" = numeric(), "report_rule" = character())
selfreport <- data.frame("pid" = numeric(), "trial" = numeric(), "rule" = numeric(), "rule_nr" = numeric(), "rule_re" = numeric(), "condition" = numeric(), "trial_order" = numeric(), "test" = numeric(), "shape_var" = numeric(), "colour_var" = numeric(), "report_test" = character())

i <- j <- k <- l <- m <- 1

for(i in 1:length(filenames)) { # Loop through each participant
  
  temp <- fromJSON(file=filenames[i])
  condition <- temp[["subject_data"]][["condition"]]
  
  
  for (j in 1:length(temp[["scenes"]])) { # Loop through each trial in order
    
    trial <- temp[["subject_data"]][["trials"]][j] # Get trial number 
    
    rule <- temp[["subject_data"]][["rules_text"]][j] # Get rule 
    rule_nr <- ifelse(rule == 'There is exactly one red rock.', 1, 
                 ifelse(rule == 'There is exactly one green rock.', 2,
                 ifelse(rule == 'There is exactly one purple rock.', 3, 
                 ifelse(rule == 'There is exactly one stick-shaped rock.', 4, 
                 ifelse(rule == 'There is exactly one triangle-shaped rock.', 5, 
                 ifelse(rule == 'There is exactly one square-shaped rock.', 6,
                 ifelse(rule == 'There are exactly two red rocks.', 7,
                 ifelse(rule == 'There are exactly two green rocks.', 8, 
                 ifelse(rule == 'There are exactly two purple rocks.', 9, 
                 ifelse(rule == 'There are exactly two stick-shaped rocks.', 10, 
                 ifelse(rule == 'There are exactly two triangle-shaped rocks.', 11, 
                 ifelse(rule == 'There are exactly two square-shaped rocks.', 12,
                 ifelse(rule == 'There are exactly three red rocks.', 13, 
                 ifelse(rule == 'There are exactly three green rocks.', 14,
                 ifelse(rule == 'There are exactly three purple rocks.', 15, 
                 ifelse(rule == 'There are exactly three stick-shaped rocks.', 16,
                 ifelse(rule == 'There are exactly three triangle-shaped rocks.', 17, 18)))))))))))))))))
  rule_re <- ifelse(rule == 'There is exactly one red rock.', 'Colour', 
                 ifelse(rule == 'There is exactly one green rock.', 'Colour',
                 ifelse(rule == 'There is exactly one purple rock.', 'Colour', 
                 ifelse(rule == 'There is exactly one stick-shaped rock.', 'Shape', 
                 ifelse(rule == 'There is exactly one triangle-shaped rock.', 'Shape', 
                 ifelse(rule == 'There is exactly one square-shaped rock.', 'Shape',
                 ifelse(rule == 'There are exactly two red rocks.', 'Colour',
                 ifelse(rule == 'There are exactly two green rocks.', 'Colour', 
                 ifelse(rule == 'There are exactly two purple rocks.', 'Colour', 
                 ifelse(rule == 'There are exactly two stick-shaped rocks.', 'Shape', 
                 ifelse(rule == 'There are exactly two triangle-shaped rocks.', 'Shape', 
                 ifelse(rule == 'There are exactly two square-shaped rocks.', 'Shape',
                 ifelse(rule == 'There are exactly three red rocks.', 'Colour', 
                 ifelse(rule == 'There are exactly three green rocks.', 'Colour',
                 ifelse(rule == 'There are exactly three purple rocks.', 'Colour', 
                 ifelse(rule == 'There are exactly three stick-shaped rocks.', 'Shape',
                 ifelse(rule == 'There are exactly three triangle-shaped rocks.', 'Shape', 'Shape')))))))))))))))))

    sum_selected_normal <- sum(temp[["selected"]][[j]] == c(TRUE,TRUE,TRUE,TRUE,FALSE,FALSE,FALSE,FALSE))
    sum_selected_flipped <- sum(c(temp[["selected"]][[j]][1], !temp[["selected"]][[j]][c(2:4)], temp[["selected"]][[j]][5], !temp[["selected"]][[j]][c(6:8)]) == c(TRUE,TRUE,TRUE,TRUE,FALSE,FALSE,FALSE,FALSE))
    accuracy <- ifelse(trial > 17 && rule_nr > 15, sum_selected_flipped, sum_selected_normal) # Get accuracy for generalisations and flip reverse coded trials

    
    report_rule <- temp[["subject_data"]][["freeresps"]][j] # Get free text report for each rule
    
    temp_row_1 <- c(i, trial, rule, rule_nr, rule_re, condition, accuracy, report_rule) # add participant id (i), trial number, rule, condition, generalisation accuracy, and free text response
    generalisations[nrow(generalisations) + 1,] <- temp_row_1 # add generalisation to dataframe
    
    for (k in 1:8) { # Loop through each test
      
      report_test <- ifelse(k==1, "NULL", temp[["subject_data"]][["freedescr"]][k - 1 + (7 * (j-1))]) # get free text report for each test
      
      shapevar <- length(unique(temp[["scenes"]][[j]][[k]][["sizes"]]))
      colourvar <- length(unique(temp[["scenes"]][[j]][[k]][["colours"]]))
      
      temp_row_2 <- c(i, trial, rule, rule_nr, rule_re, condition, j, k, shapevar, colourvar, report_test)
      selfreport[nrow(selfreport) + 1,] <- temp_row_2
      
      for (l in 1:length(temp[["scenes"]][[j]][[k]][["sizes"]])) { # Loop through each object
        
        size <- temp[["scenes"]][[j]][[k]][["sizes"]][l] # Get shape (coded as size due to historical reasons)
        colour <- temp[["scenes"]][[j]][[k]][["colours"]][l] # Get colour (red = red, blue = purple, green = green/teal)
        follow_rule <- temp[["scenes"]][[j]][[k]][["follow_rule"]]
        
        
        temp_row_3 <- c(i, trial, rule, rule_nr, rule_re, condition, j, k, size, colour, l, follow_rule) # add participant id (i), trial number (condition), trial order (j), test number (k), shape, and colour
        
        tests[nrow(tests) + 1,] <- temp_row_3 # add object to main dataframe
      }
      
      l <- 1 #reset object counter
      
    }
    
  }
  
}

setwd("../")

### Save data file for rating

write.csv(selfreport,"selfreport.csv", row.names = FALSE)
write.csv(generalisations,"gener.csv", row.names = FALSE)
write.csv(tests, "tests.csv", row.names = FALSE)


### Group level analysis

#### Shape variability

selfreport$trial <- as.numeric(selfreport$trial)
selfreport$trial_label <- ifelse(selfreport$trial > 17, "Exactly three X", ifelse(selfreport$trial < 9, "Exactly one X", "Exactly two X"))
selfreport$trial_label <- factor(selfreport$trial_label, levels = c("Exactly one X", "Exactly two X", "Exactly three X"))
selfreport$test <- as.numeric(selfreport$test)
selfreport$shape_var <- as.numeric(selfreport$shape_var)


sh_var <- ggplot(data = selfreport, aes(x = test, y = shape_var)) +
  #geom_jitter(width = .25, height = 0, alpha = .75, colour = "lightblue") +
  geom_line(aes(colour = pid), position=position_jitter(w=0.1, h=0.1)) +
  stat_summary() +
  #geom_point(data = ~filter(.x, follow_rule == TRUE), y =.55, shape = 8, size = 2, colour = "#fcba03") +
  #scale_shape_manual(name = "Shape", labels = c("Stick",  "Triangle", "Square"), values = c(73, 17, 15)) +
  #scale_colour_manual(name = "Colour", labels = c("Purple", "Green", "Red"), values = c("#6e64b2", "#6eb2a2", "#b26a6b")) +
  scale_x_continuous(breaks = c(1:8)) +
  labs(x = "Test Nr", y = "Unique shape Nr") +
  facet_grid(condition + rule_re ~ trial_label) +
  theme_bw() +
  theme(legend.position = "none")

sh_var

#### Colour variability

selfreport$colour_var <- as.numeric(selfreport$colour_var)

c_var <- ggplot(data = selfreport, aes(x = test, y = colour_var)) +
  #geom_jitter(width = .25, height = 0, alpha = .75, colour = "lightblue") +
  geom_line(aes(colour = pid), position=position_jitter(w=0.1, h=0.1)) +
  stat_summary() +
  #geom_point(data = ~filter(.x, follow_rule == TRUE), y =.55, shape = 8, size = 2, colour = "#fcba03") +
  #scale_shape_manual(name = "Shape", labels = c("Stick",  "Triangle", "Square"), values = c(73, 17, 15)) +
  #scale_colour_manual(name = "Colour", labels = c("Purple", "Green", "Red"), values = c("#6e64b2", "#6eb2a2", "#b26a6b")) +
  scale_x_continuous(breaks = c(1:8)) +
  labs(x = "Test Nr", y = "Unique colour Nr") +
  facet_grid(condition + rule_re ~ trial_label) +
  theme_bw() +
  theme(legend.position = "none")

c_var


###Group level aggregate 

#### Shape variability

#selfreport$trial_label <- as.numeric(selfreport$trial_label)

sh_var_agg <- ggplot(data = selfreport, aes(x = trial_label, y = shape_var, color = rule_re)) +
  #geom_jitter(width = .25, height = 0, alpha = .75, colour = "lightblue") +
  #geom_violin() +
  stat_summary() +
  geom_smooth(aes(x = as.numeric(trial_label)), method = lm) +
  labs(x = "Trial", y = "Unique shape Nr") +
  ylim(c(1,3)) +
  facet_grid(. ~ condition) +
  theme_bw() 

sh_var_agg

anova(lmer(shape_var ~ rule_re + trial_label + condition + rule_re * trial_label * condition + (1|pid), data = selfreport))


#### Colour variability

c_var_agg <- ggplot(data = selfreport, aes(x = trial_label, y = colour_var, color = rule_re)) +
  #geom_jitter(width = .25, height = 0, alpha = .75, colour = "lightblue") +
  stat_summary() +
  geom_smooth(aes(x = as.numeric(trial_label)), method = lm) +
  #geom_violin() +
  labs(x = "Trial", y = "Unique colour Nr") +
  ylim(c(1,3)) +
  facet_grid(. ~ condition) +
  theme_bw() 

c_var_agg

anova(lmer(colour_var ~ rule_re + trial_label + condition + rule_re * trial_label * condition + (1|pid), data = selfreport))


#### Relevant feature variability
library(reshape2)

selfreport$rel_feat <- ifelse(selfreport$rule_re=="Shape", selfreport$shape_var, selfreport$colour_var)
selfreport$irrel_feat <- ifelse(selfreport$rule_re=="Shape", selfreport$colour_var, selfreport$shape_var)
selfreport_long <- melt(selfreport, id = c("pid", "rule_re", "condition", "trial", "trial_label"), measure = c("rel_feat", "irrel_feat"))

selfreport_long$trial <- as.numeric(selfreport_long$trial)
selfreport_long$trial_label <- ifelse(selfreport_long$trial > 17, "Exactly three X", ifelse(selfreport_long$trial < 9, "Exactly one X", "Exactly two X"))
selfreport_long$trial_label <- factor(selfreport_long$trial_label, levels = c("Exactly one X", "Exactly two X", "Exactly three X"))
selfreport_long$condition <- factor(selfreport_long$condition, levels = c(1,2,3), labels = c("Vague", "Moderate", "Clear"))
selfreport_long$variable <- factor(selfreport_long$variable, labels = c("relevant feature", "irrelevant feature"))


var_agg <- ggplot(data = selfreport_long, aes(x = trial_label, y = value, colour = variable)) +
  #geom_jitter(width = .25, height = 0.1, alpha = .25) +
  #stat_summary() +
  #geom_violin() +
  geom_smooth(aes(x = as.numeric(trial_label)), method = lm) +
  labs(x = "Trial", y = "Unique feature nr") +
  ylim(c(1,3)) +
  facet_grid(. ~ condition) +
  theme_bw() +
  theme(title = element_text(face = "bold", size = 12), legend.box = "vertical", legend.text = element_text(size = 12), legend.title = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(), axis.line = element_line(), axis.title.y = element_text(face = "bold", size = 12), axis.title.x = element_text(face = "bold", size = 12), axis.text=element_text(face = "bold",size = 10), strip.text.x = element_text(face = "bold", size = 12, color = "black"), strip.background = element_rect(fill = 0, color = 0))  

var_agg

anova(lmer(value ~ variable + trial_label + condition + variable * trial_label * condition + (1|pid), data = selfreport_long))


#### Generalisation

generalisations$accuracy <- as.numeric(generalisations$accuracy)
generalisations$trial <- as.numeric(generalisations$trial)
generalisations$trial_label <- ifelse(generalisations$trial > 17, "Three X", ifelse(generalisations$trial < 9, "One X", "Two X"))
generalisations$trial_label <- factor(generalisations$trial_label, levels = c("One X", "Two X", "Three X"))
generalisations$condition <- factor(generalisations$condition, levels = c(1,2,3), labels = c("Vague", "Moderate", "Clear"))

gener_acc <- ggplot(data = generalisations, aes(x = trial_label, y = accuracy)) +
  #geom_violin(aes(fill = condition), alpha = .4) +
  geom_boxplot(aes(fill = condition), alpha = .4) +
  geom_jitter(height = .05, width = .2, alpha = .4, aes(colour = pid)) +
  #stat_summary(colour = "black") +
  #geom_line(aes(x = as.numeric(trial_label), colour = pid), position=position_jitter(w=0.1, h=0.1)) +
  scale_y_continuous(limits = c(-0.5,8.5), breaks = c(0:8)) +
  scale_fill_brewer(palette = "Greys")+
  labs(x = "Rule", y = "Generalisation accuracy") +
  facet_wrap(~ condition) +
  theme_bw() +
  theme(title = element_text(face = "bold", size = 12), legend.position = "none", legend.box = "vertical", legend.text = element_text(size = 12), legend.title = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(), axis.line = element_line(), axis.title.y = element_text(face = "bold", size = 12), axis.title.x = element_text(face = "bold", size = 12), axis.text=element_text(face = "bold",size = 10), strip.text.x = element_text(face = "bold", size = 12, color = "black"), strip.background = element_rect(fill = 0, color = 0))  

gener_acc



anova(lmer(accuracy ~ trial_label + condition + trial_label + (1|pid), data = generalisations))

