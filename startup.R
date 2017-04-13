# Exploratory Text Analysis in R
# Workshop by Carolyn Moritz and Sarah Stanley
# Last updated 2017/04/12

# SECTION 1: SETUP & INTRODUCTION

# On your own: Set up RStudio environment
# Follow along: http://guides.lib.fsu.edu/text-analysis/r/environment

# This is the Source Code pane!
# Here you can write, save, and execute a command or an entire script.

# To run multiple lines of code, highlight the text and click 'Run'
# or, type Ctrl+Enter (PC) / Cmd+Return (Mac)
a <- "The plays of Shakespeare are the most fascinating tales ever told in English."

# You can run a single command in the Console pane by typing it in
# followed by Enter/Return. Try this one below:
a

# Let's install a library:
library(stringr)

# Try different ways of running these commands.
# See what you like best!
str_to_upper(a)

str_to_lower(a)

str_to_title(a)

y <- word(a, c(1:13))

corpus <- paste(y[1:4], collapse=" ")

str_to_title(corpus)

# All warmed up? Let's get to work!
# Import datalitfsu3.RProj from GitHub
