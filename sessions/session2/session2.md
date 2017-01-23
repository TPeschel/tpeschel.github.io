session2
========================================================
author: Mandy
date: 30.05.2016
autosize: true

Recap
========================================================
type: section
incremental: true

Recap
========================================================
title: false

We learned about
- data structures:
  - vectors
  - data frames
- assigning
  - **=**
  - **<-**


Recap
========================================================
title: false
incremental: true

- Functions e.g.:
  - **c()**
  - **plot()**
  - **nrow()**
  - **summary()**


Exercise
========================================================

- use R to calculate $4^3 + 3^{2+1}$
- Let our small data set be: 2 5 4 10 8
  1. Enter these data into a data vector *v*
  2. Find the square of each number.
  3. Substract 6 from each number
  4. Substract 9 from each number and then square the answer
  



Recap
========================================================
title: false
incremental: true

- Accessing parts of data frames via
  - **$**
  - and numbers using square brackets **[ ]**
    - vectors: **my.vector[index]**
    - data frame: **my.dataframe[rows.index,col.index]**

Exercise
========================================================

- load the *ChickWeight* data set into the workspace using **data()**
- how many observations and how many variables? How did you get the information?
- what is the mean, min and max of *weight* and *Time*
- use **plot()** to produce a boxplot of *weight* dependent on *Diet*


The working directory
========================================================
incremental: true

- in every R session you can define the so called working directory
- the working directory is the place (folder) on your hard drive or usb stick where R is saving objects, or is looking for files you wanna load
- you can check what the working directory by typing **getwd()**
- you can set the working directory e.g.
  - using **setwd("path/to/the/course/session2")**
  - using the menu: Session -> Set Working Directory -> Choose Directory

The working directory
========================================================
incremental: true

Example:






































```
Error in setwd("/media/mandy/Volume/transcend/life/2016kurs/session2") : 
  cannot change working directory
```
