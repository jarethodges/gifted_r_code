setwd ("C:\\Users\\jaret\\Desktop\\ALL_WORK\\stat")

data = read.csv ("got_ratings.csv")

library ("ggplot2")
install.packages ("tidyverse")

View (data)

model = lm (formula =  moral ~ physical + gender, data = data)
summary ( model)

model = glm (formula = gender ~ physical + moral, data = data, family = "binomial")
summary (model)


ggplot(data, aes(x=moral, y= physical)) + 
geom_point(aes(color = gender)) + 
geom_text(aes(label=label))

#moving labels away from block with nudget#
ggplot(data, aes(x=moral, y= physical)) + 
geom_point(aes(color = gender)) + 
geom_text(aes(label=label), nudge_y = -.03)

#reversing scales with the scale_reverse command#
ggplot(data, aes(x=moral, y= physical)) + 
geom_point(aes(color = gender)) + 
geom_text(aes(label=label), nudge_y = -.03) +
scale_y_reverse()

#modifying the axis, there is a special parameter when using reverse. In this case lim.
For the x axis, the continuous command means you are not changing it. In stead of lim, 
we are using limits#
ggplot(data, aes(x=moral, y= physical)) + 
geom_point(aes(color = gender)) + 
geom_text(aes(label=label), nudge_y = -.03) +
scale_y_reverse(lim = c(1,0)) +
scale_x_continuous (limits = c(0,1))