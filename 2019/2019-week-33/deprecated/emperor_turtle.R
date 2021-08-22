library(TurtleGraphics)


l = 0.1
p = 10

turtle_init(width = 2, height = 2)
turtle_lwd(3)
turtle_hide()

# left wreath
turtle_setpos(1, 0.6)
turtle_left(90)

for (i in 1:p) {
  turtle_forward(l * 1.5)
  # leaf left
  turtle_left(90)
  turtle_forward(l)
  turtle_right(45)
  turtle_forward(l)
  turtle_right(135)
  turtle_forward(l)
  turtle_right(45)
  turtle_forward(l)
  turtle_left(120)
  # leaf right
  turtle_right(90)
  turtle_forward(l)
  turtle_left(45)
  turtle_forward(l)
  turtle_left(135)
  turtle_forward(l)
  turtle_left(45)
  turtle_forward(l)
  turtle_right(120)
  # 
  turtle_right(15)
  }

# right wreath
turtle_setpos(1, 0.6)
turtle_right(30)
for (i in 1:p) {
  turtle_forward(l * 1.5)
  # leaf left
  turtle_left(90)
  turtle_forward(l)
  turtle_right(45)
  turtle_forward(l)
  turtle_right(135)
  turtle_forward(l)
  turtle_right(45)
  turtle_forward(l)
  turtle_left(120)
  # leaf right
  turtle_right(90)
  turtle_forward(l)
  turtle_left(45)
  turtle_forward(l)
  turtle_left(135)
  turtle_forward(l)
  turtle_left(45)
  turtle_forward(l)
  turtle_right(120)
  # 
  turtle_right(15)
}