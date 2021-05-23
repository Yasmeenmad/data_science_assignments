# 1. TASK: print "Hello World"
print( "Hello World" )

# 2. print "Hello Noelle!" with the name in a variable
name = "Yasmeen!"
print( "Hello", name )
print( "Hello "+ name  )

# 3. print "Hello 42!" with the number in a variable
name = 7
print( "Hello", name , "!")
print( "Hello"+ name + "!" )

# 4. print "I love to eat sushi and pizza." with the foods in variables
fave_food1 = "burger"
fave_food2 = "pasta"
print( "I love to eat {} and {}.".format(fave_food1, fave_food2) )
print( f"I love to eat {fave_food1} and {fave_food2}." )

# optional task
name = "My name is %s" % "Yasmeen Aldossary" 
best_year = "and the best year of my life was %d" % 2020 
print(name, best_year)

# optional task
interest1 = "reading"
interest2 = "learning new things"
num_books = 11
print("My interests are %s and %s ,and this year I have read %d books" % (interest1, interest2, num_books))		# or with variables

# optional task
statement = "Oh finally we can travel outside our country!"
print(statement.title())
print(statement.upper())
print(statement.lower())