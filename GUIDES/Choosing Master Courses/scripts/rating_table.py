import pandas as pd

courses = ["BAN400", "BAN402", "BUS401E", "BAN432",
           "BAN403", "BAN404", "STR459", "BAN438"]

course_ratings = pd.DataFrame(columns=['Course', 'Learning Outcome',
                              'Coursework Amount', 'Difficulty', 'Hours Spent', 'Lecture Quality'])


def rate_course(course, learning_outcome, coursework_amount, difficulty, hours_spent, lecture_quality):
    # Create a new row with the course information and rating
    new_row = pd.DataFrame({'Course': [course],
                            'Learning Outcome': [learning_outcome],
                            'Coursework Amount': [coursework_amount],
                            'Difficulty': [difficulty],
                            'Hours Spent': [hours_spent],
                            'Lecture Quality': [lecture_quality]})

    # Append the new row to the course_ratings data frame
    global course_ratings
    course_ratings = pd.concat([course_ratings, new_row], ignore_index=True)

# :-------------------------------
# Rating the courses


# Rate on a scale from 1-5
rate_course('BAN400',
            learning_outcome=4,
            coursework_amount=3,
            difficulty=3,
            hours_spent=2,
            lecture_quality=1)

rate_course('BAN402',
            learning_outcome=3,
            coursework_amount=3,
            difficulty=4,
            hours_spent=3,
            lecture_quality=3)


# :-------------------------------
# Converting to markdown formatting

# Span function
def span_func(rating):
    if (rating == 1):
        amount = "Low"
        color_code = "#5ae342"
    elif (rating == 2):
        amount = "Medium"
        color_code = "#1d9908"
    elif (rating == 3):
        amount = "High"
        color_code = "#e87910"
    elif (rating == 4):
        amount = "Very High"
        color_code = "#bf0000"
    else:
        amount = "Invalid"
        color_code = "#8c8c8c"

    string = f'<span style = "color:{color_code}">{amount}</span>'
    return (string)


# Apply the function to each column
for col in course_ratings.columns[1:]:
    course_ratings[col] = course_ratings[col].apply(span_func)

# Convert DataFrame to markdown
markdown_table = course_ratings.to_markdown(index=False)

# The name of the file
filename = 'GUIDES/Choosing Master Courses/output/course_ratings.txt'

# Open the file in write mode
with open(filename, 'w', encoding="utf-8") as file:
    # Write the markdown table to the file
    file.write(markdown_table)
