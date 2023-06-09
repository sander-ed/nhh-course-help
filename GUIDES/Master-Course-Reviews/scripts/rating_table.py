import pandas as pd

courses = ["BAN400", "BAN402", "BUS401E", "BAN432",
           "BAN403", "BAN404", "STR459", "BAN438"]

course_ratings = pd.DataFrame(columns=['Course', 'Learning Outcome',
                              'Coursework Amount', 'Difficulty', 'Required Code Knowledge', 'Lecture Quality', 'Group Exam', 'Review Score'])


def rate_course(course, learning_outcome, coursework_amount, difficulty, code_skill, lecture_quality, group_exam, review):
    # Create a new row with the course information and rating
    new_row = pd.DataFrame({'Course': [course],
                            'Learning Outcome': [learning_outcome],
                            'Coursework Amount': [coursework_amount],
                            'Difficulty': [difficulty],
                            'Required Code Knowledge': [code_skill],
                            'Lecture Quality': [lecture_quality],
                            'Group Exam': [group_exam],
                            'Review Score': [review]})

    # Append the new row to the course_ratings data frame
    global course_ratings
    course_ratings = pd.concat([course_ratings, new_row], ignore_index=True)

# :-------------------------------
# Rating the courses


# Rate on a scale from 1-5
rate_course('BAN400',
            learning_outcome=4,
            coursework_amount=3,
            difficulty=2,
            code_skill=1,
            lecture_quality=4,
            group_exam = "No",
            review=9)

rate_course('BAN402',
            learning_outcome=3,
            coursework_amount=4,
            difficulty=4,
            code_skill=2,
            lecture_quality=2,
            group_exam = "Yes",
            review=7)

rate_course('BAN432',
            learning_outcome=3,
            coursework_amount=4,
            difficulty=2,
            code_skill=2,
            lecture_quality=2,
            group_exam = "Yes",
            review=7)

rate_course('BUS401E',
            learning_outcome=3,
            coursework_amount=1,
            difficulty=3,
            code_skill=1,
            lecture_quality=4,
            group_exam = "No",
            review=8)

rate_course('BAN404',
            learning_outcome=3,
            coursework_amount=1,
            difficulty=3,
            code_skill=3,
            lecture_quality=1,
            group_exam = "No",
            review=4)

rate_course('BAN438',
            learning_outcome=2,
            coursework_amount=2,
            difficulty=3,
            code_skill=2,
            lecture_quality=3,
            group_exam = "No",
            review=6)

rate_course('STR459',
            learning_outcome=2,
            coursework_amount=3,
            difficulty=2,
            code_skill=2,
            lecture_quality=3,
            group_exam = "Yes",
            review=7)


# :-------------------------------
# Converting to markdown formatting

# Span function
def span_func(rating):
    if (rating == 1):
        amount = "LOW"
        color_code = "#5ae342"
    elif (rating == 2):
        amount = "MEDIUM"
        color_code = "#1d9908"
    elif (rating == 3):
        amount = "HIGH"
        color_code = "#e87910"
    elif (rating == 4):
        amount = "VERY HIGH"
        color_code = "#e01010"
    else:
        amount = "invalid"
        color_code = "#8c8c8c"

    s = f'$${{\\color{{{color_code}}}{amount}}}$$'
    return (s)


def review_rating(review_score):
    s = f"{review_score}/10"

    formatted_s = f'$${{\\textbf{{{s}}}}}$$'

    return (formatted_s)

def group_exam_func(group_exam):
    s = f"{group_exam}"
    formatted_s = f'$${{\\textbf{{{s}}}}}$$'
    return (formatted_s)


# Apply the function to each column
for col in course_ratings.columns[1:6]:
    course_ratings[col] = course_ratings[col].apply(span_func)

course_ratings["Review Score"] = course_ratings["Review Score"].apply(
    review_rating)

course_ratings["Group Exam"] = course_ratings["Group Exam"].apply(
    group_exam_func)

# Convert DataFrame to markdown
markdown_table = course_ratings.to_markdown(index=False)

# The name of the file
filename = 'GUIDES/Master-Course-Reviews/output/course_ratings.txt'

# Open the file in write mode
with open(filename, 'w', encoding="utf-8") as file:
    # Write the markdown table to the file
    file.write(markdown_table)
