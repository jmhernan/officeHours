list_1 = [1,3,4,5,6,9,2]
list_2 = [2,7,8,9]

joined_list = list_1 + list_2 

joined_list

unique_list = list(set(joined_list))

# Finding bigrams 
sentence = """
Have free hours and love children? 
Drive kids to school, soccer practice 
and other activities.
"""
# example:
print(tokenized_text[0],tokenized_text[1])

# create a list of tokens 
tokenized_text = sentence.split()
for i in range(len(tokenized_text)):
    if i < len(tokenized_text)-1:
        # take each pair of tokens by moving a window of two
        print((tokenized_text[i], tokenized_text[i+1]))


# Finding first non-repeating character in a string
string = 'aabbdccfe'

def first_non_repeat(string_input):
    char_track = {}
    for i in string_input:
        if i in char_track:
            char_track[i] += 1
        else:
            char_track[i] = 1
    for c in string_input:
        if char_track[c] == 1:
            return c



first_non_repeat(string)
first_non_repeat('tutorialspointfordeveloper')
first_non_repeat('AABBCC')

pal = 'mad'
pal_2 = 'mom'

def isPal(string_input):
    if string_input == string_input[::-1]:
        return True
    else:
        return False

isPal(pal_2)

#######################
