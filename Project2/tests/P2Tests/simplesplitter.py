## File to take the html of tests and split them into individual .txt files
import os

filename = "part2tests.html"

# Take all lines marked <pre> and split them into individual files
with open(filename, 'r') as file:
    data = file.read()
    data = data.split('<pre>')
    for i in range(1, len(data)):
        data[i] = data[i].split('</pre>')[0]
        # get rid of leading and trailing whitespace
        data[i] = data[i].strip()
        # make all numbers two digits
        filenum = str(i)
        filenum = filenum.zfill(2)
        with open(f'test{filenum}.txt', 'w') as file:
            file.write(data[i])
