from jinja2 import Environment, FileSystemLoader
import sys
from os import listdir
from os.path import isfile, join

environment = Environment(loader=FileSystemLoader("templates/"))
moduleName = sys.argv[1]
template = environment.get_template("Problem.elm")

# with open("src/" + moduleName + ".elm", mode="w", encoding="utf-8") as message:
#     message.write(template.render(name=moduleName))

# testTemplate = environment.get_template("Test.elm")

# with open("tests/" + moduleName + "Test.elm", mode="w", encoding="utf-8") as message:
#     message.write(testTemplate.render(name=moduleName))

# dataTemplate = environment.get_template("Data.elm")

# with open("src/" + moduleName + "Data.elm", mode="w", encoding="utf-8") as message:
#     message.write(dataTemplate.render(name=moduleName))

htmlTemplate = environment.get_template("advent.html")

with open("html/" + moduleName + ".html", mode="w", encoding="utf-8") as message:
    message.write(htmlTemplate.render(name=moduleName))

onlyFiles = [f for f in listdir("src/") if isfile(join("src/", f))]
files = ""
for file in onlyFiles:
    files += "\"src/" + file + "\","
files = files.rstrip(",")

elmWatchTemplate = environment.get_template("elm-watch.json")

with open("elm-watch.json", mode="w", encoding="utf-8") as message:
    message.write(elmWatchTemplate.render(files=files))
