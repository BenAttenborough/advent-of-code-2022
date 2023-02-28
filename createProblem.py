from jinja2 import Environment, FileSystemLoader
import sys

environment = Environment(loader=FileSystemLoader("templates/"))
moduleName = sys.argv[1]
template = environment.get_template("Problem.elm")

with open("src/" + moduleName + ".elm", mode="w", encoding="utf-8") as message:
    message.write(template.render(name=moduleName))

testTemplate = environment.get_template("Test.elm")

with open("tests/" + moduleName + "Test.elm", mode="w", encoding="utf-8") as message:
    message.write(testTemplate.render(name=moduleName))

dataTemplate = environment.get_template("Data.elm")

with open("src/" + moduleName + "Data.elm", mode="w", encoding="utf-8") as message:
    message.write(dataTemplate.render(name=moduleName))
