from jinja2 import Environment, FileSystemLoader
import sys

environment = Environment(loader=FileSystemLoader("templates/"))
moduleName = sys.argv[1]

htmlTemplate = environment.get_template("advent.html")

with open("html/advent.html", mode="w", encoding="utf-8") as message:
    message.write(htmlTemplate.render(name=moduleName))
