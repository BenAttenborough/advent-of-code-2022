# advent-of-code-2022

See the code challenges here: https://adventofcode.com/2022

You can import files and run functions `elm repl`
E.g. `import Advent1`

## Developing in a browser

### TLDR;

Configure a html file, link it to js file (maybe not generated yet)
Configure `elm.watch.json` to point to js file
Run:

```bash
npx elm-watch hot
```

### Explanation

I've added a package called elm-watch, it uses an `elm-watch.json` file to compile the elm into js AND reload any html file which links to the js.

A minimum setup might be:

```html
<!DOCTYPE html>
<html>
  <head>
    <link
      rel="stylesheet"
      href="https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/css/bootstrap.min.css"
    />
    <link rel="stylesheet" href="base.css" />
  </head>

  <body>
    <div id="elm-app-is-loaded-here"></div>

    <script src="advent8.js"></script>
    <script>
      var app = Elm.Advent8.init({
        node: document.getElementById("elm-app-is-loaded-here"),
      });
    </script>
  </body>
</html>
```

elm-watch.json

```json
{
  "targets": {
    "My target name": {
      "inputs": ["src/Advent8.elm"],
      "output": "html/advent8.js"
    }
  }
}
```

Then run

```bash
npx elm-watch hot
```

## Tests

To run tests run `elm-test` or `elm-test --watch`
Or `npx elm-test` or `npx elm-test --watch`

## Python

https://docs.python.org/3/library/venv.html

`python3 -m venv /path/to/new/virtual/environment`
`source bin/activate`

pip works a bit different from npm

`source bin/activate` if python3 doesn't work
`python3 -m pip install <package>`

To create the equivilent of a package.json use:
`python3 -m pip freeze > requirements.txt`

To install requirements
`python3 -m pip install -r requirements.txt`

## Auto generating problem files

`python3 createProblem.py <ProblemName>`
Note: No extension is required but you should cap up the problem name

## Switch rendered file

`python3 switchProblem.py <ProblemName>`
