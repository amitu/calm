#!/usr/bin/env node
var fs = require('fs')
var temp = require('temp').track()
var compiler = require('node-elm-compiler')

if (!fs.existsSync('./elm-package.json')) {
    fail(
        'Error: This command needs to be executed from the root of the ' +
        'elm project.'
    )
}

if (
    process.argv.length !== 3
    || process.argv[2].substr(process.argv[2].length - 4) !== ".elm"
) {
    fail('Usage: calm <Elm-File>.elm')
}

var source = process.argv[2];

if (!fs.existsSync(source)) {
    fail(
        'Error: ' + source + ' does not exist.'
    )
}


var target = temp.path({ suffix: '.js' });
compiler.compileSync([source], {
  yes: true,
  output: target,
  processOpts: { stdio: 'pipe' }
});

var Elm = require(target);
var app = Elm.Main.worker();

app.ports.emit.subscribe(function (json) {
  console.log(json)
});

function fail (msg) {
  process.stderr.write(msg + "\n")
  process.exit(1)
}
