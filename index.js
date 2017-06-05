#!/usr/bin/env node
var fs = require('fs')
var temp = require('temp').track()
var compiler = require('node-elm-compiler')
var http = require('http');

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
var app = Elm.Acko.worker();

function fail (msg) {
  process.stderr.write(msg + "\n")
  process.exit(1)
}

reqs = {};
idx = 0;


app.ports.responses.subscribe(function(obj){
    res = reqs[obj.id];
    if (!res) {
        console.log("Invalid ID", obj.id);
        return
    }
    res = res.res;
    res.writeHead(200, {'Content-Type': 'text/plain'});
    res.write(obj.body);
    res.end();
});


http.createServer(function (req, res) {
    idx += 1;
    id = idx.toString();
    reqs[id] = {req: req, res:res};
    j = {
        id: id,
        method: req.method,
        path: req.url,
    };
    console.log(j, req);
    app.ports.requests.send(j);
}).listen(8000);
