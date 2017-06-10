#!/usr/bin/env node
const fs = require('fs')
const temp = require('temp').track()
const compiler = require('node-elm-compiler')
const http = require('http');
const url = require('url');

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

global.jsrefs = (function() {
    var objects = {};
    var counter = -1;

    var insert = function (obj) {
        counter += 1;
        var scounter = counter.toString();
        objects[scounter] = obj;
        return scounter;
    };

    var extract = function (idx) {
        // idx assumed string
        var obj = objects[idx];
        delete objects[idx];
        return obj;
    };

    return {
        insert: insert,
        extract: extract
    }
})();


var target = temp.path({ suffix: '.js' });
compiler.compileSync([source], {
  yes: true,
  output: target,
  processOpts: { stdio: 'pipe' }
});

var Elm = require(target);
var app = Elm.Main.worker();

function fail (msg) {
  process.stderr.write(msg + "\n")
  process.exit(1)
}

reqs = {};
idx = 0;

function attributes(facts, write) {
    for (var key in facts) {
        if (facts.hasOwnProperty(key)) {
            var value = facts[key];
            switch (key) {
                case "className":
                    write(" class=\""); write(value); write("\"");
                    break;
                case "STYLE":
                    write(" style=\"");
                    for (var k in value) {
                        if (facts.hasOwnProperty(key)) {
                            var v = value[k];
                            write(k); write(": "); write(v); write(";")
                        }
                    }
                    write("\"");
                    break;
                default:
                    write(" "); write(key); write("="); write(value);
            }
        }
    }
}

function htmlify(tree, write) {
    switch (tree["type"]) {
    case "node":
        write("<"); write(tree["tag"]); attributes(tree.facts, write); write(">");
        var len = tree.children.length;
        for (var i = 0; i < len; i++) {
            htmlify(tree.children[i], write);
        }
        write("</"); write(tree["tag"]); write(">");
        break;
    case "text":
        write(tree["text"]);
        break;
    case "tagger":
        htmlify(tree["node"], write);
        break;
    default:
        console.log("invalid type: " + tree["type"]);
        write(JSON.stringify(tree));
    }
}

app.ports.responses.subscribe(function(obj){
    console.log("fromElm: ", obj);
    var res = reqs[obj.id];
    if (!res) {
        console.log("Invalid ID", obj.id);
        return;
    }
    res = res.res;
    res.writeHead(obj.code, {'Content-Type': obj.mime});
    htmlify(jsrefs.extract(obj.body), function(m){res.write(m)});
    res.end();
    delete reqs[obj.id];
});


http.createServer(function (req, res) {
    idx += 1;
    var id = idx.toString();
    reqs[id] = {req: req, res:res};
    var myUrl = new url.URL("http://127.0.0.1:8000" + req.url);
    const GET = {}
    myUrl.searchParams.forEach(function(value, name) {
        if (GET[name]) {
            GET[name].push(value);
        } else {
            GET[name] = [value];
        }
    });
    var j = {
        id: id,
        location: {
            // Is a DOMString containing the entire URL.
            href: req.url,
            // Is a DOMString containing the protocol scheme of the URL,
            // including the final ':'.
            protocol: myUrl.protocol,
            // Is a DOMString containing the host, that is the hostname, a
            // ':', and the port of the URL.
            host: myUrl.host,
            // Is a DOMString containing the domain of the URL.
            hostname: myUrl.hostname,
            // Is a DOMString containing the port number of the URL.
            port: myUrl.port,
            // Is a DOMString containing an initial '/' followed by the
            // path of the URL.
            pathname: myUrl.pathname,
            // Is a DOMString containing a '?' followed by the parameters
            // or "querystring" of the URL.
            search: myUrl.search,
            // Is a DOMString containing a '#' followed by the fragment
            // identifier of the URL.
            hash: myUrl.hash,
            // Is a DOMString containing the username specified before the
            // domain name.
            username: myUrl.username,
            // Is a DOMString containing the password specified before the
            // domain name.
            password: myUrl.password,
            // Returns a DOMString containing the canonical form of the
            // origin of the specific location.
            origin: myUrl.origin
        },
        get: GET,
        method: req.method,
        headers: req.headers,
        cookies: parseCookies(req)
    };
    console.log("toElm: ", j);
    app.ports.requests.send(j);
}).listen(8000);


function parseCookies (request) {
    var list = {},
        rc = request.headers.cookie;

    rc && rc.split(';').forEach(function( cookie ) {
        var parts = cookie.split('=');
        list[parts.shift().trim()] = decodeURI(parts.join('='));
    });

    return list;
}
