var http = require('http');

var todos = [
    {text: "get milk"},
    {text: "put milk in fridge"},
    {text: "forget about milk"},
    {text: "throw milk in garbage"}
];

function success(res) {
    res.writeHead(200, {'Content-Type': "application/json"});
    res.write(JSON.stringify({success: true, result: todos}));
}

http.createServer(function (req, res) {
    if (req.url === "/list") {
        success(res);
    }
    if (req.url === "/add") {
        todos.push("new todo created")
        success(res);
    }
}).listen(8001);
