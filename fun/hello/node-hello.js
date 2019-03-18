// Copyright (c) 2016-2019 Doug Rogers under the Zero Clause BSD License.
// You are free to do whatever you want with this software. See LICENSE.txt.

// Connect with browser to http://localhost:3000 and you should get a hello.
const http = require('http');

const hostname = '127.0.0.1';
const port = 3000;

const server = http.createServer((req, res) => {
//  console.log(`request ${req.rawHeaders}`);
  console.log(`request ${req.url}`);
  res.statusCode = 200;
  res.setHeader('Content-Type', 'text/plain');
  res.end('hello\n');
});

server.listen(port, hostname, () => {
  console.log(`Server running at http://${hostname}:${port}/`);
});
