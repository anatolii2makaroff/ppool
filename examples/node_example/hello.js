//
// Nodejs example actor
// 

var readline = require('readline');

var rl = readline.createInterface({
  input: process.stdin,
  output: process.stdout,
  terminal: false

});

//API

function log(m){
    console.error(Date.now() + ": " + m);
}

function send(m){
   console.log(m);
}



//Process - actor
// line - recieve message from world
// send - send message to world
// log  -  logging anything


rl.on('line', function(line){

    line = line.replace(/\n$/, '')

    log("start working..");
    log("get message: " + line);

    var resp = line

    send(resp)

    log("message send: "+resp);


 })
