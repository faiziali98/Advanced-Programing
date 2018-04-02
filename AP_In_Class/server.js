var net = require('net');
var fs = require('fs');
var x = 0;

console.log ('1. create server socket');
var server = net.createServer(function(socket) {
	console.log('2. client connected to server');
	socket.on('data',function(data){
		x++;
		fs.writeFile('copy'+x+'.js',data,function(err,data){
			server.end(data)
		});
	});
	server.close();
	console.log('2a. end of client connected event handler');
}).listen(8000);
console.log('1a. listen for clients');
console.log('1b. create and connect client socket');
for (var i = 0; i<10; i++){
	var client = new net.Socket();
	client.connect(8000,'localhost',function(){
		fs.readFile('server.js',function(err,data){
			client.end(data)
		});
	});
}
