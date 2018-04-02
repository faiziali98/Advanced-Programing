require("http").createServer((req,res)=>{
	x++;
	res.end('Hello'+x);
}).listen(8000);
