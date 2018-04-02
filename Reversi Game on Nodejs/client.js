//Creating the game

var num_btn = 0;
for (var k = 0; k<8;k++){
  for (var k1= 0; k1<8; k1++){
    var div = document.createElement("div");
    var btn = document.createElement("BUTTON");
    btn.id = ''+num_btn;
    div.id = "but";
    if (k==7) div.id = "but2";
    if (k1==7) div.id = "but1";
    if (k1==7 && k==7) div.id ="but3";
    btn.setAttribute('onclick','move('+num_btn+')');
    div.appendChild(btn);
    document.getElementById('row0').appendChild(div);
    num_btn++;
  }
}

//Starting the game

var sock = io();
sock.on('msg',onMessage);
var hash = null;
var start = 0, can_move = 0, face, orignal;
var gid;

sock.on('move',(data)=>{
  can_move = 1;
  onMessage(data);
});

sock.on('over',(data) =>{
  start = 0;
  can_move = 0;
  orignal = data;
  onMessage(data);
});

sock.on('invalid',(data)=>{
  can_move = 1;
  onMessage(data);
  var delay = setTimeout(()=>{
    onMessage("Your Move");
  },1000);
});

sock.on('reconnecting',()=>{
  onMessage('Connection Lost. Trying to reconnect.');
});

sock.on('connect',()=>{
  sock.emit('new',hash);
});

sock.on('reconnect',()=>{
  onMessage('Reconnected Successfully');
  sock.emit('myId',hash);
});

sock.on('gid',(data) => {
  gid = data;
});

sock.on('board',(data)=>{
  start = 1;
  for (var k = 0; k<64; k++){
    if (data[k]===1){
      document.getElementById(k).style.background='white';
    }else if (data[k]===-1) {
      document.getElementById(k).style.background='black';
    }else{
      document.getElementById(k).style.background='green';
    }
  }
});
sock.on('face',(data)=>{
  face = data;
  hash = {cid:gid, clf:face};
});

function onMessage(text){
  var container=document.getElementById('tag');
  orignal = container.innerHTML;
  container.innerHTML = text;
}

function move(n){
  if (can_move!=0){
    can_move = 0;
    onMessage("Oponent's move");
    sock.emit('turn', n);
  }else if(can_move===0 && start===1){
    onMessage('You had your turn now wait!!');
    var delay = setTimeout(()=>{
      onMessage(orignal);
    },1000);
  }
}
