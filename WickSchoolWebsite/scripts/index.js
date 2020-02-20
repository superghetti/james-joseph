var map = document.getElementsByClassName("maps");
var buttons = document.getElementsByClassName("getdir");
for(var i = 0; i < map.length; i++){
  map[i].style.display = "none";
}
map[0].style.display = "block";

buttons[0].onclick = function(){
  for(var i = 0; i < map.length; i++){
    map[i].style.display = "none";
  }
  map[0].style.display = "block";
};
buttons[1].onclick = function(){
  for(var i = 0; i < map.length; i++){
    map[i].style.display = "none";
  }
  map[1].style.display = "block";
};
buttons[2].onclick = function(){
  for(var i = 0; i < map.length; i++){
    map[i].style.display = "none";
  }
  map[2].style.display = "block";
};
buttons[3].onclick = function(){
  for(var i = 0; i < map.length; i++){
    map[i].style.display = "none";
  }
  map[3].style.display = "block";
};
buttons[4].onclick = function(){
  for(var i = 0; i < map.length; i++){
    map[i].style.display = "none";
  }
  map[4].style.display = "block";
};
