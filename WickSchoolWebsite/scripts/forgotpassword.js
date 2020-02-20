var zipcode = document.getElementById("zip");
var button = document.getElementById("recover");

zipcode.onkeyup = function(){
  var zipre = /\d{6}/;
  if(!zipcode.value.match(zipre)){
    zipcode.style.color = "red";
  }
}

button.onclick = function(){
  var zipre = /\d{6}/;
  if(!zipcode.value.match(zipre)){
    alert("Your zip code is of the improper format.");
    console.log("check")
  }else{
    console.log("uncheck");
  }
}
