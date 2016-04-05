function changeClass(ID){
  var dom = document.getElementById(ID);
  if(dom.className == "hidden"){
    dom.className = "visible";
  }else{
    dom.className = "hidden";
  }
}