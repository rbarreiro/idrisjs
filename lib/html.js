(function(){

function addLongPressEventListener(x){
  var state = 0;
  x.domNode.addEventListener("pointerdown", function(e){ var downst = ++state; setTimeout(function(){ if(state == downst) { x.process(e)} }, 600) });
  x.domNode.addEventListener("pointerup", function(e){++state});
}

function addShortPressEventListener(x){
  var lastdown;
  x.domNode.addEventListener("pointerdown", function(e){lastdown = new Date()});
  x.domNode.addEventListener("pointerup", function(e){if(new Date()-lastdown < 500){ x.process(e)} });
}

this.$JSLIB$html = {
  addLongPressEventListener: addLongPressEventListener,
  addShortPressEventListener : addShortPressEventListener
};

}).call(this);
