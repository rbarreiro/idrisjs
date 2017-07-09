(function(){

function initChildsAndAttributes(root, x){
  for(let i in x.attrs){
    switch (x.attrs[i].type) {
      case 'r':
        x.domNode.setAttribute(x.attrs[i].name, x.attrs[i].value);
        break;
      case 'rn':
        x.domNode.setAttributeNS(x.attrs[i].ns, x.attrs[i].n, x.attrs[i].value);
        break;
      case 'e':
        x.domNode.addEventListener(x.attrs[i].name, function(e){$JSLIB$async.putInQueue(x.attrs[i].read(e), root.queue)});
        break;
      case 'ec':
        x.attrs[i].init({domNode: x.domNode, process: function(e){ $JSLIB$async.putInQueue(x.attrs[i].read(e), root.queue) }})
        break;
      case 'ep':
        x.domNode.addEventListener(x.attrs[i].name, function(e){e.preventDefault();$JSLIB$async.putInQueue(x.attrs[i].read(e), root.queue)});
        break;
      case 'p':
        x.domNode[x.attrs[i].name] = x.attrs[i].value
      case 's':
        for(var j in x.attrs[i].value){
          x.domNode.style[j] = x.attrs[i].value[j]
        }
    }
  }
  for(var i=0; i < x.childs.length; ++i){
    x.childs[i].parent = x.domNode;
    initialyze(root, x.childs[i]);
  }
}

function initialyze(root, x){
  switch (x.type) {
    case 'n':
      var node = document.createElement(x.tag);
      x.domNode = node;
      x.parent.appendChild(node);
      initChildsAndAttributes(root, x);
      break;
    case 'nn':
      var node = document.createElementNS(x.ns,x.tag);
      x.domNode = node;
      x.parent.appendChild(node);
      initChildsAndAttributes(root, x);
      break;
    case 't':
      var t = document.createTextNode(x.text);
      x.parent.appendChild(t);
      x.domNode = t;
      break;
  }
}

function replaceNode(root, x, y){
  switch (y.type) {
    case 'n':
      var node = document.createElement(y.tag);
      x.parent.replaceChild(node, x.domNode);
      y.parent = x.parent;
      y.domNode = node;
      initChildsAndAttributes(root, y);
      break;
    case 'nn':
      var node = document.createElementNS(y.ns , y.tag);
      x.parent.replaceChild(node, x.domNode);
      y.parent = x.parent;
      y.domNode = node;
      initChildsAndAttributes(root, y);
      break;
    case 't':
      var node = document.createTextNode(y.text);
      x.parent.replaceChild(node, x.domNode);
      y.parent = x.parent;
      y.domNode = node;
      break;
  }
  for (var member in x) delete x[member];
  for (var member in y) x[member] = y[member];
}

function attributesListToObj(x){
  var res = {};
  for(var i = x.length - 1 ; i >= 0 ; --i){
    res[x[i].name] = x[i];
  }
  return res;
}

function updateStyle(node, xstl, ystl){
  var i;
  for(i in ystl){
    if(xstl[i] !== ystl[i]){
      node.style[i] = ystl[i];
    }
  }
  for(i in xstl){
    if(ystl[i] === undefined){
      node.style[i] = "";
    }
  }
}

function stylesEq(xstl,ystl){
  var i;
  for(i in ystl){
    if(xstl[i] !== ystl[i]){
      return false;
    }
  }
  for(i in xstl){
    if(ystl[i] !== undefined){
      return false;
    }
  }
  return true;
}

function calcStyleFrames(xstl,ystl){
  var r1 = {};
  var r2 = {};
  var i;
  for(i in ystl){
    if(xstl[i] !== ystl[i]){
      r1[i] = xstl[i];
      r2[i] = ystl[i];
    }
  }
  for(i in xstl){
    if(ystl[i] !== undefined){
      r2[i] = ystl[i];
    }
  }
  return [r1,r2];
}

function updateAttributes(animations, x, y){
  for(var i in y.attrs){
    if(x.attrs[i].value !== y.attrs[i].value){
      switch (x.attrs[i].type) {
        case 'r':
          x.domNode.setAttribute(y.attrs[i].name, y.attrs[i].value);
          break;
        case 'rn':
          x.domNode.setAttributeNS(y.attrs[i].ns, y.attrs[i].n, y.attrs[i].value);
          break;
        case 'p':
          x.domNode[y.attrs[i].name] = y.attrs[i].value
          break;
        case 's':
          if(!stylesEq(x.attrs[i].value, y.attrs[i].value)){
            if(animations !== undefined){
              x.domNode.animate(calcStyleFrames(x.attrs[i].value, y.attrs[i].value), animations);
            }else{
              updateStyle(x.domNode, x.attrs[i].value, y.attrs[i].value);
            }
          }
          break;
      }
    }
  }
  for(var i in x.attrs){
    if(y.attrs[i] == undefined && x.attr[i].type[0] != 'e'){
      x.domNode.removeAttribute(i);
    }
  }
  x.attrs = y.attrs;
}

function updateChilds(animations, root, x, y){
  var i=0;
  for(; i<Math.min(x.childs.length, y.childs.length); ++i){
    update_(animations, root, x.childs[i], y.childs[i]);
  }
  for(; i<y.childs.length; ++i){
    y.childs[i].parent = x.domNode;
    initialyze(root, y.childs[i]);
    x.childs.push(y.childs[i]);
  }
  for(; i<x.childs.length; ++i){
    x.domNode.removeChild(x.childs[i].domNode);
  }
  x.childs.splice(y.childs.length);
}

function update_(animations, root, x, y){
  if(x.type === 'n' && y.type === 'n' && x.tag === y.tag){
    updateAttributes(animations, x, y);
    updateChilds(animations, root, x, y);
  }else if (x.type === 'nn' && y.type === 'nn' && x.ns === y.ns && x.tag === y.tag){
    updateAttributes(animations, x, y);
    updateChilds(animations, root, x, y);
  }else if (x.type === 't' && y.type === 't' && y.text === x.text ){
    ;
  }else{
    replaceNode(root, x, y);
  }
}

function update(x,y){
  update_(undefined, x, x, y)
}

function animate(animations, x, y){
  update_(animations, x, x, y)
}

function updateQueue(queue, x){
  x.queue = queue;
}

function initialyzeBody(queue,x){
  x.parent = document.body;
  x.queue = queue;
  initialyze(x, x)
}

function clear(x){
  x.parent.removeChildNode(x.domNode);
}

function mkStyle(x,y){
  var res = {};
  res[x] = y
  return res;
}

function availableWidth(){
  var stl = window.getComputedStyle(document.body);
  return window.innerWidth - parseInt(stl.marginLeft) - parseInt(stl.marginRight) - parseInt(stl.paddingLeft) - parseInt(stl.paddingRight)
}

function availableHeight(){
  var stl = window.getComputedStyle(document.body);
  return window.innerHeight - parseInt(stl.marginTop) - parseInt(stl.marginBottom) - parseInt(stl.paddingTop) - parseInt(stl.paddingBottom)
}

this.$JSLIB$virtualdom = {
    initialyzeBody: initialyzeBody
  , update: update
  , animate : animate
  , updateQueue: updateQueue
  , clear: clear
  , attributesListToObj: attributesListToObj
  , mkStyle: mkStyle
  , availableWidth: availableWidth
  , availableHeight: availableHeight
};

}).call(this);
