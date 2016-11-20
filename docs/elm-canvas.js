function init(input, output) {
  output.subscribe(function(e) {
    // console.log(e);
    var canvas = document.getElementById(e.canvasId);
    if(canvas) {
      var ctx = canvas.getContext('2d');
      e.elements.forEach(function(el) {
        if(el.type === 'element') {
          ctx.fillStyle = el.backgroundColor;
          ctx.fillRect(el.position.x, el.position.y, el.size.width, el.size.height);
        } else if (el.type === 'text') {
          ctx.font = "14px calibri";
          ctx.textBaseline = 'top';
          ctx.fillStyle = el.color;
          ctx.fillText(el.content, el.position.x, el.position.y);
        }
      });
    }
  });
}
