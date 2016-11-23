function init(input, output) {
  output.subscribe(function(e) {
    // console.log(e);
    var canvas = document.getElementById(e.canvasId);
    if(canvas) {
      canvas.onmousedown = function(e) {
        input.send({ type_: 'mousedown', data: e });
      }
      canvas.onmouseup = function(e) {
        input.send({ type_: 'mouseup', data: e });
      }
      canvas.onclick = function(e) {
        input.send({ type_: 'click', data: e });
      }
      canvas.ondblclick = function(e) {
        input.send({ type_: 'dblclick', data: e });
      }
      var ctx = canvas.getContext('2d');
      ctx.clearRect(0, 0, canvas.width, canvas.height);
      e.elements.forEach(function(el) {
        if(el.type === 'element') {
          if(el.border) {
            ctx.strokeStyle = el.border.color;
            ctx.lineWidth = el.border.width;
            ctx.strokeRect(el.position.x, el.position.y, el.size.width, el.size.height);
          }
          if(el.shadow) {
            ctx.shadowBlur = el.shadow.blur;
            ctx.shadowColor = el.shadow.color;
            ctx.shadowOffsetX = el.shadow.offsetX;
            ctx.shadowOffsetY = el.shadow.offsetY;
          } else {
            ctx.shadowBlur = 0;
            ctx.shadowOffsetX = 0;
            ctx.shadowOffsetY = 0;
          }
          ctx.fillStyle = el.backgroundColor;
          ctx.fillRect(el.position.x, el.position.y, el.size.width, el.size.height);
        } else if (el.type === 'text') {
          ctx.font = el.fontWeight + ' ' + el.fontSize + 'px ' + el.fontFamily;
          ctx.textBaseline = 'top';
          ctx.fillStyle = el.color;
          ctx.fillText(el.content, el.position.x, el.position.y);
        }
      });
    }
  });
}
