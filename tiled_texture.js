(function() {
  var TiledTexture = window.TiledTexture = {};

  TiledTexture.init = function(app) {
    var images = TiledTexture.images = {};

    app.ports.loadImage.subscribe(function(url) {
      var image = new Image();
      images[url] = image;

      image.addEventListener("load", function(e) {
        var urls = [];
        var x = 0;
        var y = 0;

        while (y < image.height) {
          while (x < image.width) {
            urls.push(_renderImageAt(image, x, y));

            x += 2048;
          }

          y += 2048;
        }
        app.ports.imageLoaded.send({
          width: image.width,
          height: image.height,
          urls: urls
        });
      });

      image.src = url;
    });
  };

  var _renderImageAt = function (image, x, y) {
    var canvas = document.createElement('canvas');
    canvas.width = 2048;
    canvas.height = 2048;
    var ctx = canvas.getContext('2d');

    ctx.drawImage(image, x, y, 2048, 2048, 0, 0, 2048, 2048);
    return canvas.toDataURL('image/png');
  };
})();
