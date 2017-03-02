exports.testGlyphPos = function(g) {
    console.log(g.min());
    console.log(g.max());
    console.log(g.minY());
    console.log(g.maxY());
};


exports.showGlyphSVG = function(g) {
    console.log("Printing SVG");
    var e = g.toSVG();
    console.log(e);
};

exports.addElementToDiv = function(divId) {
    return function(e) {
        return function () {
            console.log("Adding SVG to div #" + divId);
            var d = document.getElementById(divId);

            console.log(e);
            d.appendChild(e);
        };
    };
};


exports.callDraw = function(g) {
    return function() {
        console.log(g);
        console.log(g.draw);
        g.draw();
    };
};


exports.setOnLoad = function(f) {
    return function() {
        window.onload = f;
    };
};
