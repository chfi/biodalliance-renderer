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

exports.addGlyphSVG = function(g) {
    console.log("Adding SVG to div #svgDiv");
    var e = g.toSVG();
    var d = document.getElementById("svgDiv");
    d.appendChild(e);
};


exports.callDraw = function(g) {
    return function() {
        console.log(g);
        console.log(g.draw);
        g.draw();
    };
};
