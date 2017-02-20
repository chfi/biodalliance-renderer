exports.testGlyphPos = function(g) {
    console.log(g.min());
    console.log(g.max());
    console.log(g.minY());
    console.log(g.maxY());
};


exports.callDraw = function(g) {
    return function() {
        console.log(g);
        console.log(g.draw);
        g.draw();
    };
};
