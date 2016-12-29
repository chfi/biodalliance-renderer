exports.initialize = function(tier) {
    return function() {
        // set padding (3 is default used by BD)
        tier.padding = 3;
        tier.scaleVertical = false; // set in default-renderer:427, used by feature-draw:paintToContext

        tier.glyphCacheOrigin = 0;

        // create subtiers
        tier.subtiers = [];
        tier.subtiers[0] = { glyphs: [] };
    };
};

exports.canvasContext = function(tier) {
    return function() {
        return tier.viewport.getContext("2d");
    };
};

exports.setHeight = function(tier) {
    return function(height) {
        return function () {
            tier.viewport.height = height;
            tier.layoutHeight = height;
            tier.updateHeight();

            tier.norigin = tier.browser.viewStart;

            tier.originHaxx = 0;
            tier.browser.arrangeTiers();
        };
    };
};

exports.features = function(tier) {
    return function() {
        return tier.currentFeatures;
    };
};

exports.scaleFactor = function(tier) {
    return function(scaleY) {
        return function() {
            var sf = { bpPerPixel: 1/tier.browser.scale,
                       viewStart: tier.browser.viewStart,
                       canvasHeight: tier.viewport.height,
                       scaleY: scaleY(tier.viewport.height)
                     };
            return sf;
        };
    };
};

exports.render = function(f) {
    f();
};

exports.setQuant = function(tier) {
    return function(quant) {
        return function() {
            if (tier.subtiers[0])
                tier.subtiers[0].quant = quant;
            else
                tier.subtiers = [{quant: quant}];
        };
    };
};

exports.setGlyphs = function(tier) {
    return function(glyphs) {
        return function() {
            glyphs.forEach(function(g) {
                console.log(g.glyphPos.min());
                console.log(g.glyphPos.max());
                tier.subtiers[0].glyphs.push(g.glyphPos);
            });
            console.log(tier.subtiers[0]);
        };
    };
};



exports.setGlyphs2 = function(tier) {
    return function(glyphs) {
        return function() {
            glyphs.forEach(function(g) {
                console.log(g);
                var glyph = g.glyph.glyphPos;
                glyph.feature = g.feature;
                tier.subtiers[0].glyphs.push(glyph);
            });
            console.log(tier.subtiers[0]);
        };
    };
};
