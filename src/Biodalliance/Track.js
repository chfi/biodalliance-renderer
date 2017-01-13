exports.initialize = function(tier) {
    return function() {
        // set padding (3 is default used by BD)
        tier.padding = 3;
        tier.scaleVertical = false; // set in default-renderer:427, used by feature-draw:paintToContext

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

exports.prepareViewport = function(tier) {
    return function(height) {
        return function () {
            tier.viewport.height = height;
            tier.layoutHeight = height;
            tier.updateHeight();

            var canvas = tier.viewport.getContext("2d");
            canvas.translate(1000, 0);
            tier.viewportHolder.style.left = '-1000px';

            tier.norigin = tier.browser.viewStart;
            tier.glyphCacheOrigin = tier.browser.viewStart;

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

exports.hCoordTransform = function(tier) {
    return function() {
        var hCT = { bpPerPixel: 1/tier.browser.scale,
                    viewStart: tier.browser.viewStart
                  };
        return hCT;
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
                var glyph = {};
                for (var f in g.position) {
                    glyph[f] = g.position[f];
                }
                glyph.feature = g.feature;
                tier.subtiers[0].glyphs.push(glyph);
            });
        };
    };
};
