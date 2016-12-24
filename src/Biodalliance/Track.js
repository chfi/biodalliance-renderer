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
            tier.subtiers = glyphs;
        };
    };
};
