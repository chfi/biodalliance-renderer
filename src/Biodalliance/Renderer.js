"use strict";

exports.tierCanvasContext = function(tier) {
    return tier.viewport.getContext("2d");
};

exports.setTierHeight = function(tier) {
    return function(height) {
        tier.viewport.height = height;
        tier.layoutHeight = height;
        tier.updateHeight();

        tier.norigin = tier.browser.viewStart;

        tier.originHaxx = 0;
        tier.browser.arrangeTiers();
    };
};

exports.getViewport = function(tier) {
    return tier.viewport;
};

exports.tierFeatures = function(tier) {
    return tier.currentFeatures;
};

exports.getTierLength = function(tier) {
    return tier.knownCoverage.max;
};

exports.tierScaleFactor = function(tier) {
    var sf = { bpPerPixel: 1/tier.browser.scale,
               viewStart: tier.browser.viewStart,
               canvasHeight: tier.viewport.height
           };
    console.log(sf);
    return sf;
};

exports.runEff = function(f) {
    f();
};
