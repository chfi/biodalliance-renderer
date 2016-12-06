"use strict";

exports.getTierCanvasContext = function(tier) {
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

exports.getFeatures = function(tier) {
    return tier.currentFeatures;
};

exports.getTierLength = function(tier) {
    return tier.knownCoverage.max;
};

exports.runEff = function(f) {
    f();
};
