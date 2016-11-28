"use strict";

exports.getTierCanvasContext = function(tier) {
    return tier.viewport.getContext("2d");
};
