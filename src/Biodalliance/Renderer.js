"use strict";

exports.getTierCanvasContext = function(tier) {
    return tier.viewport.getContext("2d");
};

exports.setViewportHeight = function(tier) {
    return function(height) {
        tier.viewport.style.height = height + "px";
        tier.viewport.height = height;
        tier.viewport.layoutHeight = height;
        tier.viewport.updateHeight();
    };
};

exports.drawTierImpl = function(ctx) {
    return function(unparsedTier) {
        return function(tier) {
            return function(eff) {
                // ugly unwrap of parsed tier
                var t = tier.value0;

                // tier.viewport.style.width = 500;
                unparsedTier.viewport.height = 500;
                unparsedTier.viewport.style.height = "500px";
                // unparsedTier.updateHeight();
                unparsedTier.layoutHeight = 500;
                unparsedTier.updateHeight();
                // unparsedTier.norigin = unparsedTier.browser.viewStart;
                // ctx.canvas.width = 500;
                // ctx.canvas.height = 500;

                // ctx.clearRect(0, 0, 500, 500);

                console.log(unparsedTier.viewport.height);
                console.log("unsafe!!!");
                console.log(eff);
                eff();
            };
        };
    };
};
