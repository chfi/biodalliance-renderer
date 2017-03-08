exports.parseFeatureImpl = function(Just) {
    return function(Nothing) {
        return function(feature) {
            if (feature.min && feature.max && feature["pValue"]) {
                return Just({min: feature.min,
                             max: feature.max,
                             score: feature["pValue"]
                            });
            } else {
                return Nothing;
            }
        };
    };
};
