"use strict";

exports.foldlA = function(step) {
    return function(init) {
        return function(array) {
            var acc = init;
            for (var i = 0, n = array.length; i < n; ++i) {
                acc = step(acc)(array[i]);
            }
            return acc;
        };
    };
};

exports.foldrA = function(step) {
    return function(init) {
        return function(array) {
            var acc = init;
            for (var i = array.length - 1; i >= 0; --i) {
                acc = step(array[i])(acc);
            }
            return acc;
        };
    };
};
