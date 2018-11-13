"use strict";

exports.appendS = function(a) {
    return function(b) {
        return a + b;
    };
};
