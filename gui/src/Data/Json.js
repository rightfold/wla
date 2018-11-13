"use strict";

exports.parseF = function(Nothing) {
    return function(Just) {
        return function(text) {
            try {
                return Just(JSON.parse(text));
            } catch (ex) {
                return Nothing;
            };
        };
    };
};

exports.stringify = function(json) {
    return JSON.stringify(json);
};
