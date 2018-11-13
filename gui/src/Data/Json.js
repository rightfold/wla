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

exports.getNull = function(Nothing) {
    return function(Just) {
        return function(json) {
            return json === null ? Just(null) : Nothing;
        };
    };
};

exports.setNull = function(value) {
    return null;
};

exports.getBoolean = function(Nothing) {
    return function(Just) {
        return function(json) {
            return typeof json === "boolean" ? Just(json) : Nothing;
        };
    };
};

exports.setBoolean = function(value) {
    return value;
};

exports.getNumber = function(Nothing) {
    return function(Just) {
        return function(json) {
            return typeof json === "number" ? Just(json) : Nothing;
        };
    };
};

exports.setNumber = function(value) {
    return value;
};

exports.getString = function(Nothing) {
    return function(Just) {
        return function(json) {
            return typeof json === "string" ? Just(json) : Nothing;
        };
    };
};

exports.setString = function(value) {
    return value;
};

exports.getArray = function(Nothing) {
    return function(Just) {
        return function(json) {
            return json instanceof Array ? Just(json) : Nothing;
        };
    };
};

exports.setArray = function(value) {
    return value;
};
