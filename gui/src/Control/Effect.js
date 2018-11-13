"use strict";

exports.mapF = function(f) {
    return function(effect) {
        return function() {
            return f(effect());
        };
    };
};

exports.bindF = function(effect) {
    return function(k) {
        return function() {
            return k(effect())();
        };
    };
};

exports.applyF = function(effect1) {
    return function(effect2) {
        return function() {
            return effect1()(effect2());
        };
    };
};

exports.pureF = function(value) {
    return function() {
        return value;
    };
};

exports.throwF = function(exception) {
    return function() {
        throw exception;
    };
};

exports.ridF = function(action) {
    return function(handler) {
        return function() {
            try {
                return action();
            } catch (ex) {
                return handler(ex)();
            }
        };
    };
};
