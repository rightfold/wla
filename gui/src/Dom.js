"use strict";

exports.nodeAppendChild = function(parent) {
    return function(child) {
        return function() {
            parent.appendChild(child);
        };
    };
};

exports.nodeSetTextContent = function(node) {
    return function(text) {
        return function() {
            node.textContent = text;
        };
    };
};

exports.documentCreateElement = function(document) {
    return function(tagName) {
        return function() {
            return document.createElement(tagName);
        };
    };
};

exports.documentGetElementByIdF = function(Nothing) {
    return function(Just) {
        return function(document) {
            return function(id) {
                return function() {
                    var element = document.getElementById(id);
                    return element === null ? Nothing : Just(element);
                };
            };
        };
    };
};

exports.newError = function(message) {
    return new Error(message);
};

exports.window = function() {
    return window;
};

exports.windowDocument = function(window) {
    return function() {
        return window.document;
    };
};
