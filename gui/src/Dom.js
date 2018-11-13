"use strict";

exports.alert = function(message) {
    return function() {
        alert(message);
    };
};

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

exports.errorMessage = function(error) {
    return error.message;
};

exports.eventTargetAddEventListener = function(eventTarget) {
    return function(eventType) {
        return function(callback) {
            return function() {
                eventTarget.addEventListener(eventType, function(event) {
                    callback(event)();
                });
            };
        };
    };
};

exports.newError = function(message) {
    return new Error(message);
};

exports.newXmlHttpRequest = function() {
    return new XMLHttpRequest();
};

exports.window = function() {
    return window;
};

exports.windowDocument = function(window) {
    return function() {
        return window.document;
    };
};

exports.xmlHttpRequestGetResponseTextF = function(Nothing) {
    return function(Just) {
        return function(xhr) {
            return function() {
                var text = xhr.responseText;
                return text === null ? Nothing : Just(text);
            };
        };
    };
};

exports.xmlHttpRequestOpen = function(xhr) {
    return function(method) {
        return function(url) {
            return function() {
                xhr.open(method, url);
            };
        };
    };
};

exports.xmlHttpRequestSend = function(xhr) {
    return function() {
        xhr.send();
    };
};
