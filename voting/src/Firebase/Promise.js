/*eslint-env node*/
/*global firebase*/
'use strict';

exports.andCatch = function (promise) {
    return function (callback) {
        return function () {
            promise.catch(function (err) {
                callback(err.message)();
            });
        };
    };
};

exports.andThen = function (promise) {
    return function (callback) {
        return function () {
            promise.then(function (v) {
                callback(v)();
            });
        };
    };
};
