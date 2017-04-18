/*eslint-env node*/
/*global firebase*/
'use strict';

exports.andCatch = function (promise, callback) {
    return function () {
        promise.catch(function (error) {
            callback(error)();
        });
    };
};

exports.andThen = function (promise, callback) {
    return function () {
        promise.then(function (value) {
            callback(value)();
        });
    };
};
