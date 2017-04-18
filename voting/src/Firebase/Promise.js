/*eslint-env node*/
/*global firebase*/
'use strict';

exports.andCatch = function (promise, callback) {
    return function () {
        console.log("Registering catch callback.");
        promise.catch(function (err) {
            callback(err)();
        });
    };
};

exports.andThen = function (promise, callback) {
    return function () {
        promise.then(function (v) {
            callback(v)();
        });
    };
};
