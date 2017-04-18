/*eslint-env node*/
/*global firebase*/
'use strict';

// We don't actually use the app argument, but requiring it is a way
// to require that the app has been initialised.
exports.getAuth = function (appIgnored) {
    return function () {
        return firebase.auth();
    };
};

exports.signInAnonymously_ = function (auth) {
    return function () {
        return auth.signInAnonymously();
    };
};
