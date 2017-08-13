/*eslint-env node*/
/*global firebase*/
'use strict';

exports.getAuth = function (app) {
    return firebase.auth(app);
};

exports.signInAnonymously_ = function (auth) {
    return function () {
        return auth.signInAnonymously();
    };
};
