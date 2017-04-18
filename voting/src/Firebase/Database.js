/*eslint-env node*/
/*global firebase*/
'use strict';

// We don't actually use the app argument, but requiring it is a way
// to require that the app has been initialised.
exports.getDb = function (appIgnored) {
    return function () {
        return firebase.database();
    };
};

exports.getDbRef_ = function (name, db) {
    return db.ref(name);
};

exports.getDbRefChild_ = function (name, dbRef) {
    return dbRef.child(name);
};

exports.set_ = function (dbRef, value) {
    return function () {
        return dbRef.set(value);
    };
};

exports.on_ = function (dbRef, eventName, successCallback, errorsCallback) {
    return function() {
        dbRef.on(
            eventName,
            function (snapshot) {
                return successCallback(snapshot)();
            },
            function (error) {
                return errorsCallback(error)();
            }
        );

        return {};
    };
};

exports.getVal = function (snapshot) {
    return function () {
        return snapshot.val();
    };
};
