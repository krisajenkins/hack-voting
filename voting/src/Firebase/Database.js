/*eslint-env node*/
/*global firebase*/
'use strict';

exports.getDb = function (app) {
  return firebase.database(app);
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
                return successCallback(snapshot.val())();
            },
            function (error) {
                return errorsCallback(error)();
            }
        );

        return {};
    };
};
