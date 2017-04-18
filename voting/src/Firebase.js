/*eslint-env node*/
/*global firebase*/
'use strict';

exports.initializeApp = function (config) {
    return function () {
        console.log('Config', config);
        return firebase.initializeApp(config);
    };
};

// We don't actually use the app argument, but requiring it is a way
// to require that the app has been initialised.
exports.getAuth = function (appIgnored) {
    return function () {
        return firebase.auth();
    };
};

// We don't actually use the app argument, but requiring it is a way
// to require that the app has been initialised.
exports.getDb = function (appIgnored) {
    return function () {
        return firebase.database();
    };
};

exports.getDbRef = function (name) {
    return function (db) {
        return db.ref(name);
    };
};

exports.getDbRefChild = function (name) {
    return function (dbRef) {
        return dbRef.child(name);
    };
};

exports.set_ = function (dbRef) {
    return function (value) {
        return function () {
            console.log("Sending to FireBase", dbRef.key, dbRef.toString(), value);
            return dbRef.set(value);
        };
    };
};

exports.on_ = function (dbRef) {
    return function (eventName) {
        return function (successCallback) {
            return function (errorsCallback) {
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
        };
    };
};

exports.getVal = function (snapshot) {
    return function () {
        return snapshot.val();
    };
};

exports.signInAnonymously_ = function (auth) {
    return function () {
        console.log('Signin auth', auth);
        return auth.signInAnonymously();
    };
};
