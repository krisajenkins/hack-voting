"use strict";

exports.initializeApp = function (config) {
    return function () {
        console.log("Config", config);
        return firebase.initializeApp(config);
    };
};

// We don't actually use the app argument, but requiring it is a way
// to require that the app has been initialised.
exports.getAuth = function (app) {
    return function () {
        return firebase.auth();
    };
};

// We don't actually use the app argument, but requiring it is a way
// to require that the app has been initialised.
exports.getDb = function (app) {
    return function () {
        return firebase.database();
    };
};

exports.getDbRef = function (db) {
    return function (name) {
        return db.ref(name);
    };
};

exports.getDbRefChild = function (dbRef) {
    return function (name) {
        return dbRef.child(name);
    };
};

exports.on_ = function (dbRef) {
    return function (eventName) {
        return function (successCallback) {
            return function (errorsCallback) {
                return function() {
                    console.log("Listening", dbRef, eventName, dbRef.toString());
                    dbRef.on(
                        eventName,
                        function (snapshot) {
                            console.log("Success", snapshot, snapshot.val());
                            return successCallback(snapshot)();
                        },
                        function (error) {
                            console.log("Error", error);
                            return errorCallback(error)();
                        }
                    );
                    return {};
                };
            };
        };
    };
};

exports.signInAnonymously_ = function (auth) {
    return function () {
        console.log("Signin auth", auth);
        return auth.signInAnonymously();
    };
};

exports.andThen = function (promise) {
    return function (callback) {
        return function () {
            promise.then(function (v) {
                var eff = callback(v);
                eff();
            });
        };
    };
};

exports.andCatch = function (promise) {
    return function (callback) {
        return function () {
            promise.catch(function (err) {
                console.log("Err", err);
                var eff = callback(err.message);
                eff();
            });
        };
    };
};
