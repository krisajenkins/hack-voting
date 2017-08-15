/*eslint-env node*/
/*global firebase*/
'use strict';

exports.getAuthService = function (app) {
  return app.auth();
};

exports.signInAnonymously_ = function (authService) {
  return function () {
    return authService.signInAnonymously();
  };
};

exports.signInWithPopup_ = function (providerInterface) {
  return function (popupInterface) {
    return function (authService) {
      return function (provider) {
        return function () {
          return authService.signInWithPopup(provider);
        };
      };
    };
  };
};

exports.makeGithubAuthProvider = function () {
  return new firebase.auth.GithubAuthProvider();
};
