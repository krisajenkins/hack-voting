/*eslint-env node*/
/*global firebase*/
'use strict';

exports.initializeApp = function (config) {
  return function () {
    return firebase.initializeApp(config);
  };
};
