var auth0 = require('auth0-js');

exports.webAuth = function (config) {
  return new auth0.WebAuth(config);
};

exports.authorize = function (webAuth) {
  return function () {
    return webAuth.authorize();
  };
};

exports._parseHash = function (just) {
  return function (nothing) {
    return function (webAuth) {
      return function (onError, onSuccess) {
        var req = webAuth.parseHash(function (err, auth) {
          if (err) {
            return onError(err);
          }
          if (!auth) {
            return onSuccess(nothing);
          }
          return onSuccess(just(auth));
        });
        // Return a canceler, which is just another Aff effect.
        return function (cancelError, cancelerError, cancelerSuccess) {
          cancelerSuccess(); // invoke the success callback for the canceler
        };
      };
    };
  };
};
