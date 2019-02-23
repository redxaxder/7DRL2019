"use strict";

var crypto = require('crypto');

exports._hash = function(algorithm) {
  return function (buffer) {
      return crypto.createHash(algorithm).update(buffer).digest();
    }
}
